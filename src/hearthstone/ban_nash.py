"""
ban_nash.py - Nash equilibrium calculator for Hearthstone tournament ban phase

This module computes optimal ban strategies for the pre-match ban phase
in competitive Hearthstone tournaments.

Ban Phase Rules:
---------------
1. Each player brings n decks to the match
2. Before the match begins, each player simultaneously bans k of their
   opponent's decks (where k is typically 1 or 2)
3. The banned decks cannot be used for the remainder of the match
4. The main match then proceeds with (n - k) decks per player
5. The match format (Conquest or LHS) determines how the match plays out

Strategic Considerations:
------------------------
The optimal ban strategy depends on:
- The winrate matrix W (how decks match up against each other)
- The number of bans k
- The match format (Conquest vs LHS have different optimal strategies)

The ban phase is itself a game: players simultaneously choose which decks
to ban, and the payoff is the expected match win probability after bans.

Algorithm:
----------
1. Generate all possible ban combinations for each player: C(n, k) each
2. For each pair of ban choices, compute the resulting match win probability
   using conquest_nash() or lhs_nash() on the reduced deck pool
3. Build the payoff matrix G[i,j] = P(Hero wins match | Hero bans i, Opp bans j)
4. Solve the ban-phase game using solve_game() to find optimal ban strategies
"""

import numpy as np
from itertools import combinations
from typing import List, Optional, Dict
from .solve_game import solve_game
from .conquest_nash import conquest_nash
from .lhs_nash import lhs_nash
from .results import BanResult


def ban_nash(W: np.ndarray, bans: int,
             hero_names: Optional[List[str]] = None,
             opp_names: Optional[List[str]] = None,
             match_format: str = 'conquest',
             _cache: Optional[Dict[bytes, tuple]] = None,
             winrate_only: bool = False) -> BanResult:
    """
    Find optimal ban strategy for a tournament match.

    This function analyzes the ban phase by computing:
    1. The payoff matrix over all ban combinations
    2. The Nash equilibrium mixed strategy for banning
    3. The expected match win probability after optimal banning

    Parameters
    ----------
    W : np.ndarray
        Square winrate matrix of shape (n, n) where:

        - n = number of decks per player (before bans)
        - W[i,j] = probability that Hero's deck i beats Opponent's deck j
        - Values should be between 0 and 1

    bans : int
        Number of decks each player bans (must be 0 < bans < n)

    match_format : str, optional
        The format for the main match after bans:

        - 'conquest': Conquest format (winner's deck eliminated)
        - 'lhs': Last Hero Standing format (loser's deck eliminated)

        Default is 'conquest'.

    deck_names : list of str, optional
        Names for each deck. Default: ['Deck 0', 'Deck 1', ...].

    Returns
    -------
    BanResult
        Result object with easy access to ban analysis:

        - ``winrate``: Hero's expected winrate after optimal banning
        - ``hero_ban_strategy``: Hero's optimal ban selection
        - ``opp_ban_strategy``: Opponent's optimal ban selection
        - ``get_match(hero_bans, opp_bans)``: Get match analysis for
          specific ban choices

    Raises
    ------
    ValueError
        If bans < 1 or bans >= n, or if match_format is invalid

    Examples
    --------
    >>> import numpy as np
    >>> # 4-deck match with 1 ban each
    >>> W = np.array([
    ...     [0.5, 0.6, 0.4, 0.5],
    ...     [0.4, 0.5, 0.5, 0.6],
    ...     [0.6, 0.5, 0.5, 0.4],
    ...     [0.5, 0.4, 0.6, 0.5]
    ... ])
    >>> result = ban_nash(W, bans=1, match_format='conquest',
    ...                   deck_names=['Aggro', 'Combo', 'Control', 'Midrange'])
    >>> print(f"Win probability: {result.winrate:.1%}")
    >>> print(result.hero_ban_strategy)

    >>> # Get detailed match analysis if Hero bans Combo, Opp bans Aggro
    >>> match = result.get_match(hero_bans=['Combo'], opp_bans=['Aggro'])
    >>> print(match)
    """
    # Validate inputs
    W = np.asarray(W, dtype=float)
    n = W.shape[0]

    if bans < 1:
        raise ValueError("Number of bans must be at least 1")
    if bans >= n:
        raise ValueError(f"Too many bans ({bans}) for {n} decks")

    # Default names
    if hero_names is None:
        hero_names = [f"Deck {i}" for i in range(n)]
    if opp_names is None:
        opp_names = [f"Deck {i}" for i in range(n)]

    # Validate names
    if len(hero_names) != n:
        raise ValueError(f"hero_names has {len(hero_names)} elements, expected {n}")
    if len(opp_names) != n:
        raise ValueError(f"opp_names has {len(opp_names)} elements, expected {n}")

    # Validate match format
    valid_formats = {'conquest', 'lhs'}
    if match_format.lower() not in valid_formats:
        raise ValueError(f"Unknown format '{match_format}'. Must be one of: {valid_formats}")
    match_format = match_format.lower()

    # Select the appropriate Nash calculator based on format
    if match_format == 'conquest':
        nash_fn = conquest_nash
    else:  # lhs
        nash_fn = lhs_nash


    # =========================================================================
    # STEP 1: GENERATE ALL POSSIBLE BAN COMBINATIONS
    # =========================================================================
    # Each combination is a tuple of deck indices to ban.
    # hero_ban_options: tuples of opponent deck indices that Hero can ban
    # opp_ban_options: tuples of hero deck indices that Opponent can ban
    # =========================================================================
    hero_ban_options = list(combinations(range(n), bans))
    opp_ban_options = list(combinations(range(n), bans))

    num_hero_options = len(hero_ban_options)
    num_opp_options = len(opp_ban_options)

    # =========================================================================
    # STEP 2: BUILD PAYOFF MATRIX FOR BAN PHASE
    # =========================================================================
    # G[i,j] = P(Hero wins match) when:
    #   - Hero bans hero_ban_options[i] (opponent's decks)
    #   - Opponent bans opp_ban_options[j] (Hero's decks)
    #
    # For each combination, we:
    # 1. Compute remaining decks after bans
    # 2. Create the reduced winrate matrix
    # 3. Pass the correct deck NAMES to the match solver
    # 4. Store the ConquestResult/LHSResult for later retrieval
    # =========================================================================

    # Payoff matrix for the ban phase game
    G = np.zeros((num_hero_options, num_opp_options))

    # Store full match results for reference
    matches = [[None for _ in range(num_opp_options)] for _ in range(num_hero_options)]

    for i, hero_bans_idx in enumerate(hero_ban_options):
        for j, opp_bans_idx in enumerate(opp_ban_options):
            # hero_bans_idx: tuple of opponent deck indices that Hero bans
            # opp_bans_idx: tuple of hero deck indices that Opponent bans

            # Get the remaining deck indices after bans
            # Hero's remaining decks (after opponent banned some)
            hero_decks_remaining = [r for r in range(n) if r not in opp_bans_idx]
            # Opponent's remaining decks (after hero banned some)
            opp_decks_remaining = [c for c in range(n) if c not in hero_bans_idx]

            # Get the NAMES for remaining decks - this preserves identity through index shifting
            hero_remaining_names = [hero_names[r] for r in hero_decks_remaining]
            opp_remaining_names = [opp_names[c] for c in opp_decks_remaining]

            # Extract the reduced winrate matrix
            # W_reduced[i', j'] = W[hero_decks_remaining[i'], opp_decks_remaining[j']]
            W_reduced = W[np.ix_(hero_decks_remaining, opp_decks_remaining)]

            # Run the match analysis on the reduced matrix WITH CORRECT NAMES
            # Note: only pass cache for conquest - lhs_nash builds internal payoff
            # matrices that can collide with cached W submatrices
            cache_to_use = _cache if match_format == 'conquest' else None
            match_result = nash_fn(W_reduced, hero_remaining_names, opp_remaining_names,
                                   _cache=cache_to_use, winrate_only=winrate_only)

            # Store the full match result for later retrieval
            matches[i][j] = match_result

            # Get the overall match probability from the result object
            G[i, j] = match_result.winrate

    # =========================================================================
    # STEP 3: SOLVE THE BAN PHASE GAME
    # =========================================================================
    # Create names for the ban strategies (for the ban game matrix)
    hero_ban_names = [f"Ban {tuple(opp_names[i] for i in idx)}" for idx in hero_ban_options]
    opp_ban_names = [f"Ban {tuple(hero_names[i] for i in idx)}" for idx in opp_ban_options]

    solution = solve_game(G, hero_ban_names, opp_ban_names)

    return BanResult(
        solution=solution,
        hero_names=hero_names,
        opp_names=opp_names,
        match_format=match_format,
        matches=matches,
        hero_ban_options=hero_ban_options,
        opp_ban_options=opp_ban_options
    )

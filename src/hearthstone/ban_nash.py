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
from typing import List, Optional
from .solve_game import _solve_game_internal
from .conquest_nash import _conquest_nash_internal
from .lhs_nash import _lhs_nash_internal
from .results import BanResult


def ban_nash(W: np.ndarray, bans: int, match_format: str = 'conquest',
             deck_names: Optional[List[str]] = None) -> BanResult:
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

    # Default deck names
    if deck_names is None:
        deck_names = [f"Deck {i}" for i in range(n)]

    # Validate deck names
    if len(deck_names) != n:
        raise ValueError(f"deck_names has {len(deck_names)} elements, expected {n}")

    # Validate match format
    valid_formats = {'conquest', 'lhs'}
    if match_format.lower() not in valid_formats:
        raise ValueError(f"Unknown format '{match_format}'. Must be one of: {valid_formats}")
    match_format = match_format.lower()

    # Select the appropriate Nash calculator based on format
    if match_format == 'conquest':
        nash_fn = _conquest_nash_internal
    else:  # lhs
        nash_fn = _lhs_nash_internal


    # Generate all C(n, bans) combinations for each player
    # Each combination is a tuple of deck indices to ban
    hero_ban_options = list(combinations(range(n), bans))
    opp_ban_options = list(combinations(range(n), bans))

    num_hero_options = len(hero_ban_options)
    num_opp_options = len(opp_ban_options)

    # =========================================================================
    # STEP 2: BUILD PAYOFF MATRIX FOR BAN PHASE AND SOLVE THE GAME
    # =========================================================================
    # G[i,j] = P(Hero wins match) when:
    #   - Hero bans hero_ban_options[i] (these decks from Opponent)
    #   - Opponent bans opp_ban_options[j] (these decks from Hero)
    #
    # To compute this, we:
    # 1. Remove banned rows (Hero's banned decks) and columns (Opp's banned decks)
    # 2. Run the match analysis (conquest_nash or lhs_nash) on reduced W
    # 3. Extract the initial state winrate
    # =========================================================================

    # Payoff matrix for the ban phase game
    G = np.zeros((num_hero_options, num_opp_options))

    # Store full match results for reference
    matches = [[None for _ in range(num_opp_options)] for _ in range(num_hero_options)]

    for i, hero_bans in enumerate(hero_ban_options):
        for j, opp_bans in enumerate(opp_ban_options):
            # hero_bans: tuple of opponent deck indices that Hero bans (columns to remove)
            # opp_bans: tuple of hero deck indices that Opponent bans (rows to remove)

            # Create the reduced winrate matrix after bans
            # Remove rows corresponding to opp_bans (Hero's banned decks)
            # Remove columns corresponding to hero_bans (Opponent's banned decks)
            # Note: We need to delete in a way that handles index shifting

            # Get the rows and columns to keep
            hero_decks_remaining = [r for r in range(n) if r not in opp_bans]
            opp_decks_remaining = [c for c in range(n) if c not in hero_bans]

            # Extract the reduced matrix
            # W_reduced[i', j'] = W[hero_decks_remaining[i'], opp_decks_remaining[j']]
            W_reduced = W[np.ix_(hero_decks_remaining, opp_decks_remaining)]

            # Run the match analysis on the reduced matrix
            match_result = nash_fn(W_reduced)

            # Store the full match result for later retrieval
            matches[i][j] = match_result

            # The last element contains the initial state (empty score)
            # Its winrate gives us the overall match probability
            initial_state = match_result[-1]
            hero_winrate = initial_state['winrate'][0]

            G[i, j] = hero_winrate

    solution = _solve_game_internal(G)

    hero_ban_strategy = solution['hero_sol']
    opp_ban_strategy = solution['opp_sol']
    overall_winrate = solution['V']

    raw_result = {
        'bans': {
            'hero': hero_ban_strategy,
            'opp': opp_ban_strategy
        },
        'winrate': (overall_winrate, 1.0 - overall_winrate)
    }

    return BanResult(
        raw_result=raw_result,
        deck_names=deck_names,
        match_format=match_format,
        matches=matches,
        stratlist_hero=hero_ban_options,
        stratlist_opp=opp_ban_options
    )

"""
lineup_picker.py - Nash equilibrium calculator for lineup selection

This module computes optimal lineup selection strategies for tournaments
where players must choose a fixed-size lineup from a larger pool of decks.

Lineup Selection Rules:
----------------------
1. Both players have access to the same pool of n available decks
2. Each player simultaneously selects k decks to form their lineup
3. After lineups are chosen, the match proceeds with the specified format
   (conquest or lhs) and ban rules
4. The winner is determined by the resulting match

Strategic Considerations:
------------------------
The optimal lineup depends on:
- The winrate matrix W (how decks match up against each other)
- The lineup size k
- The number of bans in the subsequent match
- The match format (conquest vs lhs)

Algorithm:
----------
1. Generate all C(n, k) lineup combinations
2. For each pair of lineup choices, extract the submatrix and compute
   the match winrate using ban_nash()
3. Build the payoff matrix G[i,j] = P(Hero wins | Hero picks lineup i, Opp picks lineup j)
4. Solve the lineup selection game using solve_game()
"""

import numpy as np
from itertools import combinations
from typing import List, Optional, Union
from concurrent.futures import ProcessPoolExecutor
import os
from .solve_game import solve_game
from .ban_nash import ban_nash
from .results import LineupResult


def _compute_match(args):
    """
    Worker function for parallel computation of a single lineup matchup.

    Must be a top-level function to be picklable for multiprocessing.
    """
    i, j, W_sub, hero_names, opp_names, bans, match_format = args

    ban_result = ban_nash(
        W_sub,
        bans=bans,
        hero_names=hero_names,
        opp_names=opp_names,
        match_format=match_format
    )

    return i, j, ban_result


def lineup_picker(W: np.ndarray,
                  lineup_size: int,
                  bans: int,
                  deck_names: Optional[List[str]] = None,
                  match_format: str = 'conquest',
                  parallel: Union[bool, int] = False,
                  symmetric: bool = False) -> LineupResult:
    """
    Find optimal lineup selection strategy for a tournament.

    This function analyzes the lineup selection phase by computing:
    1. The payoff matrix over all lineup combinations
    2. The Nash equilibrium mixed strategy for lineup selection
    3. The expected match win probability after optimal selection

    Parameters
    ----------
    W : np.ndarray
        Square winrate matrix of shape (n, n) where:

        - n = size of the deck pool (available decks)
        - W[i,j] = probability that deck i beats deck j
        - Values should be between 0 and 1

    lineup_size : int
        Number of decks each player selects for their lineup.
        Must satisfy: bans < lineup_size <= n

    bans : int
        Number of decks banned in the subsequent match.
        Must satisfy: 0 < bans < lineup_size

    deck_names : list of str, optional
        Names for each deck in the pool. Default: ['Deck 0', 'Deck 1', ...].

    match_format : str, optional
        The format for the match after lineup selection:

        - 'conquest': Conquest format (winner's deck eliminated)
        - 'lhs': Last Hero Standing format (loser's deck eliminated)

        Default is 'conquest'.

    parallel : bool or int, optional
        Enable parallel computation of lineup matchups:

        - False: Sequential computation (default)
        - True: Use all available CPU cores
        - int: Use specified number of worker processes

        Parallelization provides significant speedup for large problems
        (e.g., 7+ deck pools). For small problems, the overhead may
        outweigh the benefits.

    symmetric : bool, optional
        If True, exploit the symmetry property where W[i,j] + W[j,i] = 1
        (i.e., if deck A beats deck B with probability p, then deck B
        beats deck A with probability 1-p). This reduces computation
        by ~50% since G[i,j] + G[j,i] = 1 for the payoff matrix.

        Default is False. Set to True when your winrate matrix has
        this symmetry property (common in practice).

    Returns
    -------
    LineupResult
        Result object with easy access to lineup analysis:

        - ``winrate``: Hero's expected winrate after optimal lineup selection
        - ``hero_lineup_strategy``: Hero's optimal lineup choices
        - ``opp_lineup_strategy``: Opponent's optimal lineup choices
        - ``get_match(hero_lineup, opp_lineup)``: Get ban phase analysis for
          specific lineup choices

    Raises
    ------
    ValueError
        If lineup_size or bans are invalid for the given pool size.

    Examples
    --------
    >>> import numpy as np
    >>> # Pool of 6 decks, pick 4 for lineup, 1 ban
    >>> W = np.random.uniform(0.3, 0.7, (6, 6))
    >>> result = lineup_picker(W, lineup_size=4, bans=1,
    ...                        deck_names=['A', 'B', 'C', 'D', 'E', 'F'],
    ...                        match_format='conquest')
    >>> print(f"Win probability: {result.winrate:.1%}")

    >>> # Get match analysis for specific lineups
    >>> match = result.get_match(hero_lineup=['A', 'B', 'C', 'D'],
    ...                          opp_lineup=['C', 'D', 'E', 'F'])
    >>> print(match.winrate)

    >>> # Use parallel computation for faster results
    >>> result = lineup_picker(W, lineup_size=4, bans=1, parallel=True)

    >>> # Use symmetry optimization (when W[i,j] + W[j,i] = 1)
    >>> result = lineup_picker(W, lineup_size=4, bans=1, symmetric=True)

    >>> # Combine both optimizations
    >>> result = lineup_picker(W, lineup_size=4, bans=1, parallel=True, symmetric=True)
    """
    # Validate inputs
    W = np.asarray(W, dtype=float)
    n = W.shape[0]

    if lineup_size < 2:
        raise ValueError("Lineup size must be at least 2")
    if lineup_size > n:
        raise ValueError(f"Lineup size ({lineup_size}) cannot exceed pool size ({n})")
    if bans < 1:
        raise ValueError("Number of bans must be at least 1")
    if bans >= lineup_size:
        raise ValueError(f"Too many bans ({bans}) for lineup size ({lineup_size})")

    # Default names
    if deck_names is None:
        deck_names = [f"Deck {i}" for i in range(n)]

    # Validate names
    if len(deck_names) != n:
        raise ValueError(f"deck_names has {len(deck_names)} elements, expected {n}")

    # Validate match format
    valid_formats = {'conquest', 'lhs'}
    if match_format.lower() not in valid_formats:
        raise ValueError(f"Unknown format '{match_format}'. Must be one of: {valid_formats}")
    match_format = match_format.lower()

    # =========================================================================
    # STEP 1: GENERATE ALL POSSIBLE LINEUP COMBINATIONS
    # =========================================================================
    # Both players choose from the same pool, so lineup options are the same
    # for hero and opponent.
    # =========================================================================
    lineup_options = list(combinations(range(n), lineup_size))
    num_lineups = len(lineup_options)

    # =========================================================================
    # STEP 2: BUILD PAYOFF MATRIX FOR LINEUP SELECTION
    # =========================================================================
    # G[i,j] = P(Hero wins match) when:
    #   - Hero picks lineup_options[i]
    #   - Opponent picks lineup_options[j]
    #
    # For each combination, we:
    # 1. Extract the submatrix for the chosen decks
    # 2. Run ban_nash on this submatrix
    # 3. Store the BanResult for later retrieval
    # =========================================================================

    G = np.zeros((num_lineups, num_lineups))
    matches = [[None for _ in range(num_lineups)] for _ in range(num_lineups)]

    # Prepare tasks - only upper triangle if symmetric
    tasks = []
    for i, hero_lineup_idx in enumerate(lineup_options):
        for j, opp_lineup_idx in enumerate(lineup_options):
            # Skip lower triangle if symmetric (will use G[i,j] = 1 - G[j,i])
            if symmetric and i > j:
                continue

            hero_lineup_names = [deck_names[k] for k in hero_lineup_idx]
            opp_lineup_names = [deck_names[k] for k in opp_lineup_idx]
            W_sub = W[np.ix_(list(hero_lineup_idx), list(opp_lineup_idx))]
            tasks.append((i, j, W_sub, hero_lineup_names, opp_lineup_names, bans, match_format))

    # Execute tasks (parallel or sequential)
    if parallel:
        # Determine number of workers
        if parallel is True:
            max_workers = os.cpu_count()
        else:
            max_workers = int(parallel)

        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            results = list(executor.map(_compute_match, tasks))

        for i, j, ban_result in results:
            matches[i][j] = ban_result
            G[i, j] = ban_result.winrate
    else:
        # Sequential execution
        for task in tasks:
            i, j, ban_result = _compute_match(task)
            matches[i][j] = ban_result
            G[i, j] = ban_result.winrate

    # Fill lower triangle using symmetry: G[i,j] = 1 - G[j,i]
    if symmetric:
        for i in range(num_lineups):
            for j in range(i):
                G[i, j] = 1 - G[j, i]
                # matches[i][j] stays None - computed on-demand in get_match()

    # =========================================================================
    # STEP 3: SOLVE THE LINEUP SELECTION GAME
    # =========================================================================
    # Create names for the lineup strategies
    lineup_names = [f"[{', '.join(deck_names[k] for k in idx)}]" for idx in lineup_options]

    solution = solve_game(G, lineup_names, lineup_names)

    return LineupResult(
        solution=solution,
        deck_names=deck_names,
        lineup_size=lineup_size,
        bans=bans,
        match_format=match_format,
        matches=matches,
        lineup_options=lineup_options,
        W=W if symmetric else None,
        symmetric=symmetric
    )

"""
conquest_nash.py - Nash equilibrium calculator for Hearthstone Conquest format

This module computes subgame-perfect Nash equilibria for the Conquest tournament
format used in competitive Hearthstone.

Algorithm:
----------
1. Generate all possible game states (combinations of eliminated decks)
2. Sort states by "depth" (number of games played) in decreasing order
3. For each state, compute:
   - Terminal states: deterministic payoffs (1 or 0)
   - Near-terminal states: product of remaining win probabilities
   - Other states: solve the deck selection game using LP
4. Store Nash equilibrium strategies and values for each subgame

State Representation:
--------------------
A state is represented by (hero_won, opp_won) where:
- hero_won: frozenset of deck indices that Hero has "won with" (eliminated)
- opp_won: frozenset of deck indices that Opponent has eliminated

Note: In Conquest, "won with" means the deck won a game and was eliminated.
The match winner is the first to eliminate ALL their decks.
"""

import numpy as np
from itertools import combinations
from typing import List, Optional, Tuple, Dict
from .solve_game import solve_game
from .results import ConquestResult, ConquestStateSolution, GameSolution


def conquest_nash(W: np.ndarray,
                  hero_names: Optional[List[str]] = None,
                  opp_names: Optional[List[str]] = None,
                  _cache: Optional[Dict[bytes, tuple]] = None) -> ConquestResult:
    """
    Find Nash equilibrium for all subgames in a Conquest match.

    This function analyzes a Conquest match by computing optimal play
    (Nash equilibrium in mixed strategies) for every possible game state.
    The result allows determining:

    - Overall match win probability from the start
    - Optimal deck selection probabilities at any point in the match
    - Win probabilities from any game state

    Parameters
    ----------
    W : np.ndarray
        Square winrate matrix of shape (n, n) where:

        - n = number of decks per player
        - W[i,j] = probability that Hero's deck i beats Opponent's deck j
        - Values should be between 0 and 1

    deck_names : list of str, optional
        Names for each deck. Default: ['Deck 0', 'Deck 1', ...].

    Returns
    -------
    ConquestResult
        Result object with easy access to match analysis:

        - ``winrate``: Hero's match winrate from initial state
        - ``hero_strategy``: Hero's optimal initial deck selection
        - ``opp_strategy``: Opponent's optimal initial deck selection
        - ``get_state(hero_won, opp_won)``: Get any mid-match state
        - ``all_states()``: Get all states for advanced analysis

    Examples
    --------
    >>> import numpy as np
    >>> # Simple 2-deck match with equal matchups
    >>> W = np.array([[0.5, 0.5], [0.5, 0.5]])
    >>> result = conquest_nash(W, deck_names=['Aggro', 'Control'])
    >>> print(f"Match winrate: {result.winrate:.1%}")
    Match winrate: 50.0%

    >>> # View optimal strategy
    >>> print(result.hero_strategy)
    [('Aggro', 0.5), ('Control', 0.5)]

    >>> # Check mid-match state after Hero won with Aggro
    >>> state = result.get_state(hero_won=['Aggro'])
    >>> print(f"Winrate from this state: {state.winrate:.1%}")
    """
    W = np.asarray(W, dtype=float)
    n = W.shape[0]


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

    # =========================================================================
    # STEP 1: GENERATE ALL POSSIBLE GAME STATES
    # =========================================================================
    # A game state is defined by which decks each player has "won with"
    # (i.e., eliminated). We generate all combinations of eliminated decks.
    #
    # For player P with k eliminated decks, there are C(n, k) possibilities.
    # We enumerate all pairs (hero_won, opp_won) excluding the impossible
    # state where both players have won with all decks (match would be over).
    # =========================================================================

    # Generate all subsets of deck indices (from empty set to full set)
    # For a player with n decks: {}, {0}, {1}, ..., {0,1,...,n-1}
    all_subsets = []
    for k in range(n + 1):  # k = 0, 1, ..., n
        for combo in combinations(range(n), k):
            all_subsets.append(frozenset(combo))

    # Generate all pairs of (hero_won, opp_won)
    # Exclude the state where both have won with all decks (impossible to reach)
    all_states = []
    for hero_won in all_subsets:
        for opp_won in all_subsets:
            # Skip if both players have won with all decks (match is over twice?)
            if len(hero_won) == n and len(opp_won) == n:
                continue
            all_states.append((hero_won, opp_won))

    # =========================================================================
    # STEP 2: SORT STATES FOR BACKWARD INDUCTION
    # =========================================================================
    # We need to process states from "deepest" (most games played) to "shallowest"
    # (initial state) so that when we compute a state's value, all successor
    # states have already been computed.
    #
    # Depth = total number of eliminated decks = len(hero_won) + len(opp_won)
    # =========================================================================

    # Sort by depth in decreasing order (deepest first)
    all_states.sort(key=lambda s: len(s[0]) + len(s[1]), reverse=True)

    # =========================================================================
    # STEP 3: CREATE LOOKUP TABLE FOR EFFICIENT STATE ACCESS
    # =========================================================================
    # We need O(1) lookup to find previously computed values when calculating
    # payoffs for deck selection. Use a dictionary with (hero_won, opp_won) keys.
    # =========================================================================

    # Dictionary to store computed results: state -> result dict
    state_winrates = {}
    results_list = []

    # =========================================================================
    # STEP 4: PROCESS EACH STATE (BACKWARD INDUCTION)
    # =========================================================================
    # For each state, determine if it's terminal or requires solving an LP.
    #
    # Terminal states:
    # - Hero has won with all n decks: Hero wins (V=1)
    # - Opponent has won with all n decks: Opponent wins (V=0)
    #
    # Near-terminal states (one player has n-1 wins):
    # - Only one deck remains for one player
    # - Outcome is deterministic (product of remaining matchup probabilities)
    #
    # Other states:
    # - Build payoff matrix for deck selection game
    # - Solve using Nash equilibrium (LP)
    # =========================================================================

    hero_index_to_name = {i: name for i, name in enumerate(hero_names)}
    opp_index_to_name = {i: name for i, name in enumerate(opp_names)}

    for hero_won, opp_won in all_states:
        # Number of decks each player has eliminated
        hero_wins = len(hero_won)
        opp_wins = len(opp_won)

        # Remaining decks for each player
        hero_remaining = [i for i in range(n) if i not in hero_won]
        opp_remaining = [j for j in range(n) if j not in opp_won]

        # -----------------------------------------------------------------
        # CASE 1: Hero has won with all decks (Hero wins the match)
        # -----------------------------------------------------------------
        if hero_wins == n:
            V = 1.0
            solution = GameSolution(
                value=V,
                hero_names=[],
                opp_names=[opp_index_to_name[i] for i in opp_remaining],
                hero_strategy=[],
                opp_strategy=[(opp_index_to_name[i], 1.0/len(opp_remaining)) for i in opp_remaining] if opp_remaining else []
            )

        # -----------------------------------------------------------------
        # CASE 2: Opponent has won with all decks (Opponent wins the match)
        # -----------------------------------------------------------------
        elif opp_wins == n:
            V = 0.0
            solution = GameSolution(
                value=V,
                hero_names=[hero_index_to_name[i] for i in hero_remaining],
                opp_names=[],
                hero_strategy=[(hero_index_to_name[i], 1.0/len(hero_remaining)) for i in hero_remaining] if hero_remaining else [],
                opp_strategy=[]
            )

        # -----------------------------------------------------------------
        # CASE 3: Hero has n-1 wins (needs to win one more game to win match)
        # -----------------------------------------------------------------
        # Hero has one deck left, opponent has (n - opp_wins) decks left.
        # Hero wins if they beat ALL remaining opponent decks
        # -----------------------------------------------------------------
        
        elif hero_wins == n - 1:
            V = 1 - np.prod(1 - W[hero_remaining[0], opp_remaining])
            G_hero_names = [hero_index_to_name[i] for i in hero_remaining]
            G_opp_names = [opp_index_to_name[i] for i in opp_remaining]
            solution = GameSolution(
                value=V,
                hero_names=G_hero_names,
                opp_names=G_opp_names,
                hero_strategy=[(G_hero_names[0], 1.0)],
                opp_strategy=[(name, 1.0/len(G_opp_names)) for name in G_opp_names]
            )

        # -----------------------------------------------------------------
        # CASE 4: Opponent has n-1 wins (needs to win one more to win match)
        # -----------------------------------------------------------------
        # Symmetric to Case 3
        # -----------------------------------------------------------------
        elif opp_wins == n - 1:
            V = np.prod(W[hero_remaining, opp_remaining[0]])
            G_hero_names = [hero_index_to_name[i] for i in hero_remaining]
            G_opp_names = [opp_index_to_name[i] for i in opp_remaining]
            solution = GameSolution(
                value=V,
                hero_names=G_hero_names,
                opp_names=G_opp_names,
                hero_strategy=[(name, 1.0/len(G_hero_names)) for name in G_hero_names],
                opp_strategy=[(G_opp_names[0], 1.0)]
            )

        # -----------------------------------------------------------------
        # CASE 5: General case (need to solve deck selection game)
        # -----------------------------------------------------------------
        # Both players have choices for which deck to play.
        # Build payoff matrix G where G[i,j] = expected value for Hero
        # if Hero plays deck i and Opponent plays deck j.
        #
        # G[i,j] = W[i,j] * V(win_state) + (1-W[i,j]) * V(lose_state)
        #
        # where:
        # - win_state = state if Hero wins (hero's deck i eliminated)
        # - lose_state = state if Hero loses (opp's deck j eliminated)
        # -----------------------------------------------------------------
        else:

            num_hero_decks = len(hero_remaining)
            num_opp_decks = len(opp_remaining)


            # Build payoff matrix G for deck selection game
            # Rows = Hero's deck choices
            # Columns = Opponent's deck choices
            G = np.zeros((num_hero_decks, num_opp_decks))

            G_hero_names = [hero_index_to_name[i] for i in hero_remaining]
            G_opp_names = [opp_index_to_name[i] for i in opp_remaining]

            for idx_h, h in enumerate(hero_remaining):
                for idx_o, o in enumerate(opp_remaining):
                    # If Hero wins (deck h beats deck o):
                    # Hero's deck h is eliminated (added to hero_won)
                    # Opponent's deck o remains available
                    win_state = (frozenset(hero_won | {h}), opp_won)
                    win_value = state_winrates[win_state]

                    # If Hero loses:
                    # Hero's deck h remains available
                    # Opponent's deck o is eliminated (added to opp_won)
                    lose_state = (hero_won, frozenset(opp_won | {o}))
                    lose_value = state_winrates[lose_state]

                    # Expected value = P(win) * V(win) + P(lose) * V(lose)
                    G[idx_h, idx_o] = W[h, o] * win_value + (1 - W[h, o]) * lose_value

            # Solve the deck selection game to find Nash equilibrium
            solution = solve_game(G, G_hero_names, G_opp_names, _cache=_cache)

            V = solution.value

        # Store result for lookup by future states
        state_winrates[(hero_won, opp_won)] = V

        new_state = ConquestStateSolution(
            hero_names=hero_names,
            opp_names=opp_names,
            hero_won=hero_won,
            opp_won=opp_won,
            solution=solution
        )
        results_list.append(new_state)

    return ConquestResult(results_list, hero_names, opp_names)

"""
conquest_nash.py - Nash equilibrium calculator for Hearthstone Conquest format

This module computes subgame-perfect Nash equilibria for the Conquest tournament
format used in competitive Hearthstone.

Conquest Format Rules:
----------------------
1. Each player brings n decks to the match
2. Before each game, both players simultaneously choose which deck to play
3. The winner's deck is "eliminated" (cannot be used again)
4. The loser keeps their deck and can play it in future games
5. The first player to eliminate all their decks wins the match

Key Insight:
-----------
In Conquest, each game is a subgame that depends on which decks have been
eliminated so far. We use backward induction to solve:
1. Start from terminal states (one player has eliminated all decks)
2. Work backwards, computing Nash equilibria for each possible game state
3. The payoff for a non-terminal state depends on the win/loss outcomes,
   which lead to previously-computed subgame values

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
from .solve_game import solve_game


def conquest_nash(W: np.ndarray) -> list:
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

    Returns
    -------
    list
        A list of dictionaries, one for each possible game state.
        Each dictionary contains:
        - 'score': tuple of (hero_won, opp_won) as tuples of deck indices
            hero_won = decks Hero has eliminated (won with)
            opp_won = decks Opponent has eliminated
        - 'winrate': tuple (hero_wr, opp_wr)
            Expected win probabilities for the match from this state
        - 'nash': tuple (hero_strategy, opp_strategy) [for non-terminal states]
            Optimal mixed strategies for deck selection
        - 'game': np.ndarray [for non-terminal states]
            Payoff matrix for the deck selection subgame

        The list is ordered from deepest states (most games played) to
        the initial state (no games played). The last element is the
        initial state with the overall match analysis.

    Examples
    --------
    >>> import numpy as np
    >>> # Simple 2-deck match with equal matchups
    >>> W = np.array([[0.5, 0.5], [0.5, 0.5]])
    >>> result = conquest_nash(W)
    >>> initial_state = result[-1]  # Last element is initial state
    >>> print(f"Match winrate: {initial_state['winrate'][0]:.4f}")
    Match winrate: 0.5000

    >>> # Asymmetric matchups
    >>> W = np.array([[0.6, 0.4], [0.4, 0.6]])
    >>> result = conquest_nash(W)
    >>> print(f"Optimal deck selection: {result[-1]['nash'][0]}")

    Notes
    -----
    The algorithm complexity is O(3^n * n^3):
    - There are 3^n possible game states (each deck can be: not played,
      won by hero, or won by opponent)
    - Each non-terminal state requires solving an LP of size up to n x n

    For typical Hearthstone tournaments (n=3 or n=4), this is very fast.
    """
    # Convert input to numpy array and validate
    W = np.asarray(W, dtype=float)
    n = W.shape[0]  # Number of decks per player

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
    state_results = {}

    # List to accumulate results (for return value, preserves order)
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

    for hero_won, opp_won in all_states:
        # Number of decks each player has eliminated
        hero_wins = len(hero_won)
        opp_wins = len(opp_won)

        # Initialize result for this state
        result = {
            'score': (tuple(sorted(hero_won)), tuple(sorted(opp_won)))
        }

        # -----------------------------------------------------------------
        # CASE 1: Hero has won with all decks (Hero wins the match)
        # -----------------------------------------------------------------
        if hero_wins == n:
            V = 1.0
            V_opp = 0.0
            result['winrate'] = (V, V_opp)

        # -----------------------------------------------------------------
        # CASE 2: Opponent has won with all decks (Opponent wins the match)
        # -----------------------------------------------------------------
        elif opp_wins == n:
            V = 0.0
            V_opp = 1.0
            result['winrate'] = (V, V_opp)

        # -----------------------------------------------------------------
        # CASE 3: Hero has n-1 wins (needs to win one more game to win match)
        # -----------------------------------------------------------------
        # Hero has one deck left, opponent has (n - opp_wins) decks left.
        # Hero wins if they beat ALL remaining opponent decks.
        # This is because opponent can keep using their decks until one wins.
        #
        # P(Hero wins) = 1 - P(Hero loses all remaining games)
        #              = 1 - product of (1 - W[remaining_hero, remaining_opp])
        # -----------------------------------------------------------------
        elif hero_wins == n - 1:
            # Remaining decks for each player
            hero_remaining = [i for i in range(n) if i not in hero_won]
            opp_remaining = [j for j in range(n) if j not in opp_won]

            # Hero has exactly one deck left (hero_remaining has 1 element)
            # Hero needs to beat all opponent's remaining decks
            # P(lose to all) = product of (1 - W[hero_deck, opp_deck])
            prob_lose_all = 1.0
            for h in hero_remaining:
                for o in opp_remaining:
                    prob_lose_all *= (1 - W[h, o])

            V = 1.0 - prob_lose_all
            V_opp = 1.0 - V
            result['winrate'] = (V, V_opp)

        # -----------------------------------------------------------------
        # CASE 4: Opponent has n-1 wins (needs to win one more to win match)
        # -----------------------------------------------------------------
        # Symmetric to Case 3. Hero wins if they win ALL remaining games.
        # P(Hero wins) = product of W[remaining_hero, remaining_opp]
        # -----------------------------------------------------------------
        elif opp_wins == n - 1:
            # Remaining decks for each player
            hero_remaining = [i for i in range(n) if i not in hero_won]
            opp_remaining = [j for j in range(n) if j not in opp_won]

            # Opponent has exactly one deck left
            # Hero needs to beat that deck with all their remaining decks
            prob_win_all = 1.0
            for h in hero_remaining:
                for o in opp_remaining:
                    prob_win_all *= W[h, o]

            V = prob_win_all
            V_opp = 1.0 - V
            result['winrate'] = (V, V_opp)

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
        # - win_state = state if Hero wins (opponent's deck j eliminated)
        # - lose_state = state if Hero loses (hero's deck i continues,
        #                but since Opp won, Hero must beat their deck next time)
        #
        # Wait, let me reconsider Conquest rules:
        # - Winner's deck is eliminated
        # - So if Hero wins with deck i against opp's deck j:
        #   - Hero's deck i is eliminated (hero_won + {i})
        #   - Opp's deck j stays (opp can reuse it)
        # - If Hero loses:
        #   - Hero's deck i stays
        #   - Opp's deck j is eliminated (opp_won + {j})
        # -----------------------------------------------------------------
        else:
            # Available decks for each player (not yet eliminated)
            hero_available = [i for i in range(n) if i not in hero_won]
            opp_available = [j for j in range(n) if j not in opp_won]

            num_hero_decks = len(hero_available)
            num_opp_decks = len(opp_available)

            # Build payoff matrix G for deck selection game
            # Rows = Hero's deck choices
            # Columns = Opponent's deck choices
            G = np.zeros((num_hero_decks, num_opp_decks))

            for idx_h, h in enumerate(hero_available):
                for idx_o, o in enumerate(opp_available):
                    # If Hero wins (deck h beats deck o):
                    # Hero's deck h is eliminated (added to hero_won)
                    # Opponent's deck o remains available
                    win_state = (frozenset(hero_won | {h}), opp_won)
                    win_value = state_results[win_state]['winrate'][0]

                    # If Hero loses:
                    # Hero's deck h remains available
                    # Opponent's deck o is eliminated (added to opp_won)
                    lose_state = (hero_won, frozenset(opp_won | {o}))
                    lose_value = state_results[lose_state]['winrate'][0]

                    # Expected value = P(win) * V(win) + P(lose) * V(lose)
                    G[idx_h, idx_o] = W[h, o] * win_value + (1 - W[h, o]) * lose_value

            # Solve the deck selection game to find Nash equilibrium
            solution = solve_game(G)

            V = solution['V']
            V_opp = 1.0 - V

            result['winrate'] = (V, V_opp)
            result['nash'] = (solution['hero_sol'], solution['opp_sol'])
            result['game'] = G

        # Store result for lookup by future states
        state_results[(hero_won, opp_won)] = result
        results_list.append(result)

    return results_list

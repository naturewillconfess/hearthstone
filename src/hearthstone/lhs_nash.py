"""
lhs_nash.py - Nash equilibrium calculator for Hearthstone Last Hero Standing (LHS) format

This module computes subgame-perfect Nash equilibria for the Last Hero Standing
tournament format used in competitive Hearthstone.

Last Hero Standing (LHS) Format Rules:
--------------------------------------
1. Each player brings n decks to the match
2. Before each game, both players simultaneously choose which deck to play
   (unless forced to play a specific deck - see rule 4)
3. The LOSER's deck is "eliminated" (cannot be used again)
4. The WINNER keeps playing the same deck until it loses
   (they are "forced" to play that deck in subsequent games)
5. The first player to eliminate all opponent's decks wins the match

Key Difference from Conquest:
-----------------------------
In Conquest, the WINNER's deck is eliminated.
In LHS, the LOSER's deck is eliminated, and the winner must keep using their deck.

This creates a more complex state space because we need to track not just
which decks are eliminated, but also which deck each player might be "forced"
to play (the deck that won their last game).

State Representation:
--------------------
A state is represented by (hero_lost, opp_lost, havetoplay_hero, havetoplay_opp):
- hero_lost: frozenset of deck indices Hero has lost with (eliminated)
- opp_lost: frozenset of deck indices Opponent has lost with (eliminated)
- havetoplay_hero: int or None - deck Hero must play (if they won last game)
- havetoplay_opp: int or None - deck Opponent must play (if they won last game)

Note: At most one of havetoplay_hero or havetoplay_opp can be non-None at a time
(since exactly one player won the previous game).
"""

import numpy as np
from itertools import combinations
from typing import List, Optional
from .solve_game import _solve_game_internal
from .results import LHSResult

def _lhs_nash_internal(W: np.ndarray) -> list:
    """
    Find Nash equilibrium for all subgames in a Last Hero Standing match.

    This function analyzes an LHS match by computing optimal play
    (Nash equilibrium in mixed strategies) for every possible game state.
    The state space is larger than Conquest because it includes information
    about which deck each player might be forced to play.

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

        - ``score``: tuple of (hero_lost, opp_lost) as tuples of deck indices.
          hero_lost = decks Hero has lost with (eliminated),
          opp_lost = decks Opponent has lost with (eliminated).
        - ``havetoplay_hero``: int or None.
          The deck Hero must play (if they won the previous game).
        - ``havetoplay_opp``: int or None.
          The deck Opponent must play (if they won the previous game).
        - ``winrate``: tuple (hero_wr, opp_wr).
          Expected win probabilities for the match from this state.
        - ``nash``: tuple (hero_strategy, opp_strategy) for non-terminal states.
          Optimal mixed strategies for deck selection.
        - ``game``: np.ndarray for non-terminal states.
          Payoff matrix for the deck selection subgame.

        The list is ordered from deepest states (most games played) to
        the initial state (no games played).

    Examples
    --------
    >>> import numpy as np
    >>> # Simple 2-deck match with equal matchups
    >>> W = np.array([[0.5, 0.5], [0.5, 0.5]])
    >>> result = lhs_nash(W)
    >>> # Find initial state (no losses, no forced plays)
    >>> initial = result[-1]
    >>> print(f"Match winrate: {initial['winrate'][0]:.4f}")

    """
    # Convert input to numpy array and validate
    W = np.asarray(W, dtype=float)
    n = W.shape[0]  # Number of decks per player

    # =========================================================================
    # HELPER FUNCTION: Lookup state with awareness of near-terminal states
    # =========================================================================
    # Near-terminal states (where one player has n-1 or more losses) are stored
    # without forced play information (None, None) because the outcome is
    # deterministic and doesn't depend on which deck is "forced".
    # =========================================================================

    def lookup_state(state_results, hero_lost, opp_lost, hero_forced, opp_forced):
        """
        Look up a state's winrate, handling near-terminal states.

        Near-terminal states are stored with (None, None) for forced plays
        because their outcome is deterministic.
        """
        hero_lost_count = len(hero_lost)
        opp_lost_count = len(opp_lost)

        # Near-terminal states are stored without forced plays
        if hero_lost_count >= n - 1 or opp_lost_count >= n - 1:
            key = (hero_lost, opp_lost, None, None)
        else:
            key = (hero_lost, opp_lost, hero_forced, opp_forced)

        return state_results[key]['winrate'][0]

    # =========================================================================
    # STEP 1: GENERATE ALL POSSIBLE GAME STATES
    # =========================================================================
    # A game state in LHS is defined by:
    # 1. Which decks Hero has lost with (hero_lost)
    # 2. Which decks Opponent has lost with (opp_lost)
    # 3. Which deck Hero must play (havetoplay_hero) - if any
    # 4. Which deck Opponent must play (havetoplay_opp) - if any
    #
    # For near-terminal states (one player has n-1 or more losses),
    # we don't expand with forced plays since outcome is deterministic.
    # =========================================================================

    # Generate all subsets of deck indices for eliminated decks
    all_subsets = []
    for k in range(n + 1):
        for combo in combinations(range(n), k):
            all_subsets.append(frozenset(combo))

    # Generate all states
    all_states = []

    for hero_lost in all_subsets:
        for opp_lost in all_subsets:

            hero_lost_count = len(hero_lost)
            opp_lost_count = len(opp_lost)
            hero_remaining = [i for i in range(n) if i not in hero_lost]
            opp_remaining = [j for j in range(n) if j not in opp_lost]

            # Skip impossible state where both have lost all decks
            if hero_lost_count == n and opp_lost_count == n:
                continue

            # Near-terminal states: don't need forced-play tracking
            # because outcome is deterministic
            if hero_lost_count >= n - 1 or opp_lost_count >= n - 1:
                all_states.append((hero_lost, opp_lost, None, None))

            # Initial state: no losses, no forced plays
            elif hero_lost_count == 0 and opp_lost_count == 0:
                all_states.append((hero_lost, opp_lost, None, None))

            # Only Hero has lost (Opponent won the last game)
            # Opponent is forced to play their winning deck
            elif hero_lost_count > 0 and opp_lost_count == 0:
                for opp_forced in opp_remaining:
                    all_states.append((hero_lost, opp_lost, None, opp_forced))

            # Only Opponent has lost (Hero won the last game)
            # Hero is forced to play their winning deck
            elif hero_lost_count == 0 and opp_lost_count > 0:
                for hero_forced in hero_remaining:
                    all_states.append((hero_lost, opp_lost, hero_forced, None))

            # Both have lost: either Hero or Opponent won the last game
            else:
                # Hero won last (forced to play their winning deck)
                for hero_forced in hero_remaining:
                    all_states.append((hero_lost, opp_lost, hero_forced, None))
                # Opponent won last (forced to play their winning deck)
                for opp_forced in opp_remaining:
                    all_states.append((hero_lost, opp_lost, None, opp_forced))

    # =========================================================================
    # STEP 2: SORT STATES FOR BACKWARD INDUCTION
    # =========================================================================
    # Sort by depth (total losses) in decreasing order
    # =========================================================================

    all_states.sort(key=lambda s: len(s[0]) + len(s[1]), reverse=True)

    # =========================================================================
    # STEP 3: CREATE LOOKUP TABLE AND PROCESS STATES
    # =========================================================================

    state_results = {}
    results_list = []

    for hero_lost, opp_lost, havetoplay_hero, havetoplay_opp in all_states:
        hero_lost_count = len(hero_lost)
        opp_lost_count = len(opp_lost)

        # Initialize result for this state
        result = {
            'score': (tuple(sorted(hero_lost)), tuple(sorted(opp_lost))),
            'havetoplay_hero': havetoplay_hero,
            'havetoplay_opp': havetoplay_opp
        }

        hero_remaining = [i for i in range(n) if i not in hero_lost]
        opp_remaining = [j for j in range(n) if j not in opp_lost]

        # -----------------------------------------------------------------
        # CASE 1: Hero has lost with all decks (Opponent wins the match)
        # -----------------------------------------------------------------
        if hero_lost_count == n:
            V = 0.0
            V_opp = 1.0
            result['winrate'] = (V, V_opp)

        # -----------------------------------------------------------------
        # CASE 2: Opponent has lost with all decks (Hero wins the match)
        # -----------------------------------------------------------------
        elif opp_lost_count == n:
            V = 1.0
            V_opp = 0.0
            result['winrate'] = (V, V_opp)

        # -----------------------------------------------------------------
        # CASE 3: Hero has n-1 losses (one deck left)
        # -----------------------------------------------------------------
        # Hero has one deck remaining. They win if that deck beats all
        # of opponent's remaining decks in a row.
        # P(Hero wins) = product of W[hero_deck, opp_decks]
        # -----------------------------------------------------------------
        elif hero_lost_count == n - 1:

            prob_win_all = 1.0
            for h in hero_remaining:  # Only one hero deck
                for o in opp_remaining:
                    prob_win_all *= W[h, o]

            V = prob_win_all
            V_opp = 1.0 - V
            result['winrate'] = (V, V_opp)

        # -----------------------------------------------------------------
        # CASE 4: Opponent has n-1 losses (one deck left)
        # -----------------------------------------------------------------
        # Opponent has one deck remaining. Hero wins if any of their
        # remaining decks beats opponent's last deck.
        # P(Hero wins) = 1 - P(all Hero decks lose to Opp's last deck)
        # -----------------------------------------------------------------
        elif opp_lost_count == n - 1:

            # Hero wins if at least one deck beats opponent's last deck
            prob_lose_all = 1.0
            for h in hero_remaining:
                for o in opp_remaining:  # Only one opp deck
                    prob_lose_all *= (1 - W[h, o])

            V = 1.0 - prob_lose_all
            V_opp = 1.0 - V
            result['winrate'] = (V, V_opp)

        # -----------------------------------------------------------------
        # CASE 5: General case - need to solve deck selection game
        # -----------------------------------------------------------------
        else:
            hero_remaining = [i for i in range(n) if i not in hero_lost]
            opp_remaining = [j for j in range(n) if j not in opp_lost]

            # ---------------------------------------------------------
            # SUBCASE 5A: Hero is forced to play a specific deck
            # ---------------------------------------------------------
            if havetoplay_hero is not None:
                # Hero has no choice, Opponent chooses from available decks
                h = havetoplay_hero
                G = np.zeros((1, len(opp_remaining)))

                for idx_o, o in enumerate(opp_remaining):
                    # If Hero wins: Opponent's deck o is eliminated
                    # Hero stays forced to play deck h
                    new_opp_lost = frozenset(opp_lost | {o})
                    win_value = lookup_state(
                        state_results, hero_lost, new_opp_lost, h, None
                    )

                    # If Hero loses: Hero's deck h is eliminated
                    # Opponent becomes forced to play deck o
                    new_hero_lost = frozenset(hero_lost | {h})
                    lose_value = lookup_state(
                        state_results, new_hero_lost, opp_lost, None, o
                    )

                    G[0, idx_o] = W[h, o] * win_value + (1 - W[h, o]) * lose_value

                solution = _solve_game_internal(G)
                V = solution['V']
                V_opp = 1.0 - V
                result['winrate'] = (V, V_opp)
                result['nash'] = (solution['hero_sol'], solution['opp_sol'])
                result['game'] = G

            # ---------------------------------------------------------
            # SUBCASE 5B: Opponent is forced to play a specific deck
            # ---------------------------------------------------------
            elif havetoplay_opp is not None:
                # Opponent has no choice, Hero chooses from available decks
                o = havetoplay_opp
                G = np.zeros((len(hero_remaining), 1))

                for idx_h, h in enumerate(hero_remaining):
                    # If Hero wins: Opponent's deck o is eliminated
                    # Hero becomes forced to play deck h
                    new_opp_lost = frozenset(opp_lost | {o})
                    win_value = lookup_state(
                        state_results, hero_lost, new_opp_lost, h, None
                    )

                    # If Hero loses: Hero's deck h is eliminated
                    # Opponent stays forced to play deck o
                    new_hero_lost = frozenset(hero_lost | {h})
                    lose_value = lookup_state(
                        state_results, new_hero_lost, opp_lost, None, o
                    )

                    G[idx_h, 0] = W[h, o] * win_value + (1 - W[h, o]) * lose_value

                solution = _solve_game_internal(G)
                V = solution['V']
                V_opp = 1.0 - V
                result['winrate'] = (V, V_opp)
                result['nash'] = (solution['hero_sol'], solution['opp_sol'])
                result['game'] = G

            # ---------------------------------------------------------
            # SUBCASE 5C: Neither is forced (initial state)
            # ---------------------------------------------------------
            else:
                # Both players choose freely (only happens at initial state)
                G = np.zeros((len(hero_remaining), len(opp_remaining)))

                for idx_h, h in enumerate(hero_remaining):
                    for idx_o, o in enumerate(opp_remaining):
                        # If Hero wins: Opponent's deck o is eliminated
                        # Hero becomes forced to play deck h
                        new_opp_lost = frozenset(opp_lost | {o})
                        win_value = lookup_state(
                            state_results, hero_lost, new_opp_lost, h, None
                        )

                        # If Hero loses: Hero's deck h is eliminated
                        # Opponent becomes forced to play deck o
                        new_hero_lost = frozenset(hero_lost | {h})
                        lose_value = lookup_state(
                            state_results, new_hero_lost, opp_lost, None, o
                        )

                        G[idx_h, idx_o] = W[h, o] * win_value + (1 - W[h, o]) * lose_value

                solution = _solve_game_internal(G)
                V = solution['V']
                V_opp = 1.0 - V
                result['winrate'] = (V, V_opp)
                result['nash'] = (solution['hero_sol'], solution['opp_sol'])
                result['game'] = G

        # Store result for lookup
        state_key = (hero_lost, opp_lost, havetoplay_hero, havetoplay_opp)
        state_results[state_key] = result
        results_list.append(result)

    return results_list


def lhs_nash(W: np.ndarray,
             deck_names: Optional[List[str]] = None) -> LHSResult:
    """
    Find Nash equilibrium for all subgames in a Last Hero Standing match.

    This function analyzes an LHS match by computing optimal play
    (Nash equilibrium in mixed strategies) for every possible game state.
    The state space includes information about forced plays (when a player
    must continue using a winning deck).

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
    LHSResult
        Result object with easy access to match analysis:

        - ``winrate``: Hero's match winrate from initial state
        - ``hero_strategy``: Hero's optimal initial deck selection
        - ``opp_strategy``: Opponent's optimal initial deck selection
        - ``get_state(hero_lost, opp_lost, forced_hero, forced_opp)``:
          Get any mid-match state including forced play situations
        - ``all_states()``: Get all states for advanced analysis

    Examples
    --------
    >>> import numpy as np
    >>> # Simple 2-deck match with equal matchups
    >>> W = np.array([[0.5, 0.5], [0.5, 0.5]])
    >>> result = lhs_nash(W, deck_names=['Aggro', 'Control'])
    >>> print(f"Match winrate: {result.winrate:.1%}")
    Match winrate: 50.0%

    >>> # View optimal strategy
    >>> print(result.hero_strategy)
    [('Aggro', 0.5), ('Control', 0.5)]

    >>> # Check state where Hero lost Aggro and Opponent is forced to play Control
    >>> state = result.get_state(hero_lost=['Aggro'], forced_opp='Control')
    >>> print(f"Winrate from this state: {state.winrate:.1%}")
    """
    W = np.asarray(W, dtype=float)
    n = W.shape[0]

    # Default deck names
    if deck_names is None:
        deck_names = [f"Deck {i}" for i in range(n)]

    # Validate deck names
    if len(deck_names) != n:
        raise ValueError(f"deck_names has {len(deck_names)} elements, expected {n}")

    # Compute all states
    states = _lhs_nash_internal(W)

    return LHSResult(states, deck_names)

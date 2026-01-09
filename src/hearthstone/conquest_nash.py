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


# =============================================================================
# ANALYTICAL SOLVERS FOR SMALL GAMES (winrate_only=True fast path)
# =============================================================================

def _solve_2x2_value(a: float, b: float, c: float, d: float) -> float:
    """Solve 2x2 zero-sum game, return only the value."""
    maximin = max(min(a, b), min(c, d))
    minimax = min(max(a, c), max(b, d))
    if maximin >= minimax - 1e-10:
        return maximin
    denom = a - b - c + d
    if abs(denom) < 1e-10:
        return (a + b + c + d) / 4
    return (a * d - b * c) / denom


def _conquest_2v2_winrate(W: np.ndarray) -> float:
    """
    Analytical solver for 2v2 Conquest - returns only the match winrate.

    Computes Nash equilibrium value without building full state objects.
    """
    # Near-terminal values
    # V_hero_h = V(hero_won={h}, opp_won=∅): hero eliminated deck h, must beat all opp decks
    # V_opp_o = V(hero_won=∅, opp_won={o}): opp eliminated deck o, hero must beat opp's remaining deck

    # V({0}, ∅): hero eliminated deck 0, deck 1 remains vs opp decks {0,1}
    V_hero_0 = 1 - (1 - W[1, 0]) * (1 - W[1, 1])
    # V({1}, ∅): hero eliminated deck 1, deck 0 remains vs opp decks {0,1}
    V_hero_1 = 1 - (1 - W[0, 0]) * (1 - W[0, 1])
    # V(∅, {0}): opp eliminated deck 0, opp deck 1 remains - hero must beat it with both decks
    V_opp_0 = W[0, 1] * W[1, 1]
    # V(∅, {1}): opp eliminated deck 1, opp deck 0 remains - hero must beat it with both decks
    V_opp_1 = W[0, 0] * W[1, 0]

    # Initial state payoff matrix (2x2)
    # G[h, o] = W[h,o] * V(hero_won={h}) + (1-W[h,o]) * V(opp_won={o})
    G00 = W[0, 0] * V_hero_0 + (1 - W[0, 0]) * V_opp_0
    G01 = W[0, 1] * V_hero_0 + (1 - W[0, 1]) * V_opp_1
    G10 = W[1, 0] * V_hero_1 + (1 - W[1, 0]) * V_opp_0
    G11 = W[1, 1] * V_hero_1 + (1 - W[1, 1]) * V_opp_1

    return _solve_2x2_value(G00, G01, G10, G11)


def _conquest_3v3_winrate(W: np.ndarray) -> float:
    """
    Analytical solver for 3v3 Conquest - returns only the match winrate.

    Uses bitmask state representation and hardcoded dependency structure
    to avoid dictionary lookups and object allocations.
    """
    # V[hero_mask][opp_mask] = value of state
    # mask: bit i set means deck i has been eliminated (won with)
    V = np.full((8, 8), np.nan)

    # Terminal states
    V[7, :] = 1.0  # Hero won all (mask 7 = 111 = {0,1,2})
    V[:7, 7] = 0.0  # Opp won all

    # Mapping: 2-win mask -> remaining deck
    # 3 = 011 = {0,1} -> deck 2 remains
    # 5 = 101 = {0,2} -> deck 1 remains
    # 6 = 110 = {1,2} -> deck 0 remains
    mask_to_remaining = {3: 2, 5: 1, 6: 0}

    # Near-terminal: hero has 2 wins
    for h_mask in [3, 5, 6]:
        h_deck = mask_to_remaining[h_mask]
        for o_mask in range(7):
            opp_remaining = [j for j in range(3) if not (o_mask & (1 << j))]
            V[h_mask, o_mask] = 1.0 - np.prod([1 - W[h_deck, j] for j in opp_remaining])

    # Near-terminal: opp has 2 wins (and hero doesn't have 2+ wins)
    for o_mask in [3, 5, 6]:
        o_deck = mask_to_remaining[o_mask]
        for h_mask in range(7):
            if h_mask in [3, 5, 6]:
                continue  # Already computed
            hero_remaining = [i for i in range(3) if not (h_mask & (1 << i))]
            V[h_mask, o_mask] = np.prod([W[i, o_deck] for i in hero_remaining])

    # Inline 2x3 solver (hero has 2 remaining decks, opp has 3)
    def solve_2x3(G):
        s = G[0, :] - G[1, :]
        i = G[1, :]
        candidates = [0.0, 1.0]
        for j in range(3):
            for k in range(j + 1, 3):
                d = s[j] - s[k]
                if abs(d) > 1e-10:
                    p = (i[k] - i[j]) / d
                    if 0 < p < 1:
                        candidates.append(p)
        return max(min(i[0] + s[0] * p, i[1] + s[1] * p, i[2] + s[2] * p) for p in candidates)

    # Inline 3x2 solver (hero has 3 remaining decks, opp has 2)
    def solve_3x2(G):
        s = G[:, 0] - G[:, 1]
        i = G[:, 1]
        candidates = [0.0, 1.0]
        for j in range(3):
            for k in range(j + 1, 3):
                d = s[j] - s[k]
                if abs(d) > 1e-10:
                    q = (i[k] - i[j]) / d
                    if 0 < q < 1:
                        candidates.append(q)
        return min(max(i[0] + s[0] * q, i[1] + s[1] * q, i[2] + s[2] * q) for q in candidates)

    # (1,1) states: hero has 1 win, opp has 1 win - solve 2x2 games
    for h_mask in [1, 2, 4]:
        h_elim = {1: 0, 2: 1, 4: 2}[h_mask]
        hero_remaining = [i for i in range(3) if i != h_elim]

        for o_mask in [1, 2, 4]:
            o_elim = {1: 0, 2: 1, 4: 2}[o_mask]
            opp_remaining = [j for j in range(3) if j != o_elim]

            G = np.zeros((2, 2))
            for hi, h in enumerate(hero_remaining):
                for oi, o in enumerate(opp_remaining):
                    win_mask = h_mask | (1 << h)
                    lose_mask = o_mask | (1 << o)
                    G[hi, oi] = W[h, o] * V[win_mask, o_mask] + (1 - W[h, o]) * V[h_mask, lose_mask]

            V[h_mask, o_mask] = _solve_2x2_value(G[0, 0], G[0, 1], G[1, 0], G[1, 1])

    # (1,0) states: hero has 1 win, opp has 0 - solve 2x3 games
    for h_mask in [1, 2, 4]:
        h_elim = {1: 0, 2: 1, 4: 2}[h_mask]
        hero_remaining = [i for i in range(3) if i != h_elim]

        G = np.zeros((2, 3))
        for hi, h in enumerate(hero_remaining):
            for o in range(3):
                win_mask = h_mask | (1 << h)
                lose_mask = 1 << o
                G[hi, o] = W[h, o] * V[win_mask, 0] + (1 - W[h, o]) * V[h_mask, lose_mask]

        V[h_mask, 0] = solve_2x3(G)

    # (0,1) states: hero has 0 wins, opp has 1 - solve 3x2 games
    for o_mask in [1, 2, 4]:
        o_elim = {1: 0, 2: 1, 4: 2}[o_mask]
        opp_remaining = [j for j in range(3) if j != o_elim]

        G = np.zeros((3, 2))
        for h in range(3):
            for oi, o in enumerate(opp_remaining):
                win_mask = 1 << h
                lose_mask = o_mask | (1 << o)
                G[h, oi] = W[h, o] * V[win_mask, o_mask] + (1 - W[h, o]) * V[0, lose_mask]

        V[0, o_mask] = solve_3x2(G)

    # (0,0) initial state - solve 3x3 game
    G = np.zeros((3, 3))
    for h in range(3):
        for o in range(3):
            G[h, o] = W[h, o] * V[1 << h, 0] + (1 - W[h, o]) * V[0, 1 << o]

    # Try fully mixed 3x3 equilibrium
    A = np.array([G[0, :] - G[1, :], G[1, :] - G[2, :], [1, 1, 1]])
    try:
        q = np.linalg.solve(A, [0, 0, 1])
        if (q > -1e-9).all() and (q < 1 + 1e-9).all():
            A2 = np.array([G[:, 0] - G[:, 1], G[:, 1] - G[:, 2], [1, 1, 1]])
            p = np.linalg.solve(A2, [0, 0, 1])
            if (p > -1e-9).all() and (p < 1 + 1e-9).all():
                q = np.clip(q, 0, 1)
                q /= q.sum()
                return float(G[0, :] @ q)
    except np.linalg.LinAlgError:
        pass

    # Try 2x3 subgames (drop one row)
    for drop in range(3):
        rows = [r for r in range(3) if r != drop]
        Gsub = G[rows, :]
        val = solve_2x3(Gsub)
        # Verify equilibrium
        s = Gsub[0, :] - Gsub[1, :]
        i = Gsub[1, :]
        candidates = [0.0, 1.0]
        for j in range(3):
            for k in range(j + 1, 3):
                d = s[j] - s[k]
                if abs(d) > 1e-10:
                    p_cand = (i[k] - i[j]) / d
                    if 0 < p_cand < 1:
                        candidates.append(p_cand)
        for p in candidates:
            if abs(min(i + s * p) - val) < 1e-9:
                vals = i + s * p
                active = np.where(vals <= val + 1e-9)[0]
                q = np.zeros(3)
                if len(active) == 1:
                    q[active[0]] = 1
                else:
                    c1, c2 = active[0], active[1]
                    d = s[c1] - s[c2]
                    if abs(d) > 1e-10:
                        q[c1] = np.clip(-s[c2] / d, 0, 1)
                        q[c2] = 1 - q[c1]
                    else:
                        q[c1] = q[c2] = 0.5
                if G[drop, :] @ q <= val + 1e-9:
                    return val
                break

    # Try 3x2 subgames (drop one column)
    for drop in range(3):
        cols = [c for c in range(3) if c != drop]
        Gsub = G[:, cols]
        val = solve_3x2(Gsub)
        s = Gsub[:, 0] - Gsub[:, 1]
        i = Gsub[:, 1]
        candidates = [0.0, 1.0]
        for j in range(3):
            for k in range(j + 1, 3):
                d = s[j] - s[k]
                if abs(d) > 1e-10:
                    q_cand = (i[k] - i[j]) / d
                    if 0 < q_cand < 1:
                        candidates.append(q_cand)
        for qq in candidates:
            if abs(max(i + s * qq) - val) < 1e-9:
                vals = i + s * qq
                active = np.where(vals >= val - 1e-9)[0]
                p = np.zeros(3)
                if len(active) == 1:
                    p[active[0]] = 1
                else:
                    r1, r2 = active[0], active[1]
                    d = s[r1] - s[r2]
                    if abs(d) > 1e-10:
                        p[r1] = np.clip(-s[r2] / d, 0, 1)
                        p[r2] = 1 - p[r1]
                    else:
                        p[r1] = p[r2] = 0.5
                if p @ G[:, drop] >= val - 1e-9:
                    return val
                break

    # Try 2x2 subgames (drop one row and one column)
    for dr in range(3):
        for dc in range(3):
            rows = [r for r in range(3) if r != dr]
            cols = [c for c in range(3) if c != dc]
            Gsub = G[np.ix_(rows, cols)]
            val = _solve_2x2_value(Gsub[0, 0], Gsub[0, 1], Gsub[1, 0], Gsub[1, 1])

            a, b, c, d = Gsub[0, 0], Gsub[0, 1], Gsub[1, 0], Gsub[1, 1]
            denom = a - b - c + d
            if abs(denom) < 1e-10:
                p_sub = [0.5, 0.5]
                q_sub = [0.5, 0.5]
            elif abs(max(min(a, b), min(c, d)) - min(max(a, c), max(b, d))) < 1e-10:
                p_sub = [1, 0] if min(a, b) >= min(c, d) else [0, 1]
                q_sub = [1, 0] if max(a, c) <= max(b, d) else [0, 1]
            else:
                p_sub = [np.clip((d - c) / denom, 0, 1), np.clip((a - b) / denom, 0, 1)]
                ps = sum(p_sub)
                p_sub = [x / ps for x in p_sub]
                q_sub = [np.clip((d - b) / denom, 0, 1), np.clip((a - c) / denom, 0, 1)]
                qs = sum(q_sub)
                q_sub = [x / qs for x in q_sub]

            p = np.zeros(3)
            p[rows[0]], p[rows[1]] = p_sub
            q = np.zeros(3)
            q[cols[0]], q[cols[1]] = q_sub

            if G[dr, :] @ q <= val + 1e-9 and p @ G[:, dc] >= val - 1e-9:
                return val

    raise RuntimeError("3x3 Conquest solver failed to find equilibrium")


def conquest_nash(W: np.ndarray,
                  hero_names: Optional[List[str]] = None,
                  opp_names: Optional[List[str]] = None,
                  _cache: Optional[Dict[bytes, tuple]] = None,
                  winrate_only: bool = False) -> ConquestResult:
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

    winrate_only : bool, optional
        If True, use fast analytical solvers (2-2.5x faster for n=2,3) that
        only compute the match winrate. The returned ConquestResult will have
        the correct winrate but strategies will be placeholders. Useful when
        only the win probability is needed (e.g., in ban_nash or lineup_picker).
        Default is False.

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
    # FAST PATH: Use analytical solvers for small games when only winrate needed
    # =========================================================================
    if winrate_only:
        if n == 2:
            winrate = _conquest_2v2_winrate(W)
        elif n == 3:
            winrate = _conquest_3v3_winrate(W)
        else:
            # Fall back to full computation for n > 3
            return conquest_nash(W, hero_names, opp_names, _cache, winrate_only=False)

        # Return minimal ConquestResult with just the winrate
        # Create a dummy initial state solution
        initial_solution = GameSolution(
            value=winrate,
            hero_names=hero_names,
            opp_names=opp_names,
            hero_strategy=[(name, 1.0 / n) for name in hero_names],  # Placeholder
            opp_strategy=[(name, 1.0 / n) for name in opp_names]     # Placeholder
        )
        initial_state = ConquestStateSolution(
            hero_names=hero_names,
            opp_names=opp_names,
            hero_won=frozenset(),
            opp_won=frozenset(),
            solution=initial_solution
        )
        return ConquestResult([initial_state], hero_names, opp_names)

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

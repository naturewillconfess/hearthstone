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
from typing import List, Optional, Dict
from .solve_game import solve_game
from .results import LHSResult, LHSStateSolution, GameSolution


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


def _lhs_2v2_winrate(W: np.ndarray) -> float:
    """
    Analytical solver for 2v2 LHS - returns only the match winrate.

    State notation: V[h_elim][o_elim][h_forced][o_forced]
    where h_elim/o_elim are bitmasks, h_forced/o_forced are deck indices or -1

    For 2v2, we use a dictionary-based approach for clarity.
    """
    V = {}

    # Terminal states
    # hero_elim = 3 (both decks lost) → V = 0
    # opp_elim = 3 (both decks lost) → V = 1
    for o in range(4):
        V[(3, o, -1, -1)] = 0.0
    for h in range(3):
        V[(h, 3, -1, -1)] = 1.0

    # Near-terminal: hero has 1 deck left (hero_elim in {1, 2})
    # Hero must beat all remaining opp decks in sequence
    for h_elim in [1, 2]:  # 1 = deck 0 lost, 2 = deck 1 lost
        h_deck = 1 if h_elim == 1 else 0  # remaining deck
        for o_elim in [0, 1, 2]:  # 0, 1, or 2 opp decks lost
            opp_decks = [i for i in range(2) if not (o_elim & (1 << i))]
            if not opp_decks:
                continue
            # Hero wins by beating all opp decks
            prob = 1.0
            for o in opp_decks:
                prob *= W[h_deck, o]
            V[(h_elim, o_elim, -1, -1)] = prob
            # Store for all forced variants (doesn't matter with 1 hero deck)
            for o_forced in opp_decks:
                V[(h_elim, o_elim, -1, o_forced)] = prob

    # Near-terminal: opp has 1 deck left (opp_elim in {1, 2})
    for o_elim in [1, 2]:
        o_deck = 1 if o_elim == 1 else 0  # remaining opp deck
        for h_elim in [0]:  # hero has both decks
            hero_decks = [0, 1]
            # Hero wins if any deck beats opp's last deck
            prob_all_lose = (1 - W[0, o_deck]) * (1 - W[1, o_deck])
            prob = 1.0 - prob_all_lose
            V[(h_elim, o_elim, -1, -1)] = prob
            for h_forced in hero_decks:
                V[(h_elim, o_elim, h_forced, -1)] = prob

    # States with 1 loss each: (h_elim, o_elim) where both have 1 bit set
    # V(1, 1, *, *): hero lost deck 0, opp lost deck 0 → both have deck 1
    V[(1, 1, -1, -1)] = W[1, 1]
    V[(1, 1, 1, -1)] = W[1, 1]  # hero forced to 1
    V[(1, 1, -1, 1)] = W[1, 1]  # opp forced to 1

    # V(1, 2, *, *): hero lost deck 0 (has 1), opp lost deck 1 (has 0)
    V[(1, 2, -1, -1)] = W[1, 0]
    V[(1, 2, 1, -1)] = W[1, 0]
    V[(1, 2, -1, 0)] = W[1, 0]

    # V(2, 1, *, *): hero lost deck 1 (has 0), opp lost deck 0 (has 1)
    V[(2, 1, -1, -1)] = W[0, 1]
    V[(2, 1, 0, -1)] = W[0, 1]
    V[(2, 1, -1, 1)] = W[0, 1]

    # V(2, 2, *, *): hero lost deck 1 (has 0), opp lost deck 1 (has 0)
    V[(2, 2, -1, -1)] = W[0, 0]
    V[(2, 2, 0, -1)] = W[0, 0]
    V[(2, 2, -1, 0)] = W[0, 0]

    # States with 0 hero losses, 1 opp loss, hero forced
    # V(0, 1, h, -1): opp lost deck 0 (has deck 1), hero forced to h
    for h_forced in [0, 1]:
        # Hero plays h_forced vs Opp plays 1
        # Win: opp_elim becomes 3 → V = 1
        # Lose: hero_elim becomes (1 << h_forced), opp forced to 1
        win_val = 1.0
        lose_state = (1 << h_forced, 1, -1, 1)
        lose_val = V.get(lose_state, 0)
        V[(0, 1, h_forced, -1)] = W[h_forced, 1] * win_val + (1 - W[h_forced, 1]) * lose_val

    # V(0, 2, h, -1): opp lost deck 1 (has deck 0), hero forced to h
    for h_forced in [0, 1]:
        win_val = 1.0
        lose_state = (1 << h_forced, 2, -1, 0)
        lose_val = V.get(lose_state, 0)
        V[(0, 2, h_forced, -1)] = W[h_forced, 0] * win_val + (1 - W[h_forced, 0]) * lose_val

    # States with 1 hero loss, 0 opp losses, opp forced
    # V(1, 0, -1, o): hero lost deck 0 (has deck 1), opp forced to o
    for o_forced in [0, 1]:
        # Hero plays 1 vs Opp plays o_forced
        # Win: opp_elim becomes (1 << o_forced), hero forced to 1
        win_state = (1, 1 << o_forced, 1, -1)
        win_val = V.get(win_state, 0)
        # Lose: hero_elim becomes 3 → V = 0
        lose_val = 0.0
        V[(1, 0, -1, o_forced)] = W[1, o_forced] * win_val + (1 - W[1, o_forced]) * lose_val

    # V(2, 0, -1, o): hero lost deck 1 (has deck 0), opp forced to o
    for o_forced in [0, 1]:
        win_state = (2, 1 << o_forced, 0, -1)
        win_val = V.get(win_state, 0)
        lose_val = 0.0
        V[(2, 0, -1, o_forced)] = W[0, o_forced] * win_val + (1 - W[0, o_forced]) * lose_val

    # Initial state: V(0, 0, -1, -1)
    # Build 2x2 payoff matrix
    G = np.zeros((2, 2))
    for h in range(2):
        for o in range(2):
            # Hero plays h, Opp plays o
            # Win: opp_elim = (1 << o), hero forced to h
            win_state = (0, 1 << o, h, -1)
            win_val = V.get(win_state, 0)
            # Lose: hero_elim = (1 << h), opp forced to o
            lose_state = (1 << h, 0, -1, o)
            lose_val = V.get(lose_state, 0)
            G[h, o] = W[h, o] * win_val + (1 - W[h, o]) * lose_val

    return _solve_2x2_value(G[0, 0], G[0, 1], G[1, 0], G[1, 1])


def _lhs_3v3_winrate(W: np.ndarray) -> float:
    """
    Analytical solver for 3v3 LHS - returns only the match winrate.

    Uses backward induction with explicit state enumeration.
    States are indexed by (hero_lost_mask, opp_lost_mask, forced_hero, forced_opp).
    """
    # Use dictionaries for state values
    # Key: (hero_mask, opp_mask, h_forced, o_forced) where masks are bitmasks
    # h_forced/o_forced are deck indices or -1 for None
    V = {}

    # Terminal states
    V[(7, 0, -1, -1)] = 1.0  # Hero won all (impossible but set for completeness)
    V[(7, 1, -1, -1)] = 1.0
    V[(7, 2, -1, -1)] = 1.0
    V[(7, 3, -1, -1)] = 1.0
    V[(7, 4, -1, -1)] = 1.0
    V[(7, 5, -1, -1)] = 1.0
    V[(7, 6, -1, -1)] = 1.0
    for h_mask in range(7):
        V[(h_mask, 7, -1, -1)] = 1.0  # Opp lost all decks

    for o_mask in range(7):
        V[(7, o_mask, -1, -1)] = 0.0  # Hero lost all decks

    # Helper to get remaining decks from mask
    def remaining(mask):
        return [i for i in range(3) if not (mask & (1 << i))]

    # Near-terminal: hero has 2 losses (1 deck left)
    # V(hero_mask=6/5/3, opp_mask, None, o_forced)
    for h_mask in [3, 5, 6]:  # 2 bits set = 2 losses
        h_deck = remaining(h_mask)[0]  # The one remaining deck
        for o_mask in range(7):
            if o_mask == 7:
                continue
            opp_decks = remaining(o_mask)
            if not opp_decks:
                continue
            # Hero must beat all opp decks in sequence (starting with forced one)
            # This is product of winrates
            prob = 1.0
            for o in opp_decks:
                prob *= W[h_deck, o]
            # Store for all possible forced states (doesn't matter since hero has 1 deck)
            V[(h_mask, o_mask, -1, -1)] = prob
            for o_forced in opp_decks:
                V[(h_mask, o_mask, -1, o_forced)] = prob

    # Near-terminal: opp has 2 losses (1 deck left)
    for o_mask in [3, 5, 6]:
        o_deck = remaining(o_mask)[0]
        for h_mask in range(7):
            if h_mask == 7 or h_mask in [3, 5, 6]:
                continue  # Already handled
            hero_decks = remaining(h_mask)
            if not hero_decks:
                continue
            # Hero wins if any deck beats opp's last deck, loses if all fail
            prob_all_lose = 1.0
            for h in hero_decks:
                prob_all_lose *= (1 - W[h, o_deck])
            prob = 1.0 - prob_all_lose
            V[(h_mask, o_mask, -1, -1)] = prob
            for h_forced in hero_decks:
                V[(h_mask, o_mask, h_forced, -1)] = prob

    # States with 1 loss each, one player forced
    # Process by total losses (descending) for backward induction
    for h_mask in [1, 2, 4]:  # 1 loss for hero
        h_lost = {0: 0, 1: 1, 2: 2, 4: 2}[h_mask] if h_mask in [1, 2, 4] else -1
        if h_mask == 1:
            h_lost = 0
        elif h_mask == 2:
            h_lost = 1
        elif h_mask == 4:
            h_lost = 2
        hero_decks = remaining(h_mask)

        for o_mask in [1, 2, 4]:  # 1 loss for opp
            if o_mask == 1:
                o_lost = 0
            elif o_mask == 2:
                o_lost = 1
            elif o_mask == 4:
                o_lost = 2
            opp_decks = remaining(o_mask)

            # Case: Hero is forced to play h_forced
            for h_forced in hero_decks:
                # Opp chooses which deck to play
                G = np.zeros((1, 2))
                for oi, o in enumerate(opp_decks):
                    # Hero wins: opp loses deck o
                    new_o_mask = o_mask | (1 << o)
                    win_val = V.get((h_mask, new_o_mask, h_forced, -1), 0)
                    # Hero loses: hero loses deck h_forced, opp forced to o
                    new_h_mask = h_mask | (1 << h_forced)
                    lose_val = V.get((new_h_mask, o_mask, -1, o), 0)
                    G[0, oi] = W[h_forced, o] * win_val + (1 - W[h_forced, o]) * lose_val
                # Opp minimizes, so V = min of row
                V[(h_mask, o_mask, h_forced, -1)] = float(np.min(G))

            # Case: Opp is forced to play o_forced
            for o_forced in opp_decks:
                G = np.zeros((2, 1))
                for hi, h in enumerate(hero_decks):
                    new_o_mask = o_mask | (1 << o_forced)
                    win_val = V.get((h_mask, new_o_mask, h, -1), 0)
                    new_h_mask = h_mask | (1 << h)
                    lose_val = V.get((new_h_mask, o_mask, -1, o_forced), 0)
                    G[hi, 0] = W[h, o_forced] * win_val + (1 - W[h, o_forced]) * lose_val
                # Hero maximizes
                V[(h_mask, o_mask, -1, o_forced)] = float(np.max(G))

    # States with 0 losses for one player, 1 loss for other
    # Hero has 0 losses, Opp has 1 loss, Hero is forced
    for o_mask in [1, 2, 4]:
        opp_decks = remaining(o_mask)
        for h_forced in range(3):
            G = np.zeros((1, 2))
            for oi, o in enumerate(opp_decks):
                new_o_mask = o_mask | (1 << o)
                win_val = V.get((0, new_o_mask, h_forced, -1), 0)
                new_h_mask = 1 << h_forced
                lose_val = V.get((new_h_mask, o_mask, -1, o), 0)
                G[0, oi] = W[h_forced, o] * win_val + (1 - W[h_forced, o]) * lose_val
            V[(0, o_mask, h_forced, -1)] = float(np.min(G))

    # Hero has 1 loss, Opp has 0 losses, Opp is forced
    for h_mask in [1, 2, 4]:
        hero_decks = remaining(h_mask)
        for o_forced in range(3):
            G = np.zeros((2, 1))
            for hi, h in enumerate(hero_decks):
                new_o_mask = 1 << o_forced
                win_val = V.get((h_mask, new_o_mask, h, -1), 0)
                new_h_mask = h_mask | (1 << h)
                lose_val = V.get((new_h_mask, 0, -1, o_forced), 0)
                G[hi, 0] = W[h, o_forced] * win_val + (1 - W[h, o_forced]) * lose_val
            V[(h_mask, 0, -1, o_forced)] = float(np.max(G))

    # Initial state: 0 losses each, no forced plays
    # Build 3x3 payoff matrix
    G = np.zeros((3, 3))
    for h in range(3):
        for o in range(3):
            # Hero wins: opp_lost={o}, Hero forced to h
            win_val = V.get((0, 1 << o, h, -1), 0)
            # Hero loses: hero_lost={h}, Opp forced to o
            lose_val = V.get((1 << h, 0, -1, o), 0)
            G[h, o] = W[h, o] * win_val + (1 - W[h, o]) * lose_val

    # Solve 3x3 game using the same robust approach as conquest_nash
    # Try fully mixed equilibrium first
    A_q = np.array([G[0, :] - G[1, :], G[1, :] - G[2, :], [1, 1, 1]])
    try:
        q = np.linalg.solve(A_q, [0, 0, 1])
        if (q > -1e-9).all() and (q < 1 + 1e-9).all():
            A_p = np.array([G[:, 0] - G[:, 1], G[:, 1] - G[:, 2], [1, 1, 1]])
            p = np.linalg.solve(A_p, [0, 0, 1])
            if (p > -1e-9).all() and (p < 1 + 1e-9).all():
                q = np.clip(q, 0, 1)
                q = q / q.sum()
                return float(G[0, :] @ q)
    except np.linalg.LinAlgError:
        pass

    # Helper for 2x3 solver (Hero has 2 rows, Opp has 3 cols)
    def solve_2x3(Gsub):
        s = Gsub[0, :] - Gsub[1, :]
        i = Gsub[1, :]
        candidates = [0.0, 1.0]
        for j in range(3):
            for k in range(j + 1, 3):
                d = s[j] - s[k]
                if abs(d) > 1e-10:
                    p = (i[k] - i[j]) / d
                    if 0 < p < 1:
                        candidates.append(p)
        return max(min(i[0] + s[0] * p, i[1] + s[1] * p, i[2] + s[2] * p) for p in candidates)

    # Helper for 3x2 solver (Hero has 3 rows, Opp has 2 cols)
    def solve_3x2(Gsub):
        s = Gsub[:, 0] - Gsub[:, 1]
        i = Gsub[:, 1]
        candidates = [0.0, 1.0]
        for j in range(3):
            for k in range(j + 1, 3):
                d = s[j] - s[k]
                if abs(d) > 1e-10:
                    q = (i[k] - i[j]) / d
                    if 0 < q < 1:
                        candidates.append(q)
        return min(max(i[0] + s[0] * q, i[1] + s[1] * q, i[2] + s[2] * q) for q in candidates)

    # Try 2x3 subgames (drop one row)
    for drop in range(3):
        rows = [r for r in range(3) if r != drop]
        Gsub = G[rows, :]
        val = solve_2x3(Gsub)
        # Find the equilibrium strategy
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

    # Fallback: minimax
    return float(np.max(np.min(G, axis=1)))


def lhs_nash(W: np.ndarray,
             hero_names: Optional[List[str]] = None,
             opp_names: Optional[List[str]] = None,
             _cache: Optional[Dict[bytes, tuple]] = None,
             winrate_only: bool = False) -> LHSResult:
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

    hero_names : list of str, optional
        Names for Hero's decks. Default: ['Deck 0', 'Deck 1', ...].

    opp_names : list of str, optional
        Names for Opponent's decks. Default: ['Deck 0', 'Deck 1', ...].

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
    >>> result = lhs_nash(W, hero_names=['Aggro', 'Control'])
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
            winrate = _lhs_2v2_winrate(W)
        elif n == 3:
            winrate = _lhs_3v3_winrate(W)
        else:
            # Fall back to full computation for n > 3
            return lhs_nash(W, hero_names, opp_names, _cache, winrate_only=False)

        # Return minimal LHSResult with just the winrate
        initial_solution = GameSolution(
            value=winrate,
            hero_names=hero_names,
            opp_names=opp_names,
            hero_strategy=[(name, 1.0 / n) for name in hero_names],  # Placeholder
            opp_strategy=[(name, 1.0 / n) for name in opp_names]     # Placeholder
        )
        initial_state = LHSStateSolution(
            hero_names=hero_names,
            opp_names=opp_names,
            hero_lost=frozenset(),
            opp_lost=frozenset(),
            havetoplay_hero=None,
            havetoplay_opp=None,
            solution=initial_solution
        )
        return LHSResult([initial_state], hero_names, opp_names)

    hero_index_to_name = {i: name for i, name in enumerate(hero_names)}
    opp_index_to_name = {i: name for i, name in enumerate(opp_names)}

    # =========================================================================
    # HELPER FUNCTION: Lookup state with awareness of near-terminal states
    # =========================================================================
    # Near-terminal states (where one player has n-1 or more losses) are stored
    # without forced play information (None, None) because the outcome is
    # deterministic and doesn't depend on which deck is "forced".
    # =========================================================================

    def lookup_state(state_winrate_dict, hero_lost, opp_lost, hero_forced, opp_forced):
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

        return state_winrate_dict[key]

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
    state_winrates = {}
    results_list = []

    for hero_lost, opp_lost, havetoplay_hero, havetoplay_opp in all_states:
        hero_lost_count = len(hero_lost)
        opp_lost_count = len(opp_lost)

        hero_remaining = [i for i in range(n) if i not in hero_lost]
        opp_remaining = [j for j in range(n) if j not in opp_lost]

        # -----------------------------------------------------------------
        # CASE 1: Hero has lost with all decks (Opponent wins the match)
        # -----------------------------------------------------------------
        if hero_lost_count == n:
            V = 0.0
            # Terminal state - create dummy solution
            solution = GameSolution(
                value=V,
                hero_names=[],
                opp_names=[opp_index_to_name[i] for i in opp_remaining],
                hero_strategy=[],
                opp_strategy=[(opp_index_to_name[i], 1.0/len(opp_remaining)) for i in opp_remaining] if opp_remaining else []
            )

        # -----------------------------------------------------------------
        # CASE 2: Opponent has lost with all decks (Hero wins the match)
        # -----------------------------------------------------------------
        elif opp_lost_count == n:
            V = 1.0
            # Terminal state - create dummy solution
            solution = GameSolution(
                value=V,
                hero_names=[hero_index_to_name[i] for i in hero_remaining],
                opp_names=[],
                hero_strategy=[(hero_index_to_name[i], 1.0/len(hero_remaining)) for i in hero_remaining] if hero_remaining else [],
                opp_strategy=[]
            )

        # -----------------------------------------------------------------
        # CASE 3: Hero has n-1 losses (one deck left)
        # -----------------------------------------------------------------
        # Hero has one deck remaining. They win if that deck beats all
        # of opponent's remaining decks in a row.
        # P(Hero wins) = product of W[hero_deck, opp_decks]
        # -----------------------------------------------------------------
        elif hero_lost_count == n - 1:
            prob_win_all = 1.0
            for h in hero_remaining:
                for o in opp_remaining:
                    prob_win_all *= W[h, o]

            V = prob_win_all
            # Near-terminal - Hero has one deck, must play it
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
                for o in opp_remaining:
                    prob_lose_all *= (1 - W[h, o])

            V = 1.0 - prob_lose_all
            # Near-terminal - Opponent has one deck
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
        # CASE 5: General case - need to solve deck selection game
        # -----------------------------------------------------------------
        else:
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
                        state_winrates, hero_lost, new_opp_lost, h, None
                    )

                    # If Hero loses: Hero's deck h is eliminated
                    # Opponent becomes forced to play deck o
                    new_hero_lost = frozenset(hero_lost | {h})
                    lose_value = lookup_state(
                        state_winrates, new_hero_lost, opp_lost, None, o
                    )

                    G[0, idx_o] = W[h, o] * win_value + (1 - W[h, o]) * lose_value

                G_hero_names = [hero_index_to_name[h]]
                G_opp_names = [opp_index_to_name[i] for i in opp_remaining]
                solution = solve_game(G, G_hero_names, G_opp_names, _cache=_cache)
                V = solution.value

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
                        state_winrates, hero_lost, new_opp_lost, h, None
                    )

                    # If Hero loses: Hero's deck h is eliminated
                    # Opponent stays forced to play deck o
                    new_hero_lost = frozenset(hero_lost | {h})
                    lose_value = lookup_state(
                        state_winrates, new_hero_lost, opp_lost, None, o
                    )

                    G[idx_h, 0] = W[h, o] * win_value + (1 - W[h, o]) * lose_value

                G_hero_names = [hero_index_to_name[i] for i in hero_remaining]
                G_opp_names = [opp_index_to_name[o]]
                solution = solve_game(G, G_hero_names, G_opp_names, _cache=_cache)
                V = solution.value

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
                            state_winrates, hero_lost, new_opp_lost, h, None
                        )

                        # If Hero loses: Hero's deck h is eliminated
                        # Opponent becomes forced to play deck o
                        new_hero_lost = frozenset(hero_lost | {h})
                        lose_value = lookup_state(
                            state_winrates, new_hero_lost, opp_lost, None, o
                        )

                        G[idx_h, idx_o] = W[h, o] * win_value + (1 - W[h, o]) * lose_value

                G_hero_names = [hero_index_to_name[i] for i in hero_remaining]
                G_opp_names = [opp_index_to_name[i] for i in opp_remaining]
                solution = solve_game(G, G_hero_names, G_opp_names, _cache=_cache)
                V = solution.value

        # Store winrate for lookup by future states
        state_key = (hero_lost, opp_lost, havetoplay_hero, havetoplay_opp)
        state_winrates[state_key] = V

        # Create state solution
        new_state = LHSStateSolution(
            hero_names=hero_names,
            opp_names=opp_names,
            hero_lost=hero_lost,
            opp_lost=opp_lost,
            havetoplay_hero=havetoplay_hero,
            havetoplay_opp=havetoplay_opp,
            solution=solution
        )
        results_list.append(new_state)

    return LHSResult(results_list, hero_names, opp_names)

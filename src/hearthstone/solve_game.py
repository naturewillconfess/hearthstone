import numpy as np
import highspy
from itertools import combinations
from typing import List, Optional, Dict
from .results import GameSolution


def prewarm_cache(W: np.ndarray, max_dim: int = 3) -> Dict[bytes, tuple]:
    """
    Pre-compute solutions for submatrices of W up to max_dim x max_dim.

    Parameters
    ----------
    W : np.ndarray
        The winrate matrix.
    max_dim : int
        Maximum dimension to cache. For lineup_picker with lineup_size k
        and b bans, use max_dim = k - b (the conquest game size after bans).

    Returns a cache dict mapping W.tobytes() -> (value, hero_probs, opp_probs).
    """
    W = np.asarray(W, dtype=float)
    n_rows, n_cols = W.shape
    cache = {}

    for m in range(1, min(max_dim, n_rows) + 1):
        for n in range(1, min(max_dim, n_cols) + 1):
            for rows in combinations(range(n_rows), m):
                for cols in combinations(range(n_cols), n):
                    W_sub = W[np.ix_(rows, cols)]
                    key = W_sub.tobytes()
                    if key not in cache:
                        result = _solve_core(W_sub)
                        cache[key] = result

    return cache


def _solve_1xn(W: np.ndarray) -> tuple:
    """
    Solve a 1×N zero-sum game analytically.

    Hero has only 1 strategy. Opponent picks the column that minimizes
    Hero's payoff.

    Returns (value, hero_probs, opp_probs).
    """
    n = W.shape[1]
    j_star = np.argmin(W[0, :])
    V = W[0, j_star]

    hero_probs = np.array([1.0])
    opp_probs = np.zeros(n)
    opp_probs[j_star] = 1.0

    return V, hero_probs, opp_probs


def _solve_nx1(W: np.ndarray) -> tuple:
    """
    Solve an N×1 zero-sum game analytically.

    Opponent has only 1 strategy. Hero picks the row that maximizes
    their payoff.

    Returns (value, hero_probs, opp_probs).
    """
    m = W.shape[0]
    i_star = np.argmax(W[:, 0])
    V = W[i_star, 0]

    hero_probs = np.zeros(m)
    hero_probs[i_star] = 1.0
    opp_probs = np.array([1.0])

    return V, hero_probs, opp_probs


def _solve_2x2(W: np.ndarray) -> tuple:
    """
    Solve a 2x2 zero-sum game analytically.

    Returns (value, hero_probs, opp_probs).
    """
    a, b = W[0, 0], W[0, 1]
    c, d = W[1, 0], W[1, 1]

    # Check for saddle point (pure strategy equilibrium)
    row0_min = min(a, b)
    row1_min = min(c, d)
    maximin = max(row0_min, row1_min)

    col0_max = max(a, c)
    col1_max = max(b, d)
    minimax = min(col0_max, col1_max)

    if abs(maximin - minimax) < 1e-10:
        V = maximin
        if row0_min >= row1_min:
            hero_probs = np.array([1.0, 0.0])
        else:
            hero_probs = np.array([0.0, 1.0])
        if col0_max <= col1_max:
            opp_probs = np.array([1.0, 0.0])
        else:
            opp_probs = np.array([0.0, 1.0])
        return V, hero_probs, opp_probs

    # Mixed strategy equilibrium
    denom = a - b - c + d

    if abs(denom) < 1e-10:
        return (a + b + c + d) / 4, np.array([0.5, 0.5]), np.array([0.5, 0.5])

    p = np.clip((d - c) / denom, 0, 1)
    q = np.clip((d - b) / denom, 0, 1)
    V = (a * d - b * c) / denom

    return V, np.array([p, 1 - p]), np.array([q, 1 - q])


def _solve_3x3(W: np.ndarray) -> tuple:
    """
    Solve a 3x3 zero-sum game analytically using support enumeration.

    Checks equilibria in order: fully mixed, 2x3/3x2 subgames, 2x2 subgames.

    Returns (value, hero_probs, opp_probs).
    """
    # Try fully mixed equilibrium first
    # Hero indifferent: W @ q = V * [1,1,1]^T => (W[0,:]-W[1,:]) @ q = 0, etc.
    # System: A @ q = b where A has row differences plus sum constraint
    A_q = np.array([
        W[0, :] - W[1, :],
        W[1, :] - W[2, :],
        [1.0, 1.0, 1.0]
    ])
    b_q = np.array([0.0, 0.0, 1.0])

    try:
        q = np.linalg.solve(A_q, b_q)
        if (q > -1e-9).all() and (q < 1 + 1e-9).all():
            q = np.clip(q, 0, 1)
            q = q / q.sum()

            # Solve for p similarly
            A_p = np.array([
                W[:, 0] - W[:, 1],
                W[:, 1] - W[:, 2],
                [1.0, 1.0, 1.0]
            ])
            b_p = np.array([0.0, 0.0, 1.0])
            p = np.linalg.solve(A_p, b_p)

            if (p > -1e-9).all() and (p < 1 + 1e-9).all():
                p = np.clip(p, 0, 1)
                p = p / p.sum()
                V = W[0, :] @ q
                return V, p, q
    except np.linalg.LinAlgError:
        pass

    # Try 2x3 subgames (drop one row)
    for drop_row in range(3):
        rows = [r for r in range(3) if r != drop_row]
        W_sub = W[rows, :]
        V, p_sub, q = _solve_2xn(W_sub)

        # Verify dropped row isn't better for hero
        if W[drop_row, :] @ q <= V + 1e-9:
            p = np.zeros(3)
            p[rows[0]] = p_sub[0]
            p[rows[1]] = p_sub[1]
            return V, p, q

    # Try 3x2 subgames (drop one column)
    for drop_col in range(3):
        cols = [c for c in range(3) if c != drop_col]
        W_sub = W[:, cols]
        V, p, q_sub = _solve_mx2(W_sub)

        # Verify dropped column isn't better for opponent (worse for hero)
        if p @ W[:, drop_col] >= V - 1e-9:
            q = np.zeros(3)
            q[cols[0]] = q_sub[0]
            q[cols[1]] = q_sub[1]
            return V, p, q

    # Try 2x2 subgames (drop one row and one column)
    for drop_row in range(3):
        for drop_col in range(3):
            rows = [r for r in range(3) if r != drop_row]
            cols = [c for c in range(3) if c != drop_col]
            W_sub = W[np.ix_(rows, cols)]
            V, p_sub, q_sub = _solve_2x2(W_sub)

            p = np.zeros(3)
            p[rows[0]] = p_sub[0]
            p[rows[1]] = p_sub[1]
            q = np.zeros(3)
            q[cols[0]] = q_sub[0]
            q[cols[1]] = q_sub[1]

            # Verify equilibrium: dropped row not better, dropped col not worse
            if W[drop_row, :] @ q <= V + 1e-9 and p @ W[:, drop_col] >= V - 1e-9:
                return V, p, q

    # Fallback to LP (shouldn't reach here for valid games)
    return _solve_lp(W)


def _solve_mx2(W: np.ndarray) -> tuple:
    """
    Solve an Mx2 zero-sum game analytically.

    Uses geometric interpretation: opponent's strategy q ∈ [0,1] makes each row's
    payoff a linear function V_i(q) = W[i,1] + (W[i,0] - W[i,1]) * q.
    The upper envelope gives the game value; opponent minimizes it at a vertex.

    Returns (value, hero_probs, opp_probs).
    """
    m = W.shape[0]

    # V_i(q) = intercepts[i] + slopes[i] * q
    slopes = W[:, 0] - W[:, 1]
    intercepts = W[:, 1]

    # Candidate q values: endpoints and pairwise intersections
    candidates = [0.0, 1.0]
    for i in range(m):
        for j in range(i + 1, m):
            denom = slopes[i] - slopes[j]
            if abs(denom) > 1e-10:
                q = (intercepts[j] - intercepts[i]) / denom
                if 0 < q < 1:
                    candidates.append(q)

    # Find optimal q (minimizes max payoff)
    best_q = 0.0
    best_V = float('inf')
    for q in candidates:
        V = np.max(intercepts + slopes * q)
        if V < best_V - 1e-12:
            best_V = V
            best_q = q

    opp_probs = np.array([best_q, 1 - best_q])

    # Find active rows (those achieving maximum at best_q)
    values_at_best = intercepts + slopes * best_q
    active_indices = np.where(values_at_best >= best_V - 1e-9)[0]

    hero_probs = np.zeros(m)
    if len(active_indices) == 1:
        hero_probs[active_indices[0]] = 1.0
    else:
        # Mix over 2 rows to make opponent indifferent
        # p_i * slopes[i] + p_j * slopes[j] = 0, p_i + p_j = 1
        # => p_i = -slopes[j] / (slopes[i] - slopes[j])
        i, j = active_indices[0], active_indices[1]
        denom = slopes[i] - slopes[j]
        if abs(denom) > 1e-10:
            p_i = np.clip(-slopes[j] / denom, 0, 1)
            hero_probs[i] = p_i
            hero_probs[j] = 1 - p_i
        else:
            hero_probs[i] = 0.5
            hero_probs[j] = 0.5

    return best_V, hero_probs, opp_probs


def _solve_2xn(W: np.ndarray) -> tuple:
    """
    Solve a 2xN zero-sum game analytically.

    Uses geometric interpretation: hero's strategy p ∈ [0,1] makes each column's
    payoff a linear function V_j(p) = W[1,j] + (W[0,j] - W[1,j]) * p.
    The lower envelope gives the game value; hero maximizes it at a vertex.

    Returns (value, hero_probs, opp_probs).
    """
    n = W.shape[1]

    # V_j(p) = intercepts[j] + slopes[j] * p
    slopes = W[0, :] - W[1, :]
    intercepts = W[1, :]

    # Candidate p values: endpoints and pairwise intersections
    candidates = [0.0, 1.0]
    for i in range(n):
        for j in range(i + 1, n):
            denom = slopes[i] - slopes[j]
            if abs(denom) > 1e-10:
                p = (intercepts[j] - intercepts[i]) / denom
                if 0 < p < 1:
                    candidates.append(p)

    # Find optimal p (maximizes min payoff)
    best_p = 0.0
    best_V = float('-inf')
    for p in candidates:
        V = np.min(intercepts + slopes * p)
        if V > best_V + 1e-12:
            best_V = V
            best_p = p

    hero_probs = np.array([best_p, 1 - best_p])

    # Find active columns (those achieving minimum at best_p)
    values_at_best = intercepts + slopes * best_p
    active_indices = np.where(values_at_best <= best_V + 1e-9)[0]

    opp_probs = np.zeros(n)
    if len(active_indices) == 1:
        opp_probs[active_indices[0]] = 1.0
    else:
        # Mix over 2 columns to make hero indifferent
        i, j = active_indices[0], active_indices[1]
        denom = slopes[i] - slopes[j]
        if abs(denom) > 1e-10:
            q_i = np.clip(-slopes[j] / denom, 0, 1)
            opp_probs[i] = q_i
            opp_probs[j] = 1 - q_i
        else:
            opp_probs[i] = 0.5
            opp_probs[j] = 0.5

    return best_V, hero_probs, opp_probs


def _solve_lp(W: np.ndarray) -> tuple:
    """
    Solve a zero-sum game using linear programming (HiGHS via highspy).

    Returns (value, hero_probs, opp_probs).
    """
    m, n = W.shape

    h = highspy.Highs()
    h.setOptionValue('output_flag', False)

    inf = highspy.kHighsInf

    # Variables: q[0..n-1] in [0,1], v in [-inf, inf]
    lower = [0.0] * n + [-inf]
    upper = [1.0] * n + [inf]
    h.addVars(n + 1, lower, upper)

    # Objective: minimize v (last variable)
    for i in range(n):
        h.changeColCost(i, 0.0)
    h.changeColCost(n, 1.0)

    # Constraints: W[i,:] @ q - v <= 0  (for each row i)
    for i in range(m):
        indices = list(range(n + 1))
        values = list(W[i, :]) + [-1.0]
        h.addRow(-inf, 0.0, len(indices), indices, values)

    # Constraint: sum(q) = 1
    h.addRow(1.0, 1.0, n, list(range(n)), [1.0] * n)

    status = h.run()
    if h.getModelStatus() != highspy.HighsModelStatus.kOptimal:
        raise RuntimeError(f"HiGHS LP failed: {h.getModelStatus()}")

    sol = h.getSolution()

    opp_probs = np.array(sol.col_value[:n])
    V = sol.col_value[n]
    # Hero's strategy from dual values of inequality constraints
    hero_probs = np.array([-sol.row_dual[i] for i in range(m)])

    # Ensure non-negativity and normalize
    hero_probs = np.maximum(hero_probs, 0)
    opp_probs = np.maximum(opp_probs, 0)
    hero_probs = hero_probs / np.sum(hero_probs)
    opp_probs = opp_probs / np.sum(opp_probs)

    return V, hero_probs, opp_probs


def _solve_core(W: np.ndarray) -> tuple:
    """
    Core solver dispatch - returns (value, hero_probs, opp_probs).

    This is the internal function that selects the appropriate solver
    based on matrix dimensions. Used by both solve_game and prewarm_cache.
    """
    m, n = W.shape

    if m == 1:
        return _solve_1xn(W)
    elif n == 1:
        return _solve_nx1(W)
    elif m == 2 and n == 2:
        return _solve_2x2(W)
    elif m == 3 and n == 3:
        return _solve_3x3(W)
    elif n == 2:
        return _solve_mx2(W)
    elif m == 2:
        return _solve_2xn(W)
    else:
        return _solve_lp(W)


def solve_game(W: np.ndarray,
               hero_names: Optional[List[str]] = None,
               opp_names: Optional[List[str]] = None,
               _cache: Optional[Dict[bytes, tuple]] = None) -> GameSolution:
    """
    Find Nash equilibrium in mixed strategies for a zero-sum game.

    This function takes a payoff matrix W from the Hero's perspective and
    computes the optimal mixed strategies for both players along with the
    expected game value.

    Mathematical Background:
    ------------------------
    In a two-player zero-sum game, the Hero (row player) chooses a strategy i,
    and the Opponent (column player) chooses a strategy j. The payoff to the Hero
    is given by the matrix W[i,j], and the payoff to the Opponent is -W[i,j] 
    (to put it in terms of probabilities, the payoff to the Opponent is actually 1-W[i,j]).

    A Nash equilibrium in mixed strategies is a pair of probability distributions
    (p, q) over strategies such that neither player can improve their expected
    payoff by unilaterally changing their strategy.

    Linear Programming Formulation
    ------------------------------
    Hero's optimal mixed strategy is the result of the following LP::

        Maximize V
        Subject to:
            sum_i W[i,j] * p[i] >= V    for all j (opponent strategies)
            sum_i p[i] = 1              (probability constraint)
            p[i] >= 0                   (non-negativity)

    This means that the Hero maximizes his payoff V conditional on the equilibrium
    condition - that given Hero's strategy the Opponent can't use any strategy to
    get a worse outcome for Hero.

    For Opponent the LP is::

        Minimize V
        Subject to:
            sum_j W[i,j] * q[j] <= V    for all i (Hero strategies)
            sum_j q[j] = 1              (probability constraint)
            q[j] >= 0                   (non-negativity)

    Parameters
    ----------
    W : np.ndarray
        Winrate/payoff matrix of shape (m, n) where
        m = number of Hero's strategies (rows),
        n = number of Opponent's strategies (columns), and
        W[i,j] = Hero's payoff (or win probability) when Hero plays i
        and Opponent plays j.
    hero_names : list of str, optional
        Names for Hero's strategies (rows).
        Default: ['Deck 0', 'Deck 1', ...].
    opp_names : list of str, optional
        Names for Opponent's strategies (columns).
        Default: ['Deck 0', 'Deck 1', ...].

Returns
    -------
    GameSolution
        Object containing:

        - ``value``: float, the game value (Hero's winrate at equilibrium).
        - ``hero_strategy``: list of (name, probability) tuples.
        - ``opp_strategy``: list of (name, probability) tuples.
"""

    W = np.asarray(W, dtype=float)

    # Validate input
    if W.ndim != 2:
        raise ValueError(f"W must be a 2D array, got {W.ndim}D")
    if W.shape[0] == 0 or W.shape[1] == 0:
        raise ValueError(f"W must have at least 1 row and 1 column, got shape {W.shape}")

    m, n = W.shape

    # Default names
    if hero_names is None:
        hero_names = [f"Deck {i}" for i in range(m)]
    if opp_names is None:
        opp_names = [f"Deck {i}" for i in range(n)]

    # Validate names
    if len(hero_names) != m:
        raise ValueError(f"hero_names has {len(hero_names)} elements, expected {m}")
    if len(opp_names) != n:
        raise ValueError(f"opp_names has {len(opp_names)} elements, expected {n}")

    # =========================================================================
    # SOLVE THE GAME (with optional caching)
    # =========================================================================
    cache_key = W.tobytes() if _cache is not None else None

    if cache_key and cache_key in _cache:
        V, hero_sol, opp_sol = _cache[cache_key]
    else:
        V, hero_sol, opp_sol = _solve_core(W)
        if cache_key:
            _cache[cache_key] = (V, hero_sol, opp_sol)

    hero_strategy = list(zip(hero_names, hero_sol))
    opp_strategy = list(zip(opp_names, opp_sol))

    return GameSolution(
        value=V,
        hero_names=hero_names,
        opp_names=opp_names,
        hero_strategy=hero_strategy,
        opp_strategy=opp_strategy
    )
    
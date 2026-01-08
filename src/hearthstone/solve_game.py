import numpy as np
import highspy
from typing import List, Optional
from .results import GameSolution


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


def solve_game(W: np.ndarray,
               hero_names: Optional[List[str]] = None,
               opp_names: Optional[List[str]] = None) -> GameSolution:
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
    # SOLVE THE GAME
    # =========================================================================
    # Use analytical solutions for simple cases:
    # - 1xN: Hero has one strategy, opponent minimizes
    # - Nx1: Opponent has one strategy, hero maximizes
    # - 2x2: Most common case in backward induction
    # Otherwise use LP.
    # =========================================================================

    if m == 1:
        V, hero_sol, opp_sol = _solve_1xn(W)
    elif n == 1:
        V, hero_sol, opp_sol = _solve_nx1(W)
    elif m == 2 and n == 2:
        V, hero_sol, opp_sol = _solve_2x2(W)
    else:
        V, hero_sol, opp_sol = _solve_lp(W)

    hero_strategy = list(zip(hero_names, hero_sol))
    opp_strategy = list(zip(opp_names, opp_sol))

    return GameSolution(
        value=V,
        hero_names=hero_names,
        opp_names=opp_names,
        hero_strategy=hero_strategy,
        opp_strategy=opp_strategy
    )
    
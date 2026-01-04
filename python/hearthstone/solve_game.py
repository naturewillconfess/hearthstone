"""
solve_game.py - Zero-sum game solver using linear programming

This module provides the core Nash equilibrium solver for two-player zero-sum games.
It uses scipy's linear programming solver to find mixed strategy Nash equilibria.

Mathematical Background:
------------------------
In a two-player zero-sum game, the Hero (row player) chooses a strategy i,
and the Opponent (column player) chooses a strategy j. The payoff to the Hero
is given by the matrix W[i,j], and the payoff to the Opponent is -W[i,j] (zero-sum).

A Nash equilibrium in mixed strategies is a pair of probability distributions
(p, q) over strategies such that neither player can improve their expected
payoff by unilaterally changing their strategy.

The minimax theorem guarantees that for zero-sum games:
    max_p min_q (p^T W q) = min_q max_p (p^T W q) = V (the game value)

Linear Programming Formulation:
-------------------------------
We solve for the Hero's optimal mixed strategy by formulating the following LP:

    Maximize V
    Subject to:
        sum_i W[i,j] * p[i] >= V    for all j (opponent strategies)
        sum_i p[i] = 1              (probability constraint)
        p[i] >= 0                   (non-negativity)

The dual of this LP gives the Opponent's optimal strategy.

Note: scipy.linprog minimizes, so we negate the objective or reformulate.
We use the primal-dual relationship to extract both strategies.
"""

import numpy as np
from scipy.optimize import linprog


def solve_game(W: np.ndarray) -> dict:
    """
    Find Nash equilibrium in mixed strategies for a zero-sum game.

    This function takes a payoff matrix W from the Hero's perspective and
    computes the optimal mixed strategies for both players along with the
    expected game value.

    Parameters
    ----------
    W : np.ndarray
        Winrate/payoff matrix of shape (m, n) where:
        - m = number of Hero's strategies (rows)
        - n = number of Opponent's strategies (columns)
        - W[i,j] = Hero's payoff (or win probability) when Hero plays i
                   and Opponent plays j

    Returns
    -------
    dict
        A dictionary containing:
        - 'hero_sol': np.ndarray of shape (m,)
            Hero's optimal mixed strategy (probability distribution over rows)
        - 'opp_sol': np.ndarray of shape (n,)
            Opponent's optimal mixed strategy (probability distribution over columns)
        - 'V': float
            The value of the game (Hero's expected payoff under Nash equilibrium)

    Examples
    --------
    >>> import numpy as np
    >>> W = np.array([[0.5, 0.6], [0.4, 0.5]])
    >>> result = solve_game(W)
    >>> print(f"Game value: {result['V']:.4f}")
    >>> print(f"Hero strategy: {result['hero_sol']}")
    >>> print(f"Opponent strategy: {result['opp_sol']}")

    Notes
    -----
    The implementation uses scipy.optimize.linprog with the 'highs' method.
    We solve the LP from the Opponent's perspective (minimization problem)
    to naturally fit scipy's minimization framework, then extract both
    strategies from the primal and dual solutions.

    The LP formulation for the Opponent (column player) is:
        Minimize V
        Subject to:
            sum_j W[i,j] * q[j] <= V    for all i (hero strategies)
            sum_j q[j] = 1
            q[j] >= 0

    This is equivalent to: Minimize V subject to W @ q <= V * ones

    Rewritten in standard form with variables [q; V]:
        Minimize [0, 0, ..., 0, 1] @ [q; V]
        Subject to:
            W @ q - V <= 0          (payoff constraints)
            sum(q) = 1              (probability)
            q >= 0, V unconstrained
    """
    # Convert input to numpy array and get dimensions
    W = np.asarray(W, dtype=float)

    # m = number of Hero strategies (rows)
    # n = number of Opponent strategies (columns)
    m, n = W.shape

    # =========================================================================
    # LP FORMULATION
    # =========================================================================
    # We solve from the Opponent's perspective (minimizer) since scipy minimizes.
    #
    # Variables: x = [q_1, q_2, ..., q_n, V] where q is Opponent's mixed strategy
    #
    # Objective: minimize V (which is the last variable)
    #   c = [0, 0, ..., 0, 1]  (coefficients for objective)
    #
    # Inequality constraints (W @ q - V <= 0 for each Hero strategy i):
    #   For row i of W: sum_j W[i,j] * q[j] - V <= 0
    #   In matrix form: [W | -1] @ [q; V] <= 0
    #   So A_ub = [W, -ones] and b_ub = zeros
    #
    # Equality constraint (sum of probabilities = 1):
    #   sum_j q[j] = 1
    #   In matrix form: [1, 1, ..., 1, 0] @ [q; V] = 1
    #   So A_eq = [[1, 1, ..., 1, 0]] and b_eq = [1]
    #
    # Bounds:
    #   q[j] >= 0 for all j (probabilities are non-negative)
    #   V is unbounded (can be negative if Hero has losing position)
    # =========================================================================

    # Objective: minimize V (the (n+1)-th variable, 0-indexed as n)
    # c has n+1 elements: [0, 0, ..., 0, 1]
    c = np.zeros(n + 1)
    c[n] = 1  # Coefficient of 1 for V (last variable)

    # Inequality constraints: W @ q - V <= 0
    # For each Hero strategy i, we have: sum_j W[i,j] * q[j] - V <= 0
    # This ensures that no matter what Hero plays, their expected payoff <= V
    # A_ub has shape (m, n+1): each row is [W[i,:], -1]
    A_ub = np.hstack([W, -np.ones((m, 1))])
    b_ub = np.zeros(m)

    # Equality constraint: sum of probabilities = 1
    # [1, 1, ..., 1, 0] @ [q; V] = 1
    A_eq = np.zeros((1, n + 1))
    A_eq[0, :n] = 1  # Sum of q[j] for j = 0, ..., n-1
    b_eq = np.array([1.0])

    # Bounds for variables
    # q[j] >= 0 for j = 0, ..., n-1 (probability constraints)
    # V is unbounded (None, None)
    bounds = [(0, None)] * n + [(None, None)]

    # =========================================================================
    # SOLVE THE LINEAR PROGRAM
    # =========================================================================
    # Use the 'highs' method which is robust and efficient
    # The result contains:
    #   - x: optimal solution [q*, V*]
    #   - fun: optimal objective value (should equal V*)
    #   - eqlin: information about equality constraints including dual values
    #   - ineqlin: information about inequality constraints including dual values
    # =========================================================================

    result = linprog(
        c,                    # Objective coefficients
        A_ub=A_ub,           # Inequality constraint matrix
        b_ub=b_ub,           # Inequality constraint RHS
        A_eq=A_eq,           # Equality constraint matrix
        b_eq=b_eq,           # Equality constraint RHS
        bounds=bounds,        # Variable bounds
        method='highs'        # Use HiGHS solver (default in modern scipy)
    )

    # Check if optimization was successful
    if not result.success:
        raise RuntimeError(f"Linear programming failed: {result.message}")

    # =========================================================================
    # EXTRACT SOLUTIONS
    # =========================================================================
    # Primal solution: Opponent's mixed strategy q* and game value V*
    # Dual solution: Hero's mixed strategy p* comes from the dual variables
    #                associated with the inequality constraints
    #
    # In LP duality, the dual variable for constraint i represents the
    # "shadow price" - how much the objective would improve if we relaxed
    # that constraint. For our formulation, these dual variables give us
    # the Hero's optimal mixed strategy.
    # =========================================================================

    # Opponent's mixed strategy (first n variables)
    opp_sol = result.x[:n]

    # Game value (last variable)
    V = result.x[n]

    # Hero's mixed strategy from dual variables of inequality constraints
    # The 'ineqlin' attribute contains information about inequality constraints
    # The 'marginals' (or dual values) give the Hero's strategy
    # These are the dual variables (shadow prices) for the payoff constraints
    hero_sol = -result.ineqlin.marginals

    # Normalize Hero's strategy to ensure it sums to 1
    # (numerical precision might cause small deviations)
    hero_sum = np.sum(hero_sol)
    if hero_sum > 0:
        hero_sol = hero_sol / hero_sum

    # Ensure non-negativity (clip very small negative values from numerical error)
    hero_sol = np.maximum(hero_sol, 0)
    opp_sol = np.maximum(opp_sol, 0)

    # Re-normalize after clipping
    hero_sol = hero_sol / np.sum(hero_sol)
    opp_sol = opp_sol / np.sum(opp_sol)

    return {
        'hero_sol': hero_sol,
        'opp_sol': opp_sol,
        'V': V
    }

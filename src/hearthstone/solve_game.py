import numpy as np
from scipy.optimize import linprog


def solve_game(W: np.ndarray) -> dict:
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

    Linear Programming Formulation:
    -------------------------------
    Hero's optimal mixed strategy is the result of the following LP:

    Maximize V
    Subject to:
        sum_i W[i,j] * p[i] >= V    for all j (opponent strategies)
        sum_i p[i] = 1              (probability constraint)
        p[i] >= 0                   (non-negativity)

    This means that the Hero maximizes his payoff V conditional on the equilibrium condition - 
    that given Hero's strategy the Opponent can't use any strategy to get a worse outcome for Hero

    For Opponent the LP is:


    Minimize V
    Subject to:
        sum_j W[i,j] * q[j] <= V    for all i (Hero strategies)
        sum_j q[j] = 1              (probability constraint)
        q[j] >= 0                   (non-negativity)

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
            The value of the game (Hero's expected payoff under Nash equilibrium), Hero's winrate

    Examples
    --------
    >>> import numpy as np
    >>> W = np.array([[0.5, 0.6], [0.4, 0.5]])
    >>> result = solve_game(W)
    >>> print(f"Game value: {result['V']:.4f}")
    >>> print(f"Hero strategy: {result['hero_sol']}")
    >>> print(f"Opponent strategy: {result['opp_sol']}")
    """

    W = np.asarray(W, dtype=float)
    m, n = W.shape

    # =========================================================================
    # LP FORMULATION
    # =========================================================================
    # We solve from the Opponent's perspective (minimizer) since scipy minimizes.
    # Variables: x = [q_1, q_2, ..., q_n, V] where q is Opponent's mixed strategy, V is the value of the game
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
    #   V is unbounded
    # =========================================================================

    c = np.zeros(n + 1)
    c[n] = 1 

    A_ub = np.hstack([W, -np.ones((m, 1))])
    b_ub = np.zeros(m)

    A_eq = np.zeros((1, n + 1))
    A_eq[0, :n] = 1  # Sum of q[j] for j = 0, ..., n-1
    b_eq = np.array([1])
    bounds = [(0, 1)] * n + [(None, None)]

    result = linprog(
        c,                    # Objective coefficients
        A_ub=A_ub,           # Inequality constraint matrix
        b_ub=b_ub,           # Inequality constraint RHS
        A_eq=A_eq,           # Equality constraint matrix
        b_eq=b_eq,           # Equality constraint RHS
        bounds=bounds,        # Variable bounds
        method='highs'        # Use HiGHS solver (default in modern scipy)
    )

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

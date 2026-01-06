Game Theory Background
======================

This page explains the game-theoretic concepts underlying the hearthstone package.
Understanding these concepts will help you interpret the results and make better
strategic decisions.

Two-Player Zero-Sum Games
-------------------------

A **two-player zero-sum game** is a mathematical model of strategic interaction where:

- Two players make decisions simultaneously
- One player's gain equals the other player's loss
- The sum of payoffs is always zero (or constant)

In Hearthstone deck selection, this applies naturally:

- If you win, your opponent loses
- Your win probability + opponent's win probability = 100%

Payoff Matrix
^^^^^^^^^^^^^

The game is represented by a **payoff matrix** :math:`W`:

.. math::

   W = \begin{pmatrix}
   w_{11} & w_{12} & \cdots & w_{1n} \\
   w_{21} & w_{22} & \cdots & w_{2n} \\
   \vdots & \vdots & \ddots & \vdots \\
   w_{m1} & w_{m2} & \cdots & w_{mn}
   \end{pmatrix}

Where:

- :math:`m` = number of Hero's strategies (rows)
- :math:`n` = number of Opponent's strategies (columns)
- :math:`w_{ij}` = Hero's payoff (win probability) when Hero plays :math:`i` and Opponent plays :math:`j`

Pure vs Mixed Strategies
------------------------

Pure Strategies
^^^^^^^^^^^^^^^

A **pure strategy** means always playing the same choice. For example, "always
play Aggro deck" is a pure strategy.

Pure strategies are often exploitable: if the opponent knows you always play
Aggro, they can always choose their best counter.

Mixed Strategies
^^^^^^^^^^^^^^^^

A **mixed strategy** is a probability distribution over pure strategies. For
example, "play Aggro 40%, Midrange 35%, Control 25%" is a mixed strategy.

Mixed strategies are represented as vectors that sum to 1:

.. math::

   p = (p_1, p_2, \ldots, p_m) \quad \text{where} \quad \sum_{i=1}^{m} p_i = 1

Nash Equilibrium
----------------

A **Nash equilibrium** is a pair of strategies :math:`(p^*, q^*)` where neither
player can improve their expected payoff by unilaterally changing their strategy.

Formally, for Hero's strategy :math:`p^*` and Opponent's strategy :math:`q^*`:

.. math::

   p^{*T} W q^* \geq p^T W q^* \quad \text{for all valid } p

   p^{*T} W q^* \leq p^{*T} W q \quad \text{for all valid } q

Properties of Nash Equilibrium
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. **Existence**: Every finite two-player zero-sum game has at least one Nash equilibrium
2. **Interchangeability**: If there are multiple equilibria, mixing them yields another equilibrium
3. **Value**: All equilibria have the same value :math:`V = p^{*T} W q^*`

The Minimax Theorem
^^^^^^^^^^^^^^^^^^^

Von Neumann's **Minimax Theorem** (1928) states:

.. math::

   \max_p \min_q \, p^T W q = \min_q \max_p \, p^T W q = V

This means:

- Hero can guarantee at least :math:`V` by playing :math:`p^*`
- Opponent can guarantee Hero gets at most :math:`V` by playing :math:`q^*`
- The optimal strategies are in equilibrium

Linear Programming Formulation
------------------------------

Finding Nash equilibrium is equivalent to solving a linear program.

Hero's Problem (Maximizer)
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. math::

   \begin{align}
   \text{maximize} \quad & V \\
   \text{subject to} \quad & \sum_i W_{ij} \cdot p_i \geq V \quad \forall j \\
   & \sum_i p_i = 1 \\
   & p_i \geq 0 \quad \forall i
   \end{align}

The constraints ensure that against any opponent pure strategy :math:`j`,
Hero's expected payoff is at least :math:`V`.

Opponent's Problem (Minimizer)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. math::

   \begin{align}
   \text{minimize} \quad & V \\
   \text{subject to} \quad & \sum_j W_{ij} \cdot q_j \leq V \quad \forall i \\
   & \sum_j q_j = 1 \\
   & q_j \geq 0 \quad \forall j
   \end{align}

These are dual linear programs, guaranteeing the same optimal value :math:`V`.

Backward Induction
------------------

For multi-stage games like tournament matches, we use **backward induction**:

1. Start from terminal states (match is over)
2. Work backwards, computing optimal play at each state
3. At each state, the payoff depends on continuation values from solved states

This produces a **subgame-perfect equilibrium**: optimal play at every decision point,
not just the start of the match.

.. code-block:: text

   Backward Induction Process:

   Step 1: Solve terminal states (V=1 or V=0)
           │
           ▼
   Step 2: Solve near-terminal states using terminal values
           │
           ▼
   Step 3: Work upward, each state uses previously computed values
           │
           ▼
   Step 4: Finally solve initial state → overall match value

Extensive Form Games
--------------------

Tournament matches are **extensive form games**: games with sequential structure.

Game Tree
^^^^^^^^^

The game tree represents all possible sequences of play:

- **Nodes**: Decision points or game states
- **Edges**: Possible actions/choices
- **Leaves**: Terminal states with final payoffs

For a 3-deck Conquest match:

- Initial node: 0-0 (no decks eliminated)
- After one game: either 1-0 or 0-1
- ...continues until 3-x or x-3

State Space Complexity
^^^^^^^^^^^^^^^^^^^^^^

The number of states grows exponentially:

- **Conquest (n decks)**: :math:`(2^n)^2 - 1 = 4^n - 1` states
- **LHS (n decks)**: Even more due to "forced play" tracking

For typical tournament sizes:

==================  ==============  ===========
Decks per player    Conquest        LHS
==================  ==============  ===========
2                   15              ~30
3                   63              ~150
4                   255             ~700
==================  ==============  ===========

Interpreting Results
--------------------

Support of a Strategy
^^^^^^^^^^^^^^^^^^^^^

The **support** is the set of pure strategies with positive probability:

.. math::

   \text{supp}(p) = \{i : p_i > 0\}

At Nash equilibrium, all strategies in the support yield the same expected payoff :math:`V`.
Strategies not in the support yield lower payoff.

If ``hero_sol = [0.5, 0.5, 0.0]``, deck 2 is dominated and should never be played
in this situation.

Dominated Strategies
^^^^^^^^^^^^^^^^^^^^

A strategy is **dominated** if another strategy is always at least as good:

- **Strictly dominated**: Another strategy is always strictly better
- **Weakly dominated**: Another strategy is always at least as good, sometimes better

Dominated strategies have zero probability in equilibrium.

Further Reading
---------------

- von Neumann, J., & Morgenstern, O. (1944). *Theory of Games and Economic Behavior*
- Owen, G. (2013). *Game Theory* (4th ed.). Emerald Group Publishing
- Nisan, N., et al. (2007). *Algorithmic Game Theory*. Cambridge University Press

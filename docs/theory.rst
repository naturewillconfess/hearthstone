
Tournament Formats
===============

Conquest Format
-----

Conquest is the standard tournament format for competitive Hearthstone,
used in Hearthstone Masters and most major events.

Rules
^^^^^

1. Each player brings :math:`n` decks to the match
2. Before each game, both players **simultaneously** choose which deck to play
3. The **winner's** deck is eliminated (cannot be used again in this match)
4. The loser keeps their deck and can play it in future games
5. The first player to **win with all their decks** wins the match

Last Hero Standing (LHS)
-----

Last Hero Standing is an alternative tournament format where the winner
keeps playing their winning deck until it loses.

Rules
^^^^^

1. Each player brings :math:`n` decks to the match
2. Before each game, players choose which deck to play (with a constraint - see rule 4)
3. The **loser's** deck is eliminated (cannot be used again in this match)
4. The **winner** must keep playing the same deck until it loses
5. The first player to **eliminate all opponent's decks** wins the match


Game Theory Background
======================

Zero-Sum Games
-------------------------

A **zero-sum game** is a game in which the sum of the players' scores is constant, so a player can increase their score only be decreasing the score of their opponent.

In Hearthstone tournaments, this applies naturally:

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
play Aggro" is a pure strategy.

Pure strategies are often exploitable: if the opponent knows you always play
Aggro, they can always choose their best counter.

Mixed Strategies
^^^^^^^^^^^^^^^^

A **mixed strategy** is a probability distribution over pure strategies. For
example, "play Aggro 40% of the time, Midrange 35% of the time, Control remaining 25% of the time" is a mixed strategy.

Mixed strategies are represented as vectors that sum to 1:

.. math::

   p = (p_1, p_2, \ldots, p_m) \quad \text{where} \quad \sum_{i=1}^{m} p_i = 1

Nash Equilibrium
----------------

A **Nash equilibrium** is a pair of strategies :math:`(p^*, q^*)` where neither
player can improve their expected payoff by unilaterally changing their strategy.

Formally, in matrix form, for Hero's strategy :math:`p^*` and Opponent's strategy :math:`q^*`:

.. math::

   p^{*T} W q^* \geq p^T W q^* \quad \text{for all valid } p

   p^{*T} W q^* \leq p^{*T} W q \quad \text{for all valid } q



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

The first constraint means that the Opponent can't unilaterally (meaning conditional on Hero already picking a strategy) pick another strategy and increase their payoff
The second constraint means that all probabilities in the strategy sum up to 100%
the third constraint means that all probabilities are positive

:math:`V` here is the Hero's winrate

Opponent's Problem (Minimizer)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. math::

   \begin{align}
   \text{minimize} \quad & V \\
   \text{subject to} \quad & \sum_j W_{ij} \cdot q_j \leq V \quad \forall i \\
   & \sum_j q_j = 1 \\
   & q_j \geq 0 \quad \forall j
   \end{align}

This is basically the same problem but reformulated in terms of the Opponent.
It yields the same winrate for Hero :math:`V`.


Multi-stage games
------------------

Game Tree
^^^^^^^^^

The game tree represents all possible sequences of play:

- **Nodes**: Decision points or game states
- **Edges**: Possible actions/choices

For a 3-deck Conquest match:

- Initial node: 0-0 (no decks eliminated)
- After one game: either 1-0 or 0-1
- ...continues until 3-x or x-3

Backward Induction
------------------

For multi-stage games like tournament matches, we use **backward induction**:

Every time you make a decision (pick a deck to queue or ban), you move lower on a game tree. 

For example, after the ban phase in a Conquest tournament with 4 decks, you basically play a tournament with 3 decks and 0 bans.

All the choices form a 4x4 matrix (each player can ban any deck), and you use the LP formulation described above to compute the optimal ban strategy.



Same goes for these tournaments with 3 decks and 0 bans - after you queue a deck into a deck queued by the opponent:
1. with some probability :math:`w` you move to the node where the score is 1-0 and you're playing a 3 decks vs 2 decks Conquest
2. with probability :math:`1-w` you move to a state where you've lost and you're playing a 2 decks vs 3 decs Conquest

So, this initial node is a game where choices form a 3x3 matrix (each player can queue any deck), 
and the winrate in each point of this matrix depends on the winrate in the subsequent games mentioned in points 1 and 2, 
as well as the winrate in the actual game between the queued deck

Same goes for any other node in the game tree, except the 'leaves' of the tree, the terminal nodes where the match is already won or lost, so the winrate is already known (either 0 or 1).

So here's how we calculate the optimal strategy for these multi-stage tournaments: 

0. Map all the possible states that can happen
1. Start from terminal states (match is over, the winrate is either 0 or 1)
2. Work backwards, computing optimal strategy at each node of the game tree
3. At each state, the payoff from a pick or a ban depends on the payoff of the subgame we already analyzed, since we're working backwards

This produces a **subgame-perfect equilibrium**: optimal play at every decision point, not just the start of the match.

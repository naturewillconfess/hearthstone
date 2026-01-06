Quick Start
===========

The Winrate Matrix
------------------

The core input to all functions is a **winrate matrix** ``W``, where:

- ``W[i, j]`` = probability that your deck ``i`` beats opponent's deck ``j``
- Rows represent your decks (Hero)
- Columns represent opponent's decks
- Values should be between 0 and 1

.. code-block:: python

   import numpy as np

   # Example: 3 decks per player
   # Your decks: Aggro (0), Midrange (1), Control (2)
   # Their decks: Aggro (0), Midrange (1), Control (2)
   W = np.array([
       #  vs Aggro  vs Mid  vs Control
       [    0.50,   0.55,     0.40],  # Your Aggro
       [    0.45,   0.50,     0.55],  # Your Midrange
       [    0.60,   0.45,     0.50],  # Your Control
   ])

Solving a Single Game
---------------------

Use :func:`~hearthstone.solve_game` to find the Nash equilibrium for a single
game (or ladder with only those decks available):

.. code-block:: python

   from hearthstone import solve_game

   result = solve_game(W)

   print(f"Game value (your winrate): {result['V']:.2%}")
   print(f"Your optimal strategy: {result['hero_sol']}")
   print(f"Opponent's optimal strategy: {result['opp_sol']}")

Output::

   Game value (your winrate): 50.00%
   Your optimal strategy: [0.25 0.5  0.25]
   Opponent's optimal strategy: [0.25 0.5  0.25]

You could copy the Winrate Matrix from the latest Vicious Syndicate report, pass it to this funcation and discover the equilibrial strategy for current Ladder Meta.

Conquest Format
---------------

Use :func:`~hearthstone.conquest_nash` to analyze a Conquest match:

.. code-block:: python

   from hearthstone import conquest_nash

   result = conquest_nash(W)
   initial = result[-1]  # Initial state (no games played yet)

   print(f"Match winrate: {initial['winrate'][0]:.2%}")
   print(f"Your optimal deck selection probabilities: {initial['nash'][0]}")
   print(f"Opponent's optimal deck selection probabilities: {initial['nash'][1]}")

Output::

   Match winrate: 50.00%
   Your optimal deck selection probabilities: [0.33587652 0.33352085 0.33060263]
   Opponent's optimal deck selection probabilities: [0.33587652 0.33352085 0.33060263]

Last Hero Standing (LHS)
------------------------

Use :func:`~hearthstone.lhs_nash` for Last Hero Standing format:

.. code-block:: python

   from hearthstone import lhs_nash

   result = lhs_nash(W)

   # Find initial state (no losses, no forced plays)
   initial = result[-1]

   print(f"Match winrate: {initial['winrate'][0]:.2%}")
   print(f"Your optimal deck selection probabilities: {initial['nash'][0]}")
   print(f"Opponent's optimal deck selection probabilities: {initial['nash'][1]}")

Output::

   Match winrate: 50.00%
   Your optimal deck selection probabilities: [0.32713178 0.32868217 0.34418605]
   Opponent's optimal deck selection probabilities: [0.32713178 0.32868217 0.34418605]

Ban Phase
---------

Use :func:`~hearthstone.ban_nash` to analyze matches with bans:

.. code-block:: python

   from hearthstone import ban_nash

   # 4 decks, 1 ban each, Conquest format
   W4 = np.array([
       [0.55, 0.45, 0.60, 0.50],
       [0.50, 0.50, 0.50, 0.55],
       [0.40, 0.55, 0.45, 0.60],
       [0.45, 0.50, 0.55, 0.50],
   ])

   result = ban_nash(W4, bans=1, match_format='conquest')

   print(f"Winrate after optimal bans: {result['winrate'][0]:.2%}")
   print(f"Your optimal ban probabilities: {result['bans']['hero']}")
   print(f"Opponent's optimal ban probabilities: {result['bans']['opp']}")

Output::
   
   Winrate after optimal bans: 53.50%
   Your optimal ban probabilities: Your optimal ban probabilities: [0.80077186 0.19922814 0.         0.        ]
   Opponent's optimal ban probabilities: Opponent's optimal ban probabilities: [0.17841089 0.         0.82158911 0.        ]

See :doc:`../examples` for further details
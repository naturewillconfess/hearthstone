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
   deck_names = ['Aggro', 'Midrange', 'Control']

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

   result = solve_game(W, hero_names=deck_names, opp_names=deck_names)

   print(f"Game value (your winrate): {result.value:.2%}")
   print(f"Your optimal strategy: {result.hero_strategy}")
   print(f"Opponent's optimal strategy: {result.opp_strategy}")

Output::

   Game value (your winrate): 50.00%
   Your optimal strategy: [('Aggro', 0.25), ('Midrange', 0.5), ('Control', 0.25)]
   Opponent's optimal strategy: [('Aggro', 0.25), ('Midrange', 0.5), ('Control', 0.25)]

You can also just print the result for a nice summary:

.. code-block:: python

   print(result)

Output::

   Game Solution
   ==========================
   Value: 50.0%

   Hero Strategy:
     Aggro         25.0%
     Midrange      50.0%
     Control       25.0%

   Opponent Strategy:
     Aggro         25.0%
     Midrange      50.0%
     Control       25.0%

You could copy the Winrate Matrix from the latest Vicious Syndicate report, pass it to this function and discover the equilibrial strategy for current Ladder Meta.

Conquest Format
---------------

Use :func:`~hearthstone.conquest_nash` to analyze a Conquest match:

.. code-block:: python

   from hearthstone import conquest_nash

   result = conquest_nash(W, deck_names=deck_names)

   print(f"Match winrate: {result.winrate:.2%}")
   print(f"Your optimal deck selection: {result.hero_strategy}")
   print(f"Opponent's optimal deck selection: {result.opp_strategy}")

Output::

   Match winrate: 50.00%
   Your optimal deck selection: [('Aggro', 0.336), ('Midrange', 0.334), ('Control', 0.331)]
   Opponent's optimal deck selection: [('Aggro', 0.336), ('Midrange', 0.334), ('Control', 0.331)]

Accessing Mid-Match States
~~~~~~~~~~~~~~~~~~~~~~~~~~

You can query states after games have been played:

.. code-block:: python

   # After you won with Aggro
   state = result.get_state(hero_won=['Aggro'])
   print(f"Winrate after winning with Aggro: {state.winrate:.2%}")
   print(f"Your next deck choice: {state.hero_strategy}")

Last Hero Standing (LHS)
------------------------

Use :func:`~hearthstone.lhs_nash` for Last Hero Standing format:

.. code-block:: python

   from hearthstone import lhs_nash

   result = lhs_nash(W, deck_names=deck_names)

   print(f"Match winrate: {result.winrate:.2%}")
   print(f"Your optimal deck selection: {result.hero_strategy}")
   print(f"Opponent's optimal deck selection: {result.opp_strategy}")

Output::

   Match winrate: 50.00%
   Your optimal deck selection: [('Aggro', 0.327), ('Midrange', 0.329), ('Control', 0.344)]
   Opponent's optimal deck selection: [('Aggro', 0.327), ('Midrange', 0.329), ('Control', 0.344)]

In LHS, when accessing mid-match states, you can specify forced plays:

.. code-block:: python

   # After you lost with Aggro and opponent is forced to play their winning deck
   state = result.get_state(hero_lost=['Aggro'], forced_opp='Midrange')
   print(f"Winrate: {state.winrate:.2%}")

Ban Phase
---------

Use :func:`~hearthstone.ban_nash` to analyze matches with bans:

.. code-block:: python

   from hearthstone import ban_nash
   import numpy as np

   # 4 decks, 1 ban each, Conquest format
   deck_names_4 = ['Aggro', 'Midrange', 'Control', 'Combo']

   W4 = np.array([
       [0.55, 0.45, 0.60, 0.50],
       [0.50, 0.50, 0.50, 0.55],
       [0.40, 0.55, 0.45, 0.60],
       [0.45, 0.50, 0.55, 0.50],
   ])

   result = ban_nash(W4, bans=1, match_format='conquest',
                     deck_names=deck_names_4)

   print(f"Winrate after optimal bans: {result.winrate:.2%}")
   print(f"Your optimal ban strategy: {result.hero_ban_strategy}")

Output::

   Winrate after optimal bans: 53.50%
   Your optimal ban strategy: [(('Aggro',), 0.80), (('Midrange',), 0.20), ...]

Getting Match Analysis After Specific Bans
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can get the full match analysis for any specific ban combination:

.. code-block:: python

   # Get the match after you ban Combo and opponent bans Control
   match = result.get_match(hero_bans=['Combo'], opp_bans=['Control'])
   print(f"Match winrate after these bans: {match.winrate:.2%}")
   print(f"Your deck selection: {match.hero_strategy}")

See :doc:`reference` for the complete API reference.

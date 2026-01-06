API Reference
=============

This page contains detailed documentation for all public functions and classes
in the hearthstone package.

Functions
---------

solve_game
~~~~~~~~~~

.. autofunction:: hearthstone.solve_game

**Example:**

.. code-block:: python

   import numpy as np
   from hearthstone import solve_game

   # Rock-paper-scissors style game
   W = np.array([
       [0.5, 0.7, 0.3],
       [0.3, 0.5, 0.7],
       [0.7, 0.3, 0.5],
   ])

   result = solve_game(W, hero_names=['Rock', 'Paper', 'Scissors'],
                       opp_names=['Rock', 'Paper', 'Scissors'])

   print(f"Game value: {result.value:.4f}")
   print(f"Hero strategy: {result.hero_strategy}")
   print(result)  # Pretty-printed table

   # Output:
   # Game value: 0.5000
   # Hero strategy: [('Rock', 0.333), ('Paper', 0.333), ('Scissors', 0.333)]

conquest_nash
~~~~~~~~~~~~~

.. autofunction:: hearthstone.conquest_nash

**Example:**

.. code-block:: python

   import numpy as np
   from hearthstone import conquest_nash

   W = np.array([
       [0.55, 0.45, 0.60],
       [0.50, 0.50, 0.50],
       [0.40, 0.55, 0.45],
   ])

   result = conquest_nash(W, deck_names=['Aggro', 'Midrange', 'Control'])

   # Easy access to initial state
   print(f"Match winrate: {result.winrate:.2%}")
   print(f"Deck selection: {result.hero_strategy}")

   # Access specific mid-match states
   state = result.get_state(hero_won=['Aggro'], opp_won=['Midrange'])
   print(f"Winrate after Aggro vs Midrange: {state.winrate:.2%}")

   # Print all states
   print(result)

lhs_nash
~~~~~~~~

.. autofunction:: hearthstone.lhs_nash

**Example:**

.. code-block:: python

   import numpy as np
   from hearthstone import lhs_nash

   W = np.array([
       [0.55, 0.45, 0.60],
       [0.50, 0.50, 0.50],
       [0.40, 0.55, 0.45],
   ])

   result = lhs_nash(W, deck_names=['Aggro', 'Midrange', 'Control'])

   # Easy access to initial state
   print(f"Match winrate: {result.winrate:.2%}")
   print(f"Deck selection: {result.hero_strategy}")

   # Access state with forced play
   state = result.get_state(hero_lost=['Aggro'], forced_opp='Control')
   print(f"Winrate in this situation: {state.winrate:.2%}")

ban_nash
~~~~~~~~

.. autofunction:: hearthstone.ban_nash

**Example:**

.. code-block:: python

   import numpy as np
   from hearthstone import ban_nash

   # 4 decks per player
   W = np.array([
       [0.55, 0.45, 0.60, 0.50],
       [0.50, 0.50, 0.50, 0.55],
       [0.40, 0.55, 0.45, 0.60],
       [0.45, 0.50, 0.55, 0.50],
   ])

   deck_names = ['Aggro', 'Midrange', 'Control', 'Combo']

   # Conquest with 1 ban
   result = ban_nash(W, bans=1, match_format='conquest',
                     deck_names=deck_names)

   print(f"Winrate after bans: {result.winrate:.2%}")
   print(f"Your ban strategy: {result.hero_ban_strategy}")

   # Get match analysis for specific bans
   match = result.get_match(hero_bans=['Combo'], opp_bans=['Aggro'])
   print(f"Match winrate if you ban Combo and opponent bans Aggro: {match.winrate:.2%}")

Result Classes
--------------

GameSolution
~~~~~~~~~~~~

.. autoclass:: hearthstone.GameSolution
   :members:
   :undoc-members:

ConquestResult
~~~~~~~~~~~~~~

.. autoclass:: hearthstone.ConquestResult
   :members:
   :undoc-members:

ConquestState
~~~~~~~~~~~~~

.. autoclass:: hearthstone.ConquestState
   :members:
   :undoc-members:

LHSResult
~~~~~~~~~

.. autoclass:: hearthstone.LHSResult
   :members:
   :undoc-members:

LHSState
~~~~~~~~

.. autoclass:: hearthstone.LHSState
   :members:
   :undoc-members:

BanResult
~~~~~~~~~

.. autoclass:: hearthstone.BanResult
   :members:
   :undoc-members:

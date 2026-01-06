Function Reference
==================

This page contains detailed documentation for all public functions in the
hearthstone package.

solve_game
----------

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

   result = solve_game(W)
   print(f"Game value: {result['V']:.4f}")
   print(f"Hero strategy: {result['hero_sol']}")
   print(f"Opponent strategy: {result['opp_sol']}")

   # Output:
   # Game value: 0.5000
   # Hero strategy: [0.333 0.333 0.333]
   # Opponent strategy: [0.333 0.333 0.333]

conquest_nash
-------------

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

   result = conquest_nash(W)

   # Get initial state (always last element)
   initial = result[-1]
   print(f"Match winrate: {initial['winrate'][0]:.2%}")
   print(f"Deck selection: {initial['nash'][0]}")

   # Access specific states
   for state in result:
       if state['score'] == ((0,), (1,)):
           print(f"\nState (0,)-(1,): Hero won with 0, Opp won with 1")
           print(f"  Winrate: {state['winrate'][0]:.2%}")

lhs_nash
--------

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

   result = lhs_nash(W)

   # Find initial state (no losses, no forced play)
   for state in result:
       if (state['score'] == ((), ()) and
           state.get('havetoplay_hero') is None and
           state.get('havetoplay_opp') is None):
           initial = state
           break

   print(f"Match winrate: {initial['winrate'][0]:.2%}")

   # Find forced-play states
   forced_hero = [s for s in result if s.get('havetoplay_hero') is not None]
   print(f"States where Hero is forced: {len(forced_hero)}")

ban_nash
--------

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

   # Conquest with 1 ban
   result = ban_nash(W, bans=1, match_format='conquest')

   print(f"Winrate after bans: {result['winrate'][0]:.2%}")
   print(f"Ban options: {result['stratlist']['hero']}")
   print(f"Your ban probabilities: {result['bans']['hero']}")

   # LHS with 1 ban
   result_lhs = ban_nash(W, bans=1, match_format='lhs')
   print(f"LHS winrate: {result_lhs['winrate'][0]:.2%}")

   # With 2 bans
   result_2ban = ban_nash(W, bans=2, match_format='conquest')
   print(f"2-ban options: {result_2ban['stratlist']['hero']}")

Practical Examples
==================

This page demonstrates real-world usage of the hearthstone package for
tournament preparation and analysis.

Tournament Preparation
----------------------

Preparing for a tournament match against a known opponent.

.. code-block:: python

   import numpy as np
   from hearthstone import ban_nash

   # Define your estimated winrates
   # Rows: Your decks (Aggro Druid, Ramp Druid, Control Warrior, Combo Mage)
   # Cols: Their decks (Zoo Lock, Reno Lock, Pirate Warrior, Freeze Mage)
   deck_names_hero = ['Aggro Druid', 'Ramp Druid', 'Control Warrior', 'Combo Mage']
   deck_names_opp = ['Zoo Lock', 'Reno Lock', 'Pirate Warrior', 'Freeze Mage']

   W = np.array([
       [0.55, 0.40, 0.60, 0.45],  # Aggro Druid
       [0.45, 0.55, 0.50, 0.60],  # Ramp Druid
       [0.60, 0.45, 0.50, 0.55],  # Control Warrior
       [0.40, 0.60, 0.45, 0.50],  # Combo Mage
   ])

   # Analyze with 1 ban (Conquest format)
   result = ban_nash(W, bans=1, match_format='conquest')

   print(f"Expected match winrate: {result['winrate'][0]:.1%}\n")

   print("Your optimal ban strategy:")
   for ban_combo, prob in zip(result['stratlist']['hero'], result['bans']['hero']):
       if prob > 0.01:
           banned_deck = deck_names_opp[ban_combo[0]]
           print(f"  Ban {banned_deck}: {prob:.1%}")

   print("\nTheir likely ban against you:")
   for ban_combo, prob in zip(result['stratlist']['opp'], result['bans']['opp']):
       if prob > 0.01:
           banned_deck = deck_names_hero[ban_combo[0]]
           print(f"  Ban {banned_deck}: {prob:.1%}")

Comparing Formats
-----------------

Determine whether a lineup performs better in Conquest or LHS.

.. code-block:: python

   import numpy as np
   from hearthstone import conquest_nash, lhs_nash

   # Lineup with one dominant deck
   W_dominant = np.array([
       [0.70, 0.65, 0.60],  # Very strong deck
       [0.45, 0.50, 0.45],  # Average deck
       [0.40, 0.45, 0.50],  # Weak deck
   ])

   conquest_result = conquest_nash(W_dominant)[-1]

   lhs_result = lhs_nash(W_dominant)
   lhs_initial = [s for s in lhs_result
                  if s['score'] == ((), ())
                  and s.get('havetoplay_hero') is None][-1]

   print("Lineup with one dominant deck:")
   print(f"  Conquest winrate: {conquest_result['winrate'][0]:.1%}")
   print(f"  LHS winrate: {lhs_initial['winrate'][0]:.1%}")

   # Balanced lineup
   W_balanced = np.array([
       [0.55, 0.50, 0.45],
       [0.50, 0.55, 0.50],
       [0.45, 0.50, 0.55],
   ])

   conquest_balanced = conquest_nash(W_balanced)[-1]
   lhs_balanced = lhs_nash(W_balanced)
   lhs_balanced_init = [s for s in lhs_balanced
                        if s['score'] == ((), ())
                        and s.get('havetoplay_hero') is None][-1]

   print("\nBalanced lineup:")
   print(f"  Conquest winrate: {conquest_balanced['winrate'][0]:.1%}")
   print(f"  LHS winrate: {lhs_balanced_init['winrate'][0]:.1%}")

Analyzing Mid-Match Situations
------------------------------

Compute optimal play from any point in a match.

.. code-block:: python

   import numpy as np
   from hearthstone import conquest_nash

   W = np.array([
       [0.55, 0.45, 0.60],
       [0.50, 0.50, 0.50],
       [0.40, 0.55, 0.45],
   ])

   result = conquest_nash(W)

   # Helper to find specific state
   def find_state(hero_won, opp_won):
       hero_set = tuple(sorted(hero_won))
       opp_set = tuple(sorted(opp_won))
       for state in result:
           if state['score'] == (hero_set, opp_set):
               return state
       return None

   # Scenario: You've won with deck 0, opponent has won with deck 2
   state = find_state([0], [2])

   print("Current situation: You won with deck 0, opponent won with deck 2")
   print(f"Your win probability from here: {state['winrate'][0]:.1%}")
   print(f"Your remaining decks: 1, 2")
   print(f"Opponent's remaining decks: 0, 1")
   print(f"\nOptimal next deck selection:")
   print(f"  Deck 1: {state['nash'][0][0]:.1%}")
   print(f"  Deck 2: {state['nash'][0][1]:.1%}")

Sensitivity Analysis
--------------------

See how matchup changes affect overall winrate.

.. code-block:: python

   import numpy as np
   from hearthstone import conquest_nash

   # Base lineup
   W_base = np.array([
       [0.50, 0.50, 0.50],
       [0.50, 0.50, 0.50],
       [0.50, 0.50, 0.50],
   ])

   base_wr = conquest_nash(W_base)[-1]['winrate'][0]
   print(f"Base winrate (all 50/50): {base_wr:.1%}\n")

   print("Effect of improving one matchup by 10%:")

   for i in range(3):
       for j in range(3):
           W_test = W_base.copy()
           W_test[i, j] = 0.60  # Improve this matchup

           new_wr = conquest_nash(W_test)[-1]['winrate'][0]
           delta = (new_wr - base_wr) * 100

           print(f"  W[{i},{j}] -> 60%: winrate = {new_wr:.1%} ({delta:+.1f}%)")

Ladder Analysis
---------------

For ladder (single game, repeated), use ``solve_game`` directly.

.. code-block:: python

   import numpy as np
   from hearthstone import solve_game

   # Meta winrates against the field
   # Rows: Your deck choice
   # Cols: Expected opponent distribution
   meta = np.array([
       #  Aggro  Mid  Control  Combo  (opponent prevalence: 30%, 25%, 25%, 20%)
       [   0.50, 0.55,   0.40,  0.60],  # Your Aggro
       [   0.45, 0.50,   0.55,  0.45],  # Your Midrange
       [   0.60, 0.45,   0.50,  0.55],  # Your Control
   ])

   # Weight by opponent prevalence
   opp_prevalence = np.array([0.30, 0.25, 0.25, 0.20])

   # Expected winrate for each of your decks
   expected_wr = meta @ opp_prevalence
   best_deck = np.argmax(expected_wr)

   deck_names = ['Aggro', 'Midrange', 'Control']
   print("Expected winrates on ladder:")
   for i, name in enumerate(deck_names):
       print(f"  {name}: {expected_wr[i]:.1%}")
   print(f"\nBest ladder deck: {deck_names[best_deck]} ({expected_wr[best_deck]:.1%})")

   # If you expect the opponent to adapt, solve the game
   result = solve_game(meta)
   print(f"\nAgainst adaptive opponent:")
   print(f"  Game value: {result['V']:.1%}")
   print(f"  Your optimal mix: {result['hero_sol']}")

Monte Carlo Validation
----------------------

Validate Nash equilibrium through simulation.

.. code-block:: python

   import numpy as np
   from hearthstone import solve_game

   def simulate_game(W, hero_strategy, opp_strategy, n_games=10000):
       """Simulate games and return Hero's empirical winrate."""
       wins = 0
       for _ in range(n_games):
           # Sample strategies according to mixed strategy
           hero_deck = np.random.choice(len(hero_strategy), p=hero_strategy)
           opp_deck = np.random.choice(len(opp_strategy), p=opp_strategy)

           # Simulate game outcome
           if np.random.random() < W[hero_deck, opp_deck]:
               wins += 1

       return wins / n_games

   # Create a test game
   W = np.array([
       [0.5, 0.7, 0.3],
       [0.3, 0.5, 0.7],
       [0.7, 0.3, 0.5],
   ])

   result = solve_game(W)
   print(f"Theoretical game value: {result['V']:.4f}")
   print(f"Hero Nash strategy: {result['hero_sol']}")
   print(f"Opponent Nash strategy: {result['opp_sol']}")

   # Simulate with Nash strategies
   empirical_wr = simulate_game(W, result['hero_sol'], result['opp_sol'])
   print(f"\nSimulated winrate (Nash vs Nash): {empirical_wr:.4f}")

   # Try exploiting with pure strategies
   for pure_hero in range(3):
       hero_pure = np.zeros(3)
       hero_pure[pure_hero] = 1.0
       exploit_wr = simulate_game(W, hero_pure, result['opp_sol'])
       print(f"Hero pure {pure_hero} vs Nash Opp: {exploit_wr:.4f}")

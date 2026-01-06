Last Hero Standing (LHS)
========================

Last Hero Standing is an alternative tournament format where the winner
keeps playing their winning deck until it loses.

Rules
-----

1. Each player brings :math:`n` decks to the match
2. Before each game, players choose which deck to play (with a constraint - see rule 4)
3. The **loser's** deck is eliminated (cannot be used again in this match)
4. The **winner** must keep playing the same deck until it loses
5. The first player to **eliminate all opponent's decks** wins the match

Key Strategic Insight
^^^^^^^^^^^^^^^^^^^^^

In LHS, a single dominant deck can "run the table":

- If your deck wins, you're forced to keep using it
- A winning streak with one deck can eliminate multiple opponent decks
- Having one very strong deck is valuable (unlike Conquest where you need all to win)

The "Forced Play" Mechanic
^^^^^^^^^^^^^^^^^^^^^^^^^^

After winning a game, you **must** play the same deck again. This creates
asymmetric game states:

- If Hero won the last game, Hero is "forced" to play their winning deck
- If Opponent won the last game, Opponent is "forced"
- At the start of the match, neither player is forced

Game Tree
---------

The following shows a simplified 2-deck LHS match:

.. code-block:: text

                           (0-0)
                    ┌───────┴───────┐
                 H wins           H loses
                    │               │
            H forced to deck    O forced to deck
              (0-1)               (1-0)
          ┌────┴────┐         ┌────┴────┐
       H wins    H loses   H wins    H loses
          │         │         │         │
       (0-2)     (1-1)     (1-1)     (2-0)
       Hero       H free   H forced   Opp
       Wins       to pick  to deck   Wins
                     │
                  (continues)

Note: ``(x-y)`` represents Hero lost x decks, Opponent lost y decks.

State Representation
--------------------

An LHS state requires tracking both eliminations AND forced play:

- ``hero_lost``: Set of deck indices Hero has lost with (eliminated)
- ``opp_lost``: Set of deck indices Opponent has lost with (eliminated)
- ``havetoplay_hero``: Deck index Hero must play (or None)
- ``havetoplay_opp``: Deck index Opponent must play (or None)

**Important**: At most one player can be "forced" at any time (the previous game's winner).

Terminal States
^^^^^^^^^^^^^^^

- **Hero wins**: ``len(opp_lost) == n`` (all opponent decks eliminated)
- **Opponent wins**: ``len(hero_lost) == n`` (all hero decks eliminated)

Near-Terminal Formula
^^^^^^^^^^^^^^^^^^^^^

When Hero has only one deck left (deck :math:`h`), they win if that deck beats
all remaining opponent decks **in a row** (since the winner keeps playing):

.. math::

   P(\text{Hero wins}) = \prod_{j \in \text{opp\_remaining}} W_{h,j}

This differs from Conquest! In LHS, you need consecutive wins.

Example Analysis
----------------

.. code-block:: python

   import numpy as np
   from hearthstone import lhs_nash

   # Winrate matrix
   W = np.array([
       [0.55, 0.45, 0.60],  # Your Aggro
       [0.50, 0.50, 0.50],  # Your Midrange
       [0.40, 0.55, 0.45],  # Your Control
   ])

   result = lhs_nash(W)

   # Find initial state (no losses, no forced play)
   initial = None
   for state in result:
       if (state['score'] == ((), ()) and
           state.get('havetoplay_hero') is None and
           state.get('havetoplay_opp') is None):
           initial = state
           break

   print(f"Match winrate: {initial['winrate'][0]:.2%}")
   print(f"Your deck selection: {initial['nash'][0]}")

   # Find states where Hero is forced to play
   for state in result:
       if state.get('havetoplay_hero') == 0 and state['score'] == ((), (0,)):
           print(f"\nHero won with deck 0, opp lost deck 0:")
           print(f"  Winrate from here: {state['winrate'][0]:.2%}")
           # Hero must play deck 0, opponent chooses from remaining

Interpreting Results
--------------------

The ``lhs_nash`` function returns a list of all game states. Each state contains:

- ``score``: Tuple of (hero_lost, opp_lost) as tuples
- ``havetoplay_hero``: Deck Hero must play (int or None)
- ``havetoplay_opp``: Deck Opponent must play (int or None)
- ``winrate``: Tuple (hero_wr, opp_wr)
- ``nash``: Optimal strategies for the player(s) who have a choice
- ``game``: The payoff matrix (may be 1xN or Mx1 if one player is forced)

Finding the Initial State
^^^^^^^^^^^^^^^^^^^^^^^^^

Unlike Conquest where ``result[-1]`` is always the initial state, in LHS you
need to search for the state with no losses and no forced plays:

.. code-block:: python

   def find_initial_state(result):
       for state in result:
           if (state['score'] == ((), ()) and
               state.get('havetoplay_hero') is None and
               state.get('havetoplay_opp') is None):
               return state
       return result[-1]

State Space Complexity
----------------------

LHS has significantly more states than Conquest because we track forced play:

- Each score state can have multiple "forced play" variations
- Example: score ``((), (0,))`` (opponent lost deck 0) could have Hero forced
  to deck 0, 1, or 2 (whoever won that game)

For a 3-deck match, LHS has roughly 2.5x more states than Conquest.

Strategic Implications
----------------------

1. **Hot hand matters**: A deck on a winning streak stays in play. Decks with
   consistent good matchups are more valuable.

2. **Counter-picking when free**: When you're not forced, you can choose the
   best deck against what opponent might play.

3. **Protect your forced deck**: If you're forced to play a deck, opponent
   can freely counter-pick.

4. **Snowball potential**: Unlike Conquest, a dominant deck can single-handedly
   win a match. One 70% deck might be better than three 50% decks.

Comparison with Conquest
------------------------

.. code-block:: python

   import numpy as np
   from hearthstone import conquest_nash, lhs_nash

   W = np.array([
       [0.70, 0.70, 0.70],  # Deck 0: very strong
       [0.45, 0.45, 0.45],  # Deck 1: weak
       [0.45, 0.45, 0.45],  # Deck 2: weak
   ])

   conquest = conquest_nash(W)[-1]
   lhs_result = lhs_nash(W)
   lhs_initial = [s for s in lhs_result
                  if s['score'] == ((), ())
                  and s.get('havetoplay_hero') is None][-1]

   print(f"Conquest winrate: {conquest['winrate'][0]:.2%}")
   print(f"LHS winrate: {lhs_initial['winrate'][0]:.2%}")

   # LHS often gives higher winrate when you have one dominant deck!

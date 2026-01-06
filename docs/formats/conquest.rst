Conquest Format
===============

Conquest is the standard tournament format for competitive Hearthstone,
used in Hearthstone Masters and most major events.

Rules
-----

1. Each player brings :math:`n` decks to the match
2. Before each game, both players **simultaneously** choose which deck to play
3. The **winner's** deck is eliminated (cannot be used again in this match)
4. The loser keeps their deck and can play it in future games
5. The first player to **win with all their decks** wins the match

Key Strategic Insight
^^^^^^^^^^^^^^^^^^^^^

In Conquest, you must win with **every** deck at least once. This means:

- You cannot avoid playing a weak deck forever
- A deck that's bad against the opponent's lineup will eventually need to win
- You can "protect" a weak deck by winning with other decks first

Game Tree
---------

The following diagram shows a simplified 2-deck Conquest match:

.. code-block:: text

                           (0-0)
                    ┌───────┴───────┐
                   Win             Lose
                    │               │
                  (1-0)           (0-1)
              ┌────┴────┐     ┌────┴────┐
             Win      Lose   Win      Lose
              │         │     │         │
           (2-0)      (1-1) (1-1)     (0-2)
           Hero        │     │        Opp
           Wins    ┌───┴───┐ │       Wins
                  Win    Lose│
                   │      │  │
                (2-1)  (1-2) │
                Hero    Opp  │
                Wins   Wins  │
                             └── (continues)

State Representation
--------------------

A game state is represented by which decks have been "won with" (eliminated):

- ``hero_won``: Set of deck indices Hero has won with
- ``opp_won``: Set of deck indices Opponent has won with

For example, in a 3-deck match:

- ``((), ())``: Initial state, no games played
- ``((0,), ())``: Hero won with deck 0, opponent has won with nothing
- ``((0, 1), (2,))``: Hero needs to win with deck 2; opponent needs decks 0 and 1

Terminal States
^^^^^^^^^^^^^^^

- **Hero wins**: ``hero_won = (0, 1, ..., n-1)`` (all decks)
- **Opponent wins**: ``opp_won = (0, 1, ..., n-1)`` (all decks)

Near-Terminal Formula
^^^^^^^^^^^^^^^^^^^^^

When Hero has only one deck left (deck :math:`h`), they must beat all remaining
opponent decks:

.. math::

   P(\text{Hero wins}) = 1 - \prod_{j \in \text{opp\_remaining}} (1 - W_{h,j})

This is because Hero wins if deck :math:`h` beats **at least one** remaining
opponent deck (since the match continues until all opponent decks are eliminated).

Example Analysis
----------------

.. code-block:: python

   import numpy as np
   from hearthstone import conquest_nash

   # Winrate matrix
   W = np.array([
       [0.55, 0.45, 0.60],  # Your Aggro
       [0.50, 0.50, 0.50],  # Your Midrange
       [0.40, 0.55, 0.45],  # Your Control
   ])

   result = conquest_nash(W)

   # Initial state (last element)
   initial = result[-1]
   print(f"Match winrate: {initial['winrate'][0]:.2%}")
   print(f"Your deck selection: {initial['nash'][0]}")

   # Find a specific state
   for state in result:
       if state['score'] == ((0,), ()):
           print(f"\nAfter winning with deck 0:")
           print(f"  Winrate from here: {state['winrate'][0]:.2%}")
           print(f"  Next deck choice: {state['nash'][0]}")

Interpreting Results
--------------------

The ``conquest_nash`` function returns a list of all game states, ordered from
deepest (most games played) to shallowest (initial state).

Each state dictionary contains:

- ``score``: Tuple of (hero_won, opp_won) as tuples
- ``winrate``: Tuple (hero_wr, opp_wr)
- ``nash``: Tuple (hero_strategy, opp_strategy) - optimal mixed strategies
- ``game``: The payoff matrix for deck selection at this state

Strategic Implications
----------------------

1. **Weak decks hurt more**: A deck with bad matchups across the board is a
   liability since you must eventually win with it.

2. **Counter-matchups matter**: Having polarized matchups (some very good,
   some very bad) is often better than all mediocre matchups.

3. **Order matters**: The sequence in which you eliminate decks affects
   remaining matchups. Optimal play considers continuation values.

4. **Mixed strategies**: Pure "always lead with X" strategies are often
   exploitable. Mixing deck selection prevents opponent adaptation.

Tournament Formats
==================

Hearthstone esports uses several tournament formats for matches. This package
supports the two most common formats used in competitive play.

.. toctree::
   :maxdepth: 2

   conquest
   lhs

Format Comparison
-----------------

==================  =================  =================
Feature             Conquest           Last Hero Standing
==================  =================  =================
Deck eliminated     Winner's deck      Loser's deck
Winning condition   Win with all       Eliminate all
                    your decks         opponent's decks
Forced play         No                 Yes (winner keeps)
Strategic focus     Deck sequencing    Deck resilience
==================  =================  =================

Which Format to Use
-------------------

**Conquest** (most common):

- Standard format for most Hearthstone Masters events
- Rewards having multiple strong decks
- Allows "protecting" a weak deck by winning with others first

**Last Hero Standing**:

- Used in some tournaments and custom events
- Rewards having one dominant deck
- "Hot streak" potential: a winning deck keeps playing

The Ban Phase
-------------

Both formats typically include a **ban phase** before the match:

1. Each player brings :math:`n` decks (usually 4)
2. Each player simultaneously bans :math:`k` opponent decks (usually 1)
3. The match proceeds with :math:`n-k` decks per player

Use :func:`~hearthstone.ban_nash` to compute optimal ban strategies.

Deck Pool Size
--------------

Common configurations:

- **Best of 3** (BO3): 2 decks, no bans (2v2)
- **Best of 5** (BO5): 3 decks, no bans (3v3)
- **Best of 5 with bans**: 4 decks, 1 ban (4v4 → 3v3)
- **Best of 7** (BO7): 4 decks, no bans (4v4)
- **Best of 7 with bans**: 5 decks, 1 ban (5v5 → 4v4)

The package handles any square matrix size, though computation time increases
exponentially with deck count.

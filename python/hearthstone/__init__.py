"""
hearthstone - Nash equilibrium calculators for Hearthstone competitive formats

This package provides tools for computing optimal strategies (Nash equilibria)
in competitive Hearthstone tournament formats. It's designed to help competitive
players make optimal decisions when playing in tournaments.

Main Functions
--------------
solve_game(W)
    Core zero-sum game solver using linear programming.
    Finds Nash equilibrium for any payoff matrix.

conquest_nash(W)
    Analyzes a Conquest format match.
    Conquest: Winner's deck is eliminated. First to eliminate all decks wins.

lhs_nash(W)
    Analyzes a Last Hero Standing (LHS) format match.
    LHS: Loser's deck is eliminated. Winner keeps playing same deck.

ban_nash(W, bans, match_format)
    Analyzes the ban phase preceding a match.
    Computes optimal ban strategies and expected winrates.

Examples
--------
Basic Conquest analysis:

>>> import numpy as np
>>> from hearthstone import conquest_nash
>>> # 3x3 winrate matrix
>>> W = np.array([
...     [0.5, 0.6, 0.4],
...     [0.4, 0.5, 0.6],
...     [0.6, 0.4, 0.5]
... ])
>>> result = conquest_nash(W)
>>> initial = result[-1]  # Last element is initial state
>>> print(f"Match win probability: {initial['winrate'][0]:.2%}")
>>> print(f"Optimal deck selection: {initial['nash'][0]}")

Ban phase analysis:

>>> from hearthstone import ban_nash
>>> # 4x4 matrix with 1 ban
>>> W = np.random.uniform(0.4, 0.6, (4, 4))
>>> result = ban_nash(W, bans=1, match_format='conquest')
>>> print(f"Win probability after optimal bans: {result['winrate'][0]:.2%}")

Mathematical Background
-----------------------
The package uses game theory and linear programming to find optimal strategies.
All matches are modeled as two-player zero-sum games where:
- Players simultaneously choose actions (deck selection, bans)
- The payoff to one player equals the loss to the other
- Nash equilibria represent stable strategies where no player can improve
  by unilaterally changing their strategy

The solution method uses:
1. Backward induction: Solve from terminal states upward
2. Linear programming: Find mixed strategy equilibria at each decision point
3. Subgame perfection: Ensure optimal play at every possible game state

Dependencies
------------
- numpy: Matrix operations and array handling
- scipy: Linear programming solver (scipy.optimize.linprog)
"""

# Import main functions for convenient access
from .solve_game import solve_game
from .conquest_nash import conquest_nash
from .lhs_nash import lhs_nash
from .ban_nash import ban_nash

# Define public API
__all__ = [
    'solve_game',
    'conquest_nash',
    'lhs_nash',
    'ban_nash'
]

# Package version
__version__ = '0.1.0'

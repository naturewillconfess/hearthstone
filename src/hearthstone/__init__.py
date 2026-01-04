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

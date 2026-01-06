# Import main functions for convenient access
from .solve_game import solve_game, GameSolution
from .conquest_nash import conquest_nash
from .lhs_nash import lhs_nash
from .ban_nash import ban_nash

# Import result classes for type hints
from .results import (
    ConquestResult, ConquestState,
    LHSResult, LHSState,
    BanResult
)

# Define public API
__all__ = [
    'solve_game',
    'GameSolution',
    'conquest_nash',
    'ConquestResult',
    'ConquestState',
    'lhs_nash',
    'LHSResult',
    'LHSState',
    'ban_nash',
    'BanResult'
]

# Package version
__version__ = '0.1.0'

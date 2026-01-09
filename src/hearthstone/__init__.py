# Import main functions for convenient access
from .solve_game import solve_game, prewarm_cache
from .conquest_nash import conquest_nash
from .lhs_nash import lhs_nash
from .ban_nash import ban_nash
from .lineup_picker import lineup_picker

# Import result classes for type hints
from .results import (
    GameSolution,
    ConquestResult, ConquestStateSolution,
    LHSResult, LHSStateSolution,
    BanResult,
    LineupResult
)

# Define public API
__all__ = [
    'solve_game',
    'prewarm_cache',
    'GameSolution',
    'conquest_nash',
    'ConquestResult',
    'ConquestStateSolution',
    'lhs_nash',
    'LHSResult',
    'LHSStateSolution',
    'ban_nash',
    'BanResult',
    'lineup_picker',
    'LineupResult',
]

# Package version - read from pyproject.toml via importlib.metadata
from importlib.metadata import version as _get_version
__version__ = _get_version('hearthstone')

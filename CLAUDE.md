# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Nash equilibrium calculators for competitive Hearthstone tournament formats (Conquest and Last Hero Standing).

## Common Commands

```bash
# Install as pip package
pip install .

# Install in development mode
pip install -e .

# Install with test dependencies
pip install -e ".[dev]"

# Run all tests
python3 -m pytest src/tests/ -v

# Run a single test file
python3 -m pytest src/tests/test_solve_game.py -v

# Import and use
python3 -c "from hearthstone import conquest_nash; import numpy as np; print(conquest_nash(np.full((3,3), 0.5))[-1]['winrate'])"

# Build documentation (Sphinx)
make html
```

**Dependencies:** numpy, scipy (installed automatically via pip)

## Architecture

The implementation solves game-theoretic problems using linear programming.

### Project Structure

```
src/
├── hearthstone/       # Main Python module
│   ├── solve_game.py
│   ├── conquest_nash.py
│   ├── lhs_nash.py
│   └── ban_nash.py
└── tests/             # Test suite
```

### Core Components

| Function | File | Description |
|----------|------|-------------|
| `solve_game` | `solve_game.py` | LP solver for zero-sum games |
| `conquest_nash` | `conquest_nash.py` | Conquest format solver |
| `lhs_nash` | `lhs_nash.py` | Last Hero Standing solver |
| `ban_nash` | `ban_nash.py` | Ban phase optimizer |

### Data Structures

The winrate matrix `W` is the primary input: `W[i,j]` = probability that Hero's deck i beats Opponent's deck j.

**Returns:** nested lists/dicts containing:
- `score`: eliminated decks for each player
- `winrate`: (hero_winrate, opponent_winrate)
- `nash`: mixed strategy probabilities
- `game`: payoff matrix for that subgame

### Key Dependencies

- scipy (linprog)
- numpy

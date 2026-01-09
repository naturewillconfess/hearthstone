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
pip install -e ".[docs]"
cd docs && make html
# Output in docs/_build/html/
```

**Dependencies:** numpy, highspy (installed automatically via pip)

## Architecture

The implementation solves game-theoretic problems using analytical solvers for small matrices (up to 3x3) and linear programming for larger ones.

### Project Structure

```
src/
├── hearthstone/       # Main Python module
│   ├── solve_game.py      # Core game solver
│   ├── conquest_nash.py   # Conquest format
│   ├── lhs_nash.py        # Last Hero Standing format
│   ├── ban_nash.py        # Ban phase optimizer
│   ├── lineup_picker.py   # Lineup selection optimizer
│   └── results.py         # Result classes
└── tests/             # Test suite

docs/                  # Sphinx documentation
├── conf.py
├── index.rst
├── api/               # API reference
├── formats/           # Tournament format docs
└── _build/html/       # Built documentation
```

### Core Components

| Function | File | Description |
|----------|------|-------------|
| `solve_game` | `solve_game.py` | Zero-sum game solver (analytical + LP) |
| `prewarm_cache` | `solve_game.py` | Pre-compute solutions for submatrices |
| `conquest_nash` | `conquest_nash.py` | Conquest format solver |
| `lhs_nash` | `lhs_nash.py` | Last Hero Standing solver |
| `ban_nash` | `ban_nash.py` | Ban phase optimizer |
| `lineup_picker` | `lineup_picker.py` | Lineup selection optimizer |

### Data Structures

The winrate matrix `W` is the primary input: `W[i,j]` = probability that Hero's deck i beats Opponent's deck j.

**Returns:** Result classes (e.g., `GameSolution`, `ConquestResult`, `LHSResult`, `BanResult`, `LineupResult`) containing:
- `value`/`winrate`: Hero's expected winrate at equilibrium
- `hero_strategy`/`opp_strategy`: optimal mixed strategies
- State-specific information for tournament formats

### Key Dependencies

- highspy (HiGHS LP solver, used for matrices > 3x3)
- numpy

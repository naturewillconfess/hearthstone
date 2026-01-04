# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Nash equilibrium calculators for competitive Hearthstone tournament formats (Conquest and Last Hero Standing).

**The R version is deprecated. Use the Python version for all new development.**

## Common Commands

### Python Module (Recommended)

```bash
# Install as pip package
cd python && pip install .

# Install in development mode
cd python && pip install -e .

# Install with test dependencies
cd python && pip install -e ".[dev]"

# Run all tests
cd python && python3 -m pytest tests/ -v

# Run a single test file
cd python && python3 -m pytest tests/test_solve_game.py -v

# Import and use
python3 -c "from hearthstone import conquest_nash; import numpy as np; print(conquest_nash(np.full((3,3), 0.5))[-1]['winrate'])"
```

**Dependencies:** numpy, scipy (installed automatically via pip)

### R Package (Deprecated)

> **Warning**: The R package is deprecated. All functions emit deprecation warnings.
> Use the Python version instead.

```bash
# Run all tests
R CMD check .

# Run tests with testthat
Rscript -e "testthat::test_local()"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-conquest.R')"

# Build and install locally
R CMD build . && R CMD INSTALL hearthstone_*.tar.gz

# Generate documentation (requires roxygen2)
Rscript -e "roxygen2::roxygenise()"

# Build vignettes
Rscript -e "devtools::build_vignettes()"
```

## Architecture

Both implementations solve game-theoretic problems using linear programming.

### Core Components

| Function | Python (Recommended) | R (Deprecated) | Description |
|----------|---------------------|----------------|-------------|
| `solve_game` | `solve_game.py` | `solve_game.R` | LP solver for zero-sum games |
| `conquest_nash` | `conquest_nash.py` | `conquest_nash.R` | Conquest format solver |
| `lhs_nash` | `lhs_nash.py` | `LHS_nash.R` | Last Hero Standing solver |
| `ban_nash` | `ban_nash.py` | `ban_nash.R` | Ban phase optimizer |

### Key Differences: Python vs R

| Aspect | Python | R |
|--------|--------|---|
| Indexing | 0-based | 1-based |
| LHS function | `lhs_nash()` | `LHS_nash()` |
| Format param | `match_format='lhs'` | `match_format="LHS"` |
| LP solver | scipy.linprog | lpSolveAPI |

### Data Structures

The winrate matrix `W` is the primary input: `W[i,j]` = probability that Hero's deck i beats Opponent's deck j.

**Returns:** nested lists/dicts containing:
- `score`: eliminated decks for each player
- `winrate`: (hero_winrate, opponent_winrate)
- `nash`: mixed strategy probabilities
- `game`: payoff matrix for that subgame

### Key Dependencies

- **Python:** scipy (linprog), numpy
- **R (deprecated):** lpSolveAPI

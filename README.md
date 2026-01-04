
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hearthstone: tools for competitive HearthstoneⓇ players

<!-- badges: start -->

[![R-CMD-check](https://github.com/naturewillconfess/hearthstone/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/naturewillconfess/hearthstone/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> **⚠️ DEPRECATION NOTICE**
>
> The R version of this package is **deprecated** and no longer actively maintained.
> Please use the Python version instead, which offers the same functionality with
> better performance and modern tooling.

## Python Version (Recommended)

The recommended way to use this package is via Python:

### Installation

```bash
cd python
pip install .
```

Or install in development mode:

```bash
cd python
pip install -e .
```

### Quick Start

```python
import numpy as np
from hearthstone import conquest_nash, lhs_nash, ban_nash

# Create a winrate matrix
W = np.array([
    [0.55, 0.45, 0.60],
    [0.50, 0.50, 0.50],
    [0.40, 0.55, 0.45],
])

# Conquest format
result = conquest_nash(W)
print(f"Match winrate: {result[-1]['winrate'][0]:.2%}")

# Last Hero Standing format
result = lhs_nash(W)

# With ban phase (4 decks, 1 ban)
W4 = np.random.uniform(0.4, 0.6, (4, 4))
result = ban_nash(W4, bans=1, match_format='conquest')
```

See `python/README.md` for complete documentation.

---

## R Version (Deprecated)

> **Warning**: The R version is deprecated. All functions will emit deprecation
> warnings when called. Please migrate to the Python version.

hearthstone package is designed to help competitive Hearthstone players
to make optimal decisions when playing in tournaments. In particular, it
offers tools (R functions, vignettes) for finding subgame perfect Nash
equilibria in mixed strategies for Conquest and Last Hero Standing.

### Installation (R)

You can install the latest version of hearthstone with:

``` r
install.packages("devtools")
devtools::install_github("naturewillconfess/hearthstone")
```

### What's included in this pre-alpha version

- [Vignette](https://github.com/naturewillconfess/hearthstone/tree/master/vignettes)
  on Conquest. Check it out!
- R functions (all deprecated - use Python equivalents instead)
  - `conquest_nash()` → `from hearthstone import conquest_nash`
  - `LHS_nash()` → `from hearthstone import lhs_nash`
  - `ban_nash()` → `from hearthstone import ban_nash`
  - `solve_game()` → `from hearthstone import solve_game`

### Migration Guide

| R Function | Python Equivalent |
|------------|-------------------|
| `conquest_nash(W)` | `conquest_nash(W)` |
| `LHS_nash(W)` | `lhs_nash(W)` |
| `ban_nash(W, bans, "conquest")` | `ban_nash(W, bans, match_format='conquest')` |
| `ban_nash(W, bans, "LHS")` | `ban_nash(W, bans, match_format='lhs')` |
| `solve_game(W)` | `solve_game(W)` |

Key differences:
- Python uses 0-based indexing (deck indices start at 0, not 1)
- Python function `lhs_nash` uses lowercase (not `LHS_nash`)
- Python `ban_nash` uses `match_format='lhs'` lowercase (not `"LHS"`)

## News

Version 0.4.0: R package deprecated in favor of Python implementation.

Version 0.3.0: Removed everything related to deprecated formats like Strike,
Specialist and Conquest with Shields.

## Legal disclaimer

Hearthstone is a trademark or registered trademark of Blizzard
Entertainment, Inc., in the U.S. and/or other countries. I'm not
affiliated with Blizzard Entertainment, Inc. in any way.

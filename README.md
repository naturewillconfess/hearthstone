# hearthstone

A Python library for computing Nash equilibria in Hearthstone tournament formats.

This package calculates optimal mixed strategies for deck selection in competitive Hearthstone tournaments, supporting both **Conquest** and **Last Hero Standing (LHS)** formats, with optional ban phases and lineup selection.

## Installation

```bash
pip install hearthstone
```

Or install from source:

```bash
git clone https://github.com/savakian/hearthstone.git
cd hearthstone
pip install .
```

For development:

```bash
pip install -e ".[dev]"
```

## Documentation

Full documentation is available at: **[docs/_build/html/index.html](docs/_build/html/index.html)**

To build the documentation locally:

```bash
pip install -e ".[docs]"
cd docs && make html
```

## Quick Example

```python
import numpy as np
from hearthstone import conquest_nash

# W[i,j] = probability that Hero's deck i beats Opponent's deck j
W = np.array([
    [0.55, 0.45, 0.60],
    [0.50, 0.50, 0.50],
    [0.40, 0.55, 0.45],
])

result = conquest_nash(W, hero_names=['Aggro', 'Combo', 'Control'],
                       opp_names=['Aggro', 'Combo', 'Control'])

print(f"Match winrate: {result.winrate:.1%}")
print(f"Hero strategy: {result.hero_strategy}")
```

## Features

- **Conquest format**: Winner's deck eliminated
- **Last Hero Standing (LHS)**: Loser's deck eliminated, winner keeps playing
- **Ban phase**: Optimal ban strategies before matches
- **Lineup picker**: Optimal lineup selection from a deck pool
- **Fast solvers**: Analytical 2v2/3v3 solvers with `winrate_only=True`
- **Parallel computation**: Multi-core support for large problems

## License

MIT License

## Disclaimer

Hearthstone is a trademark of Blizzard Entertainment, Inc. This project is not affiliated with Blizzard Entertainment.

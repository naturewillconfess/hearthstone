# hearthstone

A Python library for computing Nash equilibria in Hearthstone tournament formats and Ladder.

This package calculates optimal mixed strategies for deck selection in competitive Hearthstone tournaments, supporting both **Conquest** and **Last Hero Standing (LHS)** formats, with optional ban phases, as well as Ladder.

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

## Quick Start

```python
import numpy as np
from hearthstone import solve_game, conquest_nash, lhs_nash, ban_nash

# Define a winrate matrix: W[i,j] = P(your deck i beats opponent's deck j)
W = np.array([
    [0.55, 0.45, 0.60],
    [0.50, 0.50, 0.50],
    [0.40, 0.55, 0.45],
])

# Solve a single game (find Nash equilibrium)
result = solve_game(W)
print(f"Game value: {result['V']:.2%}")
print(f"Your strategy: {result['hero_sol']}")

# Solve a Conquest match
conquest = conquest_nash(W)[-1]
print(f"Conquest winrate: {conquest['winrate'][0]:.2%}")

# Solve a Last Hero Standing match
lhs = lhs_nash(W)
initial = [s for s in lhs if s['score'] == ((), ())
           and s.get('havetoplay_hero') is None][-1]
print(f"LHS winrate: {initial['winrate'][0]:.2%}")

# Solve a match with bans (4 decks, 1 ban each)
W4 = np.array([
    [0.55, 0.45, 0.60, 0.50],
    [0.50, 0.50, 0.50, 0.55],
    [0.40, 0.55, 0.45, 0.60],
    [0.45, 0.50, 0.55, 0.50],
])
ban_result = ban_nash(W4, bans=1, match_format='conquest')
print(f"Winrate after bans: {ban_result['winrate'][0]:.2%}")
print(f"Optimal ban strategy: {ban_result['bans']['hero']}")
```

## Tournament Formats

### Conquest Format

In Conquest, each player brings n decks. The **winner's** deck is eliminated after each game. First player to win with all their decks wins the match.

```python
from hearthstone import conquest_nash
import numpy as np

# 3-deck Conquest (Best of 5)
W = np.array([
    [0.55, 0.45, 0.60],
    [0.50, 0.50, 0.50],
    [0.40, 0.55, 0.45],
])

result = conquest_nash(W)
initial = result[-1]

print(f"Your match win probability: {initial['winrate'][0]:.2%}")
print(f"Optimal deck probabilities: {initial['nash'][0]}")
print(f"Opponent's optimal response: {initial['nash'][1]}")
```

### Last Hero Standing (LHS) Format

In LHS, the **loser's** deck is eliminated, and the winner must keep playing the same deck until it loses.

```python
from hearthstone import lhs_nash
import numpy as np

W = np.array([
    [0.55, 0.45, 0.60],
    [0.50, 0.50, 0.50],
    [0.40, 0.55, 0.45],
])

result = lhs_nash(W)

# Find the initial state (no losses, no forced plays)
initial = [r for r in result
           if r['score'] == ((), ())
           and r.get('havetoplay_hero') is None
           and r.get('havetoplay_opp') is None][-1]

print(f"Your match win probability: {initial['winrate'][0]:.2%}")
```

### Ban Phase

Many tournaments include a ban phase where each player bans one or more of the opponent's decks before the match begins.

```python
from hearthstone import ban_nash
import numpy as np

# 4 decks, 1 ban each
W = np.array([
    [0.55, 0.45, 0.60, 0.50],
    [0.50, 0.50, 0.50, 0.55],
    [0.40, 0.55, 0.45, 0.60],
    [0.45, 0.50, 0.55, 0.50],
])

# Conquest with 1 ban
result = ban_nash(W, bans=1, match_format='conquest')

print(f"Match winrate after optimal bans: {result['winrate'][0]:.2%}")
print(f"Your optimal ban probabilities: {result['bans']['hero']}")
print(f"Ban options: {result['stratlist']['hero']}")

# LHS with 1 ban
result_lhs = ban_nash(W, bans=1, match_format='lhs')
print(f"LHS match winrate: {result_lhs['winrate'][0]:.2%}")
```


## Practical Example: Tournament Preparation

```python
import numpy as np
from hearthstone import ban_nash

# Your estimated winrates against a specific opponent
# Rows: Your decks (Aggro, Midrange, Control, Combo)
# Cols: Their decks (Aggro, Midrange, Control, Combo)
W = np.array([
    [0.50, 0.55, 0.40, 0.60],  # Your Aggro
    [0.45, 0.50, 0.55, 0.45],  # Your Midrange
    [0.60, 0.45, 0.50, 0.40],  # Your Control
    [0.40, 0.55, 0.60, 0.50],  # Your Combo
])

deck_names = ['Aggro', 'Midrange', 'Control', 'Combo']

# Analyze with 1 ban (Conquest format)
result = ban_nash(W, bans=1, match_format='conquest')

print(f"Expected match winrate: {result['winrate'][0]:.1%}\n")

print("Your optimal ban strategy:")
for i, (ban_combo, prob) in enumerate(zip(result['stratlist']['hero'], result['bans']['hero'])):
    if prob > 0.01:
        banned_deck = deck_names[ban_combo[0]]
        print(f"  Ban their {banned_deck}: {prob:.1%}")

print("\nTheir likely ban against you:")
for i, (ban_combo, prob) in enumerate(zip(result['stratlist']['opp'], result['bans']['opp'])):
    if prob > 0.01:
        banned_deck = deck_names[ban_combo[0]]
        print(f"  Ban your {banned_deck}: {prob:.1%}")
```

## License

MIT License

## Disclaimer
Hearthstone is a trademark or registered trademark of Blizzard Entertainment, Inc., in the U.S. and/or other countries. I’m not affiliated with Blizzard Entertainment, Inc. in any way.

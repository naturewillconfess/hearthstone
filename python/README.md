# Hearthstone Nash

A Python library for computing Nash equilibria in Hearthstone tournament formats.

This package calculates optimal mixed strategies for deck selection in competitive Hearthstone tournaments, supporting both **Conquest** and **Last Hero Standing (LHS)** formats, with optional ban phases.

## Installation

### From source (recommended for development)

```bash
cd python
pip install -e .
```

### From source (standard install)

```bash
cd python
pip install .
```

### With development dependencies (for running tests)

```bash
cd python
pip install -e ".[dev]"
```

## Quick Start

```python
import numpy as np
from hearthstone import conquest_nash, lhs_nash, ban_nash, solve_game

# Create a winrate matrix
# W[i,j] = probability that your deck i beats opponent's deck j
W = np.array([
    [0.55, 0.45, 0.60],  # Your Deck 0 vs Opponent's decks
    [0.50, 0.50, 0.50],  # Your Deck 1 vs Opponent's decks
    [0.40, 0.55, 0.45],  # Your Deck 2 vs Opponent's decks
])

# Compute optimal strategy for Conquest format
result = conquest_nash(W)
initial_state = result[-1]  # Initial state is the last element

print(f"Match winrate: {initial_state['winrate'][0]:.2%}")
print(f"Optimal deck selection: {initial_state['nash'][0]}")
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

## Core Functions

### `solve_game(W)`

Solves a zero-sum game matrix using linear programming.

**Parameters:**
- `W`: numpy array of shape (m, n) - payoff matrix for the row player

**Returns:** dict with:
- `hero_sol`: optimal mixed strategy for row player
- `opp_sol`: optimal mixed strategy for column player
- `V`: game value (expected payoff for row player)

```python
from hearthstone import solve_game
import numpy as np

# Classic rock-paper-scissors
W = np.array([
    [0.5, 1.0, 0.0],  # Rock vs Rock, Paper, Scissors
    [0.0, 0.5, 1.0],  # Paper vs ...
    [1.0, 0.0, 0.5],  # Scissors vs ...
])

result = solve_game(W)
print(f"Game value: {result['V']}")  # 0.5 (fair game)
print(f"Optimal strategy: {result['hero_sol']}")  # [0.333, 0.333, 0.333]
```

### `conquest_nash(W)`

Computes Nash equilibrium for all subgames in a Conquest match.

**Parameters:**
- `W`: numpy array of shape (n, n) - winrate matrix

**Returns:** list of state dictionaries, each containing:
- `score`: tuple of (hero_eliminated, opp_eliminated) deck indices
- `winrate`: tuple (hero_win_prob, opp_win_prob)
- `nash`: tuple (hero_strategy, opp_strategy) for non-terminal states
- `game`: payoff matrix for the subgame

### `lhs_nash(W)`

Computes Nash equilibrium for all subgames in a Last Hero Standing match.

**Parameters:**
- `W`: numpy array of shape (n, n) - winrate matrix

**Returns:** list of state dictionaries with additional fields:
- `havetoplay_hero`: deck index Hero must play (if they won last game)
- `havetoplay_opp`: deck index Opponent must play (if they won last game)

### `ban_nash(W, bans, match_format='conquest')`

Computes optimal ban strategies for a tournament with ban phase.

**Parameters:**
- `W`: numpy array of shape (n, n) - winrate matrix
- `bans`: number of bans per player (must be < n)
- `match_format`: 'conquest' or 'lhs'

**Returns:** dict with:
- `bans`: dict with 'hero' and 'opp' optimal ban probabilities
- `winrate`: tuple of expected win probabilities after bans
- `stratlist`: dict with ban combination options for each player
- `matches`: nested list of full match analyses for each ban pair

## Understanding the Winrate Matrix

The winrate matrix `W` has dimensions (n, n) where n is the number of decks:

```
         Opponent
         Deck 0  Deck 1  Deck 2
Hero  0 [ 0.55    0.45    0.60 ]
      1 [ 0.50    0.50    0.50 ]
      2 [ 0.40    0.55    0.45 ]
```

- `W[i,j]` = probability that Hero's deck i beats Opponent's deck j
- Values should be between 0 and 1
- The matrix does NOT need to be symmetric
- Diagonal values are typically around 0.5 (mirror matches)

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

## Running Tests

```bash
cd python
pip install -e ".[dev]"
pytest tests/ -v
```

## Requirements

- Python >= 3.8
- NumPy >= 1.20.0
- SciPy >= 1.7.0

## License

MIT License

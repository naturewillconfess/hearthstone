# Potential Next Steps

Analysis of the Python package identifying issues and potential improvements.

## Strengths

1. **Excellent documentation** - Every module has thorough docstrings explaining the math, algorithms, and parameters
2. **Comprehensive test suite** - 86 tests covering edge cases, calibration, and known analytical solutions
3. **Clean separation of concerns** - Each module handles one responsibility
4. **Type hints** - Function signatures include type annotations

---

## Issues & Concerns

### 1. Input Validation Missing

```python
# solve_game.py - No validation that W is 2D or non-empty
W = np.asarray(W, dtype=float)
m, n = W.shape  # Crashes on 1D array or scalar
```

Same issue in `conquest_nash.py` and `lhs_nash.py` - no check that matrix is square.

### 2. No Input Range Validation

The functions accept winrates outside [0, 1] without warning. Values like -0.5 or 2.0 are mathematically valid for the LP but semantically wrong for winrates.

### 3. Memory: Stores All Match Results in ban_nash

```python
matches = [[None for _ in range(num_opp_options)] for _ in range(num_hero_options)]
```

For 4 decks with 2 bans: 6×6 = 36 full match analyses stored. This grows as C(n,k)². Could offer a `store_matches=False` option.

### 4. Inconsistent Return Types

- `conquest_nash` / `lhs_nash` return `list[dict]`
- `ban_nash` returns `dict`
- `solve_game` returns `dict`

The list return for nash functions is awkward - users must know to use `result[-1]` for the initial state.

### 5. No Caching/Memoization

`ban_nash` computes `conquest_nash` or `lhs_nash` C(n,k)² times. Some of these could potentially share subproblem results, though the reduced matrices are different.

### 6. LP Solver Failure Not User-Friendly

```python
if not result.success:
    raise RuntimeError(f"Linear programming failed: {result.message}")
```

No guidance on what caused it or how to fix it.

### 7. Package Naming Collision Risk

The package is named `hearthstone` but pip package is `hearthstone-nash`. The import name `hearthstone` could collide with other Hearthstone-related packages.

### 8. No `__repr__` for Results

Results are plain dicts. A custom class with `__repr__` would make interactive use friendlier:

```python
>>> result = conquest_nash(W)
>>> result[-1]
{'score': ((), ()), 'winrate': (0.5, 0.5), ...}  # Hard to read
```

### 9. find_initial_state Helper in Tests but Not in Package

The test file `test_lhs_nash.py` defines `find_initial_state()` to locate the initial state. This should be a public utility since LHS results are harder to navigate than Conquest.

---

## Potential Improvements (Not Bugs)

| Area | Current | Suggested |
|------|---------|-----------|
| **API** | `result[-1]` for initial state | Add `.initial_state` property or helper function |
| **Performance** | Eager computation of all states | Lazy evaluation option for large n |
| **Usability** | Raw numpy arrays in output | Named tuples or dataclasses |
| **Logging** | Silent | Optional progress callback for large matches |
| **Visualization** | None | `plot_strategy()` helper for matplotlib |
| **Serialization** | None | `to_json()` / `from_json()` methods |

---

## Minor Nitpicks

1. **Version in two places** - `__init__.py` has `__version__ = '0.1.0'` and `pyproject.toml` has `version = "0.1.0"`. Should use single source of truth.

2. **Empty `__init__.py` in tests** - `python/tests/__init__.py` exists but is empty (fine, but could be removed).

3. **`setup.py` duplicates `pyproject.toml`** - Only needed for old pip compatibility. Could add comment explaining this.

4. **No py.typed marker** - For mypy users, adding `py.typed` would indicate the package supports type checking.

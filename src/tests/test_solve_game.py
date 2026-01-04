"""
test_solve_game.py - Tests for the zero-sum game solver

These tests verify that solve_game() correctly computes Nash equilibria
for various zero-sum games, including:
1. Symmetric games (expected value 0.5)
2. Known game theory examples from Owen's textbook
3. Games with dominated strategies (pure equilibria)
4. Statistical calibration tests with random matrices
5. Probability validity checks
6. Best response verification

The test cases are ported from the R package's test-solver.R with additions.
"""

import numpy as np
import pytest
import sys
import os

# Add the parent directory to path so we can import hearthstone
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from hearthstone import solve_game


class TestEqualSquareGame:
    """
    Test symmetric games where all entries are 0.5.

    For any square matrix filled with 0.5, the game value should be exactly 0.5
    since neither player has any advantage. This also tests that the solver
    handles various matrix sizes correctly.
    """

    def test_various_sizes(self):
        """Test symmetric games of sizes 1x1 through 10x10."""
        for i in range(1, 11):
            W = np.full((i, i), 0.5)
            result = solve_game(W)
            assert result['V'] == pytest.approx(0.5, abs=1e-6), f"Failed for {i}x{i} matrix"

class TestOwenExamples:
    """
    Test cases from Guillermo Owen's "Game Theory" textbook.

    These are classic game theory examples with known analytical solutions.
    They test that the LP solver produces correct mixed strategy equilibria.
    """

    def test_owen_example_1(self):
        """
        Owen Example 1: 3x4 game with known mixed equilibrium.

        This fills column-by-column in R, so the matrix is:
            col1  col2  col3  col4
        row1  3     6     1     4
        row2  5     2     4     2
        row3  1     4     3     5

        Expected solution:
        - Game value V = 3.25
        - Hero strategy: [0.125, 0.5, 0.375]
        - Opponent strategy: [1/12, 5/12, 0.5, 0]
        """
        # R fills column-by-column, so we construct the same way
        W = np.array([3, 5, 1, 6, 2, 4, 1, 4, 3, 4, 2, 5]).reshape(3, 4, order='F')
        result = solve_game(W)

        # Check game value
        assert result['V'] == pytest.approx(3.25, abs=1e-6)

        # Check hero strategy
        expected_hero = np.array([0.125, 0.5, 0.375])
        np.testing.assert_allclose(result['hero_sol'], expected_hero, atol=1e-6)

        # Check opponent strategy
        expected_opp = np.array([1/12, 5/12, 0.5, 0])
        np.testing.assert_allclose(result['opp_sol'], expected_opp, atol=1e-6)

    def test_owen_example_2(self):
        """
        Owen Example 2: 2x4 game.

        Matrix (column-major):
            col1  col2  col3  col4
        row1  2     3     1     5
        row2  4     1     6     0

        Expected solution:
        - Game value V = 17/7 ≈ 2.4286
        - Hero strategy: [5/7, 2/7] ≈ [0.714, 0.286]
        """
        W = np.array([2, 4, 3, 1, 1, 6, 5, 0]).reshape(2, 4, order='F')
        result = solve_game(W)

        assert result['V'] == pytest.approx(17/7, abs=1e-6)

        expected_hero = np.array([5/7, 2/7])
        np.testing.assert_allclose(result['hero_sol'], expected_hero, atol=1e-6)

    def test_owen_example_3(self):
        """
        Owen Example 3: Symmetric 3x3 game with zero-sum structure.

        Matrix (column-major):
            col1  col2  col3
        row1   0     1    -2
        row2  -1     0     3
        row3   2    -3     0

        This is a symmetric game (skew-symmetric payoffs).
        Expected solution (same for both players):
        - Strategy: [0.5, 1/3, 1/6] ≈ [0.5, 0.333, 0.167]
        """
        W = np.array([0, -1, 2, 1, 0, -3, -2, 3, 0]).reshape(3, 3, order='F')
        result = solve_game(W)

        expected_strategy = np.array([0.5, 1/3, 1/6])
        np.testing.assert_allclose(result['hero_sol'], expected_strategy, atol=1e-6)
        np.testing.assert_allclose(result['opp_sol'], expected_strategy, atol=1e-6)


class TestDominatedStrategies:
    """
    Test games where one strategy dominates, leading to pure equilibria.
    """

    def test_2x2_domination(self):
        """
        2x2 game with a dominated strategy.

        R code: matrix(c(2,3,1,4),2,2)
        Matrix (column-major):
            col1  col2
        row1   2     1
        row2   3     4

        Row 2 dominates Row 1 (3>2 and 4>1).
        Column 1 dominates Column 2 for opponent (trying to minimize, conditional on Hero picking row 2).

        Pure Nash equilibrium: Hero plays row 2, Opponent plays column 1.
        Expected:
        - Hero strategy: [0, 1]
        - Opponent strategy: [1, 0]
        - Game value: 3
        """
        W = np.array([2, 3, 1, 4]).reshape(2, 2, order='F')
        result = solve_game(W)

        # Pure strategies expected
        expected_hero = np.array([0.0, 1.0])
        expected_opp = np.array([1.0, 0.0])

        np.testing.assert_allclose(result['hero_sol'], expected_hero, atol=1e-6)
        np.testing.assert_allclose(result['opp_sol'], expected_opp, atol=1e-6)
        assert result['V'] == pytest.approx(3.0, abs=1e-6)

    def test_3x3_with_dominated_row(self):
        """
        3x3 game where one row is dominated.

        Row 1 is dominated by Row 2 (all entries in row 2 are higher).
        """
        W = np.array([
            [0.3, 0.3, 0.3],  # Row 1: dominated
            [0.5, 0.5, 0.5],  # Row 2: dominates row 1
            [0.4, 0.6, 0.4]   # Row 3: mixed
        ])
        result = solve_game(W)

        # Row 1 should have zero probability (it's dominated)
        assert result['hero_sol'][0] == pytest.approx(0.0, abs=1e-6)

    def test_3x3_with_dominated_column(self):
        """
        3x3 game where one column is dominated.

        Column 3 dominates Column 1 (all entries in col 3 are lower,
        which is better for opponent who minimizes).
        """
        W = np.array([
            [0.6, 0.5, 0.4],  # Col 3 < Col 1
            [0.7, 0.5, 0.5],  # Col 3 < Col 1
            [0.8, 0.5, 0.6]   # Col 3 < Col 1
        ])
        result = solve_game(W)

        # Column 1 should have zero probability (it's dominated by col 3)
        assert result['opp_sol'][0] == pytest.approx(0.0, abs=1e-6)


class TestCalibration:
    """
    Statistical calibration tests using random matrices.

    For uniformly random payoff matrices, we expect certain statistical
    properties to hold on average. These tests verify that the solver
    behaves correctly across many random instances.
    """

    def test_random_3x3_calibration(self):
        """
        Test that random 3x3 games produce expected statistical properties.

        Over 10,000 random games:
        - Mean game value should be near 0.5 (unbiased)
        - Mean strategy probabilities should be near 1/3 (uniform tendency)

        This is a statistical test, so we use wide tolerances.
        """
        np.random.seed(42)  # For reproducibility
        n_games = 10000

        values = []
        hero_strategies = []
        opp_strategies = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = solve_game(W)
            values.append(result['V'])
            hero_strategies.append(result['hero_sol'])
            opp_strategies.append(result['opp_sol'])

        mean_value = np.mean(values)
        mean_hero = np.mean(hero_strategies, axis=0)
        mean_opp = np.mean(opp_strategies, axis=0)



        assert mean_value == pytest.approx(0.5, abs=0.05)
        assert mean_hero == pytest.approx(1/3, abs=0.03)
        assert mean_opp == pytest.approx(1/3, abs=0.03)


class TestEdgeCases:
    """
    Additional edge case tests.
    """

    def test_1x1_game(self):
        """Single-element matrix should return that element as value."""
        W = np.array([[0.7]])
        result = solve_game(W)
        assert result['V'] == pytest.approx(0.7, abs=1e-6)
        assert result['hero_sol'][0] == pytest.approx(1.0, abs=1e-6)
        assert result['opp_sol'][0] == pytest.approx(1.0, abs=1e-6)

    def test_non_square_2x3(self):
        """Test a non-square matrix."""
        W = np.array([[0.5, 0.6, 0.4], [0.4, 0.5, 0.6]])
        result = solve_game(W)

        # Verify output shapes
        assert len(result['hero_sol']) == 2
        assert len(result['opp_sol']) == 3

        # Verify probabilities sum to 1
        assert np.sum(result['hero_sol']) == pytest.approx(1.0, abs=1e-6)
        assert np.sum(result['opp_sol']) == pytest.approx(1.0, abs=1e-6)

        # Verify non-negativity
        assert np.all(result['hero_sol'] >= -1e-10)
        assert np.all(result['opp_sol'] >= -1e-10)

    def test_non_square_4x2(self):
        """Test another non-square matrix (more rows than columns)."""
        W = np.array([
            [0.6, 0.4],
            [0.5, 0.5],
            [0.4, 0.6],
            [0.3, 0.7]
        ])
        result = solve_game(W)

        assert len(result['hero_sol']) == 4
        assert len(result['opp_sol']) == 2
        assert np.sum(result['hero_sol']) == pytest.approx(1.0, abs=1e-6)
        assert np.sum(result['opp_sol']) == pytest.approx(1.0, abs=1e-6)

    def test_extreme_values(self):
        """Test with extreme (0 and 1) payoffs."""
        W = np.array([
            [1.0, 0.0],
            [0.0, 1.0]
        ])
        result = solve_game(W)

        # This is like "matching pennies" - should be mixed 50/50
        assert result['V'] == pytest.approx(0.5, abs=1e-6)
        np.testing.assert_allclose(result['hero_sol'], [0.5, 0.5], atol=1e-6)
        np.testing.assert_allclose(result['opp_sol'], [0.5, 0.5], atol=1e-6)


class TestProbabilityValidity:
    """
    Tests to ensure output probabilities are always valid.
    """

    def test_probabilities_sum_to_one(self):
        """Strategies should always sum to 1."""
        np.random.seed(123)
        for _ in range(100):
            m, n = np.random.randint(1, 6, size=2)
            W = np.random.uniform(-1, 2, (m, n))
            result = solve_game(W)

            assert np.sum(result['hero_sol']) == pytest.approx(1.0, abs=1e-6)
            assert np.sum(result['opp_sol']) == pytest.approx(1.0, abs=1e-6)

    def test_probabilities_non_negative(self):
        """Strategies should always be non-negative."""
        np.random.seed(456)
        for _ in range(100):
            m, n = np.random.randint(1, 6, size=2)
            W = np.random.uniform(-1, 2, (m, n))
            result = solve_game(W)

            assert np.all(result['hero_sol'] >= -1e-10), \
                f"Negative hero probability: {result['hero_sol']}"
            assert np.all(result['opp_sol'] >= -1e-10), \
                f"Negative opp probability: {result['opp_sol']}"


class TestBestResponse:
    """
    Tests to verify that computed strategies are best responses.

    In a Nash equilibrium, each player's strategy should be a best response
    to the opponent's strategy. We verify this by checking that the expected
    payoff equals V for any pure strategy in the support.
    """

    def test_hero_best_response(self):
        """
        Verify Hero's strategy is a best response.

        For any row i with positive probability, the expected payoff
        against Opponent's mixed strategy should equal V.
        """
        np.random.seed(789)
        for _ in range(50):
            W = np.random.uniform(0, 1, (3, 3))
            result = solve_game(W)

            # Expected payoff for each pure strategy against opp's mixed strategy
            expected_payoffs = W @ result['opp_sol']

            # For strategies with positive probability, payoff should equal V
            for i, prob in enumerate(result['hero_sol']):
                if prob > 1e-6:
                    assert expected_payoffs[i] == pytest.approx(result['V'], abs=1e-5), \
                        f"Hero strategy {i} payoff {expected_payoffs[i]} != V={result['V']}"

            # No strategy should have payoff greater than V
            assert np.all(expected_payoffs <= result['V'] + 1e-5)

    def test_opp_best_response(self):
        """
        Verify Opponent's strategy is a best response.

        For any column j with positive probability, Hero's expected payoff
        against that pure strategy should equal V (opponent can't do better
        than V by deviating).
        """
        np.random.seed(321)
        for _ in range(50):
            W = np.random.uniform(0, 1, (3, 3))
            result = solve_game(W)

            # Expected payoff for Hero using mixed strategy against each opp pure strategy
            hero_payoffs = result['hero_sol'] @ W

            # For opponent strategies with positive probability, Hero's payoff should equal V
            for j, prob in enumerate(result['opp_sol']):
                if prob > 1e-6:
                    assert hero_payoffs[j] == pytest.approx(result['V'], abs=1e-5), \
                        f"Against opp strategy {j}: payoff {hero_payoffs[j]} != V={result['V']}"

            # Opponent can't force Hero below V
            assert np.all(hero_payoffs >= result['V'] - 1e-5)

if __name__ == '__main__':
    pytest.main([__file__, '-v'])

"""
test_conquest_nash.py - Tests for the Conquest format Nash calculator

These tests verify that conquest_nash() correctly computes Nash equilibria
for the Conquest tournament format. Tests include:
1. Symmetric games (expected 50/50 match winrate)
2. Minimal 1-deck games
3. Statistical calibration with random matrices
4. Known formulas for BO3 and BO5 matches
5. State count verification
6. Asymmetric matchup verification
7. Comparison with analytical solutions

The test cases are ported from the R package's test-conquest.R with additions.
"""

import numpy as np
import pytest
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from hearthstone import conquest_nash


class TestSymmetricConquest:
    """
    Test symmetric games where all matchups are 50/50.

    For any matrix filled with 0.5, the match winrate should be exactly 0.5
    since neither player has any advantage in any individual game.
    """

    def test_3x3_symmetric(self):
        """Test a basic 3x3 symmetric Conquest match (BO5 style)."""
        W = np.full((3, 3), 0.5)
        result = conquest_nash(W)

        # Get the initial state (last element in the list)
        initial_state = result[-1]

        # Match should be 50/50
        assert initial_state['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert initial_state['winrate'][1] == pytest.approx(0.5, abs=1e-6)

    def test_2x2_symmetric(self):
        """Test a 2x2 symmetric Conquest match (BO3 style)."""
        W = np.full((2, 2), 0.5)
        result = conquest_nash(W)
        initial_state = result[-1]

        assert initial_state['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert initial_state['winrate'][1] == pytest.approx(0.5, abs=1e-6)

    def test_4x4_symmetric(self):
        """Test a 4x4 symmetric Conquest match (BO7 style)."""
        W = np.full((4, 4), 0.5)
        result = conquest_nash(W)
        initial_state = result[-1]

        assert initial_state['winrate'][0] == pytest.approx(0.5, abs=1e-6)


class TestMinimalConquest:
    """
    Test the smallest possible Conquest match (1 deck each).
    """

    def test_1x1_conquest(self):
        """
        1x1 Conquest: Single game determines the match.

        With W = [[0.5]], the match is just one coin flip.
        """
        W = np.array([[0.5]])
        result = conquest_nash(W)
        initial_state = result[-1]

        assert initial_state['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert initial_state['winrate'][1] == pytest.approx(0.5, abs=1e-6)

    def test_1x1_biased(self):
        """1x1 Conquest with biased winrate."""
        W = np.array([[0.7]])
        result = conquest_nash(W)
        initial_state = result[-1]

        # With one deck each, match winrate equals single game winrate
        assert initial_state['winrate'][0] == pytest.approx(0.7, abs=1e-6)


class TestCalibration:
    """
    Statistical calibration tests for random Conquest matches.
    """

    def test_random_3x3_calibration(self):
        """
        Test that random 3x3 Conquest matches have expected properties.

        Over 100 random matches:
        - Mean match winrate should be near 0.5
        - Mean deck selection probabilities should be near 1/3

        Note: Using 100 games instead of R's calibration count to speed up tests.
        The R test uses tolerance of 0.2, which we replicate here.
        """
        np.random.seed(42)
        n_games = 100
        tolerance = 0.2

        winrates = []
        hero_strategies = []
        opp_strategies = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = conquest_nash(W)
            initial = result[-1]

            winrates.append(initial['winrate'][0])
            hero_strategies.append(initial['nash'][0])
            opp_strategies.append(initial['nash'][1])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_strategies, axis=0)
        mean_opp = np.mean(opp_strategies, axis=0)

        # Match winrate should be near 0.5
        assert abs(mean_winrate - 0.5) < tolerance, \
            f"Mean winrate {mean_winrate} too far from 0.5"

        # Strategy probabilities should be near 1/3
        for i, prob in enumerate(mean_hero):
            assert abs(prob - 1/3) < tolerance, \
                f"Hero strategy[{i}] = {prob} too far from 0.333"
        for i, prob in enumerate(mean_opp):
            assert abs(prob - 1/3) < tolerance, \
                f"Opp strategy[{i}] = {prob} too far from 0.333"

    def test_random_2x2_calibration(self):
        """Test calibration for 2x2 Conquest matches."""
        np.random.seed(43)
        n_games = 100
        tolerance = 0.2

        winrates = []
        for _ in range(n_games):
            W = np.random.uniform(0, 1, (2, 2))
            result = conquest_nash(W)
            winrates.append(result[-1]['winrate'][0])

        mean_winrate = np.mean(winrates)
        assert abs(mean_winrate - 0.5) < tolerance


class TestBO3Formula:
    """
    Test the analytical formula for BO3 Conquest (2 decks each).

    In a 2v2 Conquest match, the expected winrate can be computed analytically.
    This test verifies that conquest_nash produces the correct value.
    """

    def test_bo3_vignette(self):
        """
        Test the BO3 formula from the vignette.

        For a 2x2 winrate matrix W, the match winrate for the Hero is:
        (2*W[0,0]*W[1,0] + W[0,0]*W[1,1] + W[1,0]*W[0,1] +
         2*W[0,1]*W[1,1] - W[0,0]*W[1,0]*W[1,1] - W[0,0]*W[0,1]*W[1,1] -
         W[1,0]*W[0,0]*W[0,1] - W[1,0]*W[0,1]*W[1,1]) / 2

        This formula comes from enumerating all possible game sequences.
        """
        np.random.seed(123)
        W = np.random.uniform(0, 1, (2, 2))
        result = conquest_nash(W)
        initial = result[-1]

        # Compute expected value using the analytical formula
        # Note: Python uses 0-based indexing
        w = W  # Shorthand
        expected = (
            2*w[0,0]*w[1,0] + w[0,0]*w[1,1] + w[1,0]*w[0,1] +
            2*w[0,1]*w[1,1] - w[0,0]*w[1,0]*w[1,1] - w[0,0]*w[0,1]*w[1,1] -
            w[1,0]*w[0,0]*w[0,1] - w[1,0]*w[0,1]*w[1,1]
        ) / 2

        assert initial['winrate'][0] == pytest.approx(expected, abs=1e-6)

    def test_bo3_multiple_random(self):
        """Test BO3 formula on multiple random matrices."""
        np.random.seed(456)
        for _ in range(20):
            W = np.random.uniform(0.2, 0.8, (2, 2))
            result = conquest_nash(W)
            initial = result[-1]

            w = W
            expected = (
                2*w[0,0]*w[1,0] + w[0,0]*w[1,1] + w[1,0]*w[0,1] +
                2*w[0,1]*w[1,1] - w[0,0]*w[1,0]*w[1,1] - w[0,0]*w[0,1]*w[1,1] -
                w[1,0]*w[0,0]*w[0,1] - w[1,0]*w[0,1]*w[1,1]
            ) / 2

            assert initial['winrate'][0] == pytest.approx(expected, abs=1e-6)


class TestBO5Formulas:
    """
    Test analytical formulas for BO5 Conquest (3 decks each).

    The vignette provides several verifiable properties for specific
    game states in a 3v3 Conquest match.
    """

    def test_bo5_specific_states(self):
        """
        Test specific subgame values in a BO5 match.

        For a 3x3 match, we verify formulas for states where Hero has
        already eliminated 2 of their decks (only deck 2 remains).
        """
        np.random.seed(456)
        W = np.random.uniform(0, 1, (3, 3))
        result = conquest_nash(W)

        # Helper function to find a specific state
        def find_state(hero_elim, opp_elim):
            """Find the state where hero eliminated hero_elim and opp eliminated opp_elim."""
            for state in result:
                if (set(state['score'][0]) == set(hero_elim) and
                    set(state['score'][1]) == set(opp_elim)):
                    return state
            return None

        # State (1,2) vs (): Hero eliminated decks 0 and 1, only deck 2 remains
        # Opponent has all decks. Hero wins if deck 2 beats all 3 opponent decks.
        # P(win) = 1 - (1-W[2,0])*(1-W[2,1])*(1-W[2,2])
        state_12 = find_state((0, 1), ())
        expected_12 = 1 - (1-W[2,0])*(1-W[2,1])*(1-W[2,2])
        assert state_12['winrate'][0] == pytest.approx(expected_12, abs=1e-6)

        # State (0,2) vs (): Hero has only deck 1, opponent has all
        state_02 = find_state((0, 2), ())
        expected_02 = 1 - (1-W[1,0])*(1-W[1,1])*(1-W[1,2])
        assert state_02['winrate'][0] == pytest.approx(expected_02, abs=1e-6)

        # State (0,) vs (0,): Hero eliminated deck 0, Opp eliminated deck 0
        # Hero has decks 1,2 vs Opp decks 1,2
        # This is a 2v2 subgame with the reduced matrix W[1:3, 1:3]
        state_0_0 = find_state((0,), (0,))
        W_sub = W[1:3, 1:3]  # Submatrix for remaining decks
        # Use the BO3 formula on the submatrix
        w = W_sub
        expected_0_0 = (
            2*w[0,0]*w[1,0] + w[0,0]*w[1,1] + w[1,0]*w[0,1] +
            2*w[0,1]*w[1,1] - w[0,0]*w[1,0]*w[1,1] - w[0,0]*w[0,1]*w[1,1] -
            w[1,0]*w[0,0]*w[0,1] - w[1,0]*w[0,1]*w[1,1]
        ) / 2
        assert state_0_0['winrate'][0] == pytest.approx(expected_0_0, abs=1e-6)


class TestGamePayoffMatrix:
    """
    Test that the payoff matrix G at each state is computed correctly.
    """

    def test_payoff_matrix_recursion(self):
        """
        Test that G[i,j] = W[i,j] * V_win + (1-W[i,j]) * V_lose

        For the initial state of a 3x3 game, verify that one entry
        of the payoff matrix satisfies the recursive formula.
        """
        np.random.seed(789)
        W = np.random.uniform(0, 1, (3, 3))
        result = conquest_nash(W)

        # Find the initial state
        initial = result[-1]
        assert initial['score'] == ((), ())

        # Find the win/lose states for playing deck 0 vs deck 0
        def find_state(hero_elim, opp_elim):
            for state in result:
                if (set(state['score'][0]) == set(hero_elim) and
                    set(state['score'][1]) == set(opp_elim)):
                    return state
            return None

        # If Hero plays deck 0 and wins -> Hero's deck 0 eliminated
        win_state = find_state((0,), ())
        # If Hero plays deck 0 and loses -> Opp's deck 0 eliminated
        lose_state = find_state((), (0,))

        # G[0,0] should equal W[0,0] * V_win + (1-W[0,0]) * V_lose
        expected_G00 = W[0,0] * win_state['winrate'][0] + (1-W[0,0]) * lose_state['winrate'][0]
        assert initial['game'][0,0] == pytest.approx(expected_G00, abs=1e-6)

    def test_all_payoff_entries(self):
        """Verify all entries of the initial state payoff matrix."""
        np.random.seed(999)
        W = np.random.uniform(0, 1, (3, 3))
        result = conquest_nash(W)

        def find_state(hero_elim, opp_elim):
            for state in result:
                if (set(state['score'][0]) == set(hero_elim) and
                    set(state['score'][1]) == set(opp_elim)):
                    return state
            return None

        initial = result[-1]

        for i in range(3):
            for j in range(3):
                win_state = find_state((i,), ())
                lose_state = find_state((), (j,))
                expected = W[i,j] * win_state['winrate'][0] + (1-W[i,j]) * lose_state['winrate'][0]
                assert initial['game'][i,j] == pytest.approx(expected, abs=1e-6)


class TestStateCount:
    """
    Test that the correct number of states are generated.
    """

    def test_2x2_state_count(self):
        """
        For 2x2 Conquest, count expected states.

        States: (hero_eliminated, opp_eliminated)
        - ((), ()): initial
        - ((0,), ()), ((1,), ()), ((), (0,)), ((), (1,)): one elimination
        - ((0,1), ()), ((), (0,1)): terminal (someone won)
        - ((0,), (0,)), ((0,), (1,)), ((1,), (0,)), ((1,), (1,)): one each

        Total should be 2^2 + 2^2 - 1 = 7 (all combinations except both full)
        Actually: need to enumerate all (subset of {0,1}, subset of {0,1}) pairs
        excluding ({0,1}, {0,1}). That's 4 * 4 - 1 = 15.
        """
        W = np.full((2, 2), 0.5)
        result = conquest_nash(W)

        # For n=2: 4 * 4 - 1 = 15 states
        assert len(result) == 15

    def test_3x3_state_count(self):
        """
        For 3x3 Conquest, count expected states.

        Each player can have any subset of {0,1,2} eliminated.
        That's 8 subsets each, so 8*8 = 64 combinations.
        Minus 1 for the impossible (all, all) state.
        Total: 63 states.
        """
        W = np.full((3, 3), 0.5)
        result = conquest_nash(W)

        # For n=3: 8 * 8 - 1 = 63 states
        assert len(result) == 63


class TestAsymmetricMatchups:
    """
    Test with asymmetric winrate matrices.
    """

    def test_strong_deck(self):
        """
        Test where one deck dominates all others.

        If deck 0 beats everything with 90% winrate, Hero should have
        an overall advantage (winrate > 0.5).

        Note: In Conquest, the WINNER's deck is eliminated, so it may
        actually be optimal to NOT play the strong deck first - saving
        it for later when it's more valuable. The Nash equilibrium
        correctly accounts for this strategic consideration.
        """
        W = np.array([
            [0.9, 0.9, 0.9],  # Deck 0 is very strong
            [0.5, 0.5, 0.5],
            [0.5, 0.5, 0.5]
        ])
        result = conquest_nash(W)
        initial = result[-1]

        # Hero should have winrate > 0.5 due to strong deck
        assert initial['winrate'][0] > 0.5

    def test_weak_deck(self):
        """
        Test where one deck is very weak.

        If deck 0 loses to everything, it should rarely be played.
        """
        W = np.array([
            [0.1, 0.1, 0.1],  # Deck 0 is very weak
            [0.5, 0.5, 0.5],
            [0.5, 0.5, 0.5]
        ])
        result = conquest_nash(W)
        initial = result[-1]

        # Hero should have winrate < 0.5 due to weak deck
        assert initial['winrate'][0] < 0.5

        # Deck 0 should be played with low probability
        assert initial['nash'][0][0] < 0.35

    def test_counter_matchups(self):
        """
        Test rock-paper-scissors style matchups.

        Each deck strongly beats one deck and loses to another.
        Should result in mixed strategy.
        """
        W = np.array([
            [0.5, 0.8, 0.2],  # Deck 0 beats 1, loses to 2
            [0.2, 0.5, 0.8],  # Deck 1 beats 2, loses to 0
            [0.8, 0.2, 0.5]   # Deck 2 beats 0, loses to 1
        ])
        result = conquest_nash(W)
        initial = result[-1]

        # Should be near 50/50 due to symmetry
        assert initial['winrate'][0] == pytest.approx(0.5, abs=0.1)

        # Strategies should be mixed (no pure strategy dominates)
        assert all(p > 0.1 for p in initial['nash'][0])


class TestTerminalStates:
    """
    Test that terminal states have correct values.
    """

    def test_hero_wins_state(self):
        """When Hero has eliminated all decks, winrate should be 1."""
        W = np.full((2, 2), 0.5)
        result = conquest_nash(W)

        # Find state where hero has eliminated both decks
        for state in result:
            if state['score'][0] == (0, 1):  # Hero eliminated both
                assert state['winrate'][0] == pytest.approx(1.0, abs=1e-6)
                assert state['winrate'][1] == pytest.approx(0.0, abs=1e-6)

    def test_opp_wins_state(self):
        """When Opponent has eliminated all decks, Hero winrate should be 0."""
        W = np.full((2, 2), 0.5)
        result = conquest_nash(W)

        for state in result:
            if state['score'][1] == (0, 1):  # Opp eliminated both
                assert state['winrate'][0] == pytest.approx(0.0, abs=1e-6)
                assert state['winrate'][1] == pytest.approx(1.0, abs=1e-6)


class TestOutputStructure:
    """
    Test the structure of conquest_nash output.
    """

    def test_output_is_list(self):
        """Output should be a list."""
        W = np.full((3, 3), 0.5)
        result = conquest_nash(W)
        assert isinstance(result, list)

    def test_state_has_required_keys(self):
        """Each state should have score and winrate."""
        W = np.full((3, 3), 0.5)
        result = conquest_nash(W)

        for state in result:
            assert 'score' in state
            assert 'winrate' in state
            assert len(state['winrate']) == 2

    def test_non_terminal_has_nash(self):
        """Non-terminal states should have nash and game."""
        W = np.full((3, 3), 0.5)
        result = conquest_nash(W)

        # Initial state is non-terminal
        initial = result[-1]
        assert 'nash' in initial
        assert 'game' in initial
        assert len(initial['nash']) == 2


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

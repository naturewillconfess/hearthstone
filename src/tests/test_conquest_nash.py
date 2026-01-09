

import numpy as np
import pytest
from hearthstone import conquest_nash


n_games = 1000
tolerance = 0.1


def two_v_two_conquest(W):
    expected = (
                2*W[0,0]*W[1,0] + W[0,0]*W[1,1] + W[1,0]*W[0,1] +
                2*W[0,1]*W[1,1] - W[0,0]*W[1,0]*W[1,1] - W[0,0]*W[0,1]*W[1,1] -
                W[1,0]*W[0,0]*W[0,1] - W[1,0]*W[0,1]*W[1,1]
            ) / 2
    return expected


def get_probs(strategy):
    """Extract probabilities from strategy list of tuples."""
    return np.array([prob for _, prob in strategy])


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

        # Match should be 50/50
        assert result.winrate == pytest.approx(0.5, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.5"

    def test_2x2_symmetric(self):
        """Test a 2x2 symmetric Conquest match (BO3 style)."""
        W = np.full((2, 2), 0.5)
        result = conquest_nash(W)

        assert result.winrate == pytest.approx(0.5, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.5"

    def test_4x4_symmetric(self):
        """Test a 4x4 symmetric Conquest match (BO7 style)."""
        W = np.full((4, 4), 0.5)
        result = conquest_nash(W)

        assert result.winrate == pytest.approx(0.5, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.5"


class TestMinimalConquest:
    """
    Test the smallest possible Conquest match (1 deck each).
    """

    def test_1x1_biased(self):
        """1x1 Conquest with biased winrate."""
        W = np.array([[0.7]])
        result = conquest_nash(W)

        # With one deck each, match winrate equals single game winrate
        assert result.winrate == pytest.approx(0.7, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.7"


class TestCalibration:
    """
    Statistical calibration tests for random Conquest matches.
    """

    def test_random_3x3_calibration(self):
        """
        Test that random 3x3 Conquest matches have expected properties.

        Over many random matches, mean winrate and strategies should be near uniform.
        """

        winrates = []
        hero_strategies = []
        opp_strategies = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = conquest_nash(W)
            winrates.append(result.winrate)
            hero_strategies.append(get_probs(result.hero_strategy))
            opp_strategies.append(get_probs(result.opp_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_strategies, axis=0)
        mean_opp = np.mean(opp_strategies, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/3]*3, atol=tolerance, err_msg=f"Hero strategy mismatch: {mean_hero}")
        np.testing.assert_allclose(mean_opp, [1/3]*3, atol=tolerance, err_msg=f"Opp strategy mismatch: {mean_opp}")

    def test_random_2x2_calibration(self):
        """Test calibration for 2x2 Conquest matches."""

        winrates = []
        hero_strategies = []
        opp_strategies = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (2, 2))
            result = conquest_nash(W)
            winrates.append(result.winrate)
            hero_strategies.append(get_probs(result.hero_strategy))
            opp_strategies.append(get_probs(result.opp_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_strategies, axis=0)
        mean_opp = np.mean(opp_strategies, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/2]*2, atol=tolerance, err_msg=f"Hero strategy mismatch: {mean_hero}")
        np.testing.assert_allclose(mean_opp, [1/2]*2, atol=tolerance, err_msg=f"Opp strategy mismatch: {mean_opp}")


class TestBO3Formula:
    """
    Test the analytical formula for BO3 Conquest (2 decks each).

    In a 2v2 Conquest match, the expected winrate can be computed analytically.
    This test verifies that conquest_nash produces the correct value.
    """

    def test_bo3_multiple_random(self):
        """Test BO3 formula on multiple random matrices."""
        for _ in range(20):
            W = np.random.uniform(0.2, 0.8, (2, 2))
            result = conquest_nash(W)

            expected = two_v_two_conquest(W)
            assert result.winrate == pytest.approx(expected, abs=1e-6), f"Winrate mismatch. Actual: {result.winrate}, Expected: {expected}"


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
        W = np.random.uniform(0, 1, (3, 3))
        result = conquest_nash(W)

        # State (0, 1) vs (): Hero eliminated decks 0 and 1, only deck 2 remains
        # Opponent has all decks. Hero wins if deck 2 beats all 3 opponent decks.
        # P(win) = 1 - (1-W[2,0])*(1-W[2,1])*(1-W[2,2])
        state_01 = result.get_state(hero_won=['Deck 0', 'Deck 1'], opp_won=[])
        expected_01 = 1 - (1-W[2,0])*(1-W[2,1])*(1-W[2,2])
        assert state_01.winrate == pytest.approx(expected_01, abs=1e-6), f"State (0,1) vs () winrate mismatch. Actual: {state_01.winrate}, Expected: {expected_01}"

        # State (0,2) vs (): Hero has only deck 1, opponent has all
        state_02 = result.get_state(hero_won=['Deck 0', 'Deck 2'], opp_won=[])
        expected_02 = 1 - (1-W[1,0])*(1-W[1,1])*(1-W[1,2])
        assert state_02.winrate == pytest.approx(expected_02, abs=1e-6), f"State (0,2) vs () winrate mismatch. Actual: {state_02.winrate}, Expected: {expected_02}"

        # State (0,) vs (0,): Hero eliminated deck 0, Opp eliminated deck 0
        # Hero has decks 1,2 vs Opp decks 1,2
        # This is a 2v2 subgame with the reduced matrix W[1:3, 1:3]
        state_0_0 = result.get_state(hero_won=['Deck 0'], opp_won=['Deck 0'])
        W_sub = W[1:3, 1:3]  # Submatrix for remaining decks
        # Use the BO3 formula on the submatrix
        expected_0_0 = two_v_two_conquest(W_sub)
        assert state_0_0.winrate == pytest.approx(expected_0_0, abs=1e-6), f"State (0,) vs (0,) winrate mismatch. Actual: {state_0_0.winrate}, Expected: {expected_0_0}"


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
        assert len(result.all_states) == 15

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
        assert len(result.all_states) == 63


class TestAsymmetricMatchups:
    """
    Test with asymmetric winrate matrices.
    """

    def test_strong_deck(self):
        """
        Test where one deck dominates all others.

        If deck 0 beats everything with 90% winrate, Hero should have
        an overall advantage (winrate > 0.5).

        """
        W = np.array([
            [0.9, 0.9, 0.9],  # Deck 0 is very strong
            [0.5, 0.5, 0.5],
            [0.5, 0.5, 0.5]
        ])
        result = conquest_nash(W)

        # Hero should have winrate > 0.5 due to strong deck
        assert result.winrate > 0.5

    def test_weak_deck(self):
        """
        Test where one deck is very weak.

        """
        W = np.array([
            [0.1, 0.1, 0.1],  # Deck 0 is very weak
            [0.5, 0.5, 0.5],
            [0.5, 0.5, 0.5]
        ])
        result = conquest_nash(W)

        # Hero should have winrate < 0.5 due to weak deck
        assert result.winrate < 0.5

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

        # Should be near 50/50 due to symmetry
        assert result.winrate == pytest.approx(0.5, abs=0.1), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: ~0.5"

        # Strategies should be mixed (no pure strategy dominates)
        hero_probs = get_probs(result.hero_strategy)
        assert all(p > 0.1 for p in hero_probs)


class TestTerminalStates:
    """
    Test that terminal states have correct values.
    """

    def test_wins_state(self):
        """When Hero has eliminated all decks, winrate should be 1."""
        W = np.full((2, 2), 0.5)
        result = conquest_nash(W)

        # Check state where hero has eliminated both decks
        hero_wins = result.get_state(hero_won=['Deck 0', 'Deck 1'], opp_won=[])
        assert hero_wins.winrate == pytest.approx(1.0, abs=1e-6), f"Hero winrate should be 1.0 when hero wins. Actual: {hero_wins.winrate}"

        # Check state where opponent has eliminated both decks
        opp_wins = result.get_state(hero_won=[], opp_won=['Deck 0', 'Deck 1'])
        assert opp_wins.winrate == pytest.approx(0.0, abs=1e-6), f"Hero winrate should be 0.0 when opp wins. Actual: {opp_wins.winrate}"


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

"""
test_lhs_nash.py - Tests for the Last Hero Standing (LHS) format Nash calculator

These tests verify that lhs_nash() correctly computes Nash equilibria
for the LHS tournament format. Tests include:
1. Symmetric games (expected 50/50 match winrate)
2. Minimal 1-deck games
3. Statistical calibration with random matrices
4. Forced play mechanics verification
5. Comparison with Conquest (should differ for asymmetric matrices)
6. Terminal state verification

The test cases are ported from the R package's test-LHS_nash.R with additions.
"""

import numpy as np
import pytest
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from hearthstone import lhs_nash, conquest_nash


def find_initial_state(result):
    """
    Find the initial state from LHS results.

    The initial state has:
    - Empty score for both players (no losses yet)
    - No forced plays (havetoplay_hero and havetoplay_opp are None)
    """
    for state in result:
        if (state['score'] == ((), ()) and
            state.get('havetoplay_hero') is None and
            state.get('havetoplay_opp') is None):
            return state
    # If no state with None forced plays, return last state
    return result[-1]


class TestSymmetricLHS:
    """
    Test symmetric games where all matchups are 50/50.
    """

    def test_3x3_symmetric(self):
        """Test a basic 3x3 symmetric LHS match."""
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)
        initial = find_initial_state(result)

        # Match should be 50/50
        assert initial['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert initial['winrate'][1] == pytest.approx(0.5, abs=1e-6)

    def test_2x2_symmetric(self):
        """Test a 2x2 symmetric LHS match."""
        W = np.full((2, 2), 0.5)
        result = lhs_nash(W)
        initial = find_initial_state(result)

        assert initial['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert initial['winrate'][1] == pytest.approx(0.5, abs=1e-6)

    def test_4x4_symmetric(self):
        """Test a 4x4 symmetric LHS match."""
        W = np.full((4, 4), 0.5)
        result = lhs_nash(W)
        initial = find_initial_state(result)

        assert initial['winrate'][0] == pytest.approx(0.5, abs=1e-6)


class TestMinimalLHS:
    """
    Test the smallest possible LHS match (1 deck each).
    """

    def test_1x1_lhs(self):
        """
        1x1 LHS: Single game determines the match.

        With W = [[0.5]], the match is just one coin flip.
        """
        W = np.array([[0.5]])
        result = lhs_nash(W)
        initial = find_initial_state(result)

        assert initial['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert initial['winrate'][1] == pytest.approx(0.5, abs=1e-6)

    def test_1x1_biased(self):
        """1x1 LHS with biased winrate."""
        W = np.array([[0.7]])
        result = lhs_nash(W)
        initial = find_initial_state(result)

        # With one deck each, match winrate equals single game winrate
        assert initial['winrate'][0] == pytest.approx(0.7, abs=1e-6)


class TestCalibration:
    """
    Statistical calibration tests for random LHS matches.
    """

    def test_random_3x3_calibration(self):
        """
        Test that random 3x3 LHS matches have expected properties.

        Over many random matches:
        - Mean match winrate should be near 0.5
        - Mean deck selection probabilities should be near 1/3

        Note: Using fewer games than R's 1000 for faster testing.
        The R test uses tolerance of 0.2.
        """
        np.random.seed(42)
        n_games = 100  # Reduced from R's 1000 for speed
        tolerance = 0.2

        winrates = []
        hero_strategies = []
        opp_strategies = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = lhs_nash(W)
            initial = find_initial_state(result)

            winrates.append(initial['winrate'][0])
            if 'nash' in initial:
                hero_strategies.append(initial['nash'][0])
                opp_strategies.append(initial['nash'][1])

        mean_winrate = np.mean(winrates)

        # Match winrate should be near 0.5
        assert abs(mean_winrate - 0.5) < tolerance, \
            f"Mean winrate {mean_winrate} too far from 0.5"

        # Strategy probabilities should be near 1/3 (if we have strategies)
        if hero_strategies:
            mean_hero = np.mean(hero_strategies, axis=0)
            mean_opp = np.mean(opp_strategies, axis=0)

            for i, prob in enumerate(mean_hero):
                assert abs(prob - 1/3) < tolerance, \
                    f"Hero strategy[{i}] = {prob} too far from 0.333"
            for i, prob in enumerate(mean_opp):
                assert abs(prob - 1/3) < tolerance, \
                    f"Opp strategy[{i}] = {prob} too far from 0.333"

    def test_random_2x2_calibration(self):
        """Test calibration for 2x2 LHS matches."""
        np.random.seed(44)
        n_games = 100
        tolerance = 0.2

        winrates = []
        for _ in range(n_games):
            W = np.random.uniform(0, 1, (2, 2))
            result = lhs_nash(W)
            initial = find_initial_state(result)
            winrates.append(initial['winrate'][0])

        mean_winrate = np.mean(winrates)
        assert abs(mean_winrate - 0.5) < tolerance


class TestLHSSpecificBehavior:
    """
    Tests specific to LHS format behavior (different from Conquest).
    """

    def test_forced_play_states_exist(self):
        """
        Verify that LHS creates states with forced plays.

        In LHS, after someone wins a game, they must keep playing
        that deck. This should create states with non-None havetoplay values.
        """
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)

        # Should have some states with forced plays
        forced_states = [s for s in result
                        if s.get('havetoplay_hero') is not None or
                           s.get('havetoplay_opp') is not None]

        assert len(forced_states) > 0, "LHS should have forced play states"

    def test_hero_forced_play(self):
        """
        Test a state where Hero is forced to play a specific deck.
        """
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)

        # Find a state where Hero is forced to play
        hero_forced = [s for s in result if s.get('havetoplay_hero') is not None]

        assert len(hero_forced) > 0, "Should have states where Hero is forced"

        # In such states, Hero has only one choice, so their "strategy"
        # should be a single-element array (or trivially determined)
        for state in hero_forced:
            if 'game' in state:
                # Game matrix should have 1 row (Hero's single choice)
                assert state['game'].shape[0] == 1

    def test_opp_forced_play(self):
        """
        Test a state where Opponent is forced to play a specific deck.
        """
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)

        # Find a state where Opponent is forced to play
        opp_forced = [s for s in result if s.get('havetoplay_opp') is not None]

        assert len(opp_forced) > 0, "Should have states where Opponent is forced"

        # In such states, Opponent has only one choice, so the game matrix
        # should have 1 column
        for state in opp_forced:
            if 'game' in state:
                assert state['game'].shape[1] == 1

    def test_forced_play_count_by_score(self):
        """
        Verify correct number of forced play variations per score.

        For a non-terminal state where only opponent has losses,
        hero should be forced (hero won last game).
        """
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)

        # States with hero_lost=(), opp_lost=(0,) should have hero forced
        # to one of the 3 available decks
        states_0_vs_1 = [s for s in result
                        if s['score'] == ((), (0,)) and
                           s.get('havetoplay_hero') is not None]

        # Should have 3 variations (hero forced to deck 0, 1, or 2)
        assert len(states_0_vs_1) == 3


class TestTerminalStates:
    """
    Test that terminal states have correct winrates.
    """

    def test_hero_wins_all(self):
        """
        When opponent has lost all decks, Hero should have winrate 1.
        """
        W = np.full((2, 2), 0.5)
        result = lhs_nash(W)

        # Find state where opponent has lost all decks
        # In LHS, score tracks lost decks, so opp_lost = (0, 1) for 2 decks
        hero_wins = [s for s in result if len(s['score'][1]) == 2]

        for state in hero_wins:
            assert state['winrate'][0] == pytest.approx(1.0, abs=1e-6)
            assert state['winrate'][1] == pytest.approx(0.0, abs=1e-6)

    def test_opponent_wins_all(self):
        """
        When Hero has lost all decks, Hero should have winrate 0.
        """
        W = np.full((2, 2), 0.5)
        result = lhs_nash(W)

        # Find state where hero has lost all decks
        opp_wins = [s for s in result if len(s['score'][0]) == 2]

        for state in opp_wins:
            assert state['winrate'][0] == pytest.approx(0.0, abs=1e-6)
            assert state['winrate'][1] == pytest.approx(1.0, abs=1e-6)

    def test_near_terminal_hero_one_left(self):
        """
        When Hero has n-1 losses, verify the formula.

        Hero has one deck left. They win if that deck beats ALL remaining
        opponent decks in a row.
        """
        np.random.seed(567)
        W = np.random.uniform(0, 1, (3, 3))
        result = lhs_nash(W)

        # Find state: hero lost {0, 1}, opp lost nothing
        # Hero has only deck 2 left
        for state in result:
            if state['score'] == ((0, 1), ()):
                # Hero wins if deck 2 beats all 3 opponent decks
                expected = W[2, 0] * W[2, 1] * W[2, 2]
                assert state['winrate'][0] == pytest.approx(expected, abs=1e-6)
                break


class TestComparisonWithConquest:
    """
    Test that LHS and Conquest give different results for asymmetric matrices.
    """

    def test_different_from_conquest(self):
        """
        LHS and Conquest should give different winrates for most matrices.

        The formats have different rules, so optimal strategies differ.
        """
        np.random.seed(888)
        different_count = 0

        for _ in range(20):
            W = np.random.uniform(0.3, 0.7, (3, 3))

            lhs_result = lhs_nash(W)
            conquest_result = conquest_nash(W)

            lhs_initial = find_initial_state(lhs_result)
            conquest_initial = conquest_result[-1]

            lhs_wr = lhs_initial['winrate'][0]
            conquest_wr = conquest_initial['winrate'][0]

            if abs(lhs_wr - conquest_wr) > 0.01:
                different_count += 1

        # Most random matrices should give different results
        assert different_count > 10, \
            f"Only {different_count}/20 matrices gave different results"

    def test_same_for_symmetric(self):
        """
        For symmetric matrices, LHS and Conquest should both give 50/50.

        While strategies might differ, the value should be 0.5 for both.
        """
        W = np.full((3, 3), 0.5)

        lhs_result = lhs_nash(W)
        conquest_result = conquest_nash(W)

        lhs_initial = find_initial_state(lhs_result)
        conquest_initial = conquest_result[-1]

        assert lhs_initial['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert conquest_initial['winrate'][0] == pytest.approx(0.5, abs=1e-6)


class TestAsymmetricMatchups:
    """
    Test with asymmetric winrate matrices.
    """

    def test_strong_deck(self):
        """
        Test where one deck dominates all others.
        """
        W = np.array([
            [0.9, 0.9, 0.9],  # Deck 0 is very strong
            [0.5, 0.5, 0.5],
            [0.5, 0.5, 0.5]
        ])
        result = lhs_nash(W)
        initial = find_initial_state(result)

        # Hero should have winrate > 0.5 due to strong deck
        assert initial['winrate'][0] > 0.5

    def test_weak_deck(self):
        """
        Test where one deck is very weak.
        """
        W = np.array([
            [0.1, 0.1, 0.1],  # Deck 0 is very weak
            [0.5, 0.5, 0.5],
            [0.5, 0.5, 0.5]
        ])
        result = lhs_nash(W)
        initial = find_initial_state(result)

        # Hero should have winrate < 0.5 due to weak deck
        assert initial['winrate'][0] < 0.5


class TestOutputStructure:
    """
    Test the structure of lhs_nash output.
    """

    def test_output_is_list(self):
        """Output should be a list."""
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)
        assert isinstance(result, list)

    def test_state_has_required_keys(self):
        """Each state should have score, winrate, and havetoplay fields."""
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)

        for state in result:
            assert 'score' in state
            assert 'winrate' in state
            assert 'havetoplay_hero' in state
            assert 'havetoplay_opp' in state
            assert len(state['winrate']) == 2

    def test_larger_state_count_than_conquest(self):
        """
        LHS should have more states than Conquest due to forced play tracking.
        """
        W = np.full((3, 3), 0.5)

        lhs_result = lhs_nash(W)
        conquest_result = conquest_nash(W)

        # LHS has more states due to forced play variations
        assert len(lhs_result) > len(conquest_result)


class TestWinrateProperties:
    """
    Test mathematical properties of winrates.
    """

    def test_winrates_sum_to_one(self):
        """Hero winrate + Opponent winrate should equal 1."""
        np.random.seed(333)
        W = np.random.uniform(0, 1, (3, 3))
        result = lhs_nash(W)

        for state in result:
            assert state['winrate'][0] + state['winrate'][1] == pytest.approx(1.0, abs=1e-10)

    def test_winrates_in_valid_range(self):
        """All winrates should be between 0 and 1."""
        np.random.seed(444)
        W = np.random.uniform(0, 1, (3, 3))
        result = lhs_nash(W)

        for state in result:
            assert 0 <= state['winrate'][0] <= 1
            assert 0 <= state['winrate'][1] <= 1


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

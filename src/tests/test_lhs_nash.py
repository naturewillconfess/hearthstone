

import numpy as np
import pytest

from hearthstone import lhs_nash, conquest_nash


n_games = 200
tolerance = 0.05


def get_probs(strategy):
    """Extract probabilities from strategy list of tuples."""
    return np.array([prob for _, prob in strategy])


class TestSymmetricLHS:
    """
    Test symmetric games where all matchups are 50/50.
    """

    def test_3x3_symmetric(self):
        """Test a basic 3x3 symmetric LHS match."""
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)

        # Match should be 50/50
        assert result.winrate == pytest.approx(0.5, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.5"

    def test_2x2_symmetric(self):
        """Test a 2x2 symmetric LHS match."""
        W = np.full((2, 2), 0.5)
        result = lhs_nash(W)

        assert result.winrate == pytest.approx(0.5, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.5"

    def test_4x4_symmetric(self):
        """Test a 4x4 symmetric LHS match."""
        W = np.full((4, 4), 0.5)
        result = lhs_nash(W)

        assert result.winrate == pytest.approx(0.5, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.5"


class TestMinimalLHS:
    """
    Test the smallest possible LHS match (1 deck each).
    """

    def test_1x1_biased(self):
        """1x1 LHS with biased winrate."""
        W = np.array([[0.7]])
        result = lhs_nash(W)

        # With one deck each, match winrate equals single game winrate
        assert result.winrate == pytest.approx(0.7, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.7"


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
        """

        winrates = []
        hero_strategies = []
        opp_strategies = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = lhs_nash(W)

            winrates.append(result.winrate)
            hero_strategies.append(get_probs(result.hero_strategy))
            opp_strategies.append(get_probs(result.opp_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_strategies, axis=0)
        mean_opp = np.mean(opp_strategies, axis=0)

        # Match winrate should be near 0.5
        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/3]*3, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[1/3]*3}")
        np.testing.assert_allclose(mean_opp, [1/3]*3, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[1/3]*3}")

    def test_random_2x2_calibration(self):
        """Test calibration for 2x2 LHS matches."""
        n_games = 300
        tolerance = 0.1

        winrates = []
        hero_strategies = []
        opp_strategies = []
        for _ in range(n_games):
            W = np.random.uniform(0, 1, (2, 2))
            result = lhs_nash(W)

            winrates.append(result.winrate)
            hero_strategies.append(get_probs(result.hero_strategy))
            opp_strategies.append(get_probs(result.opp_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_strategies, axis=0)
        mean_opp = np.mean(opp_strategies, axis=0)

        # Match winrate should be near 0.5
        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/2]*2, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[1/2]*2}")
        np.testing.assert_allclose(mean_opp, [1/2]*2, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[1/2]*2}")


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

        # Access raw states to check forced play
        raw_states = result._states

        # Should have some states with forced plays
        forced_states = [s for s in raw_states
                        if s.get('havetoplay_hero') is not None or
                           s.get('havetoplay_opp') is not None]

        assert len(forced_states) > 0, "LHS should have forced play states"

    def test_hero_forced_play(self):
        """
        Test a state where Hero is forced to play a specific deck.
        """
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)

        raw_states = result._states

        # Find a state where Hero is forced to play
        hero_forced = [s for s in raw_states if s.get('havetoplay_hero') is not None]

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

        raw_states = result._states

        # Find a state where Opponent is forced to play
        opp_forced = [s for s in raw_states if s.get('havetoplay_opp') is not None]

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

        raw_states = result._states

        # States with hero_lost=(), opp_lost=(0,) should have hero forced
        # to one of the 3 available decks
        states_0_vs_1 = [s for s in raw_states
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

        raw_states = result._states

        # Find state where opponent has lost all decks
        # In LHS, score tracks lost decks, so opp_lost = (0, 1) for 2 decks
        hero_wins = [s for s in raw_states if len(s['score'][1]) == 2]

        for state in hero_wins:
            assert state['winrate'][0] == pytest.approx(1.0, abs=1e-6), f"Hero winrate should be 1.0 when hero wins. Actual: {state['winrate'][0]}"
            assert state['winrate'][1] == pytest.approx(0.0, abs=1e-6), f"Opp winrate should be 0.0 when hero wins. Actual: {state['winrate'][1]}"

    def test_opponent_wins_all(self):
        """
        When Hero has lost all decks, Hero should have winrate 0.
        """
        W = np.full((2, 2), 0.5)
        result = lhs_nash(W)

        raw_states = result._states

        # Find state where hero has lost all decks
        opp_wins = [s for s in raw_states if len(s['score'][0]) == 2]

        for state in opp_wins:
            assert state['winrate'][0] == pytest.approx(0.0, abs=1e-6), f"Hero winrate should be 0.0 when opp wins. Actual: {state['winrate'][0]}"
            assert state['winrate'][1] == pytest.approx(1.0, abs=1e-6), f"Opp winrate should be 1.0 when opp wins. Actual: {state['winrate'][1]}"

    def test_near_terminal_hero_one_left(self):
        """
        When Hero has n-1 losses, verify the formula.

        Hero has one deck left. They win if that deck beats ALL remaining
        opponent decks in a row.
        """
        W = np.random.uniform(0, 1, (3, 3))
        result = lhs_nash(W)

        raw_states = result._states

        # Find state: hero lost {0, 1}, opp lost nothing
        # Hero has only deck 2 left
        for state in raw_states:
            if state['score'] == ((0, 1), ()):
                # Hero wins if deck 2 beats all 3 opponent decks
                expected = W[2, 0] * W[2, 1] * W[2, 2]
                assert state['winrate'][0] == pytest.approx(expected, abs=1e-6), f"Near-terminal winrate mismatch. Actual: {state['winrate'][0]}, Expected: {expected}"
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
        different_count = 0

        for _ in range(20):
            W = np.random.uniform(0.3, 0.7, (3, 3))

            lhs_result = lhs_nash(W)
            conquest_result = conquest_nash(W)

            lhs_wr = lhs_result.winrate
            conquest_wr = conquest_result.winrate

            if abs(lhs_wr - conquest_wr) > 0.005:
                different_count += 1

        # Most random matrices should give different results
        assert different_count > 10, \
            f"Only {different_count}/20 matrices gave different results"


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
        result = lhs_nash(W)

        # Hero should have winrate < 0.5 due to weak deck
        assert result.winrate < 0.5


class TestOutputStructure:
    """
    Test the structure of lhs_nash output.
    """

    def test_output_is_result_object(self):
        """Output should be an LHSResult object."""
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)
        from hearthstone import LHSResult
        assert isinstance(result, LHSResult)

    def test_state_access_via_api(self):
        """Can access states through the result API."""
        W = np.full((3, 3), 0.5)
        result = lhs_nash(W)

        # Should be able to access initial state
        assert hasattr(result, 'winrate')
        assert hasattr(result, 'hero_strategy')
        assert hasattr(result, 'opp_strategy')

        # Should be able to get all states
        all_states = result.all_states()
        assert len(all_states) > 0

    def test_larger_state_count_than_conquest(self):
        """
        LHS should have more states than Conquest due to forced play tracking.
        """
        W = np.full((3, 3), 0.5)

        lhs_result = lhs_nash(W)
        conquest_result = conquest_nash(W)

        # LHS has more states due to forced play variations
        assert len(lhs_result.all_states()) > len(conquest_result.all_states())


class TestWinrateProperties:
    """
    Test mathematical properties of winrates.
    """

    def test_winrates_in_valid_range(self):
        """All winrates should be between 0 and 1."""
        W = np.random.uniform(0, 1, (3, 3))
        result = lhs_nash(W)

        all_states = result.all_states()
        for state in all_states:
            assert 0 <= state.winrate <= 1


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

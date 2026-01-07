"""
test_ban_nash.py - Tests for the ban phase Nash calculator

These tests verify that ban_nash() correctly computes optimal ban strategies
for tournament matches with ban phases. Tests include:
1. Symmetric games (expected 50/50 match winrate after bans)
2. Statistical calibration with various matrix sizes and ban counts
3. Both Conquest and LHS formats
4. Name propagation through nested results
5. get_match functionality

The test cases are ported from the R package's test-ban_nash.R
"""

import numpy as np
import pytest

from hearthstone import ban_nash, BanResult, ConquestResult, LHSResult


# Test parameters
n_games = 200
tolerance = 0.075


def get_ban_probs(ban_strategy):
    """Extract probabilities from ban strategy list of tuples."""
    return np.array([prob for _, prob in ban_strategy])


class TestSymmetricBanNash:
    """
    Test symmetric games where all matchups are 50/50.
    """

    def test_4x4_symmetric_conquest(self):
        """Test a 4x4 symmetric game with 1 ban in Conquest format."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=1, match_format='conquest')

        # Match should be 50/50
        assert result.winrate == pytest.approx(0.5, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.5"

    def test_4x4_symmetric_lhs(self):
        """Test a 4x4 symmetric game with 1 ban in LHS format."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=1, match_format='lhs')

        # Match should be 50/50
        assert result.winrate == pytest.approx(0.5, abs=1e-6), f"Hero winrate mismatch. Actual: {result.winrate}, Expected: 0.5"


class TestCalibrationConquest:
    """
    Statistical calibration tests for Conquest format with bans.
    """

    def test_3x3_1ban_conquest(self):
        """
        Test 3x3 Conquest with 1 ban.

        For random games:
        - Mean winrate should be near 0.5
        - Mean ban probabilities should be near 1/3 (3 choices)
        """

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = ban_nash(W, bans=1, match_format='conquest')
            winrates.append(result.winrate)
            hero_bans.append(get_ban_probs(result.hero_ban_strategy))
            opp_bans.append(get_ban_probs(result.opp_ban_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        # Winrate should be near 0.5
        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"

        # Ban probabilities should be near 1/3
        np.testing.assert_allclose(mean_hero, [1/3] * 3, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[1/3] * 3}")
        np.testing.assert_allclose(mean_opp, [1/3] * 3, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[1/3] * 3}")

    def test_3x3_2ban_conquest(self):
        """
        Test 3x3 Conquest with 2 bans.

        With 2 bans from 3 decks, there are C(3,2)=3 ban combinations.
        Each ban combination should have probability near 1/3.
        """

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = ban_nash(W, bans=2, match_format='conquest')
            winrates.append(result.winrate)
            hero_bans.append(get_ban_probs(result.hero_ban_strategy))
            opp_bans.append(get_ban_probs(result.opp_ban_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/3] * 3, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[1/3] * 3}")
        np.testing.assert_allclose(mean_opp, [1/3] * 3, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[1/3] * 3}")

    def test_4x4_1ban_conquest(self):
        """
        Test 4x4 Conquest with 1 ban.

        With 1 ban from 4 decks, there are 4 ban choices.
        Each should have probability near 1/4.
        """

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (4, 4))
            result = ban_nash(W, bans=1, match_format='conquest')
            winrates.append(result.winrate)
            hero_bans.append(get_ban_probs(result.hero_ban_strategy))
            opp_bans.append(get_ban_probs(result.opp_ban_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [0.25] * 4, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[0.25] * 4}")
        np.testing.assert_allclose(mean_opp, [0.25] * 4, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[0.25] * 4}")

    def test_4x4_2ban_conquest(self):
        """
        Test 4x4 Conquest with 2 bans.

        With 2 bans from 4 decks, there are C(4,2)=6 ban combinations.
        Each should have probability near 1/6.
        """

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (4, 4))
            result = ban_nash(W, bans=2, match_format='conquest')
            winrates.append(result.winrate)
            hero_bans.append(get_ban_probs(result.hero_ban_strategy))
            opp_bans.append(get_ban_probs(result.opp_ban_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/6] * 6, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[1/6] * 6}")
        np.testing.assert_allclose(mean_opp, [1/6] * 6, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[1/6] * 6}")


class TestCalibrationLHS:
    """
    Statistical calibration tests for LHS format with bans.
    """

    def test_3x3_1ban_lhs(self):
        """Test 3x3 LHS with 1 ban."""

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = ban_nash(W, bans=1, match_format='lhs')
            winrates.append(result.winrate)
            hero_bans.append(get_ban_probs(result.hero_ban_strategy))
            opp_bans.append(get_ban_probs(result.opp_ban_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/3] * 3, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[1/3] * 3}")
        np.testing.assert_allclose(mean_opp, [1/3] * 3, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[1/3] * 3}")

    def test_3x3_2ban_lhs(self):
        """Test 3x3 LHS with 2 bans."""

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = ban_nash(W, bans=2, match_format='lhs')
            winrates.append(result.winrate)
            hero_bans.append(get_ban_probs(result.hero_ban_strategy))
            opp_bans.append(get_ban_probs(result.opp_ban_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/3] * 3, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[1/3] * 3}")
        np.testing.assert_allclose(mean_opp, [1/3] * 3, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[1/3] * 3}")

    def test_4x4_1ban_lhs(self):
        """Test 4x4 LHS with 1 ban."""

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (4, 4))
            result = ban_nash(W, bans=1, match_format='lhs')
            winrates.append(result.winrate)
            hero_bans.append(get_ban_probs(result.hero_ban_strategy))
            opp_bans.append(get_ban_probs(result.opp_ban_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [0.25] * 4, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[0.25] * 4}")
        np.testing.assert_allclose(mean_opp, [0.25] * 4, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[0.25] * 4}")

    def test_4x4_2ban_lhs(self):
        """Test 4x4 LHS with 2 bans."""

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (4, 4))
            result = ban_nash(W, bans=2, match_format='lhs')
            winrates.append(result.winrate)
            hero_bans.append(get_ban_probs(result.hero_ban_strategy))
            opp_bans.append(get_ban_probs(result.opp_ban_strategy))

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance), f"Mean winrate mismatch. Actual: {mean_winrate}, Expected: 0.5"
        np.testing.assert_allclose(mean_hero, [1/6] * 6, atol=tolerance, err_msg=f"Hero strategy does not match. Actual: {mean_hero}, Desired: {[1/6] * 6}")
        np.testing.assert_allclose(mean_opp, [1/6] * 6, atol=tolerance, err_msg=f"Opp strategy does not match. Actual: {mean_opp}, Desired: {[1/6] * 6}")


class TestBanNashStructure:
    """
    Tests for the structure of ban_nash output.
    """

    def test_output_is_ban_result(self):
        """Verify the output is a BanResult object."""
        W = np.random.uniform(0, 1, (4, 4))
        result = ban_nash(W, bans=1, match_format='conquest')

        assert isinstance(result, BanResult)
        assert hasattr(result, 'winrate')
        assert hasattr(result, 'hero_ban_strategy')
        assert hasattr(result, 'opp_ban_strategy')
        assert hasattr(result, 'get_match')

    def test_strategy_dimensions(self):
        """Verify strategy dimensions match expected ban combinations."""
        W = np.random.uniform(0, 1, (4, 4))
        result = ban_nash(W, bans=1, match_format='conquest')

        # For 4 decks with 1 ban, there are 4 ban options
        n_ban_options = 4
        assert len(result.hero_ban_strategy) == n_ban_options
        assert len(result.opp_ban_strategy) == n_ban_options

    def test_get_match_returns_conquest_result(self):
        """Verify get_match returns a ConquestResult for conquest format."""
        W = np.random.uniform(0, 1, (4, 4))
        result = ban_nash(W, bans=1, match_format='conquest',
                          hero_names=['A', 'B', 'C', 'D'],
                          opp_names=['W', 'X', 'Y', 'Z'])

        # Get a specific match after bans
        # Hero bans opponent's deck 'W', Opponent bans Hero's deck 'A'
        match = result.get_match(hero_bans=['W'], opp_bans=['A'])

        assert isinstance(match, ConquestResult)
        assert hasattr(match, 'winrate')

    def test_get_match_returns_lhs_result(self):
        """Verify get_match returns a LHSResult for lhs format."""
        W = np.random.uniform(0, 1, (4, 4))
        result = ban_nash(W, bans=1, match_format='lhs',
                          hero_names=['A', 'B', 'C', 'D'],
                          opp_names=['W', 'X', 'Y', 'Z'])

        match = result.get_match(hero_bans=['X'], opp_bans=['B'])
        assert isinstance(match, LHSResult)


class TestBanNashErrors:
    """
    Tests for error handling in ban_nash.
    """

    def test_too_many_bans(self):
        """Should raise ValueError if bans >= n."""
        W = np.random.uniform(0, 1, (3, 3))
        with pytest.raises(ValueError, match="Too many bans"):
            ban_nash(W, bans=3)

    def test_zero_bans(self):
        """Should raise ValueError if bans < 1."""
        W = np.random.uniform(0, 1, (3, 3))
        with pytest.raises(ValueError, match="at least 1"):
            ban_nash(W, bans=0)

    def test_invalid_format(self):
        """Should raise ValueError for invalid match format."""
        W = np.random.uniform(0, 1, (3, 3))
        with pytest.raises(ValueError, match="Unknown format"):
            ban_nash(W, bans=1, match_format='invalid')


class TestNamePropagation:
    """
    Test that deck names are properly propagated through nested structures.
    """

    def test_names_preserved_after_bans_conquest(self):
        """Verify deck names are preserved correctly in ConquestResult after bans."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=1,
                          hero_names=['Aggro', 'Combo', 'Control', 'Midrange'],
                          opp_names=['Tempo', 'Ramp', 'Mill', 'Zoo'],
                          match_format='conquest')

        # Hero bans 'Tempo', Opponent bans 'Aggro'
        match = result.get_match(hero_bans=['Tempo'], opp_bans=['Aggro'])

        # Hero's remaining decks should be Combo, Control, Midrange (Aggro banned)
        hero_deck_names = [name for name, _ in match.hero_strategy]
        assert 'Aggro' not in hero_deck_names
        assert set(hero_deck_names) == {'Combo', 'Control', 'Midrange'}

        # Opponent's remaining decks should be Ramp, Mill, Zoo (Tempo banned)
        opp_deck_names = [name for name, _ in match.opp_strategy]
        assert 'Tempo' not in opp_deck_names
        assert set(opp_deck_names) == {'Ramp', 'Mill', 'Zoo'}

    def test_names_preserved_after_bans_lhs(self):
        """Verify deck names are preserved correctly in LHSResult after bans."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=1,
                          hero_names=['Aggro', 'Combo', 'Control', 'Midrange'],
                          opp_names=['Tempo', 'Ramp', 'Mill', 'Zoo'],
                          match_format='lhs')

        # Hero bans 'Mill', Opponent bans 'Control'
        match = result.get_match(hero_bans=['Mill'], opp_bans=['Control'])

        hero_deck_names = [name for name, _ in match.hero_strategy]
        assert set(hero_deck_names) == {'Aggro', 'Combo', 'Midrange'}

        opp_deck_names = [name for name, _ in match.opp_strategy]
        assert set(opp_deck_names) == {'Tempo', 'Ramp', 'Zoo'}

    def test_names_in_subgame_states(self):
        """Verify names are correct when querying states within a match."""
        W = np.full((3, 3), 0.5)
        result = ban_nash(W, bans=1,
                          hero_names=['Aggro', 'Combo', 'Control'],
                          opp_names=['Tempo', 'Ramp', 'Mill'],
                          match_format='conquest')

        # Hero bans 'Tempo', Opponent bans 'Aggro'
        match = result.get_match(hero_bans=['Tempo'], opp_bans=['Aggro'])

        # The remaining decks after bans: Combo, Control vs Ramp, Mill
        # Query a state using the correct remaining deck names
        state = match.get_state(hero_won=['Combo'], opp_won=[])

        assert state.winrate >= 0 and state.winrate <= 1

        # The hero_strategy in this state should only contain 'Control'
        # since 'Combo' was already eliminated
        hero_remaining = [name for name, _ in state.hero_strategy]
        assert 'Combo' not in hero_remaining
        assert 'Control' in hero_remaining

    def test_ban_strategy_uses_correct_names(self):
        """Verify ban strategies reference correct deck names."""
        W = np.full((3, 3), 0.5)
        result = ban_nash(W, bans=1,
                          hero_names=['Aggro', 'Combo', 'Control'],
                          opp_names=['Tempo', 'Ramp', 'Mill'],
                          match_format='conquest')

        # Hero bans OPPONENT's decks
        for names, prob in result.hero_ban_strategy:
            for name in names:
                assert name in ['Tempo', 'Ramp', 'Mill'], \
                    f"Hero should ban opponent's decks, got '{name}'"

        # Opponent bans HERO's decks
        for names, prob in result.opp_ban_strategy:
            for name in names:
                assert name in ['Aggro', 'Combo', 'Control'], \
                    f"Opponent should ban Hero's decks, got '{name}'"

    def test_2_bans_name_propagation(self):
        """Test name propagation with 2 bans."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=2,
                          hero_names=['A', 'B', 'C', 'D'],
                          opp_names=['W', 'X', 'Y', 'Z'],
                          match_format='conquest')

        # Hero bans W and X, Opponent bans A and B
        match = result.get_match(hero_bans=['W', 'X'], opp_bans=['A', 'B'])

        # Hero has C, D remaining
        hero_deck_names = [name for name, _ in match.hero_strategy]
        assert set(hero_deck_names) == {'C', 'D'}

        # Opponent has Y, Z remaining
        opp_deck_names = [name for name, _ in match.opp_strategy]
        assert set(opp_deck_names) == {'Y', 'Z'}


class TestAllMatches:
    """
    Test the all_matches property.
    """

    def test_all_matches_dimensions(self):
        """all_matches should have correct dimensions."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=1,
                          hero_names=['A', 'B', 'C', 'D'],
                          opp_names=['W', 'X', 'Y', 'Z'],
                          match_format='conquest')

        # C(4,1) = 4 options each
        assert len(result.all_matches) == 4
        assert all(len(row) == 4 for row in result.all_matches)

    def test_all_matches_dimensions_2_bans(self):
        """all_matches should have correct dimensions with 2 bans."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=2,
                          hero_names=['A', 'B', 'C', 'D'],
                          opp_names=['W', 'X', 'Y', 'Z'],
                          match_format='conquest')

        # C(4,2) = 6 options each
        assert len(result.all_matches) == 6
        assert all(len(row) == 6 for row in result.all_matches)

    def test_all_matches_are_result_objects(self):
        """all_matches entries should be ConquestResult or LHSResult."""
        W = np.full((3, 3), 0.5)
        result = ban_nash(W, bans=1, match_format='conquest')

        for row in result.all_matches:
            for match in row:
                assert isinstance(match, ConquestResult)

if __name__ == '__main__':
    pytest.main([__file__, '-v'])

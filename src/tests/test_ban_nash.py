"""
test_ban_nash.py - Tests for the ban phase Nash calculator

These tests verify that ban_nash() correctly computes optimal ban strategies
for tournament matches with ban phases. Tests include:
1. Symmetric games (expected 50/50 match winrate after bans)
2. Statistical calibration with various matrix sizes and ban counts
3. Both Conquest and LHS formats

The test cases are ported from the R package's test-ban_nash.R
"""

import numpy as np
import pytest
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from hearthstone import ban_nash


# Test parameters
n_games = 200
tolerance = 0.075


class TestSymmetricBanNash:
    """
    Test symmetric games where all matchups are 50/50.
    """

    def test_4x4_symmetric_conquest(self):
        """Test a 4x4 symmetric game with 1 ban in Conquest format."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=1, match_format='conquest')

        # Match should be 50/50
        assert result['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert result['winrate'][1] == pytest.approx(0.5, abs=1e-6)

    def test_4x4_symmetric_lhs(self):
        """Test a 4x4 symmetric game with 1 ban in LHS format."""
        W = np.full((4, 4), 0.5)
        result = ban_nash(W, bans=1, match_format='lhs')

        # Match should be 50/50
        assert result['winrate'][0] == pytest.approx(0.5, abs=1e-6)
        assert result['winrate'][1] == pytest.approx(0.5, abs=1e-6)


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
            winrates.append(result['winrate'][0])
            hero_bans.append(result['bans']['hero'])
            opp_bans.append(result['bans']['opp'])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        # Winrate should be near 0.5
        assert mean_winrate == pytest.approx(0.5, abs=tolerance)

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
            winrates.append(result['winrate'][0])
            hero_bans.append(result['bans']['hero'])
            opp_bans.append(result['bans']['opp'])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance)
        np.testing.assert_allclose(mean_hero, [1/3] * 3, atol=tolerance)
        np.testing.assert_allclose(mean_opp, [1/3] * 3, atol=tolerance)

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
            winrates.append(result['winrate'][0])
            hero_bans.append(result['bans']['hero'])
            opp_bans.append(result['bans']['opp'])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance)
        np.testing.assert_allclose(mean_hero, [0.25] * 4, atol=tolerance)
        np.testing.assert_allclose(mean_opp, [0.25] * 4, atol=tolerance)

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
            winrates.append(result['winrate'][0])
            hero_bans.append(result['bans']['hero'])
            opp_bans.append(result['bans']['opp'])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance)
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
            winrates.append(result['winrate'][0])
            hero_bans.append(result['bans']['hero'])
            opp_bans.append(result['bans']['opp'])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance)
        np.testing.assert_allclose(mean_hero, [1/3] * 3, atol=tolerance)
        np.testing.assert_allclose(mean_opp, [1/3] * 3, atol=tolerance)

    def test_3x3_2ban_lhs(self):
        """Test 3x3 LHS with 2 bans."""

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (3, 3))
            result = ban_nash(W, bans=2, match_format='lhs')
            winrates.append(result['winrate'][0])
            hero_bans.append(result['bans']['hero'])
            opp_bans.append(result['bans']['opp'])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance)
        np.testing.assert_allclose(mean_hero, [1/3] * 3, atol=tolerance)
        np.testing.assert_allclose(mean_opp, [1/3] * 3, atol=tolerance)

    def test_4x4_1ban_lhs(self):
        """Test 4x4 LHS with 1 ban."""

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (4, 4))
            result = ban_nash(W, bans=1, match_format='lhs')
            winrates.append(result['winrate'][0])
            hero_bans.append(result['bans']['hero'])
            opp_bans.append(result['bans']['opp'])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance)
        np.testing.assert_allclose(mean_hero, [0.25] * 4, atol=tolerance)
        np.testing.assert_allclose(mean_opp, [0.25] * 4, atol=tolerance)

    def test_4x4_2ban_lhs(self):
        """Test 4x4 LHS with 2 bans."""

        winrates = []
        hero_bans = []
        opp_bans = []

        for _ in range(n_games):
            W = np.random.uniform(0, 1, (4, 4))
            result = ban_nash(W, bans=2, match_format='lhs')
            winrates.append(result['winrate'][0])
            hero_bans.append(result['bans']['hero'])
            opp_bans.append(result['bans']['opp'])

        mean_winrate = np.mean(winrates)
        mean_hero = np.mean(hero_bans, axis=0)
        mean_opp = np.mean(opp_bans, axis=0)

        assert mean_winrate == pytest.approx(0.5, abs=tolerance)
        np.testing.assert_allclose(mean_hero, [1/6] * 6, atol=tolerance)
        np.testing.assert_allclose(mean_opp, [1/6] * 6, atol=tolerance)


class TestBanNashStructure:
    """
    Tests for the structure of ban_nash output.
    """

    def test_output_structure(self):
        """Verify the output dictionary has all expected keys."""
        W = np.random.uniform(0, 1, (4, 4))
        result = ban_nash(W, bans=1, match_format='conquest')

        # Check top-level keys
        assert 'bans' in result
        assert 'winrate' in result
        assert 'stratlist' in result
        assert 'matches' in result

        # Check bans structure
        assert 'hero' in result['bans']
        assert 'opp' in result['bans']

        # Check stratlist structure
        assert 'hero' in result['stratlist']
        assert 'opp' in result['stratlist']

        # Check dimensions
        n_ban_options = 4  # C(4,1) = 4
        assert len(result['bans']['hero']) == n_ban_options
        assert len(result['bans']['opp']) == n_ban_options
        assert len(result['stratlist']['hero']) == n_ban_options
        assert len(result['stratlist']['opp']) == n_ban_options
        assert len(result['matches']) == n_ban_options
        assert len(result['matches'][0]) == n_ban_options

    def test_stratlist_contents(self):
        """Verify stratlist contains the correct ban combinations."""
        W = np.random.uniform(0, 1, (4, 4))
        result = ban_nash(W, bans=1, match_format='conquest')

        # For 4 decks with 1 ban, stratlist should be [(0,), (1,), (2,), (3,)]
        expected = [(0,), (1,), (2,), (3,)]
        assert result['stratlist']['hero'] == expected
        assert result['stratlist']['opp'] == expected

    def test_matches_contain_full_analysis(self):
        """Verify that matches contain full match analyses."""
        W = np.random.uniform(0, 1, (3, 3))
        result = ban_nash(W, bans=1, match_format='conquest')

        # Each matches[i][j] should be a list of subgame states
        # from conquest_nash or lhs_nash
        for i in range(3):
            for j in range(3):
                match = result['matches'][i][j]
                assert isinstance(match, list)
                assert len(match) > 0

                # Check that the last element has the expected structure
                initial = match[-1]
                assert 'score' in initial
                assert 'winrate' in initial


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


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

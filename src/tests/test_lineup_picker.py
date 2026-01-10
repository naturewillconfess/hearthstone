"""
test_lineup_picker.py - Tests for the lineup picker

These tests verify that lineup_picker() correctly computes optimal lineup
selection strategies for tournaments.

Note: Tests are deterministic (no stochastic/calibration tests) due to the
high computational cost of lineup_picker.
"""

import numpy as np
import pytest
import time

from hearthstone import lineup_picker, LineupResult, BanResult


class TestSymmetricLineups:
    """
    Test symmetric games where all matchups are 50/50.
    """

    def test_symmetric_winrate(self):
        """Symmetric game should have 50% winrate."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['A', 'B', 'C', 'D', 'E'],
                               match_format='conquest')

        assert result.winrate == pytest.approx(0.5, abs=1e-6), \
            f"Winrate mismatch. Actual: {result.winrate}, Expected: 0.5"

    def test_symmetric_lhs(self):
        """Symmetric LHS game should have 50% winrate."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['A', 'B', 'C', 'D', 'E'],
                               match_format='lhs')

        assert result.winrate == pytest.approx(0.5, abs=1e-6), \
            f"Winrate mismatch. Actual: {result.winrate}, Expected: 0.5"


class TestOutputStructure:
    """
    Test the structure of LineupResult.
    """

    def test_output_is_lineup_result(self):
        """Output should be a LineupResult object."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1, match_format='conquest')
        assert isinstance(result, LineupResult)

    def test_has_required_attributes(self):
        """LineupResult should have all required attributes."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1, match_format='conquest')

        assert hasattr(result, 'winrate')
        assert hasattr(result, 'hero_lineup_strategy')
        assert hasattr(result, 'opp_lineup_strategy')
        assert hasattr(result, 'deck_names')
        assert hasattr(result, 'all_matches')
        assert hasattr(result, 'get_match')

    def test_lineup_strategy_structure(self):
        """Lineup strategies should have correct structure."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['A', 'B', 'C', 'D', 'E'],
                               match_format='conquest')

        # Each lineup is a tuple of deck names
        for names, prob in result.hero_lineup_strategy:
            assert isinstance(names, tuple)
            assert len(names) == 3  # lineup_size
            assert all(name in ['A', 'B', 'C', 'D', 'E'] for name in names)
            assert 0 <= prob <= 1

    def test_strategies_sum_to_one(self):
        """Strategy probabilities should sum to 1."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1, match_format='conquest')

        hero_sum = sum(prob for _, prob in result.hero_lineup_strategy)
        opp_sum = sum(prob for _, prob in result.opp_lineup_strategy)

        assert hero_sum == pytest.approx(1.0, abs=1e-6), \
            f"Hero strategy should sum to 1. Actual: {hero_sum}"
        assert opp_sum == pytest.approx(1.0, abs=1e-6), \
            f"Opp strategy should sum to 1. Actual: {opp_sum}"

    def test_correct_number_of_lineups(self):
        """Should have C(n, k) lineup options."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1, match_format='conquest')

        # C(5, 3) = 10
        assert len(result.hero_lineup_strategy) == 10
        assert len(result.opp_lineup_strategy) == 10


class TestGetMatch:
    """
    Test the get_match method.
    """

    def test_get_match_returns_ban_result(self):
        """get_match should return a BanResult."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['A', 'B', 'C', 'D', 'E'],
                               match_format='conquest')

        match = result.get_match(hero_lineup=['A', 'B', 'C'],
                                 opp_lineup=['C', 'D', 'E'])
        assert isinstance(match, BanResult)

    def test_get_match_correct_deck_names(self):
        """BanResult from get_match should have correct deck names."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['A', 'B', 'C', 'D', 'E'],
                               match_format='conquest')

        match = result.get_match(hero_lineup=['A', 'B', 'C'],
                                 opp_lineup=['C', 'D', 'E'])

        # Hero's lineup is A, B, C
        assert set(match.hero_names) == {'A', 'B', 'C'}
        # Opp's lineup is C, D, E
        assert set(match.opp_names) == {'C', 'D', 'E'}

    def test_get_match_drill_down(self):
        """Should be able to drill down from lineup to ban to match state."""
        W = np.full((5, 5), 0.5)
        # winrate_only=False needed for full state drill-down
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['A', 'B', 'C', 'D', 'E'],
                               match_format='conquest',
                               winrate_only=False)

        # Get a match after lineup selection
        ban_result = result.get_match(hero_lineup=['A', 'B', 'C'],
                                      opp_lineup=['C', 'D', 'E'])

        # Get a conquest result after bans
        # Hero bans one of opp's decks (C, D, or E)
        # Opp bans one of hero's decks (A, B, or C)
        conquest_result = ban_result.get_match(hero_bans=['C'], opp_bans=['A'])

        # Get a state within the conquest match
        state = conquest_result.get_state(hero_won=['B'], opp_won=[])

        assert state.winrate >= 0 and state.winrate <= 1


class TestNamePropagation:
    """
    Test that deck names are properly propagated through all layers.
    """

    def test_names_in_lineup_strategy(self):
        """Lineup strategies should use correct deck names."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['Aggro', 'Combo', 'Control', 'Midrange', 'Tempo'],
                               match_format='conquest')

        valid_names = {'Aggro', 'Combo', 'Control', 'Midrange', 'Tempo'}
        for names, prob in result.hero_lineup_strategy:
            for name in names:
                assert name in valid_names, f"Invalid deck name: {name}"

    def test_names_propagate_to_ban_result(self):
        """Names should propagate correctly to BanResult."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['Aggro', 'Combo', 'Control', 'Midrange', 'Tempo'],
                               match_format='conquest')

        match = result.get_match(hero_lineup=['Aggro', 'Combo', 'Control'],
                                 opp_lineup=['Control', 'Midrange', 'Tempo'])

        # Verify names in ban strategies
        for names, prob in match.hero_ban_strategy:
            for name in names:
                assert name in ['Control', 'Midrange', 'Tempo'], \
                    f"Hero should ban opp's decks, got '{name}'"


class TestAllMatches:
    """
    Test the all_matches property.
    """

    def test_all_matches_dimensions(self):
        """all_matches should have correct dimensions."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1, match_format='conquest')

        # C(5, 3) = 10 lineups
        assert len(result.all_matches) == 10
        assert all(len(row) == 10 for row in result.all_matches)

    def test_all_matches_are_ban_results(self):
        """all_matches entries should be BanResult objects."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1, match_format='conquest')

        for row in result.all_matches:
            for match in row:
                assert isinstance(match, BanResult)


class TestErrorHandling:
    """
    Test error handling for invalid inputs.
    """

    def test_lineup_too_small(self):
        """Should raise error for lineup_size < 2."""
        W = np.full((5, 5), 0.5)
        with pytest.raises(ValueError, match="at least 2"):
            lineup_picker(W, lineup_size=1, bans=1)

    def test_lineup_too_large(self):
        """Should raise error for lineup_size > pool size."""
        W = np.full((5, 5), 0.5)
        with pytest.raises(ValueError, match="cannot exceed"):
            lineup_picker(W, lineup_size=6, bans=1)

    def test_too_many_bans(self):
        """Should raise error for bans >= lineup_size."""
        W = np.full((5, 5), 0.5)
        with pytest.raises(ValueError, match="Too many bans"):
            lineup_picker(W, lineup_size=3, bans=3)

    def test_zero_bans(self):
        """Should raise error for bans < 1."""
        W = np.full((5, 5), 0.5)
        with pytest.raises(ValueError, match="at least 1"):
            lineup_picker(W, lineup_size=3, bans=0)

    def test_invalid_format(self):
        """Should raise error for invalid match format."""
        W = np.full((5, 5), 0.5)
        with pytest.raises(ValueError, match="Unknown format"):
            lineup_picker(W, lineup_size=3, bans=1, match_format='invalid')

    def test_invalid_lineup_in_get_match(self):
        """Should raise error for invalid lineup in get_match."""
        W = np.full((5, 5), 0.5)
        result = lineup_picker(W, lineup_size=3, bans=1,
                               deck_names=['A', 'B', 'C', 'D', 'E'],
                               match_format='conquest')

        with pytest.raises(ValueError, match="Invalid"):
            result.get_match(hero_lineup=['A', 'B'], opp_lineup=['C', 'D', 'E'])


class TestPerformance:
    """
    Performance benchmarks (not assertions, just measurements).
    """

    def test_performance_small(self):
        """Measure time for small case: 5 pick 3 = 10 lineups."""
        W = np.full((5, 5), 0.5)

        start = time.time()
        result = lineup_picker(W, lineup_size=3, bans=1, match_format='conquest')
        elapsed = time.time() - start

        # Just log the time, don't assert (times vary by machine)
        print(f"\n5 pick 3 (100 matches): {elapsed:.2f}s")
        assert result.winrate == pytest.approx(0.5, abs=1e-6)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

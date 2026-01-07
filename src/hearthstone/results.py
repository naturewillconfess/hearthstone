"""
results.py - Result classes for match analysis

This module provides user-friendly result classes that wrap the raw
computation results from conquest_nash, lhs_nash, and ban_nash.
"""

from typing import List, Tuple, Optional, Dict, Any
import numpy as np

class GameSolution:
    """
    Result from solving a two-player zero-sum game.

    Attributes
    ----------
    value : float
        The value of the game (Hero's expected payoff at Nash equilibrium).
    hero_strategy : list of (str, float)
        Hero's optimal mixed strategy as [(name, probability), ...].
    opp_strategy : list of (str, float)
        Opponent's optimal mixed strategy as [(name, probability), ...].
    """

    def __init__(self, value: float,
                 hero_names: List[str], opp_names: List[str],
                 hero_strategy: List[Tuple[str, float]], opp_strategy: List[Tuple[str, float]]):
        self.value = value
        self.hero_names = hero_names
        self.opp_names = opp_names
        self.hero_strategy = hero_strategy
        self.opp_strategy = opp_strategy

    def __repr__(self) -> str:
        lines = [
            "Game Solution",
            "══════════════════════════",
            f"Value: {self._value:.1%}",
            "",
            "Hero Strategy:",
        ]

        for name, prob in self.hero_strategy:
            if prob > 1e-6:
                lines.append(f"  {name:<12} {prob:>6.1%}")

        lines.append("")
        lines.append("Opponent Strategy:")

        for name, prob in self.opp_strategy:
            if prob > 1e-6:
                lines.append(f"  {name:<12} {prob:>6.1%}")

        return "\n".join(lines)



class ConquestStateSolution:
    """
    The solution for a single state in a Conquest match.

    Attributes
    ----------
    hero_won : list of str
        Decks that Hero has won with (eliminated).
    opp_won : list of str
        Decks that Opponent has won with (eliminated).
    winrate : float
        Hero's probability of winning the match from this state.
    hero_strategy : list of (str, float)
        Hero's optimal deck selection as [(deck_name, probability), ...].
    opp_strategy : list of (str, float)
        Opponent's optimal deck selection.
    """
 
    def __init__(self, hero_names: List[str], opp_names: List[str],
                 hero_won: Tuple, opp_won: Tuple, 
                 solution: GameSolution
                 ):
        self._hero_names = hero_names
        self._opp_names = opp_names
        self.hero_won = hero_won
        self.opp_won = opp_won

        self.winrate = solution.value
        self.hero_strategy = solution.hero_strategy
        self.opp_strategy = solution.opp_strategy

    def __repr__(self) -> str:
        lines = [
            "Conquest State",
            "══════════════════════════",
        ]

        hero_won_names = [self._hero_names[i] for i in sorted(self._hero_won)]
        opp_won_names = [self._opp_names[i] for i in sorted(self._opp_won)]

        hero_won_str = ", ".join(hero_won_names) if hero_won_names else "(none)"
        opp_won_str = ", ".join(opp_won_names) if opp_won_names else "(none)"

        lines.append(f"Hero won with: {hero_won_str}")
        lines.append(f"Opponent won with: {opp_won_str}")
        lines.append("")
        lines.append(f"Winrate: {self.winrate:.1%}")
        lines.append("")
        lines.append("Hero Strategy:")

        for name, prob in self.hero_strategy:
            if prob > 1e-6:
                lines.append(f"  {name:<12} {prob:>6.1%}")

        lines.append("")
        lines.append("Opponent Strategy:")

        for name, prob in self.opp_strategy:
            if prob > 1e-6:
                lines.append(f"  {name:<12} {prob:>6.1%}")

        return "\n".join(lines)

class ConquestResult:
    """
    Result from analyzing a Conquest match.

    Provides easy access to the initial state (most common use case) and
    allows querying any mid-match state.

    Attributes
    ----------
    deck_names : list of str
        Names of the decks.
    winrate : float
        Hero's match winrate from the initial state.
    hero_strategy : list of (str, float)
        Hero's optimal initial deck selection.
    opp_strategy : list of (str, float)
        Opponent's optimal initial deck selection.
    """

    def __init__(self, states: List[ConquestStateSolution], hero_names: List[str], opp_names: List[str]):
        self.all_states = states
        self._hero_names = hero_names
        self._opp_names = opp_names

        # Find initial state (last element, where score is ((), ()))
        self._initial = states[-1]


    @property
    def winrate(self) -> float:
        """Hero's match winrate from initial state."""
        return self._initial.winrate

    @property
    def hero_strategy(self) -> List[Tuple[str, float]]:
        """Hero's optimal deck selection at initial state."""
        return self._initial.hero_strategy

    @property
    def opp_strategy(self) -> List[Tuple[str, float]]:
        """Opponent's optimal deck selection at initial state."""
        return self._initial.opp_strategy

    def get_state(self, hero_won: List[str] = None,
                  opp_won: List[str] = None) -> ConquestStateSolution:
        """
        Get a specific game state.
        """
        hero_won = hero_won or []
        opp_won = opp_won or []

        hero_indices = frozenset(self._hero_names.index(name) for name in hero_won)
        opp_indices = frozenset(self._opp_names.index(name) for name in opp_won)

        for state in self._states:
            if state.hero_won == hero_indices and state.opp_won == opp_indices:
                return state

        raise ValueError(f"State not found: hero_won={hero_won}, opp_won={opp_won}")

    def __repr__(self) -> str:
        n = len(self._hero_names)
        lines = [
            f"Conquest Match Analysis ({n} decks)",
            "═" * 35,
            f"Match Winrate: {self.winrate:.1%}",
            "",
            "Hero Strategy (initial):",
        ]

        for name, prob in self.hero_strategy:
            if prob > 1e-6:
                lines.append(f"  {name:<12} {prob:>6.1%}")

        lines.append("")
        lines.append("Opponent Strategy (initial):")

        for name, prob in self.opp_strategy:
            if prob > 1e-6:
                lines.append(f"  {name:<12} {prob:>6.1%}")

        lines.append("")
        lines.append(f"States analyzed: {len(self.all_states)}")

        return "\n".join(lines)



class LHSStateSolution:
    """
    The solution for a single state in a Last Hero Standing match.

    Attributes
    ----------
    hero_lost : frozenset
        Deck indices that Hero has lost with (eliminated).
    opp_lost : frozenset
        Deck indices that Opponent has lost with (eliminated).
    havetoplay_hero : int or None
        Deck index Hero must play (if they won the last game).
    havetoplay_opp : int or None
        Deck index Opponent must play (if they won the last game).
    winrate : float
        Hero's probability of winning the match from this state.
    hero_strategy : list of (str, float)
        Hero's optimal deck selection as [(deck_name, probability), ...].
    opp_strategy : list of (str, float)
        Opponent's optimal deck selection.
    """

    def __init__(self, hero_names: List[str], opp_names: List[str],
                 hero_lost: Tuple, opp_lost: Tuple,
                 havetoplay_hero: Optional[int], havetoplay_opp: Optional[int],
                 solution: GameSolution):
        self._hero_names = hero_names
        self._opp_names = opp_names
        self.hero_lost = hero_lost
        self.opp_lost = opp_lost
        self.havetoplay_hero = havetoplay_hero
        self.havetoplay_opp = havetoplay_opp

        self.winrate = solution.value
        self.hero_strategy = solution.hero_strategy
        self.opp_strategy = solution.opp_strategy


    def __repr__(self) -> str:
        lines = [
            "LHS State",
            "══════════════════════════",
        ]

        hero_lost_names = [self._hero_names[i] for i in sorted(self.hero_lost)]
        opp_lost_names = [self._opp_names[i] for i in sorted(self.opp_lost)]

        hero_lost_str = ", ".join(hero_lost_names) if hero_lost_names else "(none)"
        opp_lost_str = ", ".join(opp_lost_names) if opp_lost_names else "(none)"

        lines.append(f"Hero lost: {hero_lost_str}")
        lines.append(f"Opponent lost: {opp_lost_str}")

        if self.havetoplay_hero is not None:
            lines.append(f"Hero forced to play: {self._hero_names[self.havetoplay_hero]}")
        if self.havetoplay_opp is not None:
            lines.append(f"Opponent forced to play: {self._opp_names[self.havetoplay_opp]}")

        lines.append("")
        lines.append(f"Winrate: {self.winrate:.1%}")
        lines.append("")
        lines.append("Hero Strategy:")

        for name, prob in self.hero_strategy:
            if prob > 1e-6:
                forced_name = self._hero_names[self.havetoplay_hero] if self.havetoplay_hero is not None else None
                suffix = "  (forced)" if forced_name and name == forced_name else ""
                lines.append(f"  {name:<12} {prob:>6.1%}{suffix}")

        lines.append("")
        lines.append("Opponent Strategy:")

        for name, prob in self.opp_strategy:
            if prob > 1e-6:
                forced_name = self._opp_names[self.havetoplay_opp] if self.havetoplay_opp is not None else None
                suffix = "  (forced)" if forced_name and name == forced_name else ""
                lines.append(f"  {name:<12} {prob:>6.1%}{suffix}")

        return "\n".join(lines)



class LHSResult:
    """
    Result from analyzing a Last Hero Standing match.

    Provides easy access to the initial state and allows querying any
    mid-match state including forced play situations.

    Attributes
    ----------
    winrate : float
        Hero's match winrate from the initial state.
    hero_strategy : list of (str, float)
        Hero's optimal initial deck selection.
    opp_strategy : list of (str, float)
        Opponent's optimal initial deck selection.
    """

    def __init__(self, states: List['LHSStateSolution'], hero_names: List[str], opp_names: List[str]):
        self._states = states
        self._hero_names = hero_names
        self._opp_names = opp_names

        # Find initial state (no losses, no forced plays)
        self._initial = states[-1]

    @property
    def winrate(self) -> float:
        """Hero's match winrate from initial state."""
        return self._initial.winrate

    @property
    def hero_strategy(self) -> List[Tuple[str, float]]:
        """Hero's optimal deck selection at initial state."""
        return self._initial.hero_strategy

    @property
    def opp_strategy(self) -> List[Tuple[str, float]]:
        """Opponent's optimal deck selection at initial state."""
        return self._initial.opp_strategy
    

    def get_state(self, hero_lost: List[str] = None,
                  opp_lost: List[str] = None,
                  forced_hero: str = None,
                  forced_opp: str = None) -> 'LHSStateSolution':
        """
        Get a specific game state.
        """
        hero_lost = hero_lost or []
        opp_lost = opp_lost or []

        # Convert names to indices
        hero_indices = frozenset(self._hero_names.index(name) for name in hero_lost)
        opp_indices = frozenset(self._opp_names.index(name) for name in opp_lost)
        forced_hero_idx = self._hero_names.index(forced_hero) if forced_hero else None
        forced_opp_idx = self._opp_names.index(forced_opp) if forced_opp else None

        for state in self._states:
            if (state.hero_lost == hero_indices and
                state.opp_lost == opp_indices and
                state.havetoplay_hero == forced_hero_idx and
                state.havetoplay_opp == forced_opp_idx):
                return state

        raise ValueError(f"State not found: hero_lost={hero_lost}, opp_lost={opp_lost}, "
                        f"forced_hero={forced_hero}, forced_opp={forced_opp}")

    def all_states(self) -> List['LHSStateSolution']:
        """Get all states for advanced analysis."""
        return list(self._states)

    def __repr__(self) -> str:
        n = len(self._hero_names)
        lines = [
            f"LHS Match Analysis ({n} decks)",
            "═" * 35,
            f"Match Winrate: {self.winrate:.1%}",
            "",
            "Hero Strategy (initial):",
        ]

        for name, prob in self.hero_strategy:
            if prob > 1e-6:
                lines.append(f"  {name:<12} {prob:>6.1%}")

        lines.append("")
        lines.append("Opponent Strategy (initial):")

        for name, prob in self.opp_strategy:
            if prob > 1e-6:
                lines.append(f"  {name:<12} {prob:>6.1%}")

        lines.append("")
        lines.append(f"States analyzed: {len(self._states)}")

        return "\n".join(lines)


class BanResult:
    """
    Result from analyzing a ban phase.

    Attributes
    ----------
    deck_names : list of str
        Names of all decks (before bans).
    winrate : float
        Hero's expected winrate after optimal banning.
    hero_ban_strategy : list of (tuple of str, float)
        Hero's optimal ban strategy as [((decks_to_ban), probability), ...].
    opp_ban_strategy : list of (tuple of str, float)
        Opponent's optimal ban strategy.
    """

    def __init__(self, raw_result: Dict[str, Any], deck_names: List[str],
                 match_format: str, matches: List[List[Any]],
                 stratlist_hero: List[Tuple[int, ...]],
                 stratlist_opp: List[Tuple[int, ...]]):
        self._raw = raw_result
        self._deck_names = deck_names
        self._match_format = match_format
        self._matches = matches
        self._stratlist_hero = stratlist_hero
        self._stratlist_opp = stratlist_opp
        self._n = len(deck_names)

    @property
    def deck_names(self) -> List[str]:
        """Names of all decks."""
        return self._deck_names

    @property
    def winrate(self) -> float:
        """Hero's expected winrate after optimal banning."""
        return self._raw['winrate'][0]

    def _indices_to_names(self, indices: Tuple[int, ...]) -> Tuple[str, ...]:
        """Convert ban indices to names."""
        return tuple(self._deck_names[i] for i in indices)

    @property
    def hero_ban_strategy(self) -> List[Tuple[Tuple[str, ...], float]]:
        """Hero's optimal ban strategy as [((decks), probability), ...]."""
        probs = self._raw['bans']['hero']
        result = []
        for indices, prob in zip(self._stratlist_hero, probs):
            names = self._indices_to_names(indices)
            result.append((names, prob))
        return result

    @property
    def opp_ban_strategy(self) -> List[Tuple[Tuple[str, ...], float]]:
        """Opponent's optimal ban strategy."""
        probs = self._raw['bans']['opp']
        result = []
        for indices, prob in zip(self._stratlist_opp, probs):
            names = self._indices_to_names(indices)
            result.append((names, prob))
        return result

    def get_match(self, hero_bans: List[str],
                  opp_bans: List[str]) -> 'ConquestResult | LHSResult':
        """
        Get the match analysis for specific ban choices.

        Parameters
        ----------
        hero_bans : list of str
            Decks banned by Hero.
        opp_bans : list of str
            Decks banned by Opponent.

        Returns
        -------
        ConquestResult or LHSResult
            The match analysis after the specified bans.
        """
        # Convert names to indices
        hero_ban_indices = tuple(sorted(
            self._deck_names.index(name) if isinstance(name, str) else name
            for name in hero_bans
        ))
        opp_ban_indices = tuple(sorted(
            self._deck_names.index(name) if isinstance(name, str) else name
            for name in opp_bans
        ))

        # Find the match
        try:
            hero_idx = self._stratlist_hero.index(hero_ban_indices)
        except ValueError:
            raise ValueError(f"Invalid hero bans: {hero_bans}")

        try:
            opp_idx = self._stratlist_opp.index(opp_ban_indices)
        except ValueError:
            raise ValueError(f"Invalid opp bans: {opp_bans}")

        # Return the stored match result directly
        # (already a ConquestResult or LHSResult object)
        return self._matches[hero_idx][opp_idx]

    def __repr__(self) -> str:
        bans_per_player = len(self._stratlist_hero[0]) if self._stratlist_hero else 0

        lines = [
            f"Ban Phase Analysis ({self._n} decks, {bans_per_player} ban, {self._match_format})",
            "═" * 50,
            f"Winrate after bans: {self.winrate:.1%}",
            "",
            "Hero Ban Strategy:",
        ]

        for names, prob in self.hero_ban_strategy:
            if prob > 1e-6:
                ban_str = ", ".join(names)
                lines.append(f"  Ban ({ban_str:<15}) {prob:>6.1%}")

        lines.append("")
        lines.append("Opponent Ban Strategy:")

        for names, prob in self.opp_ban_strategy:
            if prob > 1e-6:
                ban_str = ", ".join(names)
                lines.append(f"  Ban ({ban_str:<15}) {prob:>6.1%}")

        return "\n".join(lines)

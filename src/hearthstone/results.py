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

    def __init__(self, value: float, hero_probs: np.ndarray, opp_probs: np.ndarray,
                 hero_names: List[str], opp_names: List[str]):
        self._value = value
        self._hero_probs = hero_probs
        self._opp_probs = opp_probs
        self._hero_names = hero_names
        self._opp_names = opp_names

    @property
    def hero_strategy(self) -> List[Tuple[str, float]]:
        """Hero's optimal mixed strategy as [(name, probability), ...]."""
        return list(zip(self._hero_names, self._hero_probs))

    @property
    def opp_strategy(self) -> List[Tuple[str, float]]:
        """Opponent's optimal mixed strategy as [(name, probability), ...]."""
        return list(zip(self._opp_names, self._opp_probs))

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
 
    def __init__(self, hero_won: Tuple, opp_won: Tuple, winrate: float, 
                 hero_probs: np.ndarray, opp_probs: np.ndarray, 
                 hero_names: List[str], opp_names: List[str]):
        self.hero_won = hero_won
        self.opp_won = opp_won
        self.winrate = winrate
        self.hero_prons = hero_probs
        self.opp_probs = opp_probs

    def __repr__(self) -> str:
        lines = [
            "Conquest State",
            "══════════════════════════",
        ]

        hero_won_str = ", ".join(self.hero_won) if self.hero_won else "(none)"
        opp_won_str = ", ".join(self.opp_won) if self.opp_won else "(none)"

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


class LHSState:
    """
    A single state in a Last Hero Standing match.

    Attributes
    ----------
    hero_lost : list of str
        Decks that Hero has lost with (eliminated).
    opp_lost : list of str
        Decks that Opponent has lost with (eliminated).
    forced_hero : str or None
        Deck that Hero must play (if they won the last game).
    forced_opp : str or None
        Deck that Opponent must play (if they won the last game).
    winrate : float
        Hero's probability of winning the match from this state.
    hero_strategy : list of (str, float)
        Hero's optimal deck selection.
    opp_strategy : list of (str, float)
        Opponent's optimal deck selection.
    """

    def __init__(self, hero_lost: List[str], opp_lost: List[str],
                 forced_hero: Optional[str], forced_opp: Optional[str],
                 winrate: float, hero_strategy: List[Tuple[str, float]],
                 opp_strategy: List[Tuple[str, float]]):
        self.hero_lost = hero_lost
        self.opp_lost = opp_lost
        self.forced_hero = forced_hero
        self.forced_opp = forced_opp
        self.winrate = winrate
        self.hero_strategy = hero_strategy
        self.opp_strategy = opp_strategy

    def __repr__(self) -> str:
        lines = [
            "LHS State",
            "══════════════════════════",
        ]

        hero_lost_str = ", ".join(self.hero_lost) if self.hero_lost else "(none)"
        opp_lost_str = ", ".join(self.opp_lost) if self.opp_lost else "(none)"

        lines.append(f"Hero lost: {hero_lost_str}")
        lines.append(f"Opponent lost: {opp_lost_str}")

        if self.forced_hero:
            lines.append(f"Hero forced to play: {self.forced_hero}")
        if self.forced_opp:
            lines.append(f"Opponent forced to play: {self.forced_opp}")

        lines.append("")
        lines.append(f"Winrate: {self.winrate:.1%}")
        lines.append("")
        lines.append("Hero Strategy:")

        for name, prob in self.hero_strategy:
            if prob > 1e-6:
                suffix = "  (forced)" if self.forced_hero and name == self.forced_hero else ""
                lines.append(f"  {name:<12} {prob:>6.1%}{suffix}")

        lines.append("")
        lines.append("Opponent Strategy:")

        for name, prob in self.opp_strategy:
            if prob > 1e-6:
                suffix = "  (forced)" if self.forced_opp and name == self.forced_opp else ""
                lines.append(f"  {name:<12} {prob:>6.1%}{suffix}")

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

    def __init__(self, states: List[Dict[str, Any]], deck_names: List[str]):
        self._states = states
        self._deck_names = deck_names

        # Find initial state (last element, where score is ((), ()))
        self._initial = states[-1]

    @property
    def deck_names(self) -> List[str]:
        """Names of all decks."""
        return self._deck_names

    @property
    def winrate(self) -> float:
        """Hero's match winrate from initial state."""
        return self._initial['winrate'][0]

    @property
    def hero_strategy(self) -> List[Tuple[str, float]]:
        """Hero's optimal deck selection at initial state."""
        if 'nash' not in self._initial:
            return [(name, 1.0 / self._n) for name in self._deck_names]
        return list(zip(self._deck_names, self._initial['nash'][0]))

    @property
    def opp_strategy(self) -> List[Tuple[str, float]]:
        """Opponent's optimal deck selection at initial state."""
        if 'nash' not in self._initial:
            return [(name, 1.0 / self._n) for name in self._deck_names]
        return list(zip(self._deck_names, self._initial['nash'][1]))

    def _names_to_indices(self, names: List[str]) -> Tuple[int, ...]:
        """Convert deck names to indices."""
        indices = []
        for name in names:
            if isinstance(name, int):
                indices.append(name)
            else:
                try:
                    indices.append(self._deck_names.index(name))
                except ValueError:
                    raise ValueError(f"Unknown deck name: {name}")
        return tuple(sorted(indices))

    def _indices_to_names(self, indices: Tuple[int, ...]) -> List[str]:
        """Convert indices to deck names."""
        return [self._deck_names[i] for i in indices]

    def get_state(self, hero_won: List[str] = None,
                  opp_won: List[str] = None) -> ConquestState:
        """
        Get a specific game state.

        Parameters
        ----------
        hero_won : list of str, optional
            Decks that Hero has won with. Default: [] (initial state).
        opp_won : list of str, optional
            Decks that Opponent has won with. Default: [] (initial state).

        Returns
        -------
        ConquestState
            The requested state with winrate and strategies.
        """
        hero_won = hero_won or []
        opp_won = opp_won or []

        hero_indices = self._names_to_indices(hero_won)
        opp_indices = self._names_to_indices(opp_won)

        for state in self._states:
            if state['score'] == (hero_indices, opp_indices):
                # Determine remaining decks for strategy
                hero_remaining = [i for i in range(self._n) if i not in hero_indices]
                opp_remaining = [i for i in range(self._n) if i not in opp_indices]

                hero_names_remaining = [self._deck_names[i] for i in hero_remaining]
                opp_names_remaining = [self._deck_names[i] for i in opp_remaining]

                if 'nash' in state:
                    hero_strat = list(zip(hero_names_remaining, state['nash'][0]))
                    opp_strat = list(zip(opp_names_remaining, state['nash'][1]))
                else:
                    # Terminal or near-terminal state
                    if len(hero_remaining) > 0:
                        hero_strat = [(name, 1.0 / len(hero_remaining)) for name in hero_names_remaining]
                    else:
                        hero_strat = []
                    if len(opp_remaining) > 0:
                        opp_strat = [(name, 1.0 / len(opp_remaining)) for name in opp_names_remaining]
                    else:
                        opp_strat = []

                return ConquestState(
                    hero_won=self._indices_to_names(hero_indices),
                    opp_won=self._indices_to_names(opp_indices),
                    winrate=state['winrate'][0],
                    hero_strategy=hero_strat,
                    opp_strategy=opp_strat
                )

        raise ValueError(f"State not found: hero_won={hero_won}, opp_won={opp_won}")

    def all_states(self) -> List[ConquestState]:
        """Get all states for advanced analysis."""
        result = []
        for state in self._states:
            hero_indices, opp_indices = state['score']
            result.append(self.get_state(
                hero_won=list(hero_indices),
                opp_won=list(opp_indices)
            ))
        return result

    def __repr__(self) -> str:
        lines = [
            f"Conquest Match Analysis ({self._n} decks)",
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


class LHSResult:
    """
    Result from analyzing a Last Hero Standing match.

    Provides easy access to the initial state and allows querying any
    mid-match state including forced play situations.

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

    def __init__(self, states: List[Dict[str, Any]], deck_names: List[str]):
        self._states = states
        self._deck_names = deck_names
        self._n = len(deck_names)

        # Find initial state (score ((), ()), no forced plays)
        self._initial = None
        for state in states:
            if (state['score'] == ((), ()) and
                state.get('havetoplay_hero') is None and
                state.get('havetoplay_opp') is None):
                self._initial = state
                break

        if self._initial is None:
            self._initial = states[-1]

    @property
    def deck_names(self) -> List[str]:
        """Names of all decks."""
        return self._deck_names

    @property
    def winrate(self) -> float:
        """Hero's match winrate from initial state."""
        return self._initial['winrate'][0]

    @property
    def hero_strategy(self) -> List[Tuple[str, float]]:
        """Hero's optimal deck selection at initial state."""
        if 'nash' not in self._initial:
            return [(name, 1.0 / self._n) for name in self._deck_names]
        return list(zip(self._deck_names, self._initial['nash'][0]))

    @property
    def opp_strategy(self) -> List[Tuple[str, float]]:
        """Opponent's optimal deck selection at initial state."""
        if 'nash' not in self._initial:
            return [(name, 1.0 / self._n) for name in self._deck_names]
        return list(zip(self._deck_names, self._initial['nash'][1]))

    def _name_to_index(self, name: str) -> Optional[int]:
        """Convert a single deck name to index."""
        if name is None:
            return None
        if isinstance(name, int):
            return name
        try:
            return self._deck_names.index(name)
        except ValueError:
            raise ValueError(f"Unknown deck name: {name}")

    def _index_to_name(self, idx: Optional[int]) -> Optional[str]:
        """Convert index to deck name."""
        if idx is None:
            return None
        return self._deck_names[idx]

    def _names_to_indices(self, names: List[str]) -> Tuple[int, ...]:
        """Convert deck names to indices."""
        indices = []
        for name in names:
            if isinstance(name, int):
                indices.append(name)
            else:
                try:
                    indices.append(self._deck_names.index(name))
                except ValueError:
                    raise ValueError(f"Unknown deck name: {name}")
        return tuple(sorted(indices))

    def _indices_to_names(self, indices: Tuple[int, ...]) -> List[str]:
        """Convert indices to deck names."""
        return [self._deck_names[i] for i in indices]

    def get_state(self, hero_lost: List[str] = None,
                  opp_lost: List[str] = None,
                  forced_hero: str = None,
                  forced_opp: str = None) -> LHSState:
        """
        Get a specific game state.

        Parameters
        ----------
        hero_lost : list of str, optional
            Decks that Hero has lost with. Default: [].
        opp_lost : list of str, optional
            Decks that Opponent has lost with. Default: [].
        forced_hero : str, optional
            Deck that Hero is forced to play.
        forced_opp : str, optional
            Deck that Opponent is forced to play.

        Returns
        -------
        LHSState
            The requested state with winrate and strategies.
        """
        hero_lost = hero_lost or []
        opp_lost = opp_lost or []

        hero_indices = self._names_to_indices(hero_lost)
        opp_indices = self._names_to_indices(opp_lost)
        forced_hero_idx = self._name_to_index(forced_hero)
        forced_opp_idx = self._name_to_index(forced_opp)

        for state in self._states:
            if (state['score'] == (hero_indices, opp_indices) and
                state.get('havetoplay_hero') == forced_hero_idx and
                state.get('havetoplay_opp') == forced_opp_idx):

                # Determine remaining decks for strategy
                hero_remaining = [i for i in range(self._n) if i not in hero_indices]
                opp_remaining = [i for i in range(self._n) if i not in opp_indices]

                # For forced play, strategy is just the forced deck
                if forced_hero_idx is not None:
                    hero_names_strat = [self._deck_names[forced_hero_idx]]
                    hero_probs = [1.0]
                elif len(hero_remaining) == 0:
                    # Terminal state: hero has no remaining decks
                    hero_names_strat = []
                    hero_probs = []
                else:
                    hero_names_strat = [self._deck_names[i] for i in hero_remaining]
                    hero_probs = state['nash'][0] if 'nash' in state else [1.0 / len(hero_remaining)] * len(hero_remaining)

                if forced_opp_idx is not None:
                    opp_names_strat = [self._deck_names[forced_opp_idx]]
                    opp_probs = [1.0]
                elif len(opp_remaining) == 0:
                    # Terminal state: opponent has no remaining decks
                    opp_names_strat = []
                    opp_probs = []
                else:
                    opp_names_strat = [self._deck_names[i] for i in opp_remaining]
                    opp_probs = state['nash'][1] if 'nash' in state else [1.0 / len(opp_remaining)] * len(opp_remaining)

                hero_strat = list(zip(hero_names_strat, hero_probs))
                opp_strat = list(zip(opp_names_strat, opp_probs))

                return LHSState(
                    hero_lost=self._indices_to_names(hero_indices),
                    opp_lost=self._indices_to_names(opp_indices),
                    forced_hero=self._index_to_name(forced_hero_idx),
                    forced_opp=self._index_to_name(forced_opp_idx),
                    winrate=state['winrate'][0],
                    hero_strategy=hero_strat,
                    opp_strategy=opp_strat
                )

        raise ValueError(f"State not found: hero_lost={hero_lost}, opp_lost={opp_lost}, "
                        f"forced_hero={forced_hero}, forced_opp={forced_opp}")

    def all_states(self) -> List[LHSState]:
        """Get all states for advanced analysis."""
        result = []
        for state in self._states:
            hero_indices, opp_indices = state['score']
            forced_hero = state.get('havetoplay_hero')
            forced_opp = state.get('havetoplay_opp')
            result.append(self.get_state(
                hero_lost=list(hero_indices),
                opp_lost=list(opp_indices),
                forced_hero=self._index_to_name(forced_hero),
                forced_opp=self._index_to_name(forced_opp)
            ))
        return result

    def __repr__(self) -> str:
        lines = [
            f"LHS Match Analysis ({self._n} decks)",
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

        match_states = self._matches[hero_idx][opp_idx]

        # Determine remaining deck names after bans
        remaining_indices = [i for i in range(self._n)
                           if i not in hero_ban_indices and i not in opp_ban_indices]
        remaining_names = [self._deck_names[i] for i in remaining_indices]

        # Create appropriate result type
        if self._match_format == 'conquest':
            return ConquestResult(match_states, remaining_names)
        else:
            return LHSResult(match_states, remaining_names)

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

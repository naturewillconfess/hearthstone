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

    @property
    def hero_probs(self) -> np.ndarray:
        """Hero's strategy probabilities as a numpy array."""
        return np.array([prob for _, prob in self.hero_strategy])

    @property
    def opp_probs(self) -> np.ndarray:
        """Opponent's strategy probabilities as a numpy array."""
        return np.array([prob for _, prob in self.opp_strategy])

    def __repr__(self) -> str:
        lines = [
            "Game Solution",
            "══════════════════════════",
            f"Value: {self.value:.1%}",
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

        hero_won_names = [self._hero_names[i] for i in sorted(self.hero_won)]
        opp_won_names = [self._opp_names[i] for i in sorted(self.opp_won)]

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

        for state in self.all_states:
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
        self.all_states = states
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

        for state in self.all_states:
            if (state.hero_lost == hero_indices and
                state.opp_lost == opp_indices and
                state.havetoplay_hero == forced_hero_idx and
                state.havetoplay_opp == forced_opp_idx):
                return state

        raise ValueError(f"State not found: hero_lost={hero_lost}, opp_lost={opp_lost}, "
                        f"forced_hero={forced_hero}, forced_opp={forced_opp}")

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
        lines.append(f"States analyzed: {len(self.all_states)}")

        return "\n".join(lines)


class BanResult:
    """
    Result from analyzing a ban phase.

    Provides access to the optimal ban strategies for both players, the overall
    match winrate after optimal banning, and the underlying ConquestResult or
    LHSResult objects for each ban combination.

    Attributes
    ----------
    winrate : float
        Hero's expected winrate after optimal banning.
    hero_ban_strategy : list of (tuple of str, float)
        Hero's optimal ban strategy as [((decks_to_ban), probability), ...].
    opp_ban_strategy : list of (tuple of str, float)
        Opponent's optimal ban strategy.
    """

    def __init__(self, solution: 'GameSolution',
                 hero_names: List[str], opp_names: List[str],
                 match_format: str, matches: List[List[Any]],
                 hero_ban_options: List[Tuple[int, ...]],
                 opp_ban_options: List[Tuple[int, ...]]):
        self._solution = solution
        self.hero_names = hero_names
        self.opp_names = opp_names
        self._match_format = match_format
        self.all_matches = matches
        self._hero_ban_options = hero_ban_options
        self._opp_ban_options = opp_ban_options

    @property
    def winrate(self) -> float:
        """Hero's expected winrate after optimal banning."""
        return self._solution.value

    @property
    def hero_ban_strategy(self) -> List[Tuple[Tuple[str, ...], float]]:
        """
        Hero's optimal ban strategy.

        Returns list of ((banned_deck_names), probability) tuples.
        Hero bans opponent's decks.
        """
        result = []
        for (_, prob), indices in zip(self._solution.hero_strategy, self._hero_ban_options):
            names = tuple(self.opp_names[i] for i in indices)
            result.append((names, prob))
        return result

    @property
    def opp_ban_strategy(self) -> List[Tuple[Tuple[str, ...], float]]:
        """
        Opponent's optimal ban strategy.

        Returns list of ((banned_deck_names), probability) tuples.
        Opponent bans Hero's decks.
        """
        result = []
        for (_, prob), indices in zip(self._solution.opp_strategy, self._opp_ban_options):
            names = tuple(self.hero_names[i] for i in indices)
            result.append((names, prob))
        return result

    def get_match(self, hero_bans: List[str] = None,
                  opp_bans: List[str] = None) -> 'ConquestResult | LHSResult':
        """
        Get the match analysis for specific ban choices.

        Parameters
        ----------
        hero_bans : list of str
            Opponent's decks banned by Hero.
        opp_bans : list of str
            Hero's decks banned by Opponent.

        Returns
        -------
        ConquestResult or LHSResult
            The match analysis after the specified bans.

        Examples
        --------
        >>> result = ban_nash(W, bans=1, hero_names=['A', 'B', 'C'],
        ...                   opp_names=['X', 'Y', 'Z'], match_format='conquest')
        >>> # Get match where Hero bans 'X' and Opponent bans 'A'
        >>> match = result.get_match(hero_bans=['X'], opp_bans=['A'])
        >>> print(match.winrate)
        """
        hero_bans = hero_bans or []
        opp_bans = opp_bans or []

        # Convert names to indices - Hero bans opponent's decks
        hero_ban_indices = tuple(sorted(
            self.opp_names.index(name) for name in hero_bans
        ))
        # Opponent bans Hero's decks
        opp_ban_indices = tuple(sorted(
            self.hero_names.index(name) for name in opp_bans
        ))

        # Find the indices in our ban options lists
        try:
            hero_idx = self._hero_ban_options.index(hero_ban_indices)
        except ValueError:
            raise ValueError(f"Invalid hero bans: {hero_bans}. Hero must ban opponent's decks.")

        try:
            opp_idx = self._opp_ban_options.index(opp_ban_indices)
        except ValueError:
            raise ValueError(f"Invalid opp bans: {opp_bans}. Opponent must ban Hero's decks.")

        return self.all_matches[hero_idx][opp_idx]

    def __repr__(self) -> str:
        bans_per_player = len(self._hero_ban_options[0]) if self._hero_ban_options else 0
        n_hero = len(self.hero_names)
        n_opp = len(self.opp_names)

        lines = [
            f"Ban Phase Analysis ({n_hero}v{n_opp} decks, {bans_per_player} ban, {self._match_format})",
            "═" * 50,
            f"Winrate after bans: {self.winrate:.1%}",
            "",
            "Hero Ban Strategy (banning opponent's decks):",
        ]

        for names, prob in self.hero_ban_strategy:
            if prob > 1e-6:
                ban_str = ", ".join(names)
                lines.append(f"  Ban ({ban_str:<15}) {prob:>6.1%}")

        lines.append("")
        lines.append("Opponent Ban Strategy (banning Hero's decks):")

        for names, prob in self.opp_ban_strategy:
            if prob > 1e-6:
                ban_str = ", ".join(names)
                lines.append(f"  Ban ({ban_str:<15}) {prob:>6.1%}")

        lines.append("")
        lines.append(f"Ban combinations: {len(self._hero_ban_options)} x {len(self._opp_ban_options)}")

        return "\n".join(lines)


class LineupResult:
    """
    Result from analyzing a lineup selection game.

    In a lineup selection game, each player picks a lineup of k decks from
    a shared pool of n available decks. After lineups are chosen, the match
    proceeds with the specified format (conquest/lhs) and ban rules.

    Attributes
    ----------
    winrate : float
        Hero's expected winrate after optimal lineup selection.
    hero_lineup_strategy : list of (tuple of str, float)
        Hero's optimal lineup selection as [((deck_names), probability), ...].
    opp_lineup_strategy : list of (tuple of str, float)
        Opponent's optimal lineup selection.
    """

    def __init__(self, solution: 'GameSolution',
                 deck_names: List[str],
                 lineup_size: int,
                 bans: int,
                 match_format: str,
                 matches: List[List['BanResult']],
                 lineup_options: List[Tuple[int, ...]],
                 W: Optional[np.ndarray] = None,
                 symmetric: bool = False):
        self._solution = solution
        self.deck_names = deck_names
        self._lineup_size = lineup_size
        self._bans = bans
        self._match_format = match_format
        self.all_matches = matches
        self._lineup_options = lineup_options
        self._W = W  # Stored for on-demand computation in symmetric mode
        self._symmetric = symmetric

    @property
    def winrate(self) -> float:
        """Hero's expected winrate after optimal lineup selection."""
        return self._solution.value

    @property
    def hero_lineup_strategy(self) -> List[Tuple[Tuple[str, ...], float]]:
        """
        Hero's optimal lineup selection.

        Returns list of ((deck_names), probability) tuples.
        """
        result = []
        for (_, prob), indices in zip(self._solution.hero_strategy, self._lineup_options):
            names = tuple(self.deck_names[i] for i in indices)
            result.append((names, prob))
        return result

    @property
    def opp_lineup_strategy(self) -> List[Tuple[Tuple[str, ...], float]]:
        """
        Opponent's optimal lineup selection.

        Returns list of ((deck_names), probability) tuples.
        """
        result = []
        for (_, prob), indices in zip(self._solution.opp_strategy, self._lineup_options):
            names = tuple(self.deck_names[i] for i in indices)
            result.append((names, prob))
        return result

    def get_match(self, hero_lineup: List[str],
                  opp_lineup: List[str]) -> 'BanResult':
        """
        Get the ban phase analysis for specific lineup choices.

        Parameters
        ----------
        hero_lineup : list of str
            Decks in Hero's lineup.
        opp_lineup : list of str
            Decks in Opponent's lineup.

        Returns
        -------
        BanResult
            The ban phase analysis for the specified lineups.
        """
        # Convert names to indices
        hero_lineup_indices = tuple(sorted(
            self.deck_names.index(name) for name in hero_lineup
        ))
        opp_lineup_indices = tuple(sorted(
            self.deck_names.index(name) for name in opp_lineup
        ))

        # Find the indices in our lineup options
        try:
            hero_idx = self._lineup_options.index(hero_lineup_indices)
        except ValueError:
            raise ValueError(f"Invalid hero lineup: {hero_lineup}")

        try:
            opp_idx = self._lineup_options.index(opp_lineup_indices)
        except ValueError:
            raise ValueError(f"Invalid opp lineup: {opp_lineup}")

        result = self.all_matches[hero_idx][opp_idx]

        # Handle symmetric case: lower triangle entries are computed on-demand
        if result is None and self._symmetric:
            # Import here to avoid circular dependency
            from .ban_nash import ban_nash

            # Compute ban_nash for this matchup
            hero_names = [self.deck_names[k] for k in hero_lineup_indices]
            opp_names = [self.deck_names[k] for k in opp_lineup_indices]
            W_sub = self._W[np.ix_(list(hero_lineup_indices), list(opp_lineup_indices))]

            result = ban_nash(
                W_sub,
                bans=self._bans,
                hero_names=hero_names,
                opp_names=opp_names,
                match_format=self._match_format
            )

            # Cache for future lookups
            self.all_matches[hero_idx][opp_idx] = result

        return result

    def __repr__(self) -> str:
        n_pool = len(self.deck_names)
        n_lineups = len(self._lineup_options)

        lines = [
            f"Lineup Selection ({n_pool} deck pool, pick {self._lineup_size}, {self._bans} ban, {self._match_format})",
            "═" * 60,
            f"Winrate: {self.winrate:.1%}",
            "",
            "Hero Lineup Strategy:",
        ]

        for names, prob in self.hero_lineup_strategy:
            if prob > 1e-6:
                lineup_str = ", ".join(names)
                lines.append(f"  [{lineup_str}] {prob:>6.1%}")

        lines.append("")
        lines.append("Opponent Lineup Strategy:")

        for names, prob in self.opp_lineup_strategy:
            if prob > 1e-6:
                lineup_str = ", ".join(names)
                lines.append(f"  [{lineup_str}] {prob:>6.1%}")

        lines.append("")
        lines.append(f"Lineup combinations: {n_lineups} x {n_lineups}")

        return "\n".join(lines)

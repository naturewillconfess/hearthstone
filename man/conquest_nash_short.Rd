% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conquest_nash_short.R
\name{conquest_nash_short}
\alias{conquest_nash_short}
\title{Conquest Nash calculator (short version)}
\usage{
conquest_nash_short(W)
}
\arguments{
\item{W}{Winrate matrix (from the perspective of the Hero)}
}
\value{
A list of possible state of the game with following parameters (if applicable)
\item{score}{eliminated decks}
\item{winrate}{Players' winrate}
\item{nash}{Nash equilibrium in mixed strategies}
\item{game}{payoff matrix for this subgame}
}
\description{
Finds Nash equilibrium in mixed strategies and expected winrate for both players in a Conquest match in which you have to win with all but one deck
}
\examples{
conquest_nash_short(W = matrix(runif(9), 3, 3))
}

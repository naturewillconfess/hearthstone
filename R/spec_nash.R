#' Specialist Nash calculator
#'
#' Finds Nash equilibrium in mixed strategies and expected winrate for both players in a Specialist match
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#' @param Hero Hero score
#' @param Opp Opponent score
#' @param bestof Match format.Best-of-n means a player has to win (n+1)/2 games to win the match. Has to be odd.
#'
#' @return A list with components
#' \item{nash}{nash equilibrium strategies for both players for games after the first one, data frame}
#' \item{winrates}{winrates for both players, including first game/other games breakdown, matrix}
#' \item{matchups}{winrate matrix}
#' \item{scores}{scores named vector}
#' \item{format}{best of how many games}
#'
#' @examples
#' spec_nash(W = matrix(runif(9), 3, 3))
#' @export
spec_nash <- function(W, Hero = 0, Opp = 0, bestof = 5) {
  if (bestof %% 2 == 0) stop("bestof has to be odd")
  G <- solve_game(W)
  results <- rbind(G$hero_sol, G$opp_sol)
  rownames(results) <- c("Hero", "Opponent")
  colnames(results) <- c("Primary", "Secondary", "Tertiary")

  mw <- spec_all(Hero, Opp, bestof, W)
  fg <- unname(W[1, 1])
  og <- solve_game(W)$V
  winmatrix <- data.frame(c(mw$hero, mw$opp), c(W[1, 1], 1-W[1, 1]),c(og, 1 - og))
  rownames(winmatrix) <- c("Hero", "Opponent")
  colnames(winmatrix) <- c("Match", "First game", "Subsequent games")

  nash <- list(nash = results, winrates = winmatrix, matchups = W, scores = c(Hero = Hero, Opponent = Opp), format = paste0("Best of ", bestof))
  nash
}

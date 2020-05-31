#' Optimal bans calculator
#'
#' Finds Nash equilibrium in mixed strategies and expected winrate for both players in a shield phase of a match (Conquest, Conquest where you have to win with all but one deck, LHS)
#' @importFrom stats na.omit
#' @importFrom utils combn
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#' @param bans number of bans
#' @param nash_fun Type of match
#' @param shield number of shields per player

#' @return A list with following components
#' \item{shields}{Optimal shields FROM Hero's and Opp's bans, so shield_h is a decision by the opponent}
#' \item{winrate}{Hero and Opp winrates}
#' \item{stratlist}{Hero's and Opponent's shield options}
#' \item{conqs}{list of ban_nash output for each shield combination, first layer is hero's shields (HERO's decision to shield from Opp's ban), second is opponent's}
#'
#' @examples
#' shield_nash(W = matrix(runif(16), 4, 4), bans = 1, nash_fun = "conquest_nash_short", shield = 1)
#' @export
shield_nash <- function(W, bans, nash_fun = c("conquest_nash", "conquest_nash_short", "LHS_nash"), shield) {
  n <- ncol(W)
  if (shield == 0) stop("0 shields, just run ban_nash with NULL shields instead")

  shieldcombs <- combn(n, shield, simplify = FALSE)
  m <- length(shieldcombs)
  G <- matrix(rep(NA, m^2), m, m)
  conq <- replicate(m, vector("list", m), simplify = FALSE)

  for (j in seq_along(shieldcombs)) {
    for (k in seq_along(shieldcombs)) {
      shield_h <- shieldcombs[[j]]
      shield_o <- shieldcombs[[k]]
      conq_v <- ban_nash(W, bans, nash_fun, shield_h = shield_h, shield_o = shield_o)
      G[k, j] <- unname(conq_v$winrate)[1]
      conq[[k]][[j]] <- conq_v
    }
  }
  solution <- solve_game(G)
  V <- solution$V
  S <- solution$hero_sol

  S_opp <- solution$opp_sol

  solutions <- list(
    G = G,
    shields = list(hero = S, opp = S_opp),
    winrate = c(hero = V, opp = 1-V),
    stratlist = shieldcombs,
    conqs = conq
  )
  solutions
}

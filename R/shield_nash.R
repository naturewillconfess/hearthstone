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
#' \item{Hero_shield}{Hero's optimal shielding strategy}
#' \item{Opp_shield}{Opponent's optimal shielding strategy}
#' \item{V_hero}{Hero winrate}
#' \item{V_opp}{Hero winrate}
#' \item{stratlist}{Hero's and Opponent's shield options}
#' \item{conqs}{list of ban_nash output for each shield combination}
#'
#' @examples
#' shield_nash(W = matrix(runif(16), 4, 4), bans = 1, nash_fun = "conquest_nash_short", shield = 1)
#' @export
shield_nash <- function(W, bans, nash_fun = c("conquest_nash", "conquest_nash_short", "LHS_nash"), shield) {
  nash_fun <- match.arg(nash_fun)
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
      conq_v <- ban_nash(W, bans, nash_fun, shield_h, shield_o)
      G[j, k] <- conq_v$V_hero
      conq[[j]][[k]] <- conq_v
    }
  }
  solution <- solve_game(G)
  V <- solve_game(G)$V
  S <- solve_game(G)$hero_sol

  V_opp <- 1-V
  S_opp <- solve_game(G)$opp_sol

  solutions <- list(
    Hero_shield = S,
    Opp_shield = S_opp,
    V_hero = V,
    V_opp = V_opp,
    stratlist = shieldcombs,
    conq
  )
  solutions
}

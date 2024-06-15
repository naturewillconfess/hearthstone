#' Optimal bans calculator
#'
#' Finds Nash equilibrium in mixed strategies and expected winrate for both players in a ban phase of a match (Conquest, Conquest where you have to win with all but one deck, LHS)
#' @importFrom stats na.omit
#' @importFrom utils combn
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#' @param match_format Type of match
#' @param bans number of bans

#' @return A list with following components
#' \item{bans}{decks banned BY Hero and Opponent}
#' \item{winrate}{Hero and Opponent's winrate}
#' \item{stratlist}{Hero's and Opponent's ban options}
#' \item{conqs}{list of match_format output for each ban combination - first layer is hero's options, second is opponent's}
#'
#' @examples
#' ban_nash(W = matrix(runif(16), 4, 4), bans = 1, match_format = "conquest")
#' @export
  ban_nash <- function(W, bans, match_format = c("conquest", "LHS")) {
  match_format <- match.arg(match_format)
  if (!match_format %in% c('conquest', 'LHS')) stop('Unknown format')
  n <- ncol(W)

  if (bans > n - 1) stop("Too many bans")
  if (bans == 0) stop("Number of bans should be greater than zero") #todo rework

  bans_h <- combn(n, bans, simplify = FALSE)
  bans_o <- combn(n, bans, simplify = FALSE)

  m_h <- length(bans_h)
  m_o <- length(bans_o)
  G <- matrix(rep(NA, m_h * m_o), m_h, m_o)

  conq <- replicate(m_h, vector("list", m_o), simplify = FALSE)

  nash_fun <- switch(match_format,
                     conquest = conquest_nash,
                     LHS = LHS_nash)

  for (j in seq_along(bans_h)) {
    for (k in seq_along(bans_o)) {
      ban_h <- bans_h[[j]]
      ban_o <- bans_o[[k]]
      conq_v <- nash_fun(W[-ban_o, -ban_h, drop = FALSE])
      G[j, k] <- conq_v[[length(conq_v)]]$winrate[1]
      conq[[j]][[k]] <- conq_v
    }
  }


  solution <- solve_game(G)
  V <- solve_game(G)$V
  S <- solve_game(G)$hero_sol
  S_opp <- solve_game(G)$opp_sol

  solutions <- list(
    bans = list(hero = S, opp = S_opp),
    winrate = c(hero = V, opp = 1-V),
    stratlist = list(hero = bans_h, opp = bans_o),
    conqs = conq
  )
  solutions
}

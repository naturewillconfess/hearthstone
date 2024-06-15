#' Optimal bans calculator
#'
#' Finds Nash equilibrium in mixed strategies and expected winrate for both players in a ban phase of a match (Conquest, Conquest where you have to win with all but one deck, LHS)
#' @importFrom stats na.omit
#' @importFrom utils combn
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#' @param bans number of bans
#' @param nash_fun Type of match
#' @param shield_h decks shielded FROM Hero's bans
#' @param shield_o decks shielded FROM Opponent's bans

#' @return A list with following components
#' \item{bans}{decks banned BY Hero and Opponent}
#' \item{winrate}{Hero and Opponent's winrate}
#' \item{stratlist}{Hero's and Opponent's ban options}
#' \item{conqs}{list of nash_fun output for each ban combination - first layer is hero's options, second is opponent's}
#'
#' @examples
#' ban_nash(W = matrix(runif(16), 4, 4), bans = 1, nash_fun = "conquest_nash")
#' @export
  ban_nash <- function(W, bans, nash_fun = c("conquest_nash", "conquest_nash_short", "LHS_nash"), shield_h = NULL, shield_o = NULL) {
  nash_fun <- match.arg(nash_fun)
  n <- ncol(W)
  s_len_h <- length(shield_h)
  s_len_o <- length(shield_o)

  if (s_len_h != s_len_o) stop("Hero should have the same amount of shields as the Opponent")
  if ((bans > n - 1) & (nash_fun == "conquest_nash" | nash_fun == "LHS_nash") | (bans > n - 2) & nash_fun == "conquest_nash_short") stop("Too many bans")
  if (bans > n - s_len_h) stop("Too many shields")

  if (bans == 0) stop("Number of bans should be greater than zero")

  if (is.null(shield_h)) bans_h <- combn(n, bans, simplify = FALSE) else if (length((1:n)[-shield_h]) == 1) bans_h <- list((1:n)[-shield_h]) else bans_h <- combn((1:n)[-shield_h], bans, simplify = FALSE)
  if (is.null(shield_o)) bans_o <- combn(n, bans, simplify = FALSE) else if (length((1:n)[-shield_o]) == 1) bans_o <- list((1:n)[-shield_o]) else bans_o <- combn((1:n)[-shield_o], bans, simplify = FALSE)

  m_h <- length(bans_h)
  m_o <- length(bans_o)
  G <- matrix(rep(NA, m_h * m_o), m_h, m_o)

  conq <- replicate(m_h, vector("list", m_o), simplify = FALSE)

  nash_fun <- switch(nash_fun,
                     conquest_nash = conquest_nash,
                     conquest_nash_short = conquest_nash_short,
                     LHS_nash = LHS_nash)

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

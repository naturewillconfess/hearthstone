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
#' \item{Hero_bans}{Hero bans}
#' \item{Opp_bans}{Opponent bans}
#' \item{V_hero}{Hero winrate}
#' \item{V_opp}{Hero winrate}
#' \item{stratlist}{Hero's and Opponent's ban options}
#' \item{conqs}{list of nash_fun output for each ban combination}
#'
#' @examples
#' ban_nash(W = matrix(runif(16), 4, 4), bans = 1, nash_fun = "conquest_nash")
#' @export
ban_nash <- function(W, bans, nash_fun = c("conquest_nash", "conquest_nash_short", "LHS_nash"), shield_h = NULL, shield_o = NULL) {
  nash_fun <- match.arg(nash_fun)
  conquest_fun <- get(nash_fun)
  n <- ncol(W)
  s_len_h <- length(shield_h)
  s_len_o <- length(shield_o)

  if (s_len_h != s_len_o) stop("Hero should have the same amount of shields as the Opponent")
  if ((bans + s_len_h > n - 1) & nash_fun == "conquest" | (bans + s_len_h > n - 2) & nash_fun == "conquest_short") stop("Too many bans or shields")
  if (bans == 0) stop("Number of bans should be greater than zero")

  if (is.null(shield_h)) bans_h <- combn(n, bans, simplify = FALSE) else bans_h <- combn((1:n)[-shield_h], bans, simplify = FALSE)
  if (is.null(shield_o)) bans_o <- combn(n, bans, simplify = FALSE) else bans_o <- combn((1:n)[-shield_o], bans, simplify = FALSE)

  m_h <- length(bans_h)
  m_o <- length(bans_o)
  G <- matrix(rep(NA, m_h * m_o), m_h, m_o)

  conq <- replicate(m_h, vector("list", m_o), simplify = FALSE)
  for (j in seq_along(bans_h)) {
    for (k in seq_along(bans_o)) {
      ban_h <- bans_h[[j]]
      ban_o <- bans_o[[k]]
      conq_v <- conquest_fun(W[-ban_h, -ban_o])
      G[j, k] <- conq_v[[length(conq_v)]]$winrate[1]
      conq[[j]][[k]] <- conq_v
    }
  }


  solution <- solve_game(G)
  V <- solve_game(G)$V
  S <- solve_game(G)$hero_sol

  V_opp <- 1-V
  S_opp <- solve_game(G)$opp_sol

  solutions <- list(
    Hero_bans = S,
    Opp_bans = S_opp,
    V_hero = V,
    V_opp = 1-V,
    stratlist = list(bans_h, bans_o),
    conqs = conq
  )
  solutions
}

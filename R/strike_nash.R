#' Strike winrate calculator
#'
#' Finds expected winrate for both players in a Strike match
#' @importFrom stats na.omit
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#' @param Hero Hero score
#' @param Opp Opponent score
#' @param nash_bans whether to find Nash bans, or to to use bans supplied as arguments
#' @param nbans number of bans per players. ignored if nash_bans = FALSE
#' @param HeroBans coordinate matrix of Hero Bans. ignored if nash_bans = TRUE
#' @param OppBans coordinate matrix of Opponent Bans. ignored if nash_bans = TRUE
#' @param played coordinate matrix of games that has already been played.
#'
#' @return A list with components
#' \item{winrates}{A vector of winrates of both players}
#' \item{matchups}{Winrate matrix}
#' \item{hero_bans}{Hero bans, whether optimal or supplied by the user}
#' \item{played}{Games that have already been played, supplied by the user}
#' \item{isnash}{Whether bans were chosen by the function}
#' @examples
#' strike_nash(W = matrix(runif(9), 3, 3))
#' @export
strike_nash <- function(W, Hero = 0, Opp = 0, nash_bans = TRUE, nbans = 0, HeroBans = NULL, OppBans = NULL, played = NULL) {
  if (any(W > 1 | W < 0)) stop("Winrates must be less than one and greater than zero")
  if (!(is.null(HeroBans) & is.null(OppBans))) {
    if (nash_bans) stop("Choose either manual bans or Nash bans")
    if ((is.null(HeroBans) | is.null(OppBans))) stop("Enter both bans") else if (nrow(HeroBans) != nrow(OppBans)) stop("Enter equal number of bans") else if (2 * nrow(HeroBans) > (length(W) - 1)) stop("Too many bans")
  }
  W1 <- W
  W2 <- W
  if (nash_bans) {
    if (2 * nbans > (length(W) - 1)) stop("Too many bans")
    if (nbans > 0) {
      HB <- which(W <= sort(W)[nbans], arr.ind = T)
      HeroBans <- HB[1:nbans, , drop = FALSE]
      W2[HeroBans] <- -9999
      OB <- which(W2 >= sort(W2)[length(W2) - nbans + 1], arr.ind = T)
      OppBans <- OB[1:nbans, , drop = FALSE]
    }
  }

  W[HeroBans] <- NA
  W[OppBans] <- NA
  WV <- na.omit(as.numeric(W))
  if (length(WV) %% 2 == 0) stop("Number of games after bans should be odd")

  W[played] <- NA
  WV <- na.omit(as.numeric(W))

  win_pdf <- convolve_direct(WV)
  req_win <- length(WV) %/% 2 + 1 - Hero
  win_prob <- sum(win_pdf[(req_win + 1):length(win_pdf)])
  win_prob <- matrix(c(Hero = win_prob, Opponent = 1 - win_prob), 2, 1, dimnames = list(c("Hero", "Opponent"), NULL))

  results <- list(
    winrates = win_prob,
    matchups = W1,
    scores = c(Hero = Hero, Opponent = Opp),
    hero_bans = HeroBans,
    opp_bans = OppBans,
    played = played,
    isnash = nash_bans
  )
  results
}

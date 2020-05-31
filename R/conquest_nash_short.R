#' Conquest Nash calculator (short version)
#'
#' Finds Nash equilibrium in mixed strategies and expected winrate for both players in a Conquest match in which you have to win with all but one deck
#' @importFrom stats na.omit
#' @importFrom utils combn
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#'
#' @return A list of possible state of the game with following parameters (if applicable)
#' \item{score}{eliminated decks}
#' \item{winrate}{Players' winrate}
#' \item{nash}{Nash equilibrium in mixed strategies}
#' \item{game}{payoff matrix for this subgame}
#' @examples
#' conquest_nash_short(W = matrix(runif(9), 3, 3))
#' @export
conquest_nash_short <- function(W) {
  n <- ncol(W)

  combs <- unlist(lapply(0:(n - 1), function(x) combn(1:n, x, simplify = FALSE)), recursive = FALSE)
  comblist <- lapply(combs, function(x) lapply(combs, list, x))
  comblist <- unlist(comblist, recursive = FALSE)
  ngames <- sapply(comblist, function(x) length(x[[1]]) + length(x[[2]]))
  comblist <- comblist[order(ngames, decreasing = TRUE)]
  ngames <- sapply(comblist, function(x) length(x[[1]]) + length(x[[2]]))
  comblist <- comblist[ngames != (2 * n - 2)]
  comblist <- lapply(comblist, function(x) list(score = x))
  combmat_1 <- sapply(comblist, function(x) {
    nums <- paste0(x$score[[1]], collapse = "")
    if (length(nums) == 0) nums <- "zero"
    nums
  })
  combmat_2 <- sapply(comblist, function(x) {
    nums <- paste0(x$score[[2]], collapse = "")
    if (length(nums) == 0) nums <- "zero"
    nums
  })
  combmat <- data.frame(h = combmat_1, o = combmat_2, stringsAsFactors = FALSE)
  for (i in seq_along(comblist)) {
    won <- comblist[[i]][[1]]
    wonlen <- sapply(won, length)
    if (wonlen[1] == n - 1) {
      V <- 1
      V_opp <- 0
      comblist[[i]] <- c(score = list(comblist[[i]][[1]]), winrate = list(c(V, V_opp)))
    } else if (wonlen[2] == n - 1) {
      V <- 0
      V_opp <- 1
      comblist[[i]] <- c(score = list(comblist[[i]][[1]]), winrate = list(c(V, V_opp)))
    } else {
      # Players options
      optP <- (1:n)[!(1:n) %in% won[[1]]]
      optO <- (1:n)[!(1:n) %in% won[[2]]]
      G <- W
      G[TRUE] <- NA
      ## is alpha_beta_pruning applicable?
      for (j in optP) {
        for (k in optO) {
          winner_1 <- c(won[[1]], j)
          winner_1 <- paste0(winner_1[order(winner_1)], collapse = "")
          winner_2 <- paste0(won[[2]], collapse = "")
          which_winner <- which(combmat$h == winner_1 & combmat$o == winner_2)
          winner_V <- comblist[[which_winner]]$winrate[1]

          loser_1 <- paste0(won[[1]], collapse = "")
          loser_2 <- c(won[[2]], k)
          loser_2 <- paste0(loser_2[order(loser_2)], collapse = "")
          which_loser <- which(combmat$h == loser_1 & combmat$o == loser_2)
          loser_V <- comblist[[which_loser]]$winrate[1]

          G[j, k] <- W[j, k] * winner_V + (1 - W[j, k]) * loser_V
        }
      }
      G <- G[!(1:n) %in% won[[1]], !(1:n) %in% won[[2]]]
      solution <- solve_game(G)
      V <- solve_game(G)$V
      S <- solve_game(G)$hero_sol

      V_opp <- 1-V
      S_opp <- solve_game(G)$opp_sol

      comblist[[i]] <- c(comblist[[i]], winrate = list(c(V, V_opp)), nash = list(list(S, S_opp)), game = list(G))
    }
  }
  comblist
}

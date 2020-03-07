#' LHS Nash calculator
#'
#' Finds Nash equilibrium in mixed strategies and expected winrate for both players in an LHS match
#' @importFrom stats na.omit
#' @importFrom utils combn
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#'
#' @return A list of possible state of the game with following parameters (if applicable)
#' \item{score}{eliminated decks}
#' \item{havetoplay_hero}{What deck does the hero have to play?}
#' \item{havetoplay_opp}{What deck does the opponent have to play?}
#' \item{winrate}{Players' winrate}
#' \item{nash}{Nash equilibrium in mixed strategies}
#' \item{game}{payoff matrix for this subgame}
#' @examples
#' LHS_nash(W = matrix(runif(9), 3, 3))
#' @export
LHS_nash <- function(W) {
  n <- nrow(W)
  W1 <- W
  combs <- unlist(lapply(0:n, function(x) combn(1:n, x, simplify = FALSE)), recursive = FALSE)
  comblist <- lapply(combs, function(x) lapply(combs, list, x))
  comblist <- unlist(comblist, recursive = FALSE)
  comblist <- comblist[-length(comblist)]
  ngames <- sapply(comblist, function(x) length(x[[1]]) + length(x[[2]]))
  comblist <- comblist[order(ngames, decreasing = TRUE)]
  comblist <- lapply(comblist, function(x) list(score = x))

  biglist <- vector("list", length(comblist))
  for (i in seq_along(comblist)) {
    l1 <- length(comblist[[i]]$score[[1]])
    l2 <- length(comblist[[i]]$score[[2]])
    if (l1 > n - 2 | l2 > n - 2) {
      biglist[[i]] <- list(comblist[[i]])
    } else {
      if (l1 != 0 & l2 != 0) {
        havetoplay_hero <- (1:n)[-comblist[[i]]$score[[1]]]
        havetoplay_opp <- (1:n)[-comblist[[i]]$score[[2]]]
        havetoplay_hero <- c(havetoplay_hero, rep(NA, length(havetoplay_opp)))
        havetoplay_opp <- c(rep(NA, length(havetoplay_hero[!is.na(havetoplay_hero)])), havetoplay_opp)
      } else if (l1 == 0 & l2 != 0) {
        havetoplay_hero <- 1:n
        havetoplay_opp <- rep(NA, n)
      } else if (l1 != 0 & l2 == 0) {
        havetoplay_hero <- rep(NA, n)
        havetoplay_opp <- 1:n
      } else {
        havetoplay_hero <- NA
        havetoplay_opp <- NA
      }

      biglist[[i]] <- rep(list(comblist[[i]]), length(havetoplay_hero))
      for (j in 1:length(havetoplay_hero)) {
        biglist[[i]][[j]] <- c(biglist[[i]][[j]], havetoplay_hero = list(havetoplay_hero[j]), havetoplay_opp = list(havetoplay_opp[j]))
      }
    }
  }
  comblist <- unlist(biglist, recursive = FALSE)

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

  haveto_1 <- sapply(comblist, function(x) {
    y <- x$havetoplay_hero
    if (is.null(y)) y <- "null"
    y
  })
  haveto_2 <- sapply(comblist, function(x) {
    y <- x$havetoplay_opp
    if (is.null(y)) y <- "null"
    y
  })
  combmat <- data.frame(h = combmat_1, o = combmat_2, h_haveto = haveto_1, o_haveto = haveto_2, stringsAsFactors = FALSE)

  for (i in seq_along(comblist)) {
    lost <- comblist[[i]]$score
    lostlen <- c(length(lost[[1]]), length(lost[[2]]))
    if (lostlen[1] == n) {
      V <- 0
      V_opp <- 1
      comblist[[i]] <- c(score = list(comblist[[i]]$score), winrate = list(c(V, V_opp)))
    } else if (lostlen[2] == n) {
      V <- 1
      V_opp <- 0
      comblist[[i]] <- c(score = list(comblist[[i]]$score), winrate = list(c(V, V_opp)))
    } else if (lostlen[1] == n - 1) {
      V <- prod(W[!(1:n) %in% lost[[1]], !(1:n) %in% lost[[2]]])
      V_opp <- 1 - V
      comblist[[i]] <- c(score = list(comblist[[i]]$score), winrate = list(c(V, V_opp)))
    } else if (lostlen[2] == n - 1) {
      V <- 1 - prod(1 - W[!(1:n) %in% lost[[1]], !(1:n) %in% lost[[2]]])
      V_opp <- 1 - V
      comblist[[i]] <- c(score = list(comblist[[i]]$score), winrate = list(c(V, V_opp)))
    } else {
      # Players options
      optH <- (1:n)[!(1:n) %in% lost[[1]]]
      optO <- (1:n)[!(1:n) %in% lost[[2]]]

      if (!is.na(combmat$h_haveto[i])) {
        G <- matrix(NA, 1, n)
        for (j in optO) {
          winner_1 <- paste0(lost[[1]], collapse = "")
          winner_2 <- c(lost[[2]], j)
          winner_2 <- paste0(winner_2[order(winner_2)], collapse = "")
          haveto_1 <- combmat$h_haveto[i]
          haveto_2 <- NA

          which_winner <- which(combmat$h == winner_1 & combmat$o == winner_2)

          if (length(which_winner) > 1) {
            which_winner <- which(combmat$h == winner_1 &
              combmat$o == winner_2 &
              ((combmat$h_haveto == haveto_1 & !is.na(haveto_1) & !is.na(combmat$h_haveto)) | (is.na(combmat$h_haveto) & is.na(combmat$h_haveto))) &
              ((combmat$o_haveto == haveto_2 & !is.na(haveto_2) & !is.na(combmat$o_haveto)) | (is.na(combmat$o_haveto) & is.na(combmat$o_haveto))))
          }

          winner_V <- comblist[[which_winner]]$winrate[1]

          loser_1 <- c(lost[[1]], combmat$h_haveto[i])
          loser_1 <- paste0(loser_1[order(loser_1)], collapse = "")
          loser_2 <- paste0(lost[[2]], collapse = "")
          haveto_1 <- NA
          haveto_2 <- j
          which_loser <- which(combmat$h == loser_1 & combmat$o == loser_2)

          if (length(which_loser) > 1) {
            which_loser <- which(combmat$h == loser_1 &
              combmat$o == loser_2 &
              ((combmat$h_haveto == haveto_1 & !is.na(haveto_1) & !is.na(combmat$h_haveto)) | (is.na(combmat$h_haveto) & is.na(combmat$h_haveto))) &
              ((combmat$o_haveto == haveto_2 & !is.na(haveto_2) & !is.na(combmat$o_haveto)) | (is.na(combmat$o_haveto) & is.na(combmat$o_haveto))))
          }

          loser_V <- comblist[[which_loser]]$winrate[1]


          G[1, j] <- W[as.numeric(combmat$h_haveto[i]), j] * winner_V + (1 - W[as.numeric(combmat$h_haveto[i]), j]) * loser_V
        }
        G <- G[, optO, drop = FALSE]
      } else if (!is.na(combmat$o_haveto[i])) {
        G <- matrix(NA, n, 1)
        for (j in optH) {
          winner_1 <- paste0(lost[[1]], collapse = "")
          winner_2 <- c(lost[[2]], combmat$o_haveto[i])
          winner_2 <- paste0(winner_2[order(winner_2)], collapse = "")
          haveto_1 <- j
          haveto_2 <- NA

          which_winner <- which(combmat$h == winner_1 & combmat$o == winner_2)

          if (length(which_winner) > 1) {
            which_winner <- which(combmat$h == winner_1 &
              combmat$o == winner_2 &
              ((combmat$h_haveto == haveto_1 & !is.na(haveto_1) & !is.na(combmat$h_haveto)) | (is.na(combmat$h_haveto) & is.na(combmat$h_haveto))) &
              ((combmat$o_haveto == haveto_2 & !is.na(haveto_2) & !is.na(combmat$o_haveto)) | (is.na(combmat$o_haveto) & is.na(combmat$o_haveto))))
          }

          winner_V <- comblist[[which_winner]]$winrate[1]

          loser_1 <- c(lost[[1]], j)
          loser_1 <- paste0(loser_1[order(loser_1)], collapse = "")
          loser_2 <- paste0(lost[[2]], collapse = "")
          haveto_1 <- NA
          haveto_2 <- combmat$o_haveto[i]
          which_loser <- which(combmat$h == loser_1 & combmat$o == loser_2)

          if (length(which_loser) > 1) {
            which_loser <- which(combmat$h == loser_1 &
              combmat$o == loser_2 &
              ((combmat$h_haveto == haveto_1 & !is.na(haveto_1) & !is.na(combmat$h_haveto)) | (is.na(combmat$h_haveto) & is.na(combmat$h_haveto))) &
              ((combmat$o_haveto == haveto_2 & !is.na(haveto_2) & !is.na(combmat$o_haveto)) | (is.na(combmat$o_haveto) & is.na(combmat$o_haveto))))
          }

          loser_V <- comblist[[which_loser]]$winrate[1]


          G[j, 1] <- W[j, as.numeric(combmat$o_haveto[i])] * winner_V + (1 - W[j, as.numeric(combmat$o_haveto[i])]) * loser_V
        }
        G <- G[optH, , drop = FALSE]
      } else {
        G <- matrix(NA, n, n)
        for (j in optH) {
          for (k in optO) {
            winner_1 <- paste0(lost[[1]], collapse = "")
            winner_2 <- c(lost[[2]], k)
            winner_2 <- paste0(winner_2[order(winner_2)], collapse = "")
            haveto_1 <- j
            haveto_2 <- NA

            which_winner <- which(combmat$h == winner_1 & combmat$o == winner_2)

            if (length(which_winner) > 1) {
              which_winner <- which(combmat$h == winner_1 &
                combmat$o == winner_2 &
                ((combmat$h_haveto == haveto_1 & !is.na(haveto_1) & !is.na(combmat$h_haveto)) | (is.na(combmat$h_haveto) & is.na(combmat$h_haveto))) &
                ((combmat$o_haveto == haveto_2 & !is.na(haveto_2) & !is.na(combmat$o_haveto)) | (is.na(combmat$o_haveto) & is.na(combmat$o_haveto))))
            }

            winner_V <- comblist[[which_winner]]$winrate[1]

            loser_1 <- c(lost[[1]], j)
            loser_1 <- paste0(loser_1[order(loser_1)], collapse = "")
            loser_2 <- paste0(lost[[2]], collapse = "")
            haveto_1 <- NA
            haveto_2 <- k
            which_loser <- which(combmat$h == loser_1 & combmat$o == loser_2)

            if (length(which_loser) > 1) {
              which_loser <- which(combmat$h == loser_1 &
                combmat$o == loser_2 &
                ((combmat$h_haveto == haveto_1 & !is.na(haveto_1) & !is.na(combmat$h_haveto)) | (is.na(combmat$h_haveto) & is.na(combmat$h_haveto))) &
                ((combmat$o_haveto == haveto_2 & !is.na(haveto_2) & !is.na(combmat$o_haveto)) | (is.na(combmat$o_haveto) & is.na(combmat$o_haveto))))
            }

            loser_V <- comblist[[which_loser]]$winrate[1]
            combmat$o_haveto == haveto_2

            G[j, k] <- W[j, k] * winner_V + (1 - W[j, k]) * loser_V
          }
        }
        G <- G[optH, optO, drop = FALSE]
      }
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

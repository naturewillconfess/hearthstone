#' Conquest Nash calculator
#'
#' Finds Nash equilibrium in mixed strategies and expected winrate for both players in a Conquest match
#' @importFrom stats na.omit
#' @importFrom utils combn
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#'
#' @return A list of possible states of the game with following parameters (if applicable)
#' \item{score}{eliminated decks}
#' \item{winrate}{Players' winrate}
#' \item{nash}{Nash equilibrium in mixed strategies}
#' \item{game}{payoff matrix for this subgame}
#' @examples
#' conquest_nash(W = matrix(runif(9), 3, 3))
#' @export
conquest_nash <- function(W) {
  n <- ncol(W)
  combs <- unlist(lapply(0:n, function(x) combn(1:n, x, simplify = FALSE)), recursive = FALSE) #all possible combinations of eliminated decks for a single player
  comblist <- lapply(combs, function(x) lapply(combs, list, x)) 
  comblist <- unlist(comblist, recursive = FALSE)
  comblist <- comblist[-length(comblist)] #all possible combinations of eliminated decks for two players - each combination describes a subgame of the conquest match
  ngames <- sapply(comblist, function(x) length(x[[1]]) + length(x[[2]])) #number of games that have occurred up to this point
  comblist <- comblist[order(ngames, decreasing = TRUE)] # sort so that we start from the leaves of the game tree rather than the trunk

  #convert the score data structure to a data frame for easier lookup
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
    if (wonlen[1] == n) {
      V <- 1
      V_opp <- 0
      comblist[[i]] <- c(score = list(comblist[[i]][[1]]), winrate = list(c(V, V_opp))) #if the player has won with all their decks, their payoff is 1
    } else if (wonlen[2] == n) {
      V <- 0
      V_opp <- 1
      comblist[[i]] <- c(score = list(comblist[[i]][[1]]), winrate = list(c(V, V_opp)))
    } else if (wonlen[1] == n - 1) {
      V <- 1 - prod(1 - W[!(1:n) %in% won[[1]], !(1:n) %in% won[[2]]])
      V_opp <- 1 - V
      comblist[[i]] <- c(score = list(comblist[[i]][[1]]), winrate = list(c(V, V_opp))) #if the player has won with all but 1 of their decks, their payoff is 1 - prob of losing all their remaining matches
    } else if (wonlen[2] == n - 1) {
      V <- prod(W[!(1:n) %in% won[[1]], !(1:n) %in% won[[2]]])
      V_opp <- 1 - V
      comblist[[i]] <- c(score = list(comblist[[i]][[1]]), winrate = list(c(V, V_opp)))
    } else { 
      #if the game hasn't progressed to one of the abovementioned points, payoffs might depend on deck selection for the next game in the match
      #so the payoff of the conquest subgame with a set combination of eliminated deck is the payoff of the deck selection 'game'
      #the payoff for the deck selection game is a sum of payoffs in case of a win and a loss weighted by probabilities of winning and losing
      #since we're moving up the subgame tree, we always know the payoffs for winning and losing with each combination of selected decks
      
      # Players options for deck selection in our subgame
      optP <- (1:n)[!(1:n) %in% won[[1]]]
      optO <- (1:n)[!(1:n) %in% won[[2]]]
      G <- W
      #calculate the payoff matrix G for the deck selection step
      ## is alpha_beta_pruning or other optimizations applicable? need some research
      for (j in optP) {
        for (k in optO) {
          
          winner_1 <- c(won[[1]], j)
          winner_1 <- paste0(winner_1[order(winner_1)], collapse = "")
          winner_2 <- paste0(won[[2]], collapse = "")
          which_winner <- which(combmat$h == winner_1 & combmat$o == winner_2)
          winner_V <- comblist[[which_winner]][[2]][1]

          loser_1 <- paste0(won[[1]], collapse = "")
          loser_2 <- c(won[[2]], k)
          loser_2 <- paste0(loser_2[order(loser_2)], collapse = "")
          which_loser <- which(combmat$h == loser_1 & combmat$o == loser_2)
          loser_V <- comblist[[which_loser]][[2]][1]

          G[j, k] <- W[j, k] * winner_V + (1 - W[j, k]) * loser_V
        }
      }

      G <- G[optP, optO]
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

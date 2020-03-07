spec_nonzero <- function(Hero, Opp, bestof = 5, W) {
  if (bestof %% 2 == 0) stop("bestof has to be odd")
  ####
  # i - score of the hero (of the calculation, not the matrix!)
  # j - score of the opponent
  # m - score to get
  m <- (bestof + 1) / 2
  p <- solve_game(W)$V
  n <- (2 * m - 1 - Hero - Opp)

  i <- Hero
  j <- Opp
  if (Opp == m) {
    mij_hero <- 0
  } else {
    kmin <- m - Hero
    k <- kmin:n
    mij_hero <- sum(factorial(n) / (factorial(k) * factorial(n - k)) * p^k * (1 - p)^(n - k))
  }

  if (Hero == m) {
    mij_opp <- 0
  } else {
    kmin <- m - Opp
    k <- kmin:n
    mij_opp <- sum(factorial(n) / (factorial(k) * factorial(n - k)) * p^k * (1 - p)^(n - k))
  }
  mij = list(hero = mij_hero,
             opp = mij_opp)
}

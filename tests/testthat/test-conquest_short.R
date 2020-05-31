test_that("symmetric conquest short", {
  W <- matrix(rep(0.5,9),3,3)
  n <- conquest_nash_short(W)
  expect_equal(n[[length(n)]]$winrate, c(0.5,0.5))
})

test_that("small conquest short", {
  W <- matrix(0.5,3,3)
  n <- conquest_nash_short(W)
  expect_equal(n[[length(n)]]$winrate, c(0.5,0.5))
})

test_that("calibration conquest short", {
  games <- replicate(1000, conquest_nash_short(matrix(runif(9),3,3)), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$nash[[1]])))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$nash[[2]])))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})


test_that("G11", {
  W <- matrix(runif(9),3,3)
  nash <- conquest_nash_short(W)
  G_10_11 <-  W[2,1]+(1-W[2,1])*solve_game(W[-1,-1])$V
  G_10_12 <-  W[2,2]+(1-W[2,2])*solve_game(W[-1,-2])$V
  G_10_13 <-  W[2,3]+(1-W[2,3])*solve_game(W[-1,-3])$V
  G_10_21 <-  W[3,1]+(1-W[3,1])*solve_game(W[-1,-1])$V
  G_10_22 <-  W[3,2]+(1-W[3,2])*solve_game(W[-1,-2])$V
  G_10_23 <-  W[3,3]+(1-W[3,3])*solve_game(W[-1,-3])$V
  G_10 <- matrix(c(G_10_11,G_10_12,G_10_13, G_10_21,G_10_22,G_10_23), byrow = TRUE,nrow = 2,ncol = 3)

  G_01_11 <-  W[1,2]*solve_game(W[-1,-1])$V
  G_01_21 <-  W[2,2]*solve_game(W[-2,-1])$V
  G_01_31 <-  W[3,2]*solve_game(W[-3,-1])$V

  G_01_12 <-  W[1,3]*solve_game(W[-1,-1])$V
  G_01_22 <-  W[2,3]*solve_game(W[-2,-1])$V
  G_01_32 <-  W[3,3]*solve_game(W[-3,-1])$V


  G_01 <- matrix(c(G_01_11, G_01_21, G_01_31, G_01_12, G_01_22, G_01_32), nrow = 3, ncol = 2)
  G11 <- W[1,1] * solve_game(G_10)$V + (1 - W[1,1]) * solve_game(G_01)$V
  expect_equal(G11, nash[[40]]$game[1,1])
})

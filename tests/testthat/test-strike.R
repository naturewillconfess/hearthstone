test_that("symmetric strike", {
  W <- matrix(rep(0.5,9), 3, 3)
  expect_equal(as.numeric(strike_nash(W)$winrates), c(0.5,0.5))
  expect_equal(as.numeric(strike_nash(W, nbans = 1)$winrates), c(0.5,0.5))
})



test_that("calibration strike", {
  games <- replicate(10000, strike_nash(matrix(runif(9),3,3), nbans = 1), simplify = FALSE)
  games_v <- rowMeans(do.call(cbind,lapply(games, function(x) x$winrates)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$hero_bans)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$opp_bans)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 1.9 & games_h < 2.1))
  expect_true(all(games_o > 1.9 & games_o < 2.1))
})

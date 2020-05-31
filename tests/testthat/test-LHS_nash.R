test_that("symmetric LHS", {
  W <- matrix(rep(0.5,9),3,3)
  n <- LHS_nash(W)
  expect_equal(n[[length(n)]]$winrate, c(0.5,0.5))
})

test_that("small LHS", {
  W <- matrix(0.5,1,1)
  n <- LHS_nash(W)
  expect_equal(n[[length(n)]]$winrate, c(0.5,0.5))
})

test_that("calibration LHS", {
  games <- replicate(1000, LHS_nash(matrix(runif(9),3,3)), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$nash[[1]])))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$nash[[2]])))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.25 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

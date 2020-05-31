test_that("symmetric ban nash", {
  W <- matrix(rep(0.5,16),4,4)
  n <- ban_nash(W, bans = 1)
  expect_equal(unname(n$winrate), c(0.5,0.5))

})

test_that("calibration ban nash 1", {
  games <- replicate(1000, ban_nash(matrix(runif(9),3,3), bans = 1, nash_fun = "conquest_nash"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

test_that("calibration ban nash 2", {
  games <- replicate(1000, ban_nash(matrix(runif(9),3,3), bans = 2, nash_fun = "conquest_nash"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

test_that("calibration ban nash 3", {
  games <- replicate(1000, ban_nash(matrix(runif(16),4,4), bans = 1, nash_fun = "conquest_nash"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.2 & games_h < 0.3))
  expect_true(all(games_o > 0.2 & games_o < 0.3))
})

test_that("calibration ban nash 4", {
  games <- replicate(1000, ban_nash(matrix(runif(16),4,4), bans = 2, nash_fun = "conquest_nash"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.13 & games_h < 0.21))
  expect_true(all(games_o > 0.13 & games_o < 0.21))
})
#############################
test_that("calibration ban nash 1 short", {
  games <- replicate(1000, ban_nash(matrix(runif(9),3,3), bans = 1, nash_fun = "conquest_nash_short"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})


test_that("calibration ban nash 3 short", {
  games <- replicate(1000, ban_nash(matrix(runif(16),4,4), bans = 1, nash_fun = "conquest_nash_short"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.2 & games_h < 0.3))
  expect_true(all(games_o > 0.2 & games_o < 0.3))
})

test_that("calibration ban nash 4 short", {
  games <- replicate(1000, ban_nash(matrix(runif(16),4,4), bans = 2, nash_fun = "conquest_nash_short"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.14 & games_h < 0.20))
  expect_true(all(games_o > 0.14 & games_o < 0.20))
})

#############################
test_that("symmetric ban nash LHS", {
  W <- matrix(rep(0.5,16),4,4)
  n <- ban_nash(W, bans = 1)
  expect_equal(unname(n$winrate), c(0.5,0.5))

})

test_that("calibration ban nash 1 LHS", {
  games <- replicate(1000, ban_nash(matrix(runif(9),3,3), bans = 1, nash_fun = "LHS_nash"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

test_that("calibration ban nash 2 LHS", {
  games <- replicate(1000, ban_nash(matrix(runif(9),3,3), bans = 2, nash_fun = "LHS_nash"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

test_that("calibration ban nash 3 LHS", {
  games <- replicate(100, ban_nash(matrix(runif(16),4,4), bans = 1, nash_fun = "LHS_nash"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.15 & games_h < 0.35))
  expect_true(all(games_o > 0.15 & games_o < 0.35))
})

test_that("calibration ban nash 4 LHS", {
  games <- replicate(1000, ban_nash(matrix(runif(16),4,4), bans = 2, nash_fun = "LHS_nash"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.13 & games_h < 0.21))
  expect_true(all(games_o > 0.13 & games_o < 0.21))
})

n = 100
acc = 0.2

test_that("symmetric ban nash", {
  W <- matrix(rep(0.5,16),4,4)
  n <- ban_nash(W, bans = 1)
  expect_equal(unname(n$winrate), c(0.5,0.5))

})

test_that("calibration ban nash 1", {
  games <- replicate(n, ban_nash(matrix(runif(9),3,3), bans = 1, match_format = "conquest"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))

  expect_true(all(games_v > 0.5-acc & games_v < 0.5+acc))
  expect_true(all(games_h > 0.33-acc & games_h < 0.33+acc))
  expect_true(all(games_o > 0.33-acc & games_o < 0.33+acc))
})

test_that("calibration ban nash 2", {
  games <- replicate(n, ban_nash(matrix(runif(9),3,3), bans = 2, match_format = "conquest"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))
  
  expect_true(all(games_v > 0.5-acc & games_v < 0.5+acc))
  expect_true(all(games_h > 0.33-acc & games_h < 0.33+acc))
  expect_true(all(games_o > 0.33-acc & games_o < 0.33+acc))
})

test_that("calibration ban nash 3", {
  games <- replicate(n, ban_nash(matrix(runif(16),4,4), bans = 1, match_format = "conquest"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))
  
  expect_true(all(games_v > 0.5-acc & games_v < 0.5+acc))
  expect_true(all(games_h > 0.25-acc & games_h < 0.25+acc))
  expect_true(all(games_o > 0.25-acc & games_o < 0.25+acc))
})

test_that("calibration ban nash 4", {
  games <- replicate(n, ban_nash(matrix(runif(16),4,4), bans = 2, match_format = "conquest"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))
  
  expect_true(all(games_v > 0.5-acc & games_v < 0.5+acc))
  expect_true(all(games_h > 1/6-acc & games_h < 1/6+acc))
  expect_true(all(games_o > 1/6-acc & games_o < 1/6+acc))
})
#############################
test_that("symmetric ban nash LHS", {
  W <- matrix(rep(0.5,16),4,4)
  n <- ban_nash(W, bans = 1)
  expect_equal(unname(n$winrate), c(0.5,0.5))

})

test_that("calibration ban nash 1 LHS", {
  games <- replicate(n, ban_nash(matrix(runif(9),3,3), bans = 1, match_format = "LHS"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))
  
  expect_true(all(games_v > 0.5-acc & games_v < 0.5+acc))
  expect_true(all(games_h > 0.33-acc & games_h < 0.33+acc))
  expect_true(all(games_o > 0.33-acc & games_o < 0.33+acc))
})

test_that("calibration ban nash 2 LHS", {
  games <- replicate(n, ban_nash(matrix(runif(9),3,3), bans = 2, match_format = "LHS"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))
  
  expect_true(all(games_v > 0.5-acc & games_v < 0.5+acc))
  expect_true(all(games_h > 0.33-acc & games_h < 0.33+acc))
  expect_true(all(games_o > 0.33-acc & games_o < 0.33+acc))
})

test_that("calibration ban nash 3 LHS", {
  games <- replicate(n, ban_nash(matrix(runif(16),4,4), bans = 1, match_format = "LHS"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))
  
  expect_true(all(games_v > 0.5-acc & games_v < 0.5+acc))
  expect_true(all(games_h > 0.25-acc & games_h < 0.25+acc))
  expect_true(all(games_o > 0.25-acc & games_o < 0.25+acc))
})

test_that("calibration ban nash 4 LHS", {
  games <- replicate(n, ban_nash(matrix(runif(16),4,4), bans = 2, match_format = "LHS"), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$bans$opp)))
  
  expect_true(all(games_v > 0.5-acc & games_v < 0.5+acc))
  expect_true(all(games_h > 1/6-acc & games_h < 1/6+acc))
  expect_true(all(games_o > 1/6-acc & games_o < 1/6+acc))
})

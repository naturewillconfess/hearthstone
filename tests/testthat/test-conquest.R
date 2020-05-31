test_that("symmetric conquest", {
  W <- matrix(rep(0.5,9),3,3)
  n <- conquest_nash(W)
  expect_equal(n[[length(n)]]$winrate, c(0.5,0.5))
})

test_that("small conquest", {
  W <- matrix(0.5,1,1)
  n <- conquest_nash(W)
  expect_equal(n[[length(n)]]$winrate, c(0.5,0.5))
})

test_that("calibration conquest", {
  games <- replicate(1000, conquest_nash(matrix(runif(9),3,3)), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$nash[[1]])))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x[[length(x)]]$nash[[2]])))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

test_that("bo3 vignette", {
  W <- matrix(runif(4), 2, 2)
  nash <- conquest_nash(W)
  expect_equal(nash[[length(nash)]]$winrate[1], (2*W[1,1]*W[2,1] + W[1,1]*W[2,2] + W[2,1]*W[1,2] + 2*W[1,2]*W[2,2] - W[1,1]*W[2,1]*W[2,2] - W[1,1]*W[1,2]*W[2,2] - W[2,1]*W[1,1]*W[1,2] - W[2,1]*W[1,2]*W[2,2])/2)
})

test_that("bo5 vignette", {
  W <- matrix(runif(9), 3, 3)
  nash <- conquest_nash(W)
  first_score <- lapply(nash, function(x) x$score[[1]])
  sec_score <- lapply(nash, function(x) x$score[[2]])

  m1_2 <- sapply(first_score, function(x) identical(x, c(1L,2L))) & sapply(sec_score, function(x) identical(x, integer()))
  expect_equal(nash[[which(m1_2)]]$winrate[1], 1 - (1-W[3,1]) * (1 - W[3,2]) * (1 - W[3,3]) )

  m1_3 <- sapply(first_score, function(x) identical(x, c(1L,3L))) & sapply(sec_score, function(x) identical(x, integer()))
  expect_equal(nash[[which(m1_3)]]$winrate[1], 1 - (1-W[2,1]) * (1 - W[2,2]) * (1 - W[2,3]) )

  m11 <- sapply(first_score, function(x) identical(x, c(1L))) & sapply(sec_score, function(x) identical(x, 1L))
  expect_equal(nash[[which(m11)]]$winrate[1], (2*W[2,2]*W[3,2] + W[2,2]*W[3,3] + W[3,2]*W[2,3] + 2*W[2,3]*W[3,3] - W[2,2]*W[3,2]*W[3,3] - W[2,2]*W[2,3]*W[3,3] - W[3,2]*W[2,2]*W[2,3] - W[3,2]*W[2,3]*W[3,3])/2)

  m12 <- sapply(first_score, function(x) identical(x, c(1L))) & sapply(sec_score, function(x) identical(x, 2L))
  expect_equal(nash[[which(m12)]]$winrate[1], (2*W[2,1]*W[3,1] + W[2,1]*W[3,3] + W[3,1]*W[2,3] + 2*W[2,3]*W[3,3] - W[2,1]*W[3,1]*W[3,3] - W[2,1]*W[2,3]*W[3,3] - W[3,1]*W[2,1]*W[2,3] - W[3,1]*W[2,3]*W[3,3])/2)

  m13 <- sapply(first_score, function(x) identical(x, c(1L))) & sapply(sec_score, function(x) identical(x, 3L))
  expect_equal(nash[[which(m13)]]$winrate[1], (2*W[2,1]*W[3,1] + W[2,1]*W[3,2] + W[3,1]*W[2,2] + 2*W[2,2]*W[3,2] - W[2,1]*W[3,1]*W[3,2] - W[2,1]*W[2,2]*W[3,2] - W[3,1]*W[2,1]*W[2,2] - W[3,1]*W[2,2]*W[3,2])/2)

  m1 <- sapply(first_score, function(x) identical(x, c(1L))) & sapply(sec_score, function(x) identical(x, integer()))
  expect_equal(nash[[which(m1)]]$game[1,1], W[2,1] * nash[[which(m1_2)]]$winrate[1] + (1-W[2,1]) * nash[[which(m11)]]$winrate[1])

})

test_that("symmetric specialist", {
  W <- matrix(rep(0.5,9),3,3)
  expect_equivalent(as.matrix(spec_nash(W)$winrates), matrix(rep(0.5,6),2,3))
})



test_that("calibration specialist", {
  games <- replicate(5000, spec_nash(matrix(runif(9),3,3)), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrates[1,])))
  games_v2 <- colMeans(do.call(rbind,lapply(games, function(x) x$winrates[2,])))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$nash[1,])))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$nash[2,])))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_v2 > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

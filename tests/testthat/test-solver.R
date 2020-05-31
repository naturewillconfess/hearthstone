test_that("equal square game", {
  W <- matrix(rep(0.5,9),3,3)
  expect_equal(solve_game(W)$V, 0.5)
  expect_equal(solve_game(1-t(W))$V, 0.5)
  for (i in 1:10) {
    W <- matrix(rep(0.5,i^2),i,i)
    expect_equal(solve_game(W)$V, 0.5)
    expect_equal(solve_game(1-t(W))$V, 0.5)
  }
})


test_that("Owen example 1", {
  W <- matrix(c(3,5,1,6,2,4,1,4,3,4,2,5),3,4)
  G <- solve_game(W)
  expect_equal(G$V, 3.25)
  expect_equal(G$hero_sol, c(0.125,0.5,0.375))
  expect_equal(G$opp_sol, c(1/12,5/12,0.5,0))
})

test_that("Owen example 2", {
  W <- matrix(c(2,4,3,1,1,6,5,0),2,4)
  G <- solve_game(W)
  expect_equal(G$V, 17/7)
  expect_equal(G$hero_sol, c(5/7,2/7))
})

test_that("Owen example 3", {
  W <- matrix(c(0,-1,2,1,0,-3,-2,3,0),3,3)
  G <- solve_game(W)
  expect_equal(G$hero_sol, c(0.5,1/3,1/6))
  expect_equal(G$opp_sol, c(0.5,1/3,1/6))
})

test_that("domination 2x2", {
  W <- matrix(c(2,3,1,4),2,2)
  G <- solve_game(W)
  expect_equal(G$hero_sol, c(0,1))
  expect_equal(G$opp_sol, c(1,0))
  expect_equal(G$V, 3)
})

test_that("calibration solver", {
  games <- replicate(10000, solve_game(matrix(runif(9),3,3)),simplify = FALSE)
  games_v <- mean(sapply(games, function(x) x$V))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$hero_sol)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$opp_sol)))

  expect_true(games_v > 0.45 & games_v < 0.55)
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))

})

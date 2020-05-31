##############################################
test_that("symmetric shield nash", {
  W <- matrix(rep(0.5,9),3,3)
  n <- shield_nash(W, bans = 1, nash_fun = "conquest_nash", shield = 1)
  expect_equal(unname(n$winrate), c(0.5,0.5))
})

test_that("calibration shield nash", {
  games <- replicate(1000, shield_nash(matrix(runif(9),3,3), bans = 1, nash_fun = "conquest_nash", shield = 1), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

test_that("calibration shield nash 2", {
  games <- replicate(1000, shield_nash(matrix(runif(9),3,3), bans = 1, nash_fun = "conquest_nash", shield = 2), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$opp)))
  games_G <- colMeans(do.call(rbind,lapply(games, function(x) as.numeric(x$G))))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.26 & games_h < 0.4))
  expect_true(all(games_o > 0.26 & games_o < 0.4))
})

test_that("calibration shield nash 3", {
  games <- replicate(1000, shield_nash(matrix(runif(9),3,3), bans = 2, nash_fun = "conquest_nash", shield = 1), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})
#######################################################
test_that("symmetric shield nash short", {
  W <- matrix(rep(0.5,9),3,3)
  n <- shield_nash(W, bans = 1, nash_fun = "conquest_nash_short", shield = 1)
  expect_equal(unname(n$winrate), c(0.5,0.5))
})

test_that("calibration shield nash short", {
  games <- replicate(1000, shield_nash(matrix(runif(9),3,3), bans = 1, nash_fun = "conquest_nash_short", shield = 1), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})


test_that("calibration shield nash short 2", {
  games <- replicate(1000, {
    W <- matrix(runif(9),3,3)
    nash <- shield_nash(W, bans = 1, nash_fun = "conquest_nash_short", shield = 2)
    G <- W
    G[TRUE] <- NA
    G[1,1] <- solve_game(W[-3,-3])$V
    G[1,2] <- solve_game(W[-3,-2])$V
    G[1,3] <- solve_game(W[-3,-1])$V

    G[2,1] <- solve_game(W[-2,-3])$V
    G[2,2] <- solve_game(W[-2,-2])$V
    G[2,3] <- solve_game(W[-2,-1])$V

    G[3,1] <- solve_game(W[-1,-3])$V
    G[3,2] <- solve_game(W[-1,-2])$V
    G[3,3] <- solve_game(W[-1,-1])$V
    near <- function (x, y, tol = .Machine$double.eps^0.5) {
      abs(x - y) < tol
    }
    list(nash = nash, G_equal = all(near(nash$G, G)))
  }, simplify = FALSE)

  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$nash$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$nash$shields$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$nash$shields$opp)))
  games_Ge <- all(sapply(games, function(x) x$G_equal))


  expect_true(all(games_v > 0.45 & games_v < 0.55))
  #expect_true(all(games_h > 0.3 & games_h < 0.36))
  #expect_true(all(games_o > 0.3 & games_o < 0.36))
  expect_true(games_Ge)
  #not supposed to be true - LOTS of identical elements => a lot of multiple equilibria => solver that only takes the first equilibrium fails to mix equally
})
##############################################
test_that("symmetric shield nash LHS", {
  W <- matrix(rep(0.5,9),3,3)
  n <- shield_nash(W, bans = 1, nash_fun = "LHS_nash", shield = 1)
  expect_equal(unname(n$winrate), c(0.5,0.5))
})

test_that("calibration shield nash LHS", {
  games <- replicate(1000, shield_nash(matrix(runif(9),3,3), bans = 1, nash_fun = "LHS_nash", shield = 1), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

test_that("calibration shield nash 2 LHS", {
  games <- replicate(1000, shield_nash(matrix(runif(9),3,3), bans = 1, nash_fun = "LHS_nash", shield = 2), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$opp)))
  games_G <- colMeans(do.call(rbind,lapply(games, function(x) as.numeric(x$G))))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.26 & games_h < 0.4))
  expect_true(all(games_o > 0.26 & games_o < 0.4))
})

test_that("calibration shield nash 3  LHS", {
  games <- replicate(1000, shield_nash(matrix(runif(9),3,3), bans = 2, nash_fun = "LHS_nash", shield = 1), simplify = FALSE)
  games_v <- colMeans(do.call(rbind,lapply(games, function(x) x$winrate)))
  games_h <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$hero)))
  games_o <- colMeans(do.call(rbind,lapply(games, function(x) x$shields$opp)))

  expect_true(all(games_v > 0.45 & games_v < 0.55))
  expect_true(all(games_h > 0.3 & games_h < 0.36))
  expect_true(all(games_o > 0.3 & games_o < 0.36))
})

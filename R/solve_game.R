#' zero-sum game solver based on lpSolve
#'
#' Finds Nash equilibrium in mixed strategies and expected winrate for both players in a zero-sum game specified by a payoff matrix
#'
#' @import lpSolveAPI
#'
#' @param W Winrate matrix (from the perspective of the Hero)
#'
#' @return A list  with a solution
#'
#' @examples
#' solve_game(W = matrix(runif(9), 3, 3))
#' @export
#'
solve_game <- function(W) {
  m <- ncol(W)
  n <- nrow(W)+1
  const.mat = rbind(cbind(t(W), rep(-1, m)), c(rep(1, n-1), 0), cbind(diag(n-1), rep(0, n-1)))
  const.dir = c(rep(">=", m), "=", rep(">=", n-1))
  const.rhs = c(rep(0, m), 1, rep(0, n-1))

  lp <- make.lp(nrow = nrow(const.mat))
  lp.control(lp, sense = "max")
  for (i in 1:ncol(const.mat)) {
    add.column(lp, as.numeric(const.mat[,i]))
  }
  set.objfn(lp, obj=c(rep(0,n-1), 1))
  set.constr.type(lp, const.dir)
  set.rhs(lp, const.rhs)
  solve(lp)
  solution = get.variables(lp)
  WR <- list(
    hero_sol = solution[-n],
    opp_sol = get.dual.solution(lp)[2:(m+1)] * (-1),
    V = solution[n]
  )
  WR
}

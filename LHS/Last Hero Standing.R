library(Rfast)
library(lpSolve)
n <- 9
WM <- matrix(round(runif(n^2,0,1), 2),n,n)

dagame <- function(W) {
  n <- ncol(W)
  m <- nrow(W)
  WR <- lp(direction = "max",
           objective.in = c(rep(0,m),1),
           const.mat = rbind(cbind(t(W), rep(-1,n)), c(rep(1,m),0), cbind(diag(m), rep(0,m))),
           const.dir = c(rep(">=", n), "=", rep(">=",m)),
           const.rhs = c(rep(0,n),1,rep(0,m))
  )
  WR
}


combs <- unlist(lapply(0:n, function(x) comb_n(1:n, x, simplify = FALSE)), recursive = FALSE)

comblist <- lapply(combs, function(x) lapply(combs, list, x))
comblist <- unlist(comblist, recursive = FALSE)
comblist <- comblist[-length(comblist)]
ngames <- sapply(comblist, function(x) length(x[[1]])+length(x[[2]]))
comblist <- comblist[order(ngames, decreasing = TRUE)]



for (i in seq_along(comblist)) {
  lost <- comblist[[i]]
  lostlen <- c(length(lost[[1]]), length(lost[[2]]))
  if (lostlen[1] == n) {
    V <- 0
  } else if (lostlen[2] == n) {
    V <- 1
  } else if (lostlen[1] == n-1) {
    V <- prod(WM[!(1:n) %in% lost[[1]], !(1:n) %in% lost[[2]]])
  } else if (lostlen[2] == n-1) {
    V <- 1-prod(1-WM[!(1:n) %in% lost[[1]], !(1:n) %in% lost[[2]]])
  } else {
    #Players options
    optP <- (1:n)[!(1:n) %in% lost[[1]]]
    optO <- (1:n)[!(1:n) %in% lost[[2]]]
    G <- WM
    for (j in optP) {
      for (k in optO) {
        G[j,k] <- WM[j,k]*get(paste0("P", paste0(sort(c(lost[[1]], j)), collapse = ""), "O", paste0(sort(lost[[2]]), collapse = ""))) + (1-WM[j,k])*get(paste0("P", paste0(sort(lost[[1]]), collapse = ""), "O", paste0(sort(c(lost[[2]],k)), collapse = "")))
      }
    }
    G <- G[!(1:n) %in% lost[[1]], !(1:n) %in% lost[[2]]]
    solution <- dagame(G)$solution
    V <- solution[length(solution)]
    S <- solution
    comblist[[i]] <- c(comblist[[i]], list(S), list(G))
  }
  assign(paste0("P", paste0(sort(lost[[1]]), collapse = ""), "O", paste0(sort(lost[[2]]), collapse = "")), value = V)
  print(i)
}
WM
comblist[[63]]

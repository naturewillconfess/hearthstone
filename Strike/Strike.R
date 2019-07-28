remover <- function(W, banned) {
  if (!is.null(banned)) {
    for (i in seq_along(banned[,1])) {
      W[banned[i, 1], banned[i, 2]] <- NA
    }
  }
  W
}

#https://stats.stackexchange.com/questions/41247/risk-of-extinction-of-schr%C3%B6dingers-cats
#Thanks to Whuber & Stas Kolenikov @ CrossValidated
convolve.direct <- function(p) {
  n <- length(p) + 1
  z <- c(1, rep(0, n-1))
  for (i in seq_along(p)) {
    z <- (1-p[i])*z + p[i]*c(0, z[-n])
    }
  return(z)
}



strike_results <- function (W, Hero = 0, Opp = 0, NashBans = TRUE, nbans = 0, HeroBans = NULL, OppBans = NULL, played = NULL) {
  W1 <- W
  if (NashBans) {
    if (nbans == 0) {
      HeroBans <- NULL
      OppBans <- NULL
    } else if (2*nbans > (length(W)-1)) {
      stop("Too many bans")
    } else {
      HeroBans <- unname(which(W >= sort(W)[length(W)-nbans+1], arr.ind = T))[1:nbans,]
      OppBans <- unname(which(W <= sort(W)[nbans], arr.ind = T))[1:nbans,]
    }
  }
  
  W <- remover(W, HeroBans)
  W <- remover(W, OppBans)
  WV <- na.omit(as.numeric(W))
  if (length(WV) %/% 2 == 0) {stop("Even number of matchups after bans, please set an odd number")}
  
  W <- remover(W, played)
  WV <- na.omit(as.numeric(W))
  
  win_pdf <- convolve.direct(WV)
  req_win <- length(WV) %/% 2 + 1 - Hero
  win_prob <- sum(win_pdf[(req_win + 1):length(win_pdf)])
  win_prob <- c(Hero = win_prob, Opponent = 1-win_prob)
  
  results <- list(winrates = win_prob, 
                  matchups = W1, 
                  scores = c(Hero = Hero, Opponent = Opp),
                  hero_bans = HeroBans, 
                  opp_bans = OppBans,
                  played = played,
                  isnash = NashBans)
  results
}

strike_results(WM, 0, 0, T, 2)

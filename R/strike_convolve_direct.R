convolve_direct <- function(p) {
  n <- length(p) + 1
  z <- c(1, rep(0, n - 1))
  for (i in seq_along(p)) {
    z <- (1 - p[i]) * z + p[i] * c(0, z[-n])
  }
  return(z)
}
# https://stats.stackexchange.com/questions/41247/risk-of-extinction-of-schr%C3%B6dingers-cats
# Thanks to Whuber & Stas Kolenikov @ CrossValidated

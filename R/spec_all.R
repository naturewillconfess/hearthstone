spec_all <- function(Hero = 0, Opp = 0, bestof = 5, W) {
  if (bestof %% 2 == 0) stop("bestof has to be odd")
  if (Hero == 0 & Opp == 0) {
    m01 <- spec_nonzero(0, 1, bestof, W)
    m10 <- spec_nonzero(1, 0, bestof, W)
    mij_hero <- W[1, 1] * m10$hero + (1 - W[1, 1]) * m01$hero
    mij_opp <- W[1, 1] * m10$opp + (1 - W[1, 1]) * m01$opp
    mij = list(hero = mij_hero,
               opp = mij_opp)
  } else if (Hero + Opp > bestof | Hero > (bestof + 1) / 2 | Opp > (bestof + 1) / 2) {
    mij <- "Please, enter valid score/format"
  } else {
    mij <- spec_nonzero(Hero, Opp, bestof, W)
  }
  mij
}

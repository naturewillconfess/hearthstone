
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hearthstone: tools for competitive HearthstoneⓇ players

<!-- badges: start -->

[![R-CMD-check](https://github.com/naturewillconfess/hearthstone/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/naturewillconfess/hearthstone/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

hearthstone package is designed to help competitive Hearthstone players
to make optimal decisions when playing in tournaments. In particular, it
offers tools (R functions, vignettes) for finding subgame perfect Nash
equilibria in mixed strategies for Conquest and Last Hero Standing.

## Installation

You can install the latest version of hearthstone with:

``` r
install.packages("devtools")
devtools::install_github("naturewillconfess/hearthstone")
```

## What’s included in this pre-alpha version

- [Vignette](https://github.com/naturewillconfess/hearthstone/tree/master/vignettes)
  on Conquest. Check it out!
- R functions
  - `conquest_nash()` and for determining Nash equilibrial strategy and
    winrates in Conquest
  - `LHS_nash()` for determining Nash equilibrial strategy and winrates
    in LHS
  - `ban_nash()` for determining Nash equilibrial bans in Conquest ban
    phase

## News

Version 0.3.0 is here! I’ve removed everything related to deprecated
formats like Strike, Specialist and Conquest with Shields.

## Legal disclaimer

Hearthstone is a trademark or registered trademark of Blizzard
Entertainment, Inc., in the U.S. and/or other countries. I’m not
affiliated with Blizzard Entertainment, Inc. in any way.

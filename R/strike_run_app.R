#' Shiny app for strike format
#'
#' Shiny app for finding winrates in a Strike match
#'
#' @return shiny.appobj object
#'
#' @import shiny
#' @export
run_strike_app <- function() {
  shinyApp(ui = ui_strike, server = server_strike)
}

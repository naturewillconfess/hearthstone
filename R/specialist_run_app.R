#' Shiny app for specialist format
#'
#' Shiny app for finding Nash equilirium strategies
#'
#' @return shiny.appobj object
#'
#' @importFrom shiny shinyApp
#' @export
run_specialist_app <- function() {
  shinyApp(ui = ui_spec, server = server_spec)
}

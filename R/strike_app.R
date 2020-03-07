#' @import shiny
#' @importFrom shinyMatrix matrixInput

ui_strike <- fluidPage(
  titlePanel("Strike format: Winrate calculator", windowTitle = "Strike Calculator"),
  fluidRow(
    column(
      6,
      h3("Enter the format")
    ),
    column(6, h3("Optimal bans"))
  ),
  fluidRow(
    column(3, numericInput("classes", "Classes", 3, min = 1, max = 9, step = 1)),
    column(3, uiOutput("nbans")),
    column(3, tableOutput("Results"))
  ),
  fluidRow(
    column(6, h3("Enter Hero's winrates"), uiOutput("Winrates")),
    column(6, h3("Expected winrate"), tableOutput("ExpWinrate"))
  ),

  fluidRow(column(6, "See methodology and source code at", a("my Github", href = "https://github.com/naturewillconfess/hearthstone")))
)
server_strike <- function(input, output) {
  ##############
  ##############
  output$nbans <- renderUI({
    req(input$classes)
    numericInput("nbans", "Bans", 2, min = 0, max = floor((input$classes^2 - 1) / 2), step = 1)
  })

  output$Winrates <- renderUI({
    req(input$classes)
    matrixInput(
      inputId = "Winrates",
      value = matrix(round(rep(0.5, input$classes^2), 2), nrow = input$classes, ncol = input$classes, dimnames = list(paste0("Deck ", 1:input$classes), paste0("Deck ", 1:input$classes))),
      class = "numeric",
      cols = list(names = TRUE),
      rows = list(names = TRUE),
    )
  })
  nash <- reactive({
    req(input$Winrates, input$nbans)
    strike_nash(input$Winrates, nbans = input$nbans)
  })
  output$Results <- renderTable(
    {
      req(input$Winrates, input$nbans)
      bans <- rbind(nash()$hero_bans, nash()$opp_bans)
      result <- paste0("Deck ", as.character(bans[, 1]), " - Deck ", as.character(bans[, 2]))
      player <- c(rep("Hero", input$nbans), rep("Opponent", input$nbans))
      results <- data.frame(Player = player, Ban = result)
    },
    rownames = FALSE
  )

  output$ExpWinrate <- renderTable(
    {
      req(input$Winrates, input$nbans)
      nash()$winrates
    },
    rownames = TRUE,
    colnames = FALSE
  )
}

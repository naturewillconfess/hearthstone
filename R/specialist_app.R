#' @importFrom shiny fluidPage titlePanel fluidRow column h3 numericInput uiOutput tableOutput a renderUI req renderTable reactive
#' @importFrom shinyMatrix matrixInput

ui_spec <- fluidPage(
  titlePanel("Specialist format: Nash equilibrium calculator", windowTitle = "Specialist Calculator"),
  fluidRow(
    column(6, h3("Enter Hero's winrates"), matrixInput(
      inputId = "Winrates",
      value = matrix(round(runif(9, 0, 1), 2), nrow = 3, ncol = 3, dimnames = list(c("Hero, Pr.", "Hero, Sec.", "Hero, Ter."), c("Opp, Pr.", "Opp, Sec.", "Opp, Ter."))),
      class = "numeric",
      cols = list(names = TRUE),
      rows = list(names = TRUE)
    )),
    column(6, h3("Equilibrium in mixed strategies"), tableOutput("Results"))
  ),
  fluidRow(
    column(
      6,
      h3("Enter current score and format")
    ),
    column(6, h3("Expected Winrate"))
  ),

  fluidRow(
    column(2, numericInput("bestof", "Best-of-?", 3, min = 1, max = 151, step = 2)),
    column(2, uiOutput("Hero")),
    column(2, uiOutput("Opp")),
    column(6, tableOutput("ExpWinrate"))
  ),
  fluidRow(column(6, offset = , "See methodology and source code at", a("my Github", href = "https://github.com/naturewillconfess/hearthstoneR")))
)
server_spec <- function(input, output) {
  ##############
  output$Hero <- renderUI({
    numericInput("HeroScore", "Hero", 0, min = 0, max = (input$bestof + 1) / 2, step = 1)
  })
  output$Opp <- renderUI({
    numericInput("OppScore", "Opponent", 0, min = 0, max = (input$bestof + 1) / 2, step = 1)
  })
  nash <- reactive({
    req(input$HeroScore, input$OppScore)
    spec_nash(input$Winrates, input$HeroScore, input$OppScore, input$bestof)
  })
  output$Results <- renderTable(
    {
      req(input$HeroScore, input$OppScore)
      nash()$nash
    },
    rownames = TRUE
  )
  output$ExpWinrate <- renderTable(
    {
      req(input$HeroScore, input$OppScore)
      nash()$winrates
    },
    rownames = TRUE
  )
}

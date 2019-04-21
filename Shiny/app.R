library(shiny)
library(shinyMatrix)
library(lpSolve)
library(stringr)

ui <- fluidPage(
  
  titlePanel("Specialist format: Nash equilibrium calculator", windowTitle = "Specialist Calculator"),
  fluidRow(
    column(6, h3("Enter Hero's winrates"), matrixInput(
      inputId = "Winrates",
      value = matrix(round(runif(9,0,1), 2), nrow = 3,ncol = 3, dimnames = list(c("Hero, Pr.", "Hero, Sec.", "Hero, Ter."), c("Opp, Pr.", "Opp, Sec.", "Opp, Ter."))),
      class = "numeric",
      cols = list(names = TRUE),
      rows = list(names = TRUE)
    )),
    column(6,h3("Equilibrium in mixed strategies"), tableOutput("Results"))
  ),
  fluidRow(
    column(6,
           h3("Enter current score and format")),     
    column(6,h3("Expected Winrate"))
  ),
  
  fluidRow(
    column(2, numericInput("bestof", "Best-of-?", 3, min = 1, max = 151, step = 2)),
    column(2, uiOutput("Hero")),
    column(2, uiOutput("Opp")), 
    column(6, tableOutput("ExpWinrate"))),
  fluidRow(column(6, offset = , "See methodology and source code at", a("my Github", href = "https://github.com/naturewillconfess/Specialist_Nash")))
)
server <- function(input, output) {
  ##############
  dagame <- function(W) {
    n <- ncol(W)
    WR <- lp(direction = "max",
             objective.in = c(rep(0,n),1),
             const.mat = rbind(cbind(t(W), rep(-1,n)), c(rep(1,n),0), cbind(diag(n), rep(0,n))),
             const.dir = c(rep(">=", n), "=", rep(">=",n)),
             const.rhs = c(rep(0,n),1,rep(0,n))
    )
    WR
  }
  mijwr_no_0 <- function(i, j, m = 2, W) {
    if (j == m) {
      mij <- 0
    } else {
      p <- dagame(W)$solution[4]
      n <- (2*m-1-i-j)
      kmin <- m-i
      k <- kmin:n
      mij <- sum(factorial(n)/(factorial(k)*factorial(n-k))*p^k*(1-p)^(n-k))
      mij
    }
  }
  mijwr <- function(i = 0, j = 0, m = 2, W) {
    if (i == 0 & j == 0) {
      m01 <- mijwr_no_0(0, 1, m, W)
      m10 <- mijwr_no_0(1, 0, m, W)
      mij <- W[1,1]*m10 + (1-W[1,1])*m01
    } else if (i+j > 2*m-1) {
      mij <- "Please, enter valid score/format"
    } else {
      mij <- mijwr_no_0(i,j,m,W)
    }
    mij
  }
  
  zolution <- function(W) {
    dagame(W)$solution[1:3]
  }
  topercent <- function(x) {
    if (is.numeric(x)) {
      per <- paste0(as.character(round(x,digits = 4)*100), "%")
    } else {
      per <- x
    }
    per
  }
  ##############
  output$Hero <- renderUI( {
    numericInput("HeroScore", "Hero", 0, min = 0, max = (input$bestof + 1)/2, step = 1)  
    })
  output$Opp <- renderUI( {
    numericInput("OppScore", "Opponent", 0, min = 0, max = (input$bestof + 1)/2, step = 1)
  })
  output$Results <- renderTable({
    Winrates2 <- t(1-input$Winrates)
    resultsdf <- as.data.frame(matrix(topercent(zolution(input$Winrates)), nrow = 1,ncol = 3, dimnames = list("", c("Primary", "Secondary", "Tertiary"))))
    resultsdf2 <- as.data.frame(matrix(topercent(zolution(Winrates2)), nrow = 1,ncol = 3, dimnames = list("", c("Primary", "Secondary", "Tertiary"))))
    results <- rbind(resultsdf,resultsdf2)
    rownames(results) <- c("Hero", "Opponent")
    results
  }, rownames = TRUE)
  output$ExpWinrate <- renderTable({
    req(input$HeroScore, input$OppScore)
    mw <- topercent(mijwr(i = input$HeroScore, j = input$OppScore, m = (input$bestof + 1)/2, W = input$Winrates))
    fg <- topercent(unname(input$Winrates[1,1]))
    og <- topercent(dagame(input$Winrates)$solution[4])
    wrs <- c(mw, fg, og)
    winmatrix <- matrix(wrs, 1, 3, dimnames = list("", c("Match", "First game", "Subsequent games")))
    Winrates2 <- t(1-input$Winrates)
    mw <- topercent(mijwr(i = input$OppScore, j = input$HeroScore, m = (input$bestof + 1)/2, W = Winrates2))
    fg <- topercent(unname(Winrates2[1,1]))
    og <- topercent(dagame(Winrates2)$solution[4])
    wrs <- c(mw, fg, og)
    winmatrix2 <- matrix(wrs, 1, 3, dimnames = list("", c("Match", "First game", "Subsequent games")))
    winmatrix <- rbind(winmatrix, winmatrix2)
    rownames(winmatrix) <- c("Hero", "Opponent")
    winmatrix
  }, rownames = TRUE)
}

shinyApp(ui = ui, server = server)
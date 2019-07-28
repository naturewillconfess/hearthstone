library(shiny)
library(shinyMatrix)
library(lpSolve)

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
  fluidRow(column(6, offset = , "See methodology and source code at", a("my Github", href = "https://github.com/naturewillconfess/hearthstoneR")))
)
server <- function(input, output) {
  ##############
  dagame <- function(W, player = c("Hero", "Opponent")) {
    if (player == "Opponent") W <- t(1-W)
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
  
  mijwr_no_0 <- function(Hero, Opp, bestof = 5, W, player = c("Hero", "Opponent")) {
    ####
    #i - score of the hero (of the calculation, not the matrix!)
    #j - score of the opponent
    #m - score to get
    m <- (bestof + 1)/2
    if (player == "Hero") {
      i <- Hero
      j <- Opp
    } else {
      i <- Opp
      j <- Hero
    }
    if (j == m) {
      mij <- 0
    } else {
      p <- dagame(W, player)$solution[4]
      n <- (2*m-1-i-j)
      kmin <- m-i
      k <- kmin:n
      mij <- sum(factorial(n)/(factorial(k)*factorial(n-k))*p^k*(1-p)^(n-k))
      mij
    }
  }
  
  
  
  mijwr <- function(Hero = 0, Opp = 0, bestof = 5, W, player = c("Hero", "Opponent")) {
    if (Hero == 0 & Opp == 0) {
      m01 <- mijwr_no_0(0, 1, bestof, W, player)
      m10 <- mijwr_no_0(1, 0, bestof, W, player)
      mij <- W[1,1]*m10 + (1-W[1,1])*m01
    } else if (Hero + Opp > bestof | Hero > (bestof + 1)/2 | Opp > (bestof + 1)/2) {
      mij <- "Please, enter valid score/format"
    } else {
      mij <- mijwr_no_0(Hero, Opp, bestof, W, player)
    }
    mij
  }
  
  zolution <- function(W, player) {
    dagame(W, player = player)$solution[1:3]
  }
  topercent <- function(x) {
    if (is.numeric(x)) {
      per <- paste0(as.character(round(x,digits = 4)*100), "%")
    } else {
      per <- x
    }
    per
  }
  Nash <- function(W, Hero = 0, Opp = 0, bestof = 5) {
    resultsdf <- as.data.frame(matrix(topercent(zolution(W, "Hero")), nrow = 1,ncol = 3, dimnames = list("", c("Primary", "Secondary", "Tertiary"))))
    resultsdf2 <- as.data.frame(matrix(topercent(zolution(W, "Opponent")), nrow = 1,ncol = 3, dimnames = list("", c("Primary", "Secondary", "Tertiary"))))
    results <- rbind(resultsdf,resultsdf2)
    rownames(results) <- c("Hero", "Opponent")
    
    mw <- mijwr(Hero, Opp, bestof, W, "Hero")
    fg <- unname(W[1,1])
    og <- dagame(W, "Hero")$solution[4]
    winmatrix <- matrix(topercent(c(mw, fg, og)), 1, 3, dimnames = list("", c("Match", "First game", "Subsequent games")))
    
    mw <- mijwr(Hero, Opp, bestof, W, "Opponent")
    fg <- unname(t(1-W)[1,1])
    og <- dagame(W, "Opponent")$solution[4]
    winmatrix2 <- matrix(topercent(c(mw, fg, og)), 1, 3, dimnames = list("", c("Match", "First game", "Subsequent games")))
    winmatrix <- rbind(winmatrix, winmatrix2)
    rownames(winmatrix) <- c("Hero", "Opponent")
    
    nash <- list(Nash = results, winrates = winmatrix, matchups = W, scores = c(Hero = Hero, Opponent = Opp), format = paste0("Best of ", bestof))
    nash
  }
  ##############
  output$Hero <- renderUI( {
    numericInput("HeroScore", "Hero", 0, min = 0, max = (input$bestof + 1)/2, step = 1)  
    })
  output$Opp <- renderUI( {
    numericInput("OppScore", "Opponent", 0, min = 0, max = (input$bestof + 1)/2, step = 1)
  })
  output$Results <- renderTable({
    req(input$HeroScore, input$OppScore)
    nash <- Nash(input$Winrates, input$HeroScore, input$OppScore,input$bestof)
    nash$Nash
  }, rownames = TRUE)
  output$ExpWinrate <- renderTable({
    req(input$HeroScore, input$OppScore)
    nash <- Nash(input$Winrates, input$HeroScore, input$OppScore,input$bestof)
    nash$winrates
  }, rownames = TRUE)
}

shinyApp(ui = ui, server = server)
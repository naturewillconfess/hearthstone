library(shiny)
library(shinyMatrix)
library(lpSolve)

ui <- fluidPage(
  
  titlePanel("Strike format: Winrate calculator", windowTitle = "Strike Calculator"),
  fluidRow(
    column(6,
           h3("Enter the format")),
    column(6,h3("Optimal bans"))
    ),
  fluidRow(
    column(3, numericInput("classes", "Classes", 3, min = 1, max = 9, step = 1)),
    column(3, uiOutput("nbans")),
    column(6, tableOutput("Results"))
  ),
  fluidRow(
    column(6, h3("Enter Hero's winrates"), uiOutput("Winrates")),
    column(6, h3("Expected winrate"), tableOutput("ExpWinrate"))),
  
  fluidRow(column(6, "See methodology and source code at", a("my Github", href = "https://github.com/naturewillconfess/hearthstoneR")))
)
server <- function(input, output) {
  ##############
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
  
  topercent <- function(x) {
    if (is.numeric(x)) {
      per <- paste0(as.character(round(x,digits = 4)*100), "%")
    } else {
      per <- x
    }
    per
  }
  
  
  strike <- function (W, nbans = 0) {
    W1 <- W
    if (nbans == 0) {
      HeroBans <- NULL
      OppBans <- NULL
      Bans <- NULL
    } else {
      ##make sure they don't ban the same class
      HeroBans <- unname(which(W <= sort(W)[nbans], arr.ind = T))[1:nbans,,drop = FALSE]
      Heroz <- paste0("Deck ", as.vector(HeroBans[,1]), " - Deck ",  as.vector(HeroBans[,2])) 
      
      OppBans <- unname(which(W >= sort(W)[length(W)-nbans+1], arr.ind = T))[1:nbans,,drop = FALSE]
      Oppz <- paste0("Deck ", as.vector(OppBans[,1]), " - Deck ",  as.vector(OppBans[,2])) 
      Bans <- matrix(c(Heroz, Oppz), nbans, 2,byrow = F,dimnames = list(NULL, c("Hero", "Opponent")))
    }
    
    W <- remover(W, HeroBans)
    W <- remover(W, OppBans)
    WV <- na.omit(as.numeric(W))
    WV <- na.omit(as.numeric(W))
    
    win_pdf <- convolve.direct(WV)
    req_win <- length(WV) %/% 2 + 1
    win_prob <- sum(win_pdf[(req_win + 1):length(win_pdf)])
    draw_prob <- ifelse((length(WV)) %% 2 == 0, win_pdf[(req_win - 1)], 0)
    win_prob <- c(Hero = win_prob, Opponent = 1-win_prob-draw_prob, Draw = draw_prob)
    win_prob <- matrix(c(Hero = topercent(win_prob[1]), Opponent = topercent(win_prob[2]), Draw = topercent(win_prob[3])),3,1,dimnames = list(c("Hero","Opponent", "Draw"),NULL))
    results <- list(winrates = win_prob, 
                    matchups = W1, 
                    bans = Bans
                    )
    results
  }
  ##############
  output$nbans <- renderUI( {
    req(input$classes)
    numericInput("nbans", "Bans", 2, min = 0, max = floor((input$classes^2-1)/2), step = 1)
  })
  
  output$Winrates <- renderUI( {
    req(input$classes)
    matrixInput(
      inputId = "Winrates",
      value = matrix(round(runif(input$classes^2,0,1), 2), nrow = input$classes,ncol = input$classes, dimnames = list(paste0("Deck ",1:input$classes),paste0("Deck ",1:input$classes))),
      class = "numeric",
      cols = list(names = TRUE),
      rows = list(names = TRUE),
    )
  })
  
  output$Results <- renderTable({
    req(input$Winrates, input$nbans)
    nash <- strike(input$Winrates, input$nbans)
    nash$bans
  }, rownames = FALSE)
  
  output$ExpWinrate <- renderTable({
    req(input$Winrates, input$nbans)
    nash <- strike(input$Winrates, input$nbans)
    nash$winrates
  }, rownames = TRUE, colnames = FALSE)
}

shinyApp(ui = ui, server = server)
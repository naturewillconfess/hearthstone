library(shiny)
library(shinyMatrix)
library(lpSolve)

ui <- fluidPage(
  
  titlePanel("Strike format: Winrate calculator", windowTitle = "Strike Calculator"),
  fluidRow(
    column(6,
           h3("Enter the format"))),
  fluidRow(
    column(3, numericInput("classes", "Classes", 3, min = 1, max = 9, step = 1)),
    column(3, uiOutput("nbans"))
    ),
  fluidRow(
    column(6, h3("Enter Hero's winrates"), uiOutput("Winrates")),
    column(6,h3("Optimal bans"), tableOutput("Results"))
  ),
  fluidRow(
    column(6,
           h3("Enter current score and matchups played")),     
    column(6,h3("Expected Winrate"))
  ),
  
  fluidRow(
    column(2, uiOutput("Hero")),
    column(2, uiOutput("Opp")), 
    column(6, tableOutput("ExpWinrate"))),
  fluidRow(
    column(6, h4("Matchups played"))),
  fluidRow(
    column(6, uiOutput("Played"))),
  
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
  
  
  strike <- function (W, Hero = 0, Opp = 0, NashBans = TRUE, nbans = 0, HeroBans = NULL, OppBans = NULL, played = NULL) {
    W1 <- W
    if (NashBans) {
      if (nbans == 0) {
        HeroBans <- NULL
        OppBans <- NULL
      } else if (2*nbans > (length(W)-1)) {
        stop("Too many bans")
      } else {
        HeroBans <- unname(which(W <= sort(W)[nbans], arr.ind = T))[1:nbans,]
        Heroz <- paste0("Deck ", as.vector(HeroBans[,1]), " - Deck ",  as.vector(HeroBans[,2]))
        OppBans <- unname(which(W >= sort(W)[length(W)-nbans+1], arr.ind = T))[1:nbans,]
        Oppz <- paste0("Deck ", as.vector(OppBans[,1]), " - Deck ",  as.vector(OppBans[,2]))
        Bans <- matrix(c(Heroz, Oppz), nbans, 2,byrow = F,dimnames = list(NULL, c("Hero", "Opponent")))
        
      }
    }
    
    W <- remover(W, HeroBans)
    W <- remover(W, OppBans)
    WV <- na.omit(as.numeric(W))
    if (length(WV) %/% 2 == 0) {stop("Even number of matchups after bans, please set an odd number")}
    if (Hero + Opp > 0) W <- remover(W, played)
    WV <- na.omit(as.numeric(W))
    
    win_pdf <- convolve.direct(WV)
    req_win <- length(WV) %/% 2 + 1 - Hero
    win_prob <- sum(win_pdf[(req_win + 1):length(win_pdf)])
    win_prob <- c(Hero = win_prob, Opponent = 1-win_prob)
    win_prob <- matrix(c(Hero = topercent(win_prob), Opponent = topercent(1-win_prob)),2,1,dimnames = list(c("Hero","Opponent"),NULL))
    
    results <- list(winrates = win_prob, 
                    matchups = W1, 
                    scores = c(Hero = Hero, Opponent = Opp),
                    hero_bans = HeroBans, 
                    opp_bans = OppBans,
                    bans = Bans,
                    #played = played,
                    isnash = NashBans)
    results
  }
  ##############
  output$nbans <- renderUI( {
    req(input$classes)
    numericInput("nbans", "Bans", 2, min = 0, max = input$classes^2-1, step = 1)
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
  
  
  output$Hero <- renderUI( {
    req(input$classes,input$nbans)
    numericInput("HeroScore", "Hero", 0, min = 0, max = (input$classes^2-input$nbans*2 - 1)/2, step = 1)  
    })
  output$Opp <- renderUI( {
    req(input$classes,input$nbans)
    numericInput("OppScore", "Opponent", 0, min = 0, max = (input$classes^2-input$nbans*2 - 1)/2/2, step = 1)
  })
  
  output$Played <- renderUI( {
    req(input$HeroScore,input$OppScore)
    matrixInput(
      inputId = "Played",
      value = matrix(rep(0,(input$HeroScore+input$OppScore)*2), nrow = input$HeroScore+input$OppScore,ncol = 2),
      class = "numeric"
    )
  })
  
  output$Results <- renderTable({
    req(input$Winrates, input$nbans, input$HeroScore, input$OppScore)
    nash <- strike(input$Winrates, input$HeroScore, input$OppScore, T, input$nbans, played = input$Played)
    nash$Bans
  }, rownames = TRUE)
  
  output$ExpWinrate <- renderTable({
    req(input$Winrates, input$nbans, input$HeroScore, input$OppScore)
    nash <- strike(input$Winrates, input$HeroScore, input$OppScore, T, input$nbans, played = input$Played)
    nash$winrates
  }, rownames = TRUE, colnames = FALSE)
}

shinyApp(ui = ui, server = server)
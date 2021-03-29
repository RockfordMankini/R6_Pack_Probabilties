library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Rainbow Six Alpha Pack Simulator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("winRate",
                        "Player win rate:",
                        min = 0.01,
                        max = 1,
                        value = .5),
            numericInput("packs", label = h3("Amount of Packs"), value = 1000, min=1, max=100000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("gamePlot"),
           plotlyOutput("probPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    simulatePacks <- function(winRate, packs) {
        
        games <- rep(NA, packs)
        probs <- rep(NA, packs)
        
        for(i in 1:packs) {
            
            prob <- .02
            game <- 0
            loopOver <- FALSE
            
            while (!loopOver) {
                
                game <- game + 1
                
                gameResult <- sample(c(1:2), size = 1, prob = c(winRate, 1-winRate))
                
                if(gameResult == 1) {
                    prob <- prob + .02
                }
                
                else {
                    prob <- prob + .015
                    next 
                }
                
                if(prob >= 1) {
                    gameResult <- 1
                }
                
                else {
                    gameResult <- sample(c(1:2), size = 1, prob = c(prob, 1-prob))   
                }
                
                if(gameResult == 1) {
                    
                    loopOver <- TRUE
                    
                    games[i] <- game
                    probs[i] <- prob
                    
                }
                
            }
            
            
        }
        
        data.frame(Ob=1:packs, games=games, prob=probs)
        
    }
    
    getStats <- function(df) {
        
        mean_games <- mean(df$games)
        mean_prob <- mean(df$prob)
        median(df$games)
        median(df$prob)
        
        games_plot <- df %>%
            ggplot(aes(x=games)) + 
            geom_histogram(aes(y=..density..), fill="gold", color="black", binwidth = 2) + 
            geom_density() +
            theme_minimal() +
            xlab("Games Played") +
            ylab("Probability Density") +
            labs(title = paste("Alpha Packs (", nrow(df), " packs simulated)", sep="")) +
            geom_vline(xintercept = mean_games, color = "red")
        games_plot <- ggplotly(games_plot)
        games_plot
        
        length(unique(df$prob))
        probs_plot <- ggplot(df, aes(prob)) + 
            geom_histogram(aes(y=..density..), fill="gold", color="black", binwidth = .05) + 
            geom_density() +
            theme_minimal() +
            xlab("Probability At Success") +
            ylab("Probability Density") +
            labs(title = paste("Alpha Packs (", nrow(df), " packs simulated)", sep="")) +
            geom_vline(xintercept = mean_prob, color = "red")
        probs_plot <- ggplotly(probs_plot)
        probs_plot
        
        list(games_plot, probs_plot)
        
    }
    
    data <- reactive({
        input$winRate
        df <- simulatePacks(input$winRate, input$packs)
        getStats(df)
    })
    
    output$gamePlot <- renderPlotly({
        data()[[1]]
    })
    
    output$probPlot <- renderPlotly({
        data()[[2]]
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

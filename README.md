# optimiser-dash
# portfolio optimisation in Rshiny
# setwd("G:/GRPRESCH/Louis/R/R-projects/CAPM")
# https://shiny.rstudio.com/tutorial/
# https://recology.info/2012/12/shiny-r/

# Packages
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2) 
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(tidyverse)
library(rsconnect)

# UI  
ui <- fluidPage(
          mainPanel(
            tabPanel("Model",
                     sidebarPanel(
                       h3('Adjustable Constraints'),
                       sliderInput('londonWgt','Allocation to London', 
                                   min = 0, max = 100, value = 40, step = 5, post  = "%"),
                       sliderInput('otherWgt','Maximum allocation to other cities', 
                                   min = 0, max = 100, value = 5, step = 5, post  = "%"),
                       submitButton('Submit')),
                     mainPanel(h3('Results'), verbatimTextOutput("model")))))
           
# Server
server <- function(input, output) {

  # Load data
  # load(file = "returns_gbp.Rda")
  
  # Output
  output$model <- renderPrint({
    # data
    returns = read.csv("gbp_returns.csv", stringsAsFactors = FALSE, header = TRUE)
    format(returns$Year, format="%Y-%m-%d")
    rownames(returns) = returns[,1]
    returns = returns[,-1]
    returns = as.xts(returns)
    # Build Portfolio
    port = portfolio.spec(assets=colnames(returns)) 
    port = add.constraint(portfolio=port, type="full_investment")
    min = rep(0, ncol(returns))
    min[1] = input$londonWgt/100                 # min[1] = 0.4 
    max = rep(input$otherWgt/100, ncol(returns)) # 5% max other
    max[1] = 1
    port = add.constraint(portfolio=port, type="box", min=min, max=max, enabled=TRUE)
    maxret.port = add.objective(port, type='return', name='mean')
    maxret.opt = optimize.portfolio(returns, maxret.port, optimize_method="ROI", trace=TRUE)
    print(maxret.opt)
    }
  )
}

# Run App
shinyApp(ui = ui, server = server)

# Deploy
# deployApp()
# getwd()

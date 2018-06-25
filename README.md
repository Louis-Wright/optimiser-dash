# OPT. APP

library(shiny)
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2) 
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(rsconnect)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(ROI)
library(ggmap)
library(forcats)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Portfolio Optimisation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       h3('Adjustable Constraints'),
       sliderInput('londonWgt','Allocation to London', 
                   min = 0, max = 100, value = 40, step = 5, post  = "%"),
       sliderInput('otherWgt','Maximum allocation to other City', 
                   min = 0, max = 100, value = 5, step = 5, post  = "%"),
       submitButton('Submit')),
      
      # Table Plot
      mainPanel(
        tabsetPanel(
          tabPanel("Table", tableOutput("table")),
          tabPanel("Map", plotOutput("plot1")))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Optimisation data 
  t <- reactive({
     returns = read.csv("gbp_returns.csv", stringsAsFactors = FALSE, header = TRUE)
     format(returns$Year, format="%Y-%m-%d")
     rownames(returns) = returns[,1]
     returns = returns[,-1]
     returns = as.xts(returns)
     port = portfolio.spec(assets=colnames(returns)) 
     port = add.constraint(portfolio=port, type="full_investment")
     min = rep(0, ncol(returns))
     min[1] = input$londonWgt/100                 # min[1] = 0.4 
     max = rep(input$otherWgt/100, ncol(returns)) # max = rep(0.05, ncol(returns))
     max[1] = 1
     port = add.constraint(portfolio=port, type="box", min=min, max=max, enabled=TRUE)
     maxret.port = add.objective(port, type='return', name='mean')
     maxret.opt = optimize.portfolio(returns, maxret.port, optimize_method="ROI", trace=TRUE)
     t = maxret.opt$weights %>% as.data.frame()
     colnames(t) = "Weight"
     t$City = colnames(returns)
     t <- t[ which(t$Weight > 0.001), ]
     t$City = gsub("\\.", " ", t$City) 
     t$City = gsub("Washington D C ", "Washington DC", t$City)
     t$City = gsub("Minneapolis   St  Paul", "Minneapolis", t$City)
     t$City = gsub("Ottawa   Hull", "Ottawa", t$City)
     t
   })
   
   output$table <- renderTable({
     t <- t()
     t
   })
   
   output$plot1 <- renderPlot({
     # Clean table names
     t <- t()
     # Download geo codes & merge
     geo = read.csv("city_lat_lon.csv", stringsAsFactors = FALSE, header = TRUE)
     t2 = merge(t,geo, by = "City")
     t2$lon <- as.numeric(as.character(t2$lon))
     t2$lat <- as.numeric(as.character(t2$lat))
     t2$Weight = t2$Weight * 100
     colnames(t2)[2] <- "Allocation (%)"
     # Map
     map_world <- map_data("world")
     m = ggplot() +
       geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
       geom_point(data = t2 , aes(x = lon, y = lat, size = `Allocation (%)`), color = 'red') +
       theme_void()
     m
   })
}

# Run the application 
shinyApp(ui = ui, server = server)



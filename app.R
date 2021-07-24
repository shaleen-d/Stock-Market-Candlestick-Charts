#
# 
#
# Web Application to create Canldestick Charts for Stock Markets
#
#    
#

# Load packages ----
library(IDPmisc)     # For NaRV.omit()
library(jsonlite)
library(magrittr)    
library(plotly)
library(shiny)

# Include header files ----
source("query.R")    # Queries for daily and hourly data from Yahoo Finance

# User interface ----
ui <- fluidPage(
    titlePanel("Charts for Stock Markets"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Select a symbol to plot such as:", br(), 
            
            "GSPC for S&P500,", br(),
            "NSEI for NIfty 50,", br(),
            "IXIC for NASDAQ", br(),
            "DJI for Dow Jones Industrial Average,", br(), br(),

        "Information will be collected from Yahoo finance."),
            textInput("symb", "Symbol", "GSPC"),
            
           selectInput("select_time", "Time Frame",
                        choices = list("Daily" = "daily", "Hourly" = "hourly"
                        ), selected = "daily"),
            
            br(),
            br(),
            
        ),
        
        mainPanel(plotlyOutput("chart"))
    )
)

# Server logic
server <- function(input, output) {
    
    ## Fetching Data from Yahoo ----
    # Whether to get daily or hourly data based on time frame selected
    yahoo_query <- reactive({
        ifelse(input$select_time == "hourly", Query_hour(input$symb), 
               Query_day(input$symb))
    })
    
    # Get JSON from Yahoo
    data_json <- reactive({ fromJSON(yahoo_query()) })
    
    # Getting OHLC Dataframe from Yahoo JSON, time is in Unix Time Stamp
    OHLC_df <- reactive({
        # Parse Time
        json_df <- data.frame("Time" = data_json()$chart$result$timestamp)
        colnames(json_df)[1] <- "Time"
        json_df$Time <- as.POSIXct(json_df$Time, origin="1970-01-01")
        # Parse OHLC data
        json_df$Open <- unlist(data_json()$chart$result$indicators$quote[[1]]$open)
        json_df$High <- unlist(data_json()$chart$result$indicators$quote[[1]]$high)
        json_df$Low <- unlist(data_json()$chart$result$indicators$quote[[1]]$low)
        json_df$Close <- unlist(data_json()$chart$result$indicators$quote[[1]]$close)
        json_df
    })
    
    
    ## Output Candlestick chart for the chosen Time frame ----
    output$chart <- renderPlotly({
        fig <- OHLC_df() %>% plot_ly(x = ~Time, type="candlestick",
                                      open = ~Open, close = ~Close,
                                      high = ~High, low = ~Low) 
        fig %>% layout(title = paste("Candlestick Chart"))
    })
    
}

# Run the app
shinyApp(ui, server)

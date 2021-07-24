## Queries to Yahoo API ----

# Function to return query to Yahoo API for Hourly data for last 6 months ----
Query_hour <- function(symbol) paste0(
  "https://query1.finance.yahoo.com/v8/finance/chart/%5E", symbol, 
  "?symbol=%5E", symbol, 
  "&range=6mo&interval=60m&includePrePost=true&events=div%7Csplit%7Cearn&lang=en-INregion=IN&crumb=%2FZalRC5ItLf&corsDomain=in.finance.yahoo.com"
 )

# Function to return query to Yahoo API for Daily data for last 600 days ----
Query_day <- function(symbol) {
  query_daily_first <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/%5E", 
                              symbol, "?symbol=%5E", symbol, "&period1=")
  current_time <- as.integer(Sys.time())
  query_daily_third <- "&period2="
  # Around 640 days is the limit of single query of Yahoo Finance
  past_time <- current_time - 55506096
  query_daily_fifth <- "&interval=1d&includePrePost=true&events=div%7Csplit%7Cearn&lang=en-IN&region=IN&crumb=%2FZalRC5ItLf&corsDomain=in.finance.yahoo.com"
  paste0(query_daily_first, past_time, query_daily_third, current_time, query_daily_fifth)
}
# Created By Daniel Hadley Tue Apr 5 09:30:53 EDT 2016 #
setwd("/Users/DHadley/Github/My_Dashboard_Functions")

library(dplyr)
library(lubridate)

# Load Data
my_data <- read.csv("./Baltimore.csv")
my_data$CrimeDate <- as.Date(my_data$CrimeDate, "%m/%d/%Y")


# For all of these the date_var should be formatted as a month, day, year
# It goes into the function as the name wrapped in quotes, e.g., "CrimeDate"



## Adds new time variables to your data
add_date_vars <- function(my_data, date_var){
  
  ## Takes the date from data and adds time variables ##
  
  today <- Sys.Date()
  
  my_data$date <- as.Date(my_data[,date_var])
  my_data$year_month <- format(my_data$date, '%Y-%m')
  my_data$month <- format(my_data$date, '%m')
  my_data$year <- format(my_data$date, '%Y')
  my_data$days_ago <- difftime(my_data$date, today, units = "days")
  
  return(my_data)
}



## Daily time series
make_daily_ts <- function(my_data, date_var){
  
  ##  Turns the data into a complete time series ##
  ## Good for when you have data missing from a ts ##
  
  days <- my_data %>%
    group_by(date) %>% 
    summarise(n = n())
  
  first_day <- min(days$date)
  last_day <- max(days$date)
  
  all_days <- seq.Date(from=first_day, to = last_day, b='days')
  all_days <- all_days  %>%  as.data.frame() 
  colnames(all_days)[1] = "date"
  
  # After this we will have a time series df with every date and how many work orders
  ts = merge(days, all_days, by='date', all=TRUE)
  ts[is.na(ts)] <- 0
  
  return(ts)
  
}



## All below this require the daily ts function

## Weekly time series
make_weekly_ts <- function(my_data, date_var){
  
  ##  Turns the data into a complete time series ##
  ## Good for when you have data missing from a ts ##
  
  # First we get the daily
  daily_ts <- make_daily_ts(my_data, date_var)
  
  ## Now weekly
  # First we get the day number of the last day in our daily time series
  # Because otherwise it ends on Sunday
  # stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  week_ending_num <- as.numeric(max(daily_ts$date))
  
  # Ok now we can group by 7-day periods that end on the last day of the daily ts
  weekly_ts <- daily_ts %>% 
    mutate(week = (week_ending_num - as.numeric(date)) %/% 7) %>% 
    group_by(week) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-week)
  
  weekly_ts <- weekly_ts[-1,] # Drop first row because it's an incomplete week
  
  weekly_ts <- weekly_ts %>% 
    select(date_sum:date_max) %>% 
    select(-date_sum)
  
  # weekly_ts$date_max <- format(weekly_ts$date_max, format = "%b %d")
  
  names(weekly_ts) <- gsub("_sum", "", names(weekly_ts))
  names(weekly_ts)[2] <- "week_ending"
  
  return(weekly_ts)
  
}



## 30-day time series
# This is better than grouping by month for when you are 1/2 through a month
make_monthly_ts <- function(my_data, date_var){
  
  ##  Turns the data into a complete time series ##
  ## Good for when you have data missing from a ts ##
  ## And for when you are 1/2 through a month but want to compare to past ##
  
  # First we get the daily
  daily_ts <- make_daily_ts(my_data, date_var)
  
  ## Now weekly
  # First we get the day number of the last day in our daily time series
  # Because otherwise it ends on Sunday
  # stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  month_ending_num <- as.numeric(max(daily_ts$date))
  
  # Ok now we can group by 7-day periods that end on the last day of the daily ts
  monthly_ts <- daily_ts %>% 
    mutate(month = (month_ending_num - as.numeric(date)) %/% 30) %>% 
    group_by(month) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-month)
  
  monthly_ts <- monthly_ts[-1,] # Drop first row because it's an incomplete month
  
  monthly_ts <- monthly_ts %>% 
    select(date_sum:date_max) %>% 
    select(-date_sum)
  
  # monthly_ts$date_max <- format(monthly_ts$date_max, format = "%b %d")
  
  names(monthly_ts) <- gsub("_sum", "", names(monthly_ts))
  names(monthly_ts)[2] <- "month_ending"
  
  return(monthly_ts)
  
}



## 365-day time series
# This is better than grouping by month for when you are 1/2 through a month
make_yearly_ts <- function(my_data, date_var){
  
  ##  Turns the data into a complete time series ##
  ## Good for when you have data missing from a ts ##
  ## And for when you are 1/2 through a year but want to compare to past ##
  
  # First we get the daily
  daily_ts <- make_daily_ts(my_data, date_var)
  
  ## Now weekly
  # First we get the day number of the last day in our daily time series
  # Because otherwise it ends on Sunday
  # stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  year_ending_num <- as.numeric(max(daily_ts$date))
  
  # Ok now we can group by 7-day periods that end on the last day of the daily ts
  yearly_ts <- daily_ts %>% 
    mutate(year = (year_ending_num - as.numeric(date)) %/% 365) %>% 
    group_by(year) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-year)
  
  yearly_ts <- yearly_ts[-1,] # Drop first row because it's an incomplete year
  
  yearly_ts <- yearly_ts %>% 
    select(date_sum:date_max) %>% 
    select(-date_sum)
  
  # yearly_ts$date_max <- format(yearly_ts$date_max, format = "%b %d")
  
  names(yearly_ts) <- gsub("_sum", "", names(yearly_ts))
  names(yearly_ts)[2] <- "year_ending"
  
  return(yearly_ts)
  
}
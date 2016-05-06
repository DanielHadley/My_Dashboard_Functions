# Created By Daniel Hadley Tue Apr 5 09:30:53 EDT 2016 #
setwd("/Users/DHadley/Github/My_Dashboard_Functions")
setwd("c:/Users/dhadley/Documents/GitHub/My_Dashboard_Functions/")

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




## x-day time series
# Decide the time increments 7, 365, whatever
# group an summarize based on x
make_x_day_ts <- function(my_data, date_var, x_days){
  
  ## Turns the data into a complete time series ##
  ## The time series starts at the end, and groups by periods of X ##
  ## Good for when you have data missing from a ts ##
  ## And for when you want to compare current periods with the past ##
  
  # First we get the daily
  my_data$date <- as.Date(my_data[,date_var])
  
  days <- my_data %>%
    group_by(date) %>% 
    summarise(n = n())
  
  first_day <- min(days$date)
  last_day <- max(days$date)
  
  all_days <- seq.Date(from=first_day, to = last_day, b='days')
  all_days <- all_days  %>%  as.data.frame() 
  colnames(all_days)[1] = "date"
  
  # After this we will have a time series df with every date and how many of the variable
  daily_ts = merge(days, all_days, by='date', all=TRUE)
  daily_ts[is.na(daily_ts)] <- 0
  
  
  
  ## Now x-ly
  # First we get the day number of the last day in our daily time series
  # Because otherwise it ends on Sunday
  # stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  period_ending_num <- as.numeric(max(daily_ts$date))
  
  # Ok now we can group by x-day periods that end on the last day of the daily ts
  x_ts <- daily_ts %>% 
    mutate(period = (period_ending_num - as.numeric(date)) %/% x_days) %>% 
    group_by(period) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-period)
  
  x_ts <- x_ts[-1,] # Drop first row because it's likely an incomplete period
  
  # Clean up
  x_ts <- x_ts %>% 
    select(date_sum:date_max) %>% 
    select(-date_sum)
  
  names(x_ts) <- gsub("_sum", "", names(x_ts))
  names(x_ts)[2] <- "period_ending"
  
  return(x_ts)
  
}




### Time series statistical comparisons ###
comp_last_day_avg <- function(my_data, date_var){
  
  ## Compare last day to average apples to apples
  # Turns the data into a complete time series #
  # Filters by week day, or week end, depending on the last day
  # Then compares the last day to the rest to see if it is 
  # significantly above average or not
  
  # First we get the daily
  daily_ts <- make_x_day_ts(my_data, date_var, 1)
  
  # Now find the type
  daily_ts <- daily_ts %>% 
    mutate(day_type = ifelse(wday(date) == 1, "weekend",
                             ifelse(wday(date) == 7, "weekend",
                                    "weekday")))
  
  last_day_type <- daily_ts$day_type[which.max(daily_ts$date)]
  
  # Filter for that type
  daily_ts <- filter(daily_ts, day_type == last_day_type)
  
  # Now find the means and sd
  avg_n <- mean(daily_ts$n)
  stdev <- sd(daily_ts$n)
  
  last_day_n <- daily_ts$n[which.max(daily_ts$date)]
  delta <- last_day_n - avg_n
  
  comparison <- ifelse(delta > 0 & delta > stdev, "significantly above average",
                       ifelse(delta > 0 & delta < stdev, 
                              "slightly above average",
                              ifelse(delta < 0 & abs(delta) > stdev, 
                                     "significantly below average",
                                     ifelse(delta < 0 & abs(delta) < stdev, 
                                            "slightly below average",
                                            "average"))))
  
  return(paste(comparison, "for a", last_day_type))
  
}




 

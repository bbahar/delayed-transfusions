# Load libraries
library(tidyverse)
library(lubridate)
library(anomalize)

# Load data
example_data <- read_csv("example_data.csv")

# function
anomalies <- function(datafile, datetime, timetotransfuse, interval) {
  
  # when 'interval' is week, specify the reference day with week_start. 7 represents Sunday and 1 (default) represents Monday.
  # when 'interval' is day, no need to remove week_start.
  
  tresults <- datafile %>% 
    mutate(Date = floor_date({{datetime}}, unit = {{interval}}, week_start = 1)) %>%
    group_by(Date) %>%
    summarise(time = median({{timetotransfuse}})) %>% 
    arrange(Date) %>% 
    mutate(Date=as_date(Date))
  
  tresults %>%
    time_decompose(time, method='stl') %>%
    anomalize(remainder, method='gesd')
  

  # uncomment below to obtain figures
  
#  tresults %>%
#    arrange(Date) %>%
#    time_decompose(time, method='stl') %>%
#    anomalize(remainder, method='gesd') %>%
#    time_recompose() %>%
#    plot_anomalies(time_recomposed = TRUE) + geom_smooth(method='loess', se= FALSE)
  
}

# examples 
anomalies(example_data,time,hours, 'day')
anomalies(example_data,time,hours,'3 days')

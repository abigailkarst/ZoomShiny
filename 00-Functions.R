zoomVTTtoDF <- function(file) {
  require(tibble)
  require(dplyr)
  require(lubridate)
  
  raw_text <- readLines(file)
  
  ## Split each block into vectors
  id <- as.integer(raw_text[seq(3, length(raw_text), 4)])
  interval <- raw_text[seq(4, length(raw_text), 4)]
  text <- raw_text[seq(5, length(raw_text), 4)]
  
  ## Rejoin to a dataframe
  df <- tibble(id = id,
               text = text,
               interval = interval)
  
  ## Convert the time to start and end times
  df <- df %>% 
    mutate(start = gsub("(.*) --> (.*)", "\\1", interval),
           end = gsub("(.*) --> (.*)", "\\2", interval)) %>% 
    mutate(start_period = 
             hours(substr(start, 1, 2)) +
             minutes(substr(start, 4, 5)) +
             seconds(substr(start, 7, 8)) +
             milliseconds(as.numeric(substr(start, 10, 12)))
           ) %>% 
    mutate(end_period = 
             hours(substr(end, 1, 2)) +
             minutes(substr(end, 4, 5)) +
             seconds(substr(end, 7, 8)) +
             milliseconds(as.numeric(substr(end, 10, 12)))
    ) %>% 
    mutate(duration = end_period - start_period,
           start_seconds = period_to_seconds(start_period),
           end_seconds = period_to_seconds(end_period),
           duration_seconds = period_to_seconds(duration))
  
  ## Parse the speaker from the text
  df <- df %>% 
    mutate(speaker = gsub("(^.*): (.*)$", "\\1", text),
           text = gsub("(^.*): (.*)$", "\\2", text)) %>% 
    relocate(speaker, .after = id)

  return(df)
  
}

TotalSpokenPlot <- function(dataframe){
  library(tidyverse)
  library(ggplot2)
  
  plotdata <- zoomVTTtoDF(file = dataframe) %>%
    group_by(speaker) %>% 
    transmute(Total = sum(duration_seconds)) %>%
    distinct()
  
  totalplot <- ggplot(plotdata, aes(x = speaker, y = Total)) + 
    ggtitle("Total Time Spoken") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Set1") +
    theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust = 0.5))
  
  return(totalplot)
}


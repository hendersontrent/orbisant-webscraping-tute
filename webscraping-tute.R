#----------------------------------------------------
# This script sets out to build a basic
# webscraper for use in the tutorial on
# the Orbisant Analytics website
#
# NOTE: https://www.orbisantanalytics.com/webscraping
#----------------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 10 February 2020
#------------------------------------------

# Load packages

library(tidyverse)
library(rvest)
library(XML)
library(stringr)
library(data.table)

# Define a vector of city names
# This lets us write a paste0 function to loop through
# them all in the overall webscraping function


city_names <- c("Sydney", "Melbourne", "Brisbane", "Perth", "Canberra", 
                "Adelaide", "Hobart", "Darwin")

# Define an empty list to store iterative webscraped data in

empty.list <- list()

# Writing webscraper using city_names as the loop

for(i in city_names) {
  
  url <- paste0("https://www.numbeo.com/cost-of-living/in/", i, "?displayCurrency=AUD")
  
  print(url)
  
  try({
    
    link  <- read_html(url)
    
    the_data <- html_table(link) [[3]] %>%
      setNames (c('item', 'cost', 'range')) %>%
      filter (range != '') %>%
      mutate(city = i)
    
    empty.list[[i]] <- the_data
    
  })
  
}

col_data <- rbindlist(empty.list, use.names = TRUE)

#----------------------
# Cleaning scraped data
#----------------------

# Removing A$ from Cost and making it numeric for future analysis

col_data_clean <- col_data %>%
  mutate(cost = gsub("\\s.*", "", cost)) %>%
  mutate(cost = gsub(",", "", cost)) %>%
  mutate(cost = as.numeric(cost))

# Parsing range values to new columns and making them numeric

col_data_clean <- col_data_clean %>%
  mutate(min_range = str_extract(range, ".*-")) %>%
  mutate(min_range = gsub("-", "", min_range)) %>%
  mutate(min_range = gsub(",", "", min_range)) %>%
  mutate(max_range = str_extract(range, "-.*")) %>%
  mutate(max_range = gsub("-", "", max_range)) %>%
  mutate(max_range = gsub(",", "", max_range)) %>%
  mutate(min_range = as.numeric(min_range)) %>%
  mutate(max_range = as.numeric(max_range)) %>%
  dplyr::select(-c(range))

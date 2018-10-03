
library('shiny')
library('ggplot2')
library('DT')
library('dplyr')
library('scales')
library('reshape2')

  
  
  data <- read.csv("../../data/example.csv")
  data$variable <- as.Date(data$variable)
  
  data_map <- read.csv("../../data/map_data.csv")
  coordinates_region <- read.csv("coordinates_region.csv")
  
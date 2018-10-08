
library('shiny')
library('ggplot2')
library('DT')
library('dplyr')
library('scales')
library('reshape2')
library('data.table')

print('Loading Data')
  
  coordinates_region <- read.csv("../../data/coordinates_region.csv")
  de_availability_map <- read.csv2("../../data/IHP_de_availabilty_map.csv", dec = ".")
  de_availability_timeline <- read.csv2("../../data/IHP_de_availability_timeline.csv",  dec = ".")
  de_availability_table <- read.csv2("../../data/IHP_de_availability_table.csv",  dec = ".")
  de_availability_map$value <- round(as.numeric(de_availability_map$value),2)
  de_availability_timeline$value <- round(as.numeric(de_availability_timeline$value),2)
  de_availability_table$value <- round(as.numeric(de_availability_table$value),2)

print('Launching App')


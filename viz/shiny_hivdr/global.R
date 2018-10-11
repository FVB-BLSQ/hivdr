
library('shiny')
library('ggplot2')
library('DT')
library('dplyr')
library('scales')
library('reshape2')
library('data.table')

print('Loading Data')
  
  coordinates_region <- read.csv("../../data/coordinates_region.csv")
  coordinates_region$id <- as.character(coordinates_region$id)
  
  # df1 : number of patients (combined and per source)
  df1 <- read.csv("../../data/hivdr_patients.csv", dec = ".")
  df1 <-as.data.frame(df1)
  df1$value <- as.numeric(df1$value)
  df1$level_2_id <- as.character(df1$level_2_id)
  df1$X <- NULL
  
  df1_combine <- df1 %>% filter(source == "combine")
  df1_cordaid <- df1 %>% filter(source == "cordaid")
  df1_pnls <- df1 %>% filter(source == "pnls")
  
  df1_all <- merge(df1_pnls, df1_cordaid, by = c("level_2_id", "level_2_name", "level_3_id", "level_3_name", "periods"), all = T)
  df1_all <- merge(df1_all, df1_combine, by = c("level_2_id", "level_2_name", "level_3_id", "level_3_name", "periods"), all = T)
  df1_all <- df1_all %>% select(level_2_name, level_3_name, periods, value.x, value.y, value) %>% rename(pnls = "value.x", cordaid = "value.y", combine = "value")
  
  df1_region <- df1 %>% group_by(level_2_id, level_2_name, source, periods) %>% summarize(value = sum(value))
  
  df1_region_combine <- df1_region %>% filter(source == "combine")
  df1_region_cordaid <- df1_region %>% filter(source == "cordaid")
  df1_region_pnls <- df1_region  %>% filter(source == "pnls")
  
  df1_region_all <- merge(df1_region_pnls, df1_region_cordaid, by = c("level_2_id", "level_2_name", "periods"), all = T)
  df1_region_all <- merge(df1_region_all, df1_region_combine, by = c("level_2_id", "level_2_name", "periods"), all = T)
  df1_region_all <- df1_region_all %>% select(level_2_name, periods, value.x, value.y, value) %>% rename(pnls = "value.x", cordaid = "value.y", combine = "value")

  
  # df2 : number of patients per line or treatment (combined)
  df2 <- read.csv("../../data/hivdr_lines.csv", dec = ".")
  df2$value <- as.numeric(df2$value)
  df2$X <- NULL
  
  # df3 : number of HIV facilities (combined)
  df3 <- read.csv("../../data/hivdr_facilities.csv", dec = ".")
  df3$value <- as.numeric(df3$value)
  df3$X <- NULL
  
  # df4 : number of HIV facilities (combined)
  df4 <- read.csv("../../data/hivdr_facilities_stockout.csv", dec = ".")
  colnames(df4)[colnames(df4)=="fac_rupture"] <- "value"
  df4$value <- as.numeric(df4$value)
  df4$X <- NULL
  
print('Launching App')


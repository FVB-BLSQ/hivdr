library('shiny')
library('ggplot2')
library('DT')
library('dplyr')
library('scales')
library('reshape2')
library('data.table')
library('scales')

print('Loading Data')

coordinates_region <- read.csv("../../data/coordinates_region.csv")
coordinates_region$id <- as.character(coordinates_region$id)

# df1 : number of patients (combined and per source)
df1 <- read.csv("../../data/hivdr_patients.csv", dec = ".")
df1 <-as.data.frame(df1)
df1$value <- as.numeric(df1$value)
df1$level_2_id <- as.character(df1$level_2_id)
df1$X <- NULL
df1 <- df1 %>% filter(grepl("2017", periods))

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
df2$X <- NULL
df2$value <- as.numeric(df2$value)
df2$value[is.na(df2$value)] <- 0
df2$level_2_id <- as.character(df2$level_2_id)
df2 <- df2 %>% filter(grepl("2017", periods))

df2_region <- df2 %>% group_by(level_2_id, level_2_name, periods, line) %>% summarize(value = sum(value))
df2_region_all <- df2_region %>% group_by(level_2_id, level_2_name, periods) %>% summarize(all = sum(value))
df2_region <- merge(df2_region, df2_region_all, by = c("level_2_id", "level_2_name", "periods"), all.x = T)
df2_region <- df2_region %>% arrange(periods)

df2_all <- df2 %>% group_by(level_2_id, level_2_name, level_3_id, level_3_name, periods) %>% summarize(all = sum(value))
df2 <- merge(df2, df2_all, by = c("level_2_id", "level_2_name", "level_3_id", "level_3_name", "periods"), all.x = T)

# df3 : number of HIV facilities (combined)
df3 <- read.csv("../../data/hivdr_facilities.csv", dec = ".")
df3 <-as.data.frame(df3)
df3$value <- as.numeric(df3$value)
df3$level_2_id <- as.character(df3$level_2_id)
df3$X <- NULL

df3_region <- df3 %>% group_by(level_2_id, level_2_name) %>% summarize(value = sum(value))

# df4 : number of HIV facilities (combined)
df4 <- read.csv("../../data/hivdr_facilities_stockout.csv", dec = ".")
df4 <-as.data.frame(df4)
df4 <- df4 %>% rename("level_2_id" = uidlevel2, "level_2_name" = namelevel2, "periods" = period, "value" = fac_rupture)
df4$value <- as.numeric(df4$value)
df4$level_2_id <- as.character(df4$level_2_id)
df4$X <- NULL
df4 <- df4 %>% filter(grepl("2017", periods))


df4_region <- df4 %>% group_by(level_2_id, level_2_name, periods) %>% summarize(value = sum(value))
df4_region <- merge(df4_region, df3_region, by = c("level_2_id", "level_2_name"), all.x = T) %>% 
  rename(value = value.x, all = value.y)
df4_region$percentage <- round(df4_region$value / df4_region$all,2)
df4_region <- df4_region %>% arrange(periods)


blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

group.colors <- c("ABC + 3TC + EFV" = "#B9F2F0", "ABC + 3TC + LPV/r" = "#FFB482", "ABC + 3TC + NVP" = "#8DE5A1", "AZT + 3TC + LPV/r" = "#FF9F9B", "AZT+3TC+ EFV" = "#D0BBFF",
                  "AZT+3TC+NVP" = "#DEBB9B", "TDF + 3TC + LPV/r" = "#FAB0E4", "TDF + FTC + NVP" = "#CFCFCF", "TDF+ FTC + EFV" = "#FFFEA3", "TDF+3TC+EFV" = "#A1C9F4", "TDF+3TC+NVP" = "#3333F0", "Autres (à préciser)" = "#BB5B58")

# df5 : Drug consumption
df5 <- read.csv("../../data/hivdr_conso.csv", dec = ".")
df5 <- df5[df5$period < 201801,]
df5$period <- as.Date(paste0(substr(df5$period,1,4), '-', 
                             substr(df5$period, 5,6), 
                             '-01'))
df5_region <- df5 %>% group_by(uidlevel2, namelevel2, stand_name, period) %>% summarize(value = sum(boxes))

print('Launching App')


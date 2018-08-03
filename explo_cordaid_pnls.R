data_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/sources/'
metadata_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/metadata/'

### Load Data

cordaid <- readRDS(paste0(data_dir, 'Cordaid_TRAITEMENTS.rds'))
pnls <- readRDS(paste0(data_dir, 'PNLS.rds'))
pnls <- subset(pnls, orgUnit %in% cordaid$orgUnit)

View(cordaid)
View(pnls)
View(M_org_units)
View(M_data_sets)
View(pnls)

load_metadata <- function(metadata_dir, suffix = ''){
  env <- globalenv()
  assign(paste0('M_category_combos', suffix) , readRDS(paste0(metadata_dir, 'CC_metadata.rds')), env)
  assign(paste0('M_data_element_group', suffix) , readRDS(paste0(metadata_dir, 'DEG_metadata.rds')), env)
  assign(paste0('M_data_sets', suffix) , readRDS(paste0(metadata_dir, 'DS_metadata.rds')), env)
  assign(paste0('M_org_units', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata_DSinfo.rds')), env)
  assign(paste0('M_hierarchy', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata_flat.rds')), env)
  assign(paste0('M_org_units', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata.rds')), env)
}

load_metadata(metadata_dir)

### Find Data Elements of interest

M_data_sets[M_data_sets$DE_id %in% cordaid$dataElement,c('DE_id', 'DE_name')]
M_data_sets[M_data_sets$DE_id %in% pnls$dataElement,c('DE_id', 'DE_name')]


summary(cordaid$value[cordaid$dataElement == 'Yj8caUQs178'])
summary(pnls$value[pnls$dataElement == 'Dd2G5zI0o0a'])

length(unique(cordaid$orgUnit))
length(unique(pnls$orgUnit))


table(M_org_units$parent.parent.parent.name[M_org_units$id %in% unique(cordaid$orgUnit)])
table(M_org_units$parent.parent.parent.name[M_org_units$id %in% unique(pnls$orgUnit)])


M_org_units$parent.parent.parent.name[M_org_units$id %in% unique(cordaid$orgUnit)]
M_org_units$parent.parent.parent.name[M_org_units$id %in% unique(pnls$orgUnit)]


cordaid = merge(cordaid, M_org_units, by.x = 'orgUnit', by.y='id', all.y = FALSE)
pnls = merge(pnls, M_org_units, by.x = 'orgUnit', by.y='id', all.y = FALSE)


library(ggplot2)
library(zoo)
library(dplyr)
library(data.table)

cordaid$period <- as.character(cordaid$period)
pnls$period <- as.character(pnls$period)

cordaid$month <- as.yearmon(paste0(substr(cordaid$period, 1, 4), '-', substr(cordaid$period, 5,6)))
pnls$month <- as.yearmon(paste0(substr(pnls$period, 1, 4), '-', substr(pnls$period, 5,6)))

cat_comb_ancien <- M_category_combos$CatComboOpt_id[M_category_combos$CatOpt_id.1 == 'vZ6Os4BJvum']


cordaid_sous_traitement <- data.table(subset(cordaid, dataElement == 'Yj8caUQs178' & categoryOptionCombo %in% cat_comb_ancien))
pnls_sous_traitement <- data.table(subset(pnls, dataElement == 'Dd2G5zI0o0a' & categoryOptionCombo %in% cat_comb_ancien))



cordaid_sous_traitement <- cordaid_sous_traitement[, .(n_patients = sum(value)),
                                                   by=c('orgUnit',"month","parent.parent.parent.name", "parent.parent.parent.id", "parent.parent.name", "parent.parent.id")]

pnls_sous_traitement <- pnls_sous_traitement[, .(n_patients = sum(value)),
                                                   by=c('orgUnit',"month","parent.parent.parent.name", "parent.parent.parent.id", "parent.parent.name", "parent.parent.id")]

ggplot(data = cordaid_sous_traitement) +
  geom_line(aes(x=month, y=n_patients, col= orgUnit)) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free_y')


ggplot(data = pnls_sous_traitement) +
  geom_line(aes(x=month, y=n_patients_p, col= orgUnit)) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free_y')

compare <- merge(pnls_sous_traitement, cordaid_sous_traitement,all=T)
View(compare)

ggplot(data = compare) +
  geom_point(aes(x=n_patients, y=n_patients_p, col= orgUnit)) +
  geom_abline(intercept = 0) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free') +
  xlab('Total patients declared') + ylab('Total patients by treatment line')


compare$ratio = compare$n_patients_p / compare$n_patients
compare$div = abs(compare$n_patients - compare$n_patients_p) / compare$n_patients
compare$alpha = 1 - compare$div
compare$alpha[compare$alpha < 0] <- 0


compare$zscore = ((compare$n_patients_p / compare$n_patients) - 1) / sd(compare$n_patients_p / compare$n_patients)
compare$zscore[compare$zscore > 5] <- 5
ggplot(data = compare) +
  geom_point(aes(x= n_patients , y = zscore , col = orgUnit)) +
  guides(col=FALSE) 



ggplot(data = compare) +
  geom_point(aes(x=month, y=n_patients_p, col= orgUnit, alpha = alpha)) +
  geom_line(aes(x=month, y=n_patients_p, col= orgUnit, alpha = .2)) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free_y')


## Making rolling average
compare$month <- as.Date(compare$month)

make_moving_average <- function(data){
  dat <- subset(data, select= c(n_patients_p,month))
  dat <- zoo(dat$n_patients_p, dat$month)
  rollmean(dat, 3, fill = NA, align = "right")
}

make_serie <- function(data1, data2){
  values <- c()
  source <- c()
  expecteds <- c()
  periods <- sort(unique(c(data1$month, data2$month)))
  for(i in seq(1, length(periods))){
    ## Extract useful values
    period_i <- periods[i]
    value1 <- data1$value[data1$month == period_i]
    value2 <- data2$value[data2$month == period_i]
    if(length(values) >= 3){expected <- mean(values[(length(values)-2):length(values)], na.rm=TRUE)}
    if(length(values) < 3){expected <- mean(c(value1, value2, na.rm=TRUE))}
    ##taking into account zeros
    if(i==1){
      window_months <- periods[c(min(i+1, length(periods)), min(i+2, length(periods)))]
      if((!is.na(value1)) & (value1 == 0) & (min(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- NA
      } 
      if((!is.na(value2)) & (value2 == 0) & (min(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
        value2 <- NA
      }
    }
    if(i==2){
      window_months <- periods[c(1,min(i+1, length(periods)), min(i+2, length(periods)))]
      if((!is.na(value1)) & (value1 == 0) & (min(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- NA
      } 
      if((!is.na(value2)) & (value2 == 0) & (min(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
        value2 <- NA
      } 
      
    }
    if(i >= 3){
      window_months <- periods[c(i-2,i-1, min(i+1, length(periods)), min(i+2, length(periods)))]
      if((!is.na(value1)) & (value1 == 0) & (min(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- NA
      }
      if((!is.na(value2)) & (value2 == 0) & (min(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
        value2 <- NA
      }
    }
    
    ## Compute Expectations
    if(length(values) < 3){
      expected <- mean(c(value1, value2, na.rm=TRUE))
    }
    if(length(values) >= 3){
      expected <- mean(values[(length(values)-2):length(values)], na.rm=TRUE)
    }
    print(paste('value 1', value1))
    print(paste('value 2', value2))
    print(paste('expectation', expected))
    
    expecteds <- c(expecteds, expected)
    
    ## Make tests
    ### Test 1: value1 is missinng
    test_1 <- is.na(value1)
    ### Test 2: value2 is missing
    test_2 <- is.na(value2)
    ### Test 3: value 1 and 2 are very different
    to_check <- (abs(value1-value2)/(max(value1,value2, na.rm=TRUE) + 0.0001))
    test_3 <- is.finite(to_check) & (to_check < .1)
    ### Test 4: value 1 is very different from expectation
    test_4 <- (abs(value1-expected)/(max(value1,expected,na.rm = TRUE) + 0.0001)) < .1
    ### Test 5 : value 2 is very different from expectation
    test_5 <- (abs(value2-expected)/(max(value2,expected,na.rm = TRUE) + 0.0001)) < .1
    
    
    ## Assign values
    print('Doing some Tests')
    
    print('Check 1')
    if(test_1 & !test_2){
      values <- c(values, value2)
      source <- c(source, paste0(unique(data2$source), ' - only value' ))
    }
    print('Check 2')
    if(!test_1 & test_2){
      values <- c(values, value1)
      source <- c(source, paste0(unique(data1$source), ' - only value' ))
    }
    print('Check 3')
    if(test_1 & test_2){
      values <- c(values, expected)
      source <- c(source, 'expectation - no value')
    }
    print('Check 4')
    if(!test_1 & !test_2 & test_3){
      values <- c(values, value1)
      source <- c(source, paste0(unique(data1$source), ' - data in accordance'))
    }
    print('Check 5')
    if(!test_1 & !test_2 & !test_3 & test_4){
      values <- c(values, value1)
      source <- c(source, paste0(unique(data1$source), ' - ', unique(data1$source), ' seems strange'))
    }
    print('Check 6')
    if(!test_1 & !test_2 & !test_3 & !test_4 & test_5){
      values <- c(values, value2)
      source <- c(source, paste0(unique(data2$source), ' - ', unique(data1$source), ' seems strange'))
    }
    print('Check 7')
    if(!test_1 & !test_2 & !test_3 & !test_4 & !test_5){
      values <- c(values, expected)
      source <- c(source, paste0('expectation - data is strange'))
    }
  }
  print(periods)
  print(values)
  print(source)
  print(expecteds)
  out <- data.frame('periods'=periods,'values'=values, 'source'=source, 'expected' = expecteds , 
                    value_1 = data1$value[data1$month == periods], 
                    value_2 = data2$value[data2$month == periods])
  
  
  return(out)
}

## pb 1: 0 still taken in the sliding averages
## pb 2: 0 not managed in beginning of periods

## Standardizing data sources and making unique data frame
serie_data_1 <- subset(compare , select=c('month', 'orgUnit','n_patients', 'parent.parent.parent.name'))
serie_data_2 <- subset(compare , select=c('month', 'orgUnit','n_patients_p', 'parent.parent.parent.name'))
colnames(serie_data_1) <- colnames(serie_data_2) <- c('month', 'orgUnit', 'value', 'province')
serie_data_1$source <- 'total_serie1'
serie_data_2$source <-'total_serie2'

full_data <- rbind(serie_data_1, serie_data_2)

serying <- function(data){
  data1 <- data[data$source == 'total_serie1',]
  data2 <- data[data$source == 'total_serie2',]
  print('making a serie')
  out <- make_serie(data1, data2)
  return(out)
}

## Building the full series
completed_data <- full_data %>% group_by(.,orgUnit) %>% do(serying(.))

## Making a plotable df
to_plot <- merge(completed_data, M_hierarchy, by.x = 'orgUnit' , by.y = 'id', all.y = FALSE)

## Plotting

cols <- c("Patients source 1"="#e31a1c","Patients source 2"="#1f78b4","Expectation"="#33a02c", "Final Values"="#000000")

## A sample for troubleshooting
sample <- sample(unique(to_plot$orgUnit),size = 16)
ggplot(to_plot[to_plot$orgUnit %in% sample, ])+
  geom_point(aes(x=periods, y=value_1, colour= 'Patients source 1') , alpha=.5) +
  geom_line(aes(x=periods, y=value_1, colour= 'Patients source 1'), alpha=.5)+    
  geom_point(aes(x=periods, y=value_2, colour= 'Patients source 2'), alpha=.5) +
  geom_line(aes(x=periods, y=value_2, colour= 'Patients source 2'), alpha=.5)+
  geom_point(aes(x=periods, y=expected, colour= 'Expectation'), alpha=.5) +
  geom_line(aes(x=periods, y=expected, colour= 'Expectation'), alpha=.5)+
  geom_point(aes(x=periods, y=values, colour="Final Values")) +
  geom_line(aes(x=periods, y = values, colour="Final Values")) +
  scale_colour_manual(name="Source", values=cols) +
  guides(alpha = FALSE)+
  facet_wrap(~name, scales = 'free_y') +
  #ylim(0,max(dat_plot[,c('value_1','value_2','expected','values')])) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
  ylab("N Patients") + xlab("Year 2017")

to_plot$source <- factor(to_plot$source,levels = c('total_serie1', 'total_serie2','estimation'), ordered = TRUE)
ggplot(to_plot)+
  geom_point(aes(x=periods, y=values, colour= orgUnit, shape=source), size = 1.2) +
  geom_line(aes(x=periods, y=values, col= orgUnit), alpha = .2) +
  guides(col=FALSE, alpha = FALSE) + facet_wrap(~level_2_name, scales = 'free_y')

pdf("plots.pdf", onefile = TRUE)
for( i in unique(to_plot$orgUnit)){
  dat_plot <- to_plot[to_plot$orgUnit == i, ]
  p<- ggplot(dat_plot)+
    geom_point(aes(x=periods, y=value_1, colour= 'Patients source 1') , alpha=.5) +
    geom_line(aes(x=periods, y=value_1, colour= 'Patients source 1'), alpha=.5)+    
    geom_point(aes(x=periods, y=value_2, colour= 'Patients source 2'), alpha=.5) +
    geom_line(aes(x=periods, y=value_2, colour= 'Patients source 2'), alpha=.5)+
    geom_point(aes(x=periods, y=expected, colour= 'Expectation'), alpha=.5) +
    geom_line(aes(x=periods, y=expected, colour= 'Expectation'), alpha=.5)+
    geom_point(aes(x=periods, y=values, colour="Final Values")) +
    geom_line(aes(x=periods, y = values, colour="Final Values")) +
    scale_colour_manual(name="Source", values=cols) +
    guides(alpha = FALSE)+
    ylim(0,max(dat_plot[,c('value_1','value_2','expected','values')])) +
    theme_bw() +
    theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
    theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
    ylab("N Patients") + xlab("Year 2017")
  print(p)
}
dev.off()





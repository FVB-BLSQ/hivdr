library(ggplot2)
library(zoo)
library(dplyr)
library(data.table)

data_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/sources/'
metadata_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/metadata/'

data_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/' 
metadata_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/metadata/' 

### Load Data
cordaid <- readRDS(paste0(data_dir, 'Cordaid_TRAITEMENTS.rds'))
pnls <- readRDS(paste0(data_dir, 'PNLS.rds'))

### Load Metadata
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

## Find common facilities between PNLS and Cordaid
pnls_cordaid <- pnls[pnls$orgUnit %in% cordaid$orgUnit,]
cordaid_pnls <-  cordaid[cordaid$orgUnit %in% pnls$orgUnit,]

## Format Cordaid Data
cordaid = merge(cordaid, M_org_units, by.x = 'orgUnit', by.y='id', all.y = FALSE)
cordaid$period <- as.character(cordaid$period)
cordaid$month <- as.yearmon(paste0(substr(cordaid$period, 1, 4), '-', substr(cordaid$period, 5,6)))

## Select CatCombos with ancient patiennts
cat_comb_ancien <- M_category_combos$CatComboOpt_id[M_category_combos$CatOpt_id.1 == 'vZ6Os4BJvum']

## Select and build reported data on current patients in cordaid
cordaid_sous_traitement <- data.table(subset(cordaid, dataElement == 'Yj8caUQs178' & categoryOptionCombo %in% cat_comb_ancien))
cordaid_sous_traitement <- cordaid_sous_traitement[, .(n_patients = sum(value)),
                                                   by=c('orgUnit',"month","parent.parent.parent.name", 
                                                        "parent.parent.parent.id", "parent.parent.name", "parent.parent.id")]

ggplot(data = cordaid_sous_traitement) +
  geom_line(aes(x=month, y=n_patients, col= parent.parent.parent.name, group=orgUnit), alpha = .5) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free_y')


## Select and build reported data on current patients on each line in cordaid
cordaid_traitements_id <- M_data_sets$DE_id[grep('\\+|(à préciser)' , M_data_sets$DE_name[M_data_sets$DE_id %in% cordaid$dataElement])]
cordaid_traitements <- data.table(subset(cordaid, dataElement %in% cordaid_traitements_id & categoryOptionCombo %in% cat_comb_ancien))
cordaid_traitements <- cordaid_traitements[, .(n_patients_ligne = sum(value)),
                                                   by=c('orgUnit',"month","parent.parent.parent.name", 
                                                        "parent.parent.parent.id", "parent.parent.name", "parent.parent.id")]

## Compare different types of reporting in Cordaid
compare <- merge(cordaid_traitements, cordaid_sous_traitement,all=TRUE)

ggplot(data = compare) +
  geom_point(aes(x=n_patients, y=n_patients_ligne, col= parent.parent.parent.name), alpha=0.4) +
  geom_abline(intercept = 0) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free') +
  xlab('Total patients declared') + ylab('Total patients by treatment line')

## First try at providing coherence in graphic scale
compare$div = abs(compare$n_patients - compare$n_patients_ligne) / compare$n_patients
compare$alpha = 1 - compare$div
compare$alpha[compare$alpha < 0] <- 0

ggplot(data = compare) +
  geom_point(aes(x=month, y=n_patients_ligne, col= orgUnit, alpha = alpha)) +
  geom_line(aes(x=month, y=n_patients_ligne, col= orgUnit, alpha = .2)) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free_y')


## Function to build prefered serie from diverse data sources
make_serie <- function(data1, data2){
  values <- c()
  source <- c()
  expecteds <- c()
  comment <- c()
  periods <- sort(unique(c(data1$month, data2$month)))
  for(i in seq(1, length(periods))){
    ## Extract useful values
    period_i <- periods[i]
    print(period_i)
    value1 <- data1$value[data1$month == period_i]
    if (length(value1) == 0){value1 <- NA}
    value2 <- data2$value[data2$month == period_i]
    if (length(value2) == 0){value2 <- NA}
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
    
    print('Check 1') #> rajouter consistence locale
    if(test_1 & !test_2){
      values <- c(values, value2)
      source <- c(source, unique(data2$source))
      comment <- c(comment, 'Unique source')
    }
    print('Check 2')
    if(!test_1 & test_2){
      values <- c(values, value1)
      source <- c(source, unique(data1$source))
      comment <- c(comment, 'Unique source')
    }
    print('Check 3')
    if(test_1 & test_2){
      values <- c(values, expected)
      source <- c(source, 'estimation')
      comment <- c(comment, 'No data')
    }
    print('Check 4')
    if(!test_1 & !test_2 & test_3){
      values <- c(values, value1)
      source <- c(source, unique(data1$source))
      comment <- c(comment, 'Coherent data')
    }
    print('Check 5')
    if(!test_1 & !test_2 & !test_3 & test_4){
      values <- c(values, value1)
      source <- c(source, unique(data1$source))
      comment <- c(comment, 'Incoherent data')
    }
    print('Check 6')
    if(!test_1 & !test_2 & !test_3 & !test_4 & test_5){
      values <- c(values, value2)
      source <- c(source, unique(data2$source))
      comment <- c(comment, 'Incoherent data')
    }
    print('Check 7')
    if(!test_1 & !test_2 & !test_3 & !test_4 & !test_5){
      values <- c(values, expected)
      source <- c(source,'estimation')
      comment <- c(comment, 'Incoherent data')
    }
  }
  print(periods)
  print(values)
  print(source)
  print(expecteds)
  out <- data.frame('periods'=periods,'values'=values, 'expected' = expecteds , 
                    'source'=source, 'comment'= comment)
  out$value_1[out$period == periods]<- data1$value[data1$month == periods]
  out$value_2[out$period == periods]<- data2$value[data2$month == periods]
  return(out)
}

## Standardizing data sources and making unique data frame
serie_data_1 <- subset(compare , select=c('month', 'orgUnit','n_patients', 'parent.parent.parent.name'))
serie_data_2 <- subset(compare , select=c('month', 'orgUnit','n_patients_ligne', 'parent.parent.parent.name'))
colnames(serie_data_1) <- colnames(serie_data_2) <- c('month', 'orgUnit', 'value', 'province')
serie_data_1$source <- 'total'
serie_data_2$source <-'by line'

full_data <- rbind(serie_data_1, serie_data_2)




serying <- function(data, name_1, name_2){
  data1 <- data[data$source == name_1,]
  data2 <- data[data$source == name_2,]
  print('making a serie')
  out <- make_serie(data1, data2)
  return(out)
}

completed_data <- function(full_data, name_1, name_2){
  data <- full_data %>% group_by(.,orgUnit) %>% do(serying(., name_1, name_2))
  return(data)
}

plot_sample_completed <- function(completed_serie, sample_size, colors){
  sample <- sample(unique(completed_serie$orgUnit), size = sample_size)
  plot <- ggplot(completed_serie[completed_serie$orgUnit %in% sample, ])+
    geom_point(aes(x=periods, y=value_1, colour= 'Declared Patients') , alpha=.5) +
    geom_line(aes(x=periods, y=value_1, colour= 'Declared Patients'), alpha=.5)+    
    geom_point(aes(x=periods, y=value_2, colour= 'Treatment Lines'), alpha=.5) +
    geom_line(aes(x=periods, y=value_2, colour= 'Treatment Lines'), alpha=.5)+
    geom_point(aes(x=periods, y=expected, colour= 'Expectation'), alpha=.5) +
    geom_line(aes(x=periods, y=expected, colour= 'Expectation'), alpha=.5)+
    geom_point(aes(x=periods, y=values, colour="Final Values", shape=source), size = 2) +
    geom_line(aes(x=periods, y = values, colour="Final Values")) +
    scale_colour_manual(name="Source", values=cols) +
    guides(alpha = FALSE)+
    facet_wrap(~name, scales = 'free_y') +
    theme_bw() +
    theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
    theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
    ylab("N Patients") + xlab("Year 2017")  +
    guides(shape=guide_legend(title="Comment"))
  return(plot)
} 

pdf_plot <- function(complete_data, plots.pdf, dir0){
  pdf(paste(dir0,plots.pdf), onefile = TRUE)
  for( i in unique(complete_data$orgUnit)){
    dat_plot <- complete_data[complete_data$orgUnit == i, ]
    p <- ggplot(dat_plot)+
      geom_point(aes(x=periods, y=value_1, colour= 'Declared Patients') , alpha=.5) +
      geom_line(aes(x=periods, y=value_1, colour= 'Declared Patients'), alpha=.5)+    
      geom_point(aes(x=periods, y=value_2, colour= 'Treatment Lines'), alpha=.5) +
      geom_line(aes(x=periods, y=value_2, colour= 'Treatment Lines'), alpha=.5)+
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
}

exported <- function(completed_data, dir0, file_name){
  export_1 <- completed_data[c('orgUnit','periods','values')]
  export_2 <-  completed_data[c('orgUnit','periods','source')]
  export_3 <-  completed_data[c('orgUnit','periods','comment')]
  colnames(export_1) <- colnames(export_2) <- colnames(export_3) <- c('OU_id','Period','data_value')
  export_1$data_value <- as.character(export_1$data_value)
  export_1$DE_id <- 'ACCEPTED_VALUE'
  export_2$DE_id <- 'ACCEPTED_SOURCE'
  export_3$DE_id <- 'DATA_VALUE_COMMENT'
  export <- rbind(as.data.frame(export_1), as.data.frame(export_2), as.data.frame(export_3))
  write.csv(export, paste(dir0, file_name))
  return(export)
}

## Plotting parameter
cols <- c("Declared Patients"="#e31a1c","Treatment Lines"="#1f78b4","Expectation"="#33a02c", "Final Values"="#000000")

completed_data_cordaid <- completed_data(as.data.frame(full_data), 'total', 'by line')

## TODO Integrer ça 
completed_data_cordaid <- merge(completed_data_cordaid, M_hierarchy, by.x = 'orgUnit' , by.y = 'id', all.y = FALSE)
plot_sample_completed(completed_data_cordaid, sample_size = 16, cols)

## TODO Make Function for this
ggplot(to_plot[to_plot$orgUnit %in% sample, ])+
  geom_line(aes(x= periods, y=values), alpha=.5)+
  geom_point(aes(x= periods, y=values, color=source, shape=comment))+
  facet_wrap(~name, scales='free_y')

pdf_plot(completed_data_cordaid, plots.pdf = 'cordaid_compare.pdf', dir0='')


## COMPARE PNLS AND CORDAID

# series with total numbers currently on ART
cordaid_id <- 'Yj8caUQs178'
pnls_id <- 'Dd2G5zI0o0a'

cat_comb_ancien <- M_category_combos$CatComboOpt_id[M_category_combos$CatOpt_id.1 %in% c('vZ6Os4BJvum','ggod3chlUCG')]

cordaid_total_arv <- cordaid_pnls[(cordaid_pnls$dataElement == cordaid_id) & (cordaid_pnls$categoryOptionCombo %in% cat_comb_ancien),]
pnls_total_arv <- pnls_cordaid[(pnls_cordaid$dataElement == pnls_id)& (pnls_cordaid$categoryOptionCombo %in% cat_comb_ancien),]

cordaid_total_arv <- cordaid_total_arv %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))
pnls_total_arv <- pnls_total_arv %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))

compare <- merge(pnls_total_arv,cordaid_total_arv,by=c('period', 'orgUnit'), suffixes = c('pnls', 'cordaid'))

ggplot(compare) +
  geom_point(aes(valuepnls, valuecordaid))

cordaid_total_arv <- subset(cordaid_total_arv, select=c(period, value, orgUnit))
pnls_total_arv$period <- as.character(pnls_total_arv$period)
pnls_total_arv$orgUnit <- as.character(pnls_total_arv$orgUnit)
cordaid_total_arv$orgUnit <- as.character(cordaid_total_arv$orgUnit)
pnls_total_arv <- subset(pnls_total_arv, select=c(period, value, orgUnit))

colnames(cordaid_total_arv) <-colnames(pnls_total_arv) <- c('month', 'value', 'orgUnit')
cordaid_total_arv$source <- 'cordaid'
pnls_total_arv$source <- 'pnls'

full_data <- rbind(as.data.frame(pnls_total_arv),
                   as.data.frame(cordaid_total_arv))


completed_data_cordaid_pnls <- completed_data(as.data.frame(full_data), 'cordaid', 'pnls')

## TODO Integrer ça 
completed_data_cordaid_pnls <- merge(completed_data_cordaid_pnls, M_hierarchy, by.x = 'orgUnit' , by.y = 'id', all.y = FALSE)
plot_sample_completed(completed_data_cordaid_pnls, sample_size = 16, cols)

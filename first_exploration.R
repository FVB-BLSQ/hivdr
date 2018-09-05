library(ggplot2)
library(zoo)
library(dplyr)
library(data.table)

### working directory for Data and Metadata
data_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/sources/'
metadata_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/metadata/'

#data_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/' 
#metadata_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/metadata/' 

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


## Function to build preferred serie from diverse data sources
make_serie <- function(data1, data2){
  values <- c()
  source <- c()
  expecteds <- c()
  comment <- c()
  periods <- sort(unique(c(data1$month, data2$month)))
  outlier_1 <- rep(0, length(periods))
  outlier_2 <- rep(0, length(periods))
  for(i in seq(1, length(periods))){
    ## Extract useful values
    period_i <- periods[i]
    print(period_i)
    value1 <- data1$value[data1$month == period_i]
    if (length(value1) == 0){value1 <- NA}
    value2 <- data2$value[data2$month == period_i]
    if (length(value2) == 0){value2 <- NA}
    
    ###calculating "expected" from different situations
     #-----with two periods and two sources 
    if(length(values) < 3 & (!is.na(value1)) & (!is.na(value2))){
      expected <- mean(c(value1, value2, na.rm=TRUE))
    }
    #-----with two periods and two sources
    if(length(values) < 3 & (!is.na(value1)) & (is.na(value2))){
      expected <- value1
    }
    
    if(length(values) < 3 & (is.na(value1)) & (!is.na(value2))){
      expected <- value2
    }
    #-----at least three periods and two sources : making rolling average
    if(length(values) >= 3 & (!is.na(value1)) & (!is.na(value2))){
      expected <- mean(values[(length(values)-2):length(values)], na.rm=TRUE)
    }
    
    expecteds <- c(expecteds, expected)
    
    ##taking into account zeros and outliers
     #first period 
    if(i==1){
      window_months <- periods[c(min(i+1, length(periods)), min(i+2, length(periods)))]
      if((!is.na(value1)) & (value1 == 0) & (min(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- NA
      }
      if((!is.na(value1)) & (value1 > 0) & (value1 < quantile(data1$value[data1$month %in% window_months],0.25, na.rm=T) 
                             - 1.5 * IQR(data1$value[data1$month %in% window_months], na.rm=T) 
                           | value1 > quantile(data1$value[data1$month %in% window_months],0.75, na.rm=T) 
                           + 1.5 * IQR(data1$value[data1$month %in% window_months], na.rm=T))
                          & (max(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- median(data1$value[data1$month %in% window_months], na.rm=TRUE)
        outlier_1[i] <- 1
      }
      if((!is.na(value2)) & (value2 == 0) & (min(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
        value2 <- NA
      }
      if((!is.na(value2)) & (value2 > 0) & (value2 < quantile(data2$value[data2$month %in% window_months],0.25, na.rm=T) 
                             - 1.5 * IQR(data2$value[data2$month %in% window_months], na.rm=T) 
                           | value2 > quantile(data2$value[data2$month %in% window_months],0.75, na.rm=T) 
                             + 1.5 * IQR(data2$value[data2$month %in% window_months], na.rm=T))
                          & (max(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
        value2 <- median(data2$value[data2$month %in% window_months], na.rm=TRUE)
        outlier_2[i] <- 1
      }
    }
    #second period 
    if(i==2){
      window_months <- periods[c(1,min(i+1, length(periods)), min(i+2, length(periods)))]
      if((!is.na(value1)) & (value1 == 0) & (min(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- NA
      } 
      if((!is.na(value1)) & (value1 > 0) & (value1 < quantile(data1$value[data1$month %in% window_months],0.25, na.rm=T) 
                             - 1.5 * IQR(data1$value[data1$month %in% window_months], na.rm=T) 
                             | value1 > quantile(data1$value[data1$month %in% window_months],0.75, na.rm=T) 
                             + 1.5 * IQR(data1$value[data1$month %in% window_months], na.rm=T))
                          & (max(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- median(data1$value[data1$month %in% window_months], na.rm=TRUE)
        outlier_1[i] <- 1
      }
      if((!is.na(value2)) & (value2 == 0) & (min(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
        value2 <- NA
      } 
      if((!is.na(value2)) & (value2 > 0) & (value2 < quantile(data2$value[data2$month %in% window_months],0.25, na.rm=T) 
                             - 1.5 * IQR(data2$value[data2$month %in% window_months], na.rm=T) 
                             | value2 > quantile(data2$value[data2$month %in% window_months],0.75, na.rm=T) 
                             + 1.5 * IQR(data2$value[data2$month %in% window_months], na.rm=T))
                          & (max(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
        value2 <- median(data2$value[data2$month %in% window_months], na.rm=TRUE)
        outlier_2[i] <- 1
      }
    }
    #from the third period
    if(i >= 3){
      window_months <- periods[c(i-2,i-1, min(i+1, length(periods)), min(i+2, length(periods)))]
      if((!is.na(value1)) & (value1 == 0) & (min(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- NA
      }
      if((!is.na(value1)) & (value1 > 0) & (value1 < quantile(data1$value[data1$month %in% window_months],0.25, na.rm=T) 
                             - 1.5 * IQR(data1$value[data1$month %in% window_months], na.rm=T) 
                           | value1 > quantile(data1$value[data1$month %in% window_months],0.75, na.rm=T) 
                           + 1.5 * IQR(data1$value[data1$month %in% window_months], na.rm=T))
                          & (max(data1$value[data1$month %in% window_months], na.rm=TRUE) > 0)){
        value1 <- median(data1$value[data1$month %in% window_months], na.rm=TRUE)
        outlier_1[i] <- 1
      }
      if((!is.na(value2)) & (value2 == 0) & (min(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
        value2 <- NA
      }
      if((!is.na(value2)) & (value2 > 0) & (value2 < quantile(data2$value[data2$month %in% window_months],0.25, na.rm=T) 
                             - 1.5 * IQR(data2$value[data2$month %in% window_months], na.rm=T) 
                             | value2 > quantile(data2$value[data2$month %in% window_months],0.75, na.rm=T) 
                           + 1.5 * IQR(data2$value[data2$month %in% window_months], na.rm=T))
                          & (max(data2$value[data2$month %in% window_months], na.rm=TRUE) > 0)){
       value2 <- median(data2$value[data2$month %in% window_months], na.rm=TRUE)
       outlier_2[i] <- 1
      }
    }
   
    ## Make tests
    
    ### Test 1: value1 is missinng
    test_1 <- is.na(value1)
    ### Test 2: value2 is missing
    test_2 <- is.na(value2)
    ### Test 3: values 1 and 2 are equal
    test_3 <- value1==value2
    ### Test 4: values 1 and 2 are a little different
    to_check <- (abs(value1-value2)/(max(value1,value2, na.rm=TRUE) + 0.0001))
    test_4 <- is.finite(to_check) & (to_check < .15)
    ### Test 5: value 1 is very different from expectation
    test_5 <- (abs(value1-expected)/(max(value1,expected,na.rm = TRUE) + 0.0001)) > .15
    ### Test 6 : value 2 is very different from expectation
    test_6 <- (abs(value2-expected)/(max(value2,expected,na.rm = TRUE) + 0.0001)) > .15
    ### other situations
    to_check_1 <- (abs(value1-expected)/(max(value1,expected,na.rm = TRUE) + 0.0001))
    to_check_2 <- (abs(value2-expected)/(max(value2,expected,na.rm = TRUE) + 0.0001))
    ### Test 7 and 8: 
    test_7 <- to_check< to_check_1 #take value1
    test_8 <- to_check< to_check_2 #take value1
    
    
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
      comment <- c(comment, 'identical values')
    }
    print('Check 5')
    if(!test_1 & !test_2 & !test_3 & test_4){
      values <- c(values, value1)
      source <- c(source, unique(data1$source))
      comment <- c(comment, 'Coherent data')
    }
    print('Check 6')
    if(!test_1 & !test_2 & !test_3 & !test_4 & test_5){
      values <- c(values, expected)
      source <- c(source,'estimation')
      comment <- c(comment, 'Incoherent data')
    }
    print('Check 7')
    if(!test_1 & !test_2 & !test_3 & !test_4 & !test_5 & test_6){
      values <- c(values, expected)
      source <- c(source,'estimation')
      comment <- c(comment, 'Incoherent data')
    }
    print('Check 8')
    if(!test_1 & !test_2 & !test_3 & !test_4 & !test_5 & !test_6 & (test_7 | test_8)){
      values <- c(values, value1)
      source <- c(source, unique(data1$source))
      comment <- c(comment, 'Incoherent data')
    }
    print('Check 9')
    if(!test_1 & !test_2 & !test_3 & !test_4 & !test_5 & !test_6 & !test_7 & !test_8){
      values <- c(values, expected)
      source <- c(source,'estimation')
      comment <- c(comment, 'Incoherent data')
    }
  }
  print(periods)
  print(values)
  print(source)
  print(expecteds)
  out <- data.frame('periods'=periods,'values'=values, 'expected' = expecteds, 'source'=source, 
                    'comment'= comment, 'outlier_1'=outlier_1, 'outlier_2'=outlier_2)
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
  completed_data <- as.data.frame(data)
  completed_data_name <- merge(completed_data, M_hierarchy, by.x = 'orgUnit', by.y = 'id', all.y = FALSE)
  return(completed_data_name)
}

#-----------functions to represent and compare series--------------------#
##represent graphs on a sample of OrgUnits
plot_sample__completed <- function(completed_serie, sample_size, title_series){
  sample <- sample(unique(completed_serie$orgUnit), size = sample_size)
  plot <- ggplot(completed_serie[completed_serie$orgUnit %in% sample, ])+
    geom_point(data = completed_serie[(completed_serie$outlier_1) == 1 & 
                                        (completed_serie$orgUnit %in% sample),], 
               aes(x=periods, y = value_1, fill="outlier_1"), show.legend=FALSE, size = 3)+
    geom_point(data = completed_serie[(completed_serie$outlier_2) == 1 & 
                                        (completed_serie$orgUnit %in% sample),], 
               aes(x=periods, y = value_2, fill="outlier_2"), show.legend=FALSE, size = 3)+
    geom_point(aes(x=periods, y=expected, fill='Expectation'), show.legend=FALSE, alpha=.5) +
    geom_line(aes(x=periods, y=expected, colour='Expectation', group=orgUnit), alpha=.5)+
    geom_point(aes(x=periods, y=values, shape=comment, fill='Preferred serie'), size = 2) +
    geom_line(aes(x=periods, y = values, colour='Preferred serie', group=orgUnit))+
    geom_point(aes(x=periods, y=value_1, fill= serie1), show.legend=FALSE, alpha=.5) +
    geom_line(aes(x=periods, y=value_1, colour= serie1, group=orgUnit), alpha=.5)+    
    geom_point(aes(x=periods, y=value_2, fill= serie2), show.legend=FALSE, alpha=.5) +
    geom_line(aes(x=periods, y=value_2, colour= serie2, group=orgUnit), alpha=.5)+
    scale_colour_manual(name="Sources", values = cols1)+
    scale_shape_manual(name="Series", values = 0:4)+ 
    guides(alpha = FALSE,
           shape=guide_legend(title="Comment"), 
           fill=FALSE,
           colour = guide_legend(title="Source", 
                                 override.aes = list(shape='')))+
    facet_wrap(~name, scales = 'free_y') +
    theme_bw() +
    theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
    theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
    ylab("N Patients") + xlab("Year 2017")+
    labs(title = title_series)
  return(plot)
} 

#export in a pdf file, by orgunit
pdf_plot<- function(complete_data, plots.pdf, dir0, title_series){
  complete_data$size1 <- complete_data$outlier_1*2
  complete_data$size2 <- complete_data$outlier_2*2
  pdf(paste(dir0,plots.pdf), onefile = TRUE)
  for( i in unique(complete_data$orgUnit)){
    dat_plot <- complete_data[complete_data$orgUnit == i, ]
    p <- ggplot(dat_plot)+
      geom_point(aes(x=periods, y = value_1, fill='outlier_1', size = size1), show.legend=FALSE)+
      geom_point(aes(x=periods, y = value_2, fill='outlier_2', size = size2), show.legend=FALSE)+
      geom_point(aes(x=periods, y=value_1, fill= serie1) , show.legend=FALSE, alpha=.5) +
      geom_line(aes(x=periods, y=value_1, colour= serie1, group=orgUnit), alpha=.5)+    
      geom_point(aes(x=periods, y=value_2, fill= serie2), show.legend=FALSE, alpha=.5) +
      geom_line(aes(x=periods, y=value_2, colour= serie2, group=orgUnit), alpha=.5)+
      geom_point(aes(x=periods, y=expected, fill= 'Expectation'), show.legend=FALSE, alpha=.5) +
      geom_line(aes(x=periods, y=expected, colour= 'Expectation', group=orgUnit), alpha=.5)+
      geom_point(aes(x=periods, y=values, shape=comment, fill='Preferred serie')) +
      geom_line(aes(x=periods, y = values, colour='Preferred serie', group=orgUnit)) +
      scale_colour_manual(name="Source", values=cols1)+
      scale_shape_manual(name="Series", values = 0:4)+ 
      guides(alpha = FALSE,
             shape=guide_legend(title="Comment"), 
             fill=FALSE,
             colour = guide_legend(title="Source", 
                                   override.aes = list(shape='')))+
      ylim(0,max(dat_plot[,c('value_1','value_2','expected','values')])) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
      theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
      ylab("N Patients") + xlab("Year 2017")+
      labs(title = title_series)
    print(p)
  }
  dev.off()
}

#to plot only preferred series
f_plot <- function(completed_data_name, sample_size, title_series){ 
  sample <- sample(unique(completed_data_name$orgUnit), size = sample_size)  
  p<-ggplot(completed_data_name[completed_data_name$orgUnit %in% sample, ])+ 
    geom_line(aes(x= periods, y=values), alpha=.5)+ 
    geom_point(aes(x= periods, y=values, color=source, shape=comment))+ 
    facet_wrap(~name, scales='free_y')+
    labs(title = title_series)
  return(p) 
} 

#export data in csv format, by month
exported <- function(completed_data, dir0, file_name){
  export_1 <- completed_data[c('orgUnit','periods','values')]
  export_2 <-  completed_data[c('orgUnit','periods','source')]
  export_3 <-  completed_data[c('orgUnit','periods','comment')]
  export_1$values <- round(export_1$values,0)
  colnames(export_1) <- colnames(export_2) <- colnames(export_3) <- c('OU_id','Period','data_value')
  export_1$data_value <- as.character(export_1$data_value)
  export_1$DE_id <- 'ACCEPTED_VALUE'
  export_2$DE_id <- 'ACCEPTED_SOURCE'
  export_3$DE_id <- 'DATA_VALUE_COMMENT'
  export <- rbind(as.data.frame(export_1), as.data.frame(export_2), as.data.frame(export_3))
  export$catoptcombo <- 'HllvX50cXC0'
  export$attroptcombo <- NA
  export <- export[c('DE_id','Period','OU_id','catoptcombo','attroptcombo','data_value')]
  write.csv(export, paste(dir0, file_name), row.names = FALSE)
  return(export)
}

#export data in csv format, by quarter
exportedbis <- function(completed_data, dir0, file_name){
  export_1 <- completed_data[c('orgUnit','periods','values')]
  export_2 <-  completed_data[c('orgUnit','periods','source')]
  export_3 <-  completed_data[c('orgUnit','periods','comment')]
  export_1$values <- round(export_1$values,0)
  colnames(export_1) <- colnames(export_2) <- colnames(export_3) <- c('OU_id','Period','data_value')
  export_1$data_value <- as.character(export_1$data_value)
  export_1$DE_id <- 'ACCEPTED_VALUE'
  export_2$DE_id <- 'ACCEPTED_SOURCE'
  export_3$DE_id <- 'DATA_VALUE_COMMENT'
  export <- rbind(as.data.frame(export_1), as.data.frame(export_2), as.data.frame(export_3))
  export$catoptcombo <- 'HllvX50cXC0'
  export$attroptcombo <- NA
  export <- export[(export$Period==201703 | export$Period==201706 | export$Period==201709 | export$Period==201712),]
  export <- export[c('DE_id','Period','OU_id','catoptcombo','attroptcombo','data_value')]
  write.csv(export, paste(dir0, file_name), row.names = FALSE)
  return(export)
}

## Plotting parameter
serie1<-'Declared Patients'
serie2<-'Treatment Lines'

cols1 <- c("Declared Patients"="#e31a1c","Treatment Lines"="#1f78b4","Expectation"="#33a02c", "Preferred serie"="#000000")

title_series<-'total numbers of patients currently on ART'


##call functions
completed_data_cordaid<-completed_data(full_data, 'total', 'by line')
plot_sample_completed(completed_data_cordaid, sample_size = 25, title_series)
pdf_plot(completed_data_cordaid, plots.pdf = 'cordaid_compare.pdf', dir0='', title_series)
f_plot(completed_data_cordaid, 25, title_series)
exported (completed_data_cordaid, dir0='', file_name='export_cordaid.csv')
exportedbis (completed_data_cordaid, dir0='', file_name='export_cordaid_bis.csv')


###########################################################################
###########################################################################
##------ COMPARE PNLS AND CORDAID : total numbers currently on ART-------##
###########################################################################
###########################################################################

# series with total numbers currently on ART
cordaid_id <- 'Yj8caUQs178'
pnls_id <- 'Dd2G5zI0o0a'

## Select correponding CatCombos
cat_comb_ancien <- M_category_combos$CatComboOpt_id[M_category_combos$CatOpt_id.1 %in% c('vZ6Os4BJvum','ggod3chlUCG')]

## Select correponding DataElement
cordaid_total_arv <- cordaid_pnls[(cordaid_pnls$dataElement == cordaid_id) & (cordaid_pnls$categoryOptionCombo %in% cat_comb_ancien),]
pnls_total_arv <- pnls_cordaid[(pnls_cordaid$dataElement == pnls_id)& (pnls_cordaid$categoryOptionCombo %in% cat_comb_ancien),]

## aggregate values of each DE by period and OrgUnit
cordaid_total_arv <- cordaid_total_arv %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))
pnls_total_arv <- pnls_total_arv %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))

#compare and plot two series with same DE
compare <- merge(pnls_total_arv,cordaid_total_arv,by=c('period', 'orgUnit'), suffixes = c('pnls', 'cordaid'))
ggplot(compare) +
  geom_point(aes(valuepnls, valuecordaid))

## Standardizing data sources and making unique data frame
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


## Plotting parameter
serie1<-'serie cordaid'
serie2<-'serie pnls'
cols1 <- c('serie cordaid'="#e31a1c",'serie pnls'="#1f78b4","Expectation"="#33a02c", "Preferred serie"="#000000")

title_series<-'total numbers of patients currently on ART'
 
##call functions
completed_data_cordaid_pnls_total<-completed_data(full_data, 'cordaid', 'pnls')
plot_sample_completed(completed_data_cordaid_pnls_total, sample_size = 25, title_series)
f_plot(completed_data_cordaid_pnls, sample_size = 25, title_series)
pdf_plot(completed_data_cordaid_pnls_total, plots.pdf = 'cordaid_pnls_totalART.pdf', dir0='',title_series)


###########################################################################
###########################################################################
##------------- COMPARE PNLS AND CORDAID BY TREATMENT LINES--------------##
###########################################################################
###########################################################################

#Find DE of interest
###cordaid=TDF+3TC+EFV & PNLS-DRUG-TDF+FTC+EFV

cordaid_id <- 'iOW0K3mjoxe'
pnls_id <- 'hKZX17adANF'


#TDF+3TC+NVP
#cordaid_id <- 'lqA1LfMt9C6'
#pnls_id <- 'KJ7qFDiAftt'

#AZT+3TC+NVP
#cordaid_id <- 'wrRjiD0fz8j'
#pnls_id <-'aozs6mB8T8n'

## Select correponding CatCombos  
cat_comb_ancien <- M_category_combos$CatComboOpt_id[M_category_combos$CatOpt_id.1 %in% c('vZ6Os4BJvum','ggod3chlUCG')]

## Select correponding DataElement
cordaid_line <- cordaid_pnls[(cordaid_pnls$dataElement == cordaid_id),]
pnls_line <- pnls_cordaid[(pnls_cordaid$dataElement == pnls_id),]

## aggregate values of each DE by period and OrgUnit
cordaid_line <- cordaid_line %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))
pnls_line <- pnls_line %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))

## Plotting parameter
serie1<-'serie cordaid'
serie2<-'serie pnls'
cols1 <- c('serie cordaid'="#e31a1c",'serie pnls'="#1f78b4","Expectation"="#33a02c", "Preferred serie"="#000000")

cols1 <- c('serie cordaid'="#e31a1c",'serie pnls'="#1f78b4","Expectation"="#33a02c", "Preferred serie"="#000000")


title_series<-'Number of patients under TDF+3TC+EFV'

#compare and plot two series with same DE
compare <- merge(pnls_line,cordaid_line,by=c('period', 'orgUnit'), suffixes = c('pnls', 'cordaid'))
ggplot(compare) +
  geom_point(aes(valuepnls, valuecordaid))+
  labs(title = title_series)

## Standardizing data sources and making unique data frame
cordaid_line <- subset(cordaid_line, select=c(period, orgUnit, value))
pnls_line$period <- as.character(pnls_line$period)
pnls_line$orgUnit <- as.character(pnls_line$orgUnit)
cordaid_line$orgUnit <- as.character(cordaid_line$orgUnit)
pnls_line <- subset(pnls_line, select=c(period, orgUnit, value))

colnames(cordaid_line) <-colnames(pnls_line) <- c('month', 'orgUnit', 'value')
cordaid_line$source <- 'cordaid'
pnls_line$source <- 'pnls'

full_data <- rbind(as.data.frame(pnls_line),
                   as.data.frame(cordaid_line))


##call functions
completed_data_cordaid_pnls<-completed_data(full_data, 'cordaid', 'pnls')
new_completed_data_cordaid_pnls<-completed_data_cordaid_pnls[completed_data_cordaid_pnls$values> 50,]
plot_sample__completed(completed_data_cordaid_pnls, sample_size = 25, title_series)
f_plot(completed_data_cordaid_pnls, sample_size = 25, title_series)
exported (completed_data_cordaid_pnls, dir0='', file_name='export_TDF3TCEFV.csv')
exportedbis (completed_data_cordaid_pnls, dir0='', file_name='export_TDF3TCEFV_bis.csv')
pdf_plot(new_completed_data_cordaid_pnls, plots.pdf = 'cordaid_pnls_TDF3TCEFV.pdf', dir0='',title_series)

###------------------Functions----------------------###

### Function to Load Metadata
load_metadata <- function(metadata_dir, suffix = ''){
  env <- globalenv()
  assign(paste0('M_category_combos', suffix) , readRDS(paste0(metadata_dir, 'CC_metadata.rds')), env)
  assign(paste0('M_data_element_group', suffix) , readRDS(paste0(metadata_dir, 'DEG_metadata.rds')), env)
  assign(paste0('M_data_sets', suffix) , readRDS(paste0(metadata_dir, 'DS_metadata.rds')), env)
  assign(paste0('M_org_units', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata_DSinfo.rds')), env)
  assign(paste0('M_hierarchy', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata_flat.rds')), env)
  assign(paste0('M_org_units', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata.rds')), env)
}

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
    if(length(values) < 3 & (!is.na(value1)) & (!is.na(value2))){
      expected <- mean(c(value1, value2, na.rm=TRUE))
    }
    
    if(length(values) < 3 & (!is.na(value1)) & (is.na(value2))){
      expected <- value1
    }
    
    if(length(values) < 3 & (is.na(value1)) & (!is.na(value2))){
      expected <- value2
    }
    
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

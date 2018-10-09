###------------------Functions----------------------###

## Function to build preferred serie from diverse data sources

##        4. Post processing => classification + imputation

######## Data Reconciliation
load_metadata <- function(metadata_dir, suffix = ''){
  env <- globalenv()
  assign(paste0('M_category_combos', suffix) , readRDS(paste0(metadata_dir, 'CC_metadata.rds')), env)
  assign(paste0('M_data_element_group', suffix) , readRDS(paste0(metadata_dir, 'DEG_metadata.rds')), env)
  assign(paste0('M_data_sets', suffix) , readRDS(paste0(metadata_dir, 'DS_metadata.rds')), env)
  assign(paste0('M_org_units', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata_DSinfo.rds')), env)
  assign(paste0('M_hierarchy', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata_flat.rds')), env)
  assign(paste0('M_org_units', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata.rds')), env)
}


make_serie <- function(data1, data2){
  values <- c()
  source <- c()
  expecteds <- c()
  comment <- c()
  expected <- NA
  period_start <- min(c(data1$month, data2$month))
  period_end <- max(c(data1$month, data2$month))
  periods <- as.yearmon(seq(as.Date(period_start), as.Date(period_end), by='mon'))
  for(i in seq(1, length(periods))){
    ## Extract useful values
    period_i <- periods[i]
    value1 <- data1$value[data1$month == period_i]
    if (length(value1) == 0){value1 <- NA}
    value2 <- data2$value[data2$month == period_i]
    if (length(value2) == 0){value2 <- NA}
    
    if(sum(c(value1,value2) %in% c(0))==1 & sum(values %in% c(0)) == 0 ){
      if( value1 == 0 & !is.na(value1)){value1 <- NA}
      if( value2 == 0 & !is.na(value2)){value2 <- NA}
    }
    
    ## Make tests
    ### Test 1: Both values are available
    test_1 <- sum(is.na(c(value1,value2)))
    ### Test 2: values 1 and 2 are similar enough
    test_2 <- (is.finite(sd(values, na.rm = TRUE)) & abs(value1-value2) < sd(values, na.rm = TRUE)) |
                ((!is.finite(sd(values, na.rm = TRUE)) | sd(values, na.rm = TRUE) < 5) & sd(c(value1,value2)) < 50)
    ### Test 3: values are similar to expectations
    test_3a <- (abs(c(value1,value2) - expected) < 2.2*sd(values, na.rm=TRUE)  & is.finite(c(value1,value2) - expected))
    test_3b <-   (!is.na(c(value1, value2)) & is.na(expected))
    test_3 <- test_3a | test_3b

    ## Assign values
    if(test_1 == 0 & test_2){
      k <- which.min(abs(c(value1,value2)-max(c(expected,0), na.rm=TRUE)))
      values <- c(values,c(value1,value2)[k])
      source <- c(source, c(unique(data1$source),unique(data2$source))[k])
      if(value1 == value2){
        comment <- c(comment, 'Identical values')
      }
      if(value1 != value2){
        comment <- c(comment, 'Equivalent values')
      }
    }
    if(test_1 == 0 & !test_2 & sum(test_3, na.rm=TRUE) == 1){
      values <- c(values, c(value1,value2)[test_3])
      source <- c(source, c(unique(data1$source),unique(data2$source))[test_3])
      comment <- c(comment, 'Non equivalent values')
    }
    if(test_1 == 0 & !test_2 & (sum(test_3, na.rm=TRUE) %in% c(0,2))){
      values <- c(values, NA)
      source <- c(source, NA)
      comment <- c(comment, 'Incoherent values')
    }
#    print('Check 2')
    if(test_1 == 1 & sum(test_3, na.rm=TRUE) == 1){
      values <- c(values, c(value1,value2)[test_3])
      source <- c(source, c(unique(data1$source),unique(data2$source))[test_3])
      comment <- c(comment, 'Unique coherent value')
    }
#    print('Check 3')
    if(test_1 == 2 | (test_1 == 1 & sum(test_3, na.rm=TRUE) == 0)){
      values <- c(values, NA)
      source <- c(source, NA)
      comment <- c(comment, 'No value')
    }
    
    ###calculating "expected" for next round
    if(length(values) < 3 ){
      expected <- NA
    }
    
    if(length(values) >= 3){
      expected <- mean(values[(length(values)-2):length(values)], na.rm=TRUE)
    }
    
    expecteds <- c(expecteds, expected)
  }
  
  out <- data.frame('periods'=periods,'value'=values, 'expected' = expecteds, 'source'=source, 
                    'comment'= comment)
  out <- merge(out, data1[,c('month','value')], by.x = 'periods', by.y = 'month', suffixes = c('','_1'), all.x = TRUE)
  out <- merge(out, data2[,c('month','value')], by.x = 'periods', by.y = 'month', suffixes = c('','_2'), all.x = TRUE)
  return(out)
}

## Standardizing data sources and making unique data frame
serying <- function(data, name_1, name_2){
  data1 <- data[data$source == name_1,]
  data2 <- data[data$source == name_2,]
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
plot_sample_completed <- function(completed_serie, sample_size, title_series){
  sample <- sample(unique(completed_serie$orgUnit), size = sample_size)
  plot <- ggplot(completed_serie[completed_serie$orgUnit %in% sample, ])+
    geom_point(aes(x=periods, y=expected, fill='Expectation'), show.legend=FALSE, alpha=.5) +
    geom_line(aes(x=periods, y=expected, colour='Expectation', group=orgUnit), alpha=.5)+
    geom_point(aes(x=periods, y=value, shape=comment, fill='Preferred serie'), size = 2) +
    geom_line(aes(x=periods, y = value, colour='Preferred serie', group=orgUnit))+
    geom_point(aes(x=periods, y=value_1, fill= serie1), show.legend=FALSE, alpha=.5) +
    geom_line(aes(x=periods, y=value_1, colour= serie1, group=orgUnit), alpha=.5)+    
    geom_point(aes(x=periods, y=value_2, fill= serie2), show.legend=FALSE, alpha=.5) +
    geom_line(aes(x=periods, y=value_2, colour= serie2, group=orgUnit), alpha=.5)+
    scale_colour_manual(name="Sources", values = cols1)+
    scale_shape_manual(name="Comment", values = 0:5)+ 
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
    geom_line(aes(x= periods, y=value), alpha=.5)+ 
    geom_point(aes(x= periods, y=value, color=source, shape=comment))+ 
    facet_wrap(~name, scales='free_y')+
    labs(title = title_series)
  return(p) 
} 

#export data in csv format, by month
format_for_export <- function(completed_data){
  export_1 <- completed_data[c('orgUnit','periods','value')]
  export_2 <-  completed_data[c('orgUnit','periods','source')]
  export_3 <-  completed_data[c('orgUnit','periods','comment')]
  export_1$value <- round(export_1$value,0)
  colnames(export_1) <- colnames(export_2) <- colnames(export_3) <- c('OU_id','Period','data_value')
  export_1$data_value <- as.character(export_1$data_value)
  export_1$DE_id <- 'ACCEPTED_VALUE'
  export_2$DE_id <- 'ACCEPTED_SOURCE'
  export_3$DE_id <- 'DATA_VALUE_COMMENT'
  export <- rbind(as.data.frame(export_1), as.data.frame(export_2), as.data.frame(export_3))
  export$catoptcombo <- 'HllvX50cXC0'
  export$attroptcombo <- NA
  export <- export[c('DE_id','Period','OU_id','catoptcombo','attroptcombo','data_value')]
  return(export)
}


to_quarterly <- function(data){
  cols <- colnames(data)
  data$quarter[data$periods %in% c('201701','201702','201703')] <- '2017Q1'
  data$quarter[data$periods %in% c('201704','201705','201706')] <- '2017Q2'
  data$quarter[data$periods %in% c('201707','201708','201709')] <- '2017Q3'
  data$quarter[data$periods %in% c('201710','201711','201712')] <- '2017Q4'
  months <- data %>% group_by(quarter, orgUnit) %>% summarise('quarter_month' = max(periods))
  data <- merge(data, months, by=c('quarter', 'orgUnit'))
  out <- subset(data, periods == quarter_month)
  out$periods <- out$quarter
  out <- out[,cols]
  return(out)
}


make_full_unique_set <- function(data){
  expectation <- melt(dcast(data,  orgUnit ~ period, 
                            value.var = 'value', fill =NA), id.vars = c("orgUnit"),
                      value.name = 'value', variable.name='periods')
  expectation$periods <- as.character(expectation$periods)
  return(expectation)
}


predict_missing <- function(data){
  if(sum(is.na(data$value)) > 0 & sum(!is.na(data$value)) > 0 ){
    #data$periods <- as.character(data$periods)
    model <- splinefun(x = data$periods, 
                       y = data$value, ties = 4)
    imputed <- data.frame('periods'=c(data$periods[is.na(data$value)]),
                          'value'=c(round(model(data$periods[is.na(data$value)]),0)),
                          'comment'='Imputed Value')
    imputed$periods <- as.character(imputed$periods)
    out <- merge(data, imputed, by =c('periods'),
                 suffixes = c('','_imputation'), all.x=TRUE)
    out$value[!is.na(out$value_imputation)] <- out$value_imputation[!is.na(out$value_imputation)]
    out$value[out$value < 0] <- 0
    if('comment_imputation' %in% colnames(out)){
      out$comment[!is.na(out$value_imputation)] <- as.character(out$comment_imputation[!is.na(out$value_imputation)])
    }
    out$source[!is.na(out$value_imputation)] <-'Imputation'
    if(class(out$periods) == 'numeric'){
      out <- merge(adhoc_period, out, by.x = 'month', by.y='periods', all.y=TRUE)
      out$periods <- as.character(out$periods)
    }
    return(out)
  }
  else{
    data$value_imputation <- NA
    data$comment_imputation <- NA
    data <- merge(adhoc_period, data, by.x = 'month', by.y='periods', all.y=TRUE)
    data$periods <- as.character(data$periods)
    return(data)
  }
}

period_monthly <- function(period){
  period <- as.character(period)
  month <- as.yearmon(paste0(substr(period, 1, 4), '-', substr(period, 5,6)))
  return(month)
}

build_common_set <- function(data1, data2, source1, source2){
  data1_2 <- data1[data1$orgUnit %in% data2$orgUnit,]
  data1_2$source <- source1
  data2_1 <- data2[data2$orgUnit %in% data1$orgUnit,]
  data2_1$source <- source2
  common_set <- rbind(data1_2, data2_1)
  return(common_set)
}

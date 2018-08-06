data_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/sources/'
metadata_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/metadata/'




#modif


### Load Data

cordaid <- readRDS(paste0(data_dir, 'Cordaid_TRAITEMENTS.rds'))
pnls <- readRDS(paste0(data_dir, 'PNLS.rds'))
#sigl2 <- readRDS(paste0(data_dir, 'SIGL2.rds'))


## pnls and cordaid

pnls_cordaid <- pnls[pnls$orgUnit %in% cordaid$orgUnit,]
cordaid_pnls <-  cordaid[cordaid$orgUnit %in% pnls$orgUnit,]





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

M_data_sets[M_data_sets$DE_id %in% cordaid$dataElement,c('DE_id', 'DE_name')]
M_data_sets[M_data_sets$DE_id %in% pnls$dataElement,c('DE_id', 'DE_name')]



table(cordaid$value[cordaid$dataElement == 'uSoD3GUgQSF'])
summary(cordaid$value[cordaid$dataElement == 'lqA1LfMt9C6'])

length(unique(cordaid$orgUnit))


table(M_org_units$parent.parent.parent.name[M_org_units$id %in% unique(cordaid$orgUnit)])

M_org_units$parent.parent.parent.name[M_org_units$id %in% unique(cordaid$orgUnit)]

cordaid = merge(cordaid, M_org_units, by.x = 'orgUnit', by.y='id', all.y = FALSE)

library(ggplot2)
library(zoo)
library(dplyr)
library(data.table)

cordaid$period <- as.character(cordaid$period)
cordaid$month <- as.yearmon(paste0(substr(cordaid$period, 1, 4), '-', substr(cordaid$period, 5,6)))
cat_comb_ancien <- M_category_combos$CatComboOpt_id[M_category_combos$CatOpt_id.1 == 'vZ6Os4BJvum']

cordaid_sous_traitement <- data.table(subset(cordaid, dataElement == 'Yj8caUQs178' & categoryOptionCombo %in% cat_comb_ancien))



cordaid_sous_traitement <- cordaid_sous_traitement[, .(n_patients = sum(value)),
                                                   by=c('orgUnit',"month","parent.parent.parent.name", "parent.parent.parent.id", "parent.parent.name", "parent.parent.id")]

ggplot(data = cordaid_sous_traitement) +
  geom_line(aes(x=month, y=n_patients, col= orgUnit)) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free_y')

cordaid_traitements_id <- M_data_sets$DE_id[grep('\\+|(à préciser)' , M_data_sets$DE_name[M_data_sets$DE_id %in% cordaid$dataElement])]
cordaid_traitements <- data.table(subset(cordaid, dataElement %in% cordaid_traitements_id & categoryOptionCombo %in% cat_comb_ancien))
cordaid_traitements <- cordaid_traitements[, .(n_patients_ligne = sum(value)),
                                                   by=c('orgUnit',"month","parent.parent.parent.name", "parent.parent.parent.id", "parent.parent.name", "parent.parent.id")]


compare <- merge(cordaid_traitements, cordaid_sous_traitement,all=TRUE)


ggplot(data = compare) +
  geom_point(aes(x=n_patients, y=n_patients_ligne, col= orgUnit)) +
  geom_abline(intercept = 0) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free') +
  xlab('Total patients declared') + ylab('Total patients by treatment line')


compare$ratio = compare$n_patients_ligne / compare$n_patients
compare$div = abs(compare$n_patients - compare$n_patients_ligne) / compare$n_patients
compare$alpha = 1 - compare$div
compare$alpha[compare$alpha < 0] <- 0


compare$zscore = ((compare$n_patients_ligne / compare$n_patients) - 1) / sd(compare$n_patients_ligne / compare$n_patients)
compare$zscore[compare$zscore > 5] <- 5
ggplot(data = compare) +
  geom_point(aes(x= n_patients , y = zscore , col = orgUnit)) +
  guides(col=FALSE) 
  


ggplot(data = compare) +
  geom_point(aes(x=month, y=n_patients_ligne, col= orgUnit, alpha = alpha)) +
  geom_line(aes(x=month, y=n_patients_ligne, col= orgUnit, alpha = .2)) +
  guides(col=FALSE) + facet_wrap(~parent.parent.parent.name, scales = 'free_y')


## Making rolling average
compare$month <- as.Date(compare$month)

make_moving_average <- function(data){
  dat <- subset(data, select= c(n_patients_ligne,month))
  dat <- zoo(dat$n_patients_ligne, dat$month)
  rollmean(dat, 3, fill = NA, align = "right")
}







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
    print(value1)
    value2 <- data2$value[data2$month == period_i]
    if (length(value2) == 0){value2 <- NA}
    print(value2)
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
  msdata <- data.frame('periods'=periods,'values'=values, 'source'=source, 'expected' = expecteds , 
                    #'value_1' = data1$value[data1$month == periods], 
                    #'value_2' = data2$value[data2$month == periods],
                    'comment'= comment)
  msdata$value_1[msdata$period == periods]<- data1$value[data1$month == periods]
  msdata$value_2[msdata$period == periods]<- data2$value[data2$month == periods]
  return(msdata )
}

## pb 1: 0 still taken in the sliding averages
## pb 2: 0 not managed in beginning of periods

## Standardizing data sources and making unique data frame
serie_data_1 <- subset(compare , select=c('month', 'orgUnit','n_patients', 'parent.parent.parent.name'))
serie_data_2 <- subset(compare , select=c('month', 'orgUnit','n_patients_ligne', 'parent.parent.parent.name'))
colnames(serie_data_1) <- colnames(serie_data_2) <- c('month', 'orgUnit', 'value', 'province')
serie_data_1$source <- 'total'
serie_data_2$source <-'by line'

full_data <- rbind(serie_data_1, serie_data_2)

serying <- function(data){
  data1 <- data[data$source == 'total',]
  data2 <- data[data$source == 'by line',]
  print('making a serie')
  out <- make_serie(data1, data2)
  return(out)
}

## Building the full series
completed_data <- function(data){
  data<- full_data %>% group_by(.,orgUnit) %>% do(serying(.))
  return(data)
}

## Building the full series
#completed_data <- full_data %>% group_by(.,orgUnit) %>% do(serying(.))

## Making a plotable df
to_plot <- merge(completed_data, M_hierarchy, by.x = 'orgUnit' , by.y = 'id', all.y = FALSE)

## Plotting

cols <- c("Declared Patients"="#e31a1c","Treatment Lines"="#1f78b4","Expectation"="#33a02c", "Final Values"="#000000")



## A sample for troubleshooting




plotfunction<- function(toplot, sample){
sample <- sample(unique(to_plot$orgUnit),size = 16)
toplot=ggplot(to_plot[to_plot$orgUnit %in% sample, ])+
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
    #ylim(0,max(dat_plot[,c('value_1','value_2','expected','values')])) +
    theme_bw() +
    theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
    theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
    ylab("N Patients") + xlab("Year 2017")  +
    guides(shape=guide_legend(title="Comment"))
return(toplot)
} 
plotfunction(msdata, sample)

ggplot(to_plot[to_plot$orgUnit %in% sample, ])+
  geom_line(aes(x= periods, y=values), alpha=.5)+
  geom_point(aes(x= periods, y=values, color=source, shape=comment))+
  facet_wrap(~name, scales='free_y')



to_plot$source <- factor(to_plot$source,levels = c('total', 'by line','estimation'), ordered = TRUE)
ggplot(to_plot)+
  geom_point(aes(x=periods, y=values, colour= orgUnit, shape=source), size = 1.2) +
  geom_line(aes(x=periods, y=values, col= orgUnit), alpha = .2) +
  guides(col=FALSE, alpha = FALSE) + facet_wrap(~level_2_name, scales = 'free_y')

pdf_plot <- function(p, plots.pdf, dir0){
pdf(paste(dir0, "plots.pdf"), onefile = TRUE)
  for( i in unique(to_plot$orgUnit)){
  dat_plot <- to_plot[to_plot$orgUnit == i, ]
  p<- ggplot(dat_plot)+
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
}
dev.off()
pdf_plot(msdata, plots.pdf, data_dir)



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
  write.csv(export, paste(dir0, "file_name"))
  
  return(export)
}
exported(completed_data, dir0 = data_dir, file_name)


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


## Building the full series


serying <- function(data){
  data1 <- data[data$source == 'cordaid',]
  data2 <- data[data$source == 'pnls',]
  print('making a serie')
  out <- make_serie(data1, data2)
  return(out)
}
completed_data <- full_data %>% group_by(.,orgUnit) %>% do(serying(.))

## Making a plotable df
to_plot <- merge(completed_data, M_hierarchy, by.x = 'orgUnit' , by.y = 'id', all.y = FALSE)

## Plotting
cols <- c("Cordaid"="#e31a1c","PNLS"="#1f78b4","Expectation"="#33a02c", "Final Values"="#000000")



## A sample for troubleshooting
sample <- sample(unique(to_plot$orgUnit),size = 16)
ggplot(to_plot[to_plot$orgUnit %in% sample, ])+
  geom_point(aes(x=periods, y=value_1, colour= 'Cordaid') , alpha=.5) +
  geom_line(aes(x=periods, y=value_1, colour= 'Cordaid'), alpha=.5)+    
  geom_point(aes(x=periods, y=value_2, colour= 'PNLS'), alpha=.5) +
  geom_line(aes(x=periods, y=value_2, colour= 'PNLS'), alpha=.5)+
  geom_point(aes(x=periods, y=expected, colour= 'Expectation'), alpha=.5) +
  geom_line(aes(x=periods, y=expected, colour= 'Expectation'), alpha=.5)+
  geom_point(aes(x=periods, y=values, colour="Final Values", shape=source), size = 2) +
  geom_line(aes(x=periods, y = values, colour="Final Values")) +
  scale_colour_manual(name="Source", values=cols) +
  guides(alpha = FALSE)+
  facet_wrap(~name, scales = 'free_y') +
  #ylim(0,max(dat_plot[,c('value_1','value_2','expected','values')])) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
  ylab("N Patients") + xlab("Year 2017")  +
  guides(shape=guide_legend(title="Comment"))

ggplot(to_plot[to_plot$orgUnit %in% sample, ])+
  geom_line(aes(x= periods, y=values))+
  geom_point(aes(x= periods, y=values, color=source, shape=comment))+
  facet_wrap(~name, scales='free_y')


exported(completed_data, dir0 = data_dir)




#export_1 <- completed_data[c('orgUnit','periods','values')]
#export_2 <-  completed_data[c('orgUnit','periods','source')]
#export_3 <-  completed_data[c('orgUnit','periods','comment')]
#colnames(export_1) <- colnames(export_2) <- colnames(export_3) <- c('OU_id','Period','data_value')
#export_1$data_value <- as.character(export_1$data_value)
#export_1$DE_id <- 'ACCEPTED_VALUE'
#export_2$DE_id <- 'ACCEPTED_SOURCE'
#export_3$DE_id <- 'DATA_VALUE_COMMENT'
#export <- rbind(as.data.frame(export_1), as.data.frame(export_2), as.data.frame(export_3))
#write.csv(export, 'export_to_dataviz_pnls_cordaid.csv')



library(ggplot2)
library(zoo)
library(dplyr)
library(data.table)


### working directory for Data and Metadata
setwd('C:/Users/Saliou/Documents/consultant/BlueSquare/hivdr/hivdr/')
data_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/sources/'
metadata_dir <- 'C:/Users/Saliou/Documents/consultant/BlueSquare/metadata/'

#data_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/' 
#metadata_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/metadata/' 

### Load functions

source("patient_data.R")

### Load Data
cordaid <- readRDS(paste0(data_dir, 'Cordaid_TRAITEMENTS.rds'))
pnls <- readRDS(paste0(data_dir, 'PNLS.rds'))

### Load Metadata
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

## Standardizing data sources and making unique data frame
serie_data_1 <- subset(compare , select=c('month', 'orgUnit','n_patients', 'parent.parent.parent.name'))
serie_data_2 <- subset(compare , select=c('month', 'orgUnit','n_patients_ligne', 'parent.parent.parent.name'))
colnames(serie_data_1) <- colnames(serie_data_2) <- c('month', 'orgUnit', 'value', 'province')
serie_data_1$source <- 'total'
serie_data_2$source <-'by line'

full_data <- rbind(serie_data_1, serie_data_2)

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

library(ggplot2)
library(zoo)
library(dplyr)
library(data.table)
library(dhisextractr)

load_env()

### Load Metadata
load_metadata(metadata_dir)

### Load Data
cordaid <- readRDS(paste0(data_dir, 'Cordaid_TRAITEMENTS.rds'))
cordaid = merge(cordaid, M_org_units, by.x = 'orgUnit', by.y='id', all.y = FALSE)
pnls <- readRDS(paste0(data_dir, 'PNLS.rds'))
pnls = merge(pnls, M_org_units, by.x = 'orgUnit', by.y='id', all.y = FALSE)



## Questions :
# Validation
# chained reconciliation (lines / patients => cordaid) or 3 steps ?
# reorganise repos by blocks / projects / pipelines
# capitalisation results => blogs / docs

### Load functions
source("patient_data.R")

### Format periods as months
cordaid$month <- period_monthly(cordaid$period)
pnls$month <- period_monthly(pnls$period)

## Build common data set
common_set <- build_common_set(cordaid, pnls, 'cordaid', 'pnls')

## Declare important data sets
ancient_cat_option <- 'vZ6Os4BJvum'
ancient_cat_combo <- M_category_combos$CatComboOpt_id[M_category_combos$CatOpt_id.1 == ancient_cat_option]

## Select and build reported data on current patients in cordaid

DE_on_treatment <- 'Yj8caUQs178'

cordaid_sous_traitement <- data.table(subset(cordaid, dataElement == DE_on_treatment & 
                                               categoryOptionCombo %in% cat_comb_ancien))
cordaid_sous_traitement <- cordaid_sous_traitement[, .(n_patients = sum(value)),
                                                   by=c('orgUnit',"month",
                                                        "parent.parent.parent.name", "parent.parent.parent.id", 
                                                        "parent.parent.name", "parent.parent.id")]

## Select and build reported data on current patients on each line in cordaid
cordaid_traitements_id <- M_data_sets$DE_id[grep('\\+|(à préciser)' , 
                                                 M_data_sets$DE_name[M_data_sets$DE_id %in% cordaid$dataElement])]
cordaid_traitements <- data.table(subset(cordaid, dataElement %in% cordaid_traitements_id & 
                                           categoryOptionCombo %in% cat_comb_ancien))
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

## Standardizing data sources and making unique data frame
serie_data_1 <- subset(compare , select=c('month', 'orgUnit','n_patients', 'parent.parent.parent.name'))
serie_data_2 <- subset(compare , select=c('month', 'orgUnit','n_patients_ligne', 'parent.parent.parent.name'))
colnames(serie_data_1) <- colnames(serie_data_2) <- c('month', 'orgUnit', 'value', 'province')
serie_data_1$source <- 'total'
serie_data_2$source <-'by line'

cordaid_patients_lines <- rbind(serie_data_1, serie_data_2)

## Plotting parameter
serie1 <-'Declared Patients'
serie2 <-'Treatment Lines'
cols1 <- c("Declared Patients"="#e31a1c","Treatment Lines"="#1f78b4","Expectation"="#33a02c", "Preferred serie"="#000000")
title_series<-'total numbers of patients currently on ART'


##call functions
completed_data_cordaid<-completed_data(cordaid_patients_lines, 'total', 'by line')
plot_sample_completed(completed_data_cordaid, sample_size = 25, title_series)
pdf_plot(completed_data_cordaid, plots.pdf = 'cordaid_compare.pdf', dir0='', title_series)
f_plot(completed_data_cordaid, 25, title_series)
exported_month(completed_data_cordaid, dir0='', file_name='export_cordaid.csv')
exported_quarter(completed_data_cordaid, dir0='', file_name='export_cordaid_bis.csv')


###########################################################################
###########################################################################
##------ COMPARE PNLS AND CORDAID : total numbers currently on ART-------##
###########################################################################
###########################################################################

# series with total numbers currently on ART
cordaid_id <- 'Yj8caUQs178'
pnls_id <- 'Dd2G5zI0o0a'

## Select correponding CatCombos
cat_comb_ancien <- M_category_combos$CatComboOpt_id[M_category_combos$CatOpt_id.1 %in% 
                                                      c('vZ6Os4BJvum','ggod3chlUCG')]

## Select correponding DataElement
total_arv <- common_set[(common_set$dataElement %in% c(cordaid_id, pnls_id)) & 
                          (common_set$categoryOptionCombo %in% cat_comb_ancien),]

## aggregate values of each DE by period and OrgUnit
total_arv <- total_arv %>% group_by(period, orgUnit, source) %>% summarize('value' = sum(value))
total_arv$month <- period_monthly(total_arv$period)

##call functions
completed_data_cordaid_pnls_total <- completed_data(total_arv, 'cordaid', 'pnls')
plot_sample_completed(completed_data_cordaid_pnls_total, sample_size = 25, title_series)
f_plot(completed_data_cordaid_pnls_total, sample_size = 25, title_series)
pdf_plot(completed_data_cordaid_pnls_total, plots.pdf = 'cordaid_pnls_totalART.pdf', dir0='',title_series)


###########################################################################
###########################################################################
##------------- COMPARE PNLS AND CORDAID BY TREATMENT LINES--------------##
###########################################################################
###########################################################################

#Find DE of interest
#cordaid=TDF+3TC+EFV
tdf_3tc_efv_cordaid <- 'iOW0K3mjoxe'
tdf_3tc_efv_pnls <- 'hKZX17adANF'

#TDF+3TC+NVP
tdf_3tc_nvp_cordaid <- 'lqA1LfMt9C6'
tdf_3tc_nvp_pnls <- 'KJ7qFDiAftt'

#AZT+3TC+NVP
azt_3tc_nvp_cordaid <- 'wrRjiD0fz8j'
azt_3tc_nvp_pnls <-'aozs6mB8T8n'

## Select correponding DataElement
cordaid_line <- cordaid_pnls[(cordaid_pnls$dataElement == cordaid_id),]
pnls_line <- pnls_cordaid[(pnls_cordaid$dataElement == pnls_id),]

## aggregate values of each DE by period and OrgUnit
cordaid_line <- cordaid_line %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))
pnls_line <- pnls_line %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))

##call functions
completed_data_cordaid_pnls<-completed_data(full_data, 'cordaid', 'pnls')
plot_sample_completed(completed_data_cordaid_pnls, sample_size = 25, title_series)
f_plot(completed_data_cordaid_pnls, sample_size = 25, title_series)

adhoc_period <- data.frame('month' = unique(completed_data_cordaid_pnls_total$periods),
                           'periods' = seq(201701,201711))

completed_data_cordaid_pnls_ah <- merge(adhoc_period, completed_data_cordaid_pnls_total,by.x = 'month', by.y='periods')

completed_data_cordaid_pnls<-completed_data(total_arv, 'cordaid', 'pnls')

completed_data_cordaid_pnls <- completed_data_cordaid_pnls %>% group_by(orgUnit) %>% do(predict_missing(.))

########
## Make imputation for single series

cordaid_exp <- subset(cordaid, dataElement==cordaid_id ,  
                      c(dataElement, period, orgUnit, categoryOptionCombo, attributeOptionCombo,value)  )
pnls_exp <- subset(pnls, dataElement==pnls_id & !(orgUnit %in% out$OU_id)& 
                   c(dataElement, period, orgUnit, categoryOptionCombo, attributeOptionCombo,value))


cordaid_full <- cordaid_exp %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))
pnls_full <- pnls_exp %>% group_by(period, orgUnit) %>% summarize('value' = sum(value))

cordaid_full <- make_full_unique_set(cordaid_full)
pnls_full <- make_full_unique_set(pnls_full)

cordaid_full$source <- 'cordaid'
pnls_full$source <- 'pnls'

## IMPUTATION CORDAID

cordaid_imputed <- cordaid_full %>% group_by(orgUnit) %>% do(predict_missing(.))

## IMPUTATION PNLS

pnls_imputed <- pnls_full %>% group_by(orgUnit) %>% do(predict_missing(.))

####

d_out <- rbind(completed_data_cordaid_pnls[,c('periods','orgUnit','value','source','comment')],
               cordaid_imputed[,c('periods','orgUnit','value','source','comment')],
               pnls_imputed[,c('periods','orgUnit','value','source','comment')])

q_out <- to_quarterly(d_out)

to_write <- format_for_export(q_out)

write.csv(to_write, 'data_full_TDF3TCEFV.csv', row.names = FALSE)
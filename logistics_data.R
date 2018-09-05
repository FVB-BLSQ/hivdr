library(ggplot2)
library(zoo)
library(dplyr)
library(data.table)

data_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/' 
metadata_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/metadata/' 

### Load Data
cordaid <- readRDS(paste0(data_dir, 'Cordaid_TRAITEMENTS.rds'))
pnls <- readRDS(paste0(data_dir, 'PNLS.rds'))
sigl <- readRDS(paste0(data_dir, 'SIGL2.rds'))


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

de_snis <- read.csv('/Users/grlurton/data/dhis/rdc/snis/data_elements_list.csv')

M_data_sets[M_data_sets$DE_id %in% pnls$dataElement,][grep('TDF', M_data_sets[M_data_sets$DE_id %in% pnls$dataElement,])]
de_snis[de_snis$id %in% sigl$dataElement,]

M_data_sets[grep('PNLS-DRUG', M_data_sets$DE_name),]

drugs_pnls <- M_data_sets[M_data_sets$categoryCombo.id == 'Q3ONIkE9JN5',]
pnls_drugs <- pnls[pnls$dataElement %in% drugs_pnls$DE_id, ]

pnls_drugs_cc <- merge(pnls_drugs, M_category_combos[,c('CatComboOpt_id', 'CatOpt_name.1')], by.x ='categoryOptionCombo', by.y = 'CatComboOpt_id'  )
pnls_drugs <- merge(pnls_drugs_cc, M_hierarchy[, c('id','name','level_2_name','level_3_name')], by.x =  'orgUnit', by.y = 'id')
pnls_drugs <- merge(pnls_drugs, M_data_sets[,c('DE_name', 'DE_id')], by.x = 'dataElement', by.y = 'DE_id')

library(ggplot2)

drugs <- c("PNLS-DRUG-TDF/3TC/EFV(300/300/600 mg) - 30 ces") #,"PNLS-DRUG-AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces"                                     

pnls_drugs <- pnls_drugs[pnls_drugs$DE_name %in% drugs ,]

pnls_drugs$period <- as.Date(paste0(pnls_drugs$period, '28'), format = "%Y%m%d")
ggplot(pnls_drugs[pnls_drugs$CatOpt_name.1 == 'Stock disponible utilisable ',])+
  geom_line(aes(x=period, y = value, col = DE_name, group=orgUnit), alpha=.3)+
  #facet_grid(level_2_name~DE_name, scales = 'free_y')
  facet_wrap(~level_2_name, scales = 'free_y')


# cmm moyenne mobile
conso <- pnls_drugs %>% filter(CatOpt_name.1 == 'Sortie ')

conso_rm <- conso %>% group_by(name, DE_name) %>% select(period, value) %>% mutate(cmm = rollmean(value, k = 3, align = 'left', fill=NA))

sample_plot <- sample(unique(conso_rm$name), size = 4)
ggplot(conso_rm[conso_rm$name %in% sample_plot , ])+
  geom_point(aes(x = period, y = value)) +
  geom_line(aes(x = period, y = value))+
  geom_line(aes(x=period, y=cmm), col='red')+
  facet_grid( name ~ DE_name, scales = 'free_y')

large <- dcast(pnls_drugs, name + DE_name + period ~ CatOpt_name.1, fun=mean)
colnames(large) <- gsub(' ', '_',trimws(colnames(large)))

large_cmm <- merge(large, conso_rm, all.x=TRUE , by = c('name','DE_name','period'))
large_cmm$alert[large_cmm$Stock_disponible_utilisable <= large_cmm$cmm] <- 'Insufficient Stocks'
large_cmm$alert[large_cmm$Stock_disponible_utilisable > large_cmm$cmm] <- 'Sufficient Stocks'
large_cmm$alert[is.na(large_cmm$alert)] <- 'Insufficient Info'
large_cmm$rupture[(large_cmm$Nbr_de_jours_RS > 0) & !is.na(large_cmm$Nbr_de_jours_RS)] <- large_cmm$period[large_cmm$Nbr_de_jours_RS > 0 & !is.na(large_cmm$Nbr_de_jours_RS)]

sample_plot <- sample(unique(large_cmm$name), size = 12)

# predict stock out time 
# 1. raw : juste stock and CMM based on raw data

cols <- c("Insufficient Stocks"="#e31a1c","Sufficient Stocks"='green',"Expectation"="#1f78b4", "Insufficient Info"="#000000") #"#33a02c"
ggplot(large_cmm[large_cmm$name %in% sample_plot , ])+
  geom_point(aes(x = period, y = Stock_disponible_utilisable, col = alert)) +
  geom_line(aes(x=period, y=Stock_disponible_utilisable), col='black', alpha=.5)+
  geom_line(aes(x=period, y=cmm), col='orange', size=1.5)+
  geom_vline(aes(xintercept=rupture), col='red', size=1)+
  facet_grid( name ~ DE_name, scales = 'free_y')+
  scale_colour_manual(name="Stock Watch", values=cols)+
  theme_bw()


# 2. completed : stock and CMM based on modelled data

tdf_ftc_efv <- c('TDF+3TC+EFV', 'PNLS-DRUG-TDF+3TC+EFV sex', 'PNLS-DRUG-TDF+3TC+EFV')
tdf_ftc_efv_data <- pnls[pnls$dataElement %in% M_data_sets$DE_id[M_data_sets$DE_name %in% tdf_ftc_efv] ,]

tdf_ftc_efv_data$period <- as.Date(paste0(tdf_ftc_efv_data$period, '28'), format = "%Y%m%d")
patient_data <- tdf_ftc_efv_data %>% group_by(orgUnit, period) %>% summarise(n_patients = sum(value))

drug_data <- dcast(pnls_drugs, orgUnit + name + period ~ CatOpt_name.1, fun=mean)
colnames(drug_data) <- gsub(' ', '_',trimws(colnames(drug_data)))

full_data <- full_join(patient_data, drug_data, by= c('orgUnit','period'))

#full_data<- full_data[full_data$`Sortie ` < 150000,]

plot(data_test$n_patients, data_test$Sortie)

make_ratio <- function(data){
  data <- data[!is.na(data$n_patients),]
  ratio <- median(data$Sortie/data$n_patients, na.rm = TRUE)
  return(ratio)
}

fosa_ratio <- full_data %>% group_by(orgUnit) %>% do(data.frame(ratio = make_ratio(.)))

data_test <- full_data[full_data$orgUnit %in% fosa_ratio$orgUnit[fosa_ratio$ratio < 2],]

full_framework <- melt(dcast(data_test[,c('orgUnit','period')], orgUnit~period), variable.name = "period")

full_framework$period <- as.Date(full_framework$period)

data_test <- full_join(data_test, full_framework[,c('orgUnit','period')])


dat <- data_test[data_test$orgUnit == 'SIqwiCBwiL5',]

logisitics_path <- function(data, periods){
  cmm <- NA
  cmm_trace <- c()
  stock <- c()
  stock_comment <- c()
  sortie <- c()
  sortie_comment <- c()
  for( period in periods){
    period_data <- data[data$period == period,]
  ## Manage Sorties
    sortie_p <- period_data$Sortie
    sortie_c <- 'declared sortie'
    #if(is.na(sortie_p)){
    #  sortie_p <- period_data$n_patients
    #  sortie_c <- 'declared patients'
    #}
    if(is.na(sortie_p)){
      sortie_p <- cmm
      sortie_c <- 'CMM'
    }
    sortie <- c(sortie, sortie_p)
    sortie_comment <- c(sortie_comment, sortie_c)
    win_open <- max(1, length(sortie)-2)
    cmm <- mean(sortie[win_open:length(sortie)], na.rm=TRUE)
    cmm_trace <- c(cmm_trace, cmm)
  
  
    ## Manage Stock
    stock_p <- period_data$Stock_disponible_utilisable
    stock_i <- period_data$Stock_Initial
    stock_c <- 'declared_stock'
    if(is.na(stock_i)){
      stock_i <- stock[length(stock)]
      stock_c <- 'computed stock'
    }
    if(is.na(stock_p)){
      stock_p <- stock_i - sortie_p
      stock_c <- 'computed stock'
    }
    stock <- c(stock, max(0,stock_p))
    stock_comment <- c(stock_comment, stock_c)
  }
  out <- data.frame(stock, stock_comment, cmm_trace, sortie, sortie_comment, 'period'=periods)
  return(out)
}

out <- data_test %>% group_by(orgUnit) %>% do(logisitics_path(., sort(unique(data_test$period))))


stock_to_plot <- dcast(out[out$orgUnit == 'SIqwiCBwiL5',], period ~ stock_comment, value.var = 'stock')
sortie_to_plot <- dcast(out[out$orgUnit == 'SIqwiCBwiL5',], period ~ sortie_comment, value.var = 'sortie')

ggplot(out[out$orgUnit == 'SIqwiCBwiL5',])+
  geom_line(aes(period, cmm_trace), col='orange', alpha = .4) +
  #geom_point(data=stock_to_plot, aes(period, `computed stock`), col='red')+
  geom_line(data=stock_to_plot, aes(period, declared_stock), col='red') +
  geom_line(data=out[out$orgUnit == 'SIqwiCBwiL5',], aes(period, stock), col='red', alpha = .2) +
 # geom_point(data=sortie_to_plot, aes(period, `declared patients`), col='blue')+
  #geom_point(data=sortie_to_plot, aes(period, `CMM`), col='blue', shape=17, size = 2)+
  geom_line(data=sortie_to_plot, aes(period, `declared sortie`), col='blue') +
  geom_line(data=out[out$orgUnit == 'SIqwiCBwiL5',], aes(period, sortie), col='blue', alpha = .2)
  
export_logistics <- function(completed_data, dir0, file_name){
  completed_data$period <- paste0(substr(completed_data$period,1,4),substr(completed_data$period,6,7))
  stock <- data.frame(completed_data[,c('orgUnit','period','stock')], c('STOCK'))
  stock_comment <- data.frame(completed_data[,c('orgUnit','period','stock_comment')], c('STOCK_COMMENT'))
  cmm <- data.frame(completed_data[,c('orgUnit','period','cmm_trace')], c('CMM'))
  sorties <- data.frame(completed_data[,c('orgUnit','period','sortie')], c('SORTIES'))
  sorties_comment <- data.frame(completed_data[,c('orgUnit','period','sortie_comment')], c('SORTIES_COMMENT'))
  colnames(stock) <- colnames(stock_comment) <- colnames(cmm) <- colnames(sorties)<- colnames(sorties_comment) <- c('OU_id','period','data_value','DE_id')
  out <- rbind(stock, stock_comment, cmm, sorties, sorties_comment)
  out['catoptcombo'] <- 'HllvX50cXC0'
  out['attroptcombo'] <- NA
  out <- out[,c("DE_id","period","OU_id","catoptcombo","attroptcombo","data_value")]
  write.csv(out, paste(dir0, file_name, sep='/'), row.names = FALSE)
  return(out)
}

export_logistics(out,  getwd(), 'logistics_tdf_ftc_efv.csv')

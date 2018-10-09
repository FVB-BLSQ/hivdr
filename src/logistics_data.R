library(ggplot2)
library(zoo)
library(dplyr)
library(data.table)
library(dhisextractr)

load_env()
load_metadata(metadata_dir)

### Load Data
cordaid <- readRDS(paste0(data_dir, 'Cordaid_TRAITEMENTS.rds'))
pnls <- readRDS(paste0(data_dir, 'PNLS.rds'))
### Load functions

M_data_sets[M_data_sets$DE_id %in% pnls$dataElement,][grep('TDF', M_data_sets[M_data_sets$DE_id %in% pnls$dataElement,])]de_snis[de_snis$id %in% sigl$dataElement,]

M_data_sets[grep('PNLS-DRUG', M_data_sets$DE_name),]

drugs_pnls <- M_data_sets[M_data_sets$categoryCombo.id == 'Q3ONIkE9JN5',]
pnls_drugs <- pnls[pnls$dataElement %in% drugs_pnls$DE_id, ]

pnls_drugs_cc <- merge(pnls_drugs, M_category_combos[,c('CatComboOpt_id', 'CatOpt_name.1')], by.x ='categoryOptionCombo', by.y = 'CatComboOpt_id'  )
pnls_drugs <- merge(pnls_drugs_cc, M_hierarchy[, c('id','name','level_2_name','level_3_name')], by.x =  'orgUnit', by.y = 'id')
pnls_drugs <- merge(pnls_drugs, M_data_sets[,c('DE_name', 'DE_id')], by.x = 'dataElement', by.y = 'DE_id')

library(ggplot2)

#drugs <- c("PNLS-DRUG-TDF/3TC/EFV(300/300/600 mg) - 30 ces") #,"PNLS-DRUG-AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces"                                     
drugs <- c("PNLS-DRUG-AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces","PNLS-DRUG-AZT/3TC/NVP(300/150/200 mg)",
           "PNLS-DRUG-TDF/3TC/EFV(300/300/600 mg)","PNLS-DRUG-TDF/3TC/EFV(300/300/600 mg) - 30 ces")


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
tdf_ftc_efv <- c('AZT+3TC+NVP', 'PNLS-DRUG-AZT+3TC+NVP', 'PNLS-DRUG-AZT/3TC/NVP(300/150/200 mg) - 60 ces')


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

data_test$Sortie[data_test$Sortie == 0] <- NA
data_test$Stock_disponible_utilisable[data_test$Stock_disponible_utilisable == 0] <- NA



out <- data_test %>% group_by(orgUnit) %>% do(logisitics_path(., sort(unique(data_test$period))))
out[out$stock_comment == 'computed stock' & !is.na(out$stock) & out$stock > 0,] %>% group_by(orgUnit) %>% do(data.frame(nrow(.)))

p <- out[out$stock_comment == 'computed stock' & !is.na(out$stock) & out$stock > 0 & out$sortie > 100,] %>% group_by(orgUnit) %>% do(data.frame(nrow(.)))

p[order(p$nrow..., decreasing = TRUE),]

p[p$nrow... ==  2,]

id <- 'LC8yuswy9SX'
stock_to_plot <- dcast(out[out$orgUnit == id,], period ~ stock_comment, value.var = 'stock')
sortie_to_plot <- dcast(out[out$orgUnit == id,], period ~ sortie_comment, value.var = 'sortie')

stock_to_plot$declared_stock[stock_to_plot$declared_stock == 0] <- NA
sortie_to_plot$`declared sortie`[sortie_to_plot$`declared sortie` == 0] <- NA


cols1 <- c("Stocks"="#e31a1c","Sorties"="#1f78b4","CMM"="#33a02c", "Preferred serie"="#000000")
shapes <- c("Data"=0,"Computed"=2)

out$rupture <- NA
out$rupture[out$stock == 0& !is.na(out$stock)] <- out$period[out$stock == 0 & !is.na(out$stock)]

ggplot(out[out$orgUnit == id,])+
  geom_line(aes(period, cmm_trace,col="CMM"), alpha = .4) +
  geom_point(aes(period, cmm_trace,col="CMM", shape="Computed"), alpha = .4) +
  geom_point(data=stock_to_plot, aes(period, `computed stock`, colour='Stocks', shape="Computed"))+
  geom_point(data=stock_to_plot, aes(period, declared_stock, colour='Stocks'), size =1.4) +
  geom_line(data=stock_to_plot, aes(period, declared_stock, colour='Stocks'), alpha= .2) +
  geom_line(data=out[out$orgUnit == id,], aes(period, stock), col='red', alpha = .2) +
 # geom_point(data=sortie_to_plot, aes(period, `declared patients`), col='blue')+
  #geom_point(data=sortie_to_plot, aes(period, `CMM`), col='blue', shape=17, size = 2)+
  geom_point(data=sortie_to_plot, aes(period, `declared sortie`, col='Sorties')) +
  geom_line(data=sortie_to_plot, aes(period,  `declared sortie`, col='Sorties'), alpha = .2) +
  scale_colour_manual(name="Series", values = cols1)+
  scale_shape_manual(name="Source", values = shapes)+ 
  geom_vline(aes(xintercept=rupture), col='red', size=1)+
  theme_bw()# +
  #ylim(c(0,25)) +
  #labs(title ="El'Shaddai Centre de Santé")

format_period <- function(completed_data){
  completed_data$period <- paste0(substr(completed_data$period,1,4),substr(completed_data$period,6,7))
  return(completed_data)
}
  
out <- format_period(out)

make_quarterly <- function(out){
  outq <- subset(out, period %in% c('201703', '201706', '201709', '201711'))
  outq$stockout_perc <- NA
  outq$stockout_perc[outq$period == '201703'] <- sum(out$stock[out$period %in% c('201701','201702','201703')] == 0, na.rm=TRUE) / 3#sum(!is.na(out$stock[out$period %in% c('201701','201702','201703')] ))
  outq$stockout_perc[outq$period == '201706'] <- sum(out$stock[out$period %in% c('201704','201705','201706')] == 0, na.rm=TRUE) / 3#sum(!is.na(out$stock[out$period %in% c('201704','201705','201706')] ))
  outq$stockout_perc[outq$period == '201709'] <- sum(out$stock[out$period %in% c('201707','201708','201709')] == 0, na.rm=TRUE) / 3#sum(!is.na(out$stock[out$period %in% c('201707','201708','201709')] ))
  outq$stockout_perc[outq$period == '201711'] <- sum(out$stock[out$period %in% c('201710','201711','201711')] == 0, na.rm=TRUE) / 3#sum(!is.na(out$stock[out$period %in% c('201710','201711','201712')] ))
  print('to Period')
  outq$period[outq$period == '201703'] <- '2017Q1'
  outq$period[outq$period == '201706'] <- '2017Q2'
  outq$period[outq$period == '201709'] <- '2017Q3'
  outq$period[outq$period == '201711'] <- '2017Q4'
  return(outq)
}

out_q <- out %>% group_by(orgUnit) %>% do(make_quarterly(.))

export_logistics <- function(completed_data, dir0, file_name){
  stock <- data.frame(completed_data[,c('orgUnit','period','stock')], c('STOCK'))
  stock_comment <- data.frame(completed_data[,c('orgUnit','period','stock_comment')], c('STOCK_COMMENT'))
  cmm <- data.frame(completed_data[,c('orgUnit','period','cmm_trace')], c('CMM'))
  sorties <- data.frame(completed_data[,c('orgUnit','period','sortie')], c('SORTIES'))
  sorties_comment <- data.frame(completed_data[,c('orgUnit','period','sortie_comment')], c('SORTIES_COMMENT'))
  colnames(stock) <- colnames(stock_comment) <- colnames(cmm) <- colnames(sorties)<- colnames(sorties_comment) <- c('OU_id','period','data_value','DE_id')
  out <- rbind(stock, stock_comment, cmm, sorties, sorties_comment)
  if(('stockout_perc' %in% colnames(completed_data)) == TRUE){
    stockout_perc <- data.frame(completed_data[,c('orgUnit','period','stockout_perc')], c('STOCKOUT_PERC'))
    colnames(stockout_perc) <- c('OU_id','period','data_value','DE_id')
    out <- rbind(out, stockout_perc)
  }
  out['catoptcombo'] <- 'HllvX50cXC0'
  out['attroptcombo'] <- NA
  out <- out[,c("DE_id","period","OU_id","catoptcombo","attroptcombo","data_value")]
  write.csv(out, paste(dir0, file_name, sep='/'), row.names = FALSE)
  return(out)
}

#out_tdf <- export_logistics(out_q,  getwd(), 'logistics_tdf_ftc_efv.csv')


out_azt <- export_logistics(out_q,  getwd(), 'logistics_azt_3tc_nvp.csv')



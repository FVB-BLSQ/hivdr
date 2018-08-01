#sigl1 <- readRDS("/Users/grlurton/data/dhis/rdc/snis/sigl/SIGL1.rds")
data_dir <- '/Users/grlurton/data/dhis/rdc/hivdr/'
sigl2 <- readRDS(paste0(data_dir, 'SIGL2.rds'))
pnls <- read.csv("/Users/grlurton/data/dhis/rdc/snis/pnls/data.csv")


load_metadata <- function(metadata_dir, suffix = ''){
  env <- globalenv()
  assign(paste0('M_category_combos', suffix) , readRDS(paste0(metadata_dir, 'CC_metadata.rds')), env)
  assign(paste0('M_data_element_group', suffix) , readRDS(paste0(metadata_dir, 'DEG_metadata.rds')), env)
  assign(paste0('M_data_sets', suffix) , readRDS(paste0(metadata_dir, 'DS_metadata.rds')), env)
  assign(paste0('M_org_units', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata_DSinfo.rds')), env)
  assign(paste0('M_hierarchy', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata_flat.rds')), env)
  assign(paste0('M_org_units', suffix) , readRDS(paste0(metadata_dir, 'OU_metadata.rds')), env)
}

load_metadata("/Users/grlurton/data/dhis/rdc/snis/", '_snis')

M_data_sets[M_data_sets$DE_id %in% pnls$dataElement,c('DE_id', 'DE_name')]
M_data_sets[M_data_sets$DE_id %in% sigl2$dataElement,c('DE_id', 'DE_name')]
M_data_sets_snis[M_data_sets_snis$DE_id %in% sigl2$dataElement,c('DE_id', 'DE_name')]

art_data_elements <- M_data_sets[substr(M_data_sets$DE_name, 1,8) %in% c('PNLS-DRU','PNLS-ARV') , ]
art_data_elements <- art_data_elements[!grepl("TB|CTX|INH|Kit|charge|nutri|victimes|indésirables|CD4|décédées|attente|perdues|enrôlées", art_data_elements$DE_name) , ]


sigl_art <- sigl2[sigl2$dataElement %in% data_art$DE_id,]
pnls_art <- pnls[pnls$dataElement %in% art_data_elements$DE_id,]


sigl_art <- merge(sigl2, M_data_sets_snis , all.y = FALSE , by.x = 'dataElement' , by.y = 'DE_id')
sigl_art <- sigl_art[grepl('ABC|AZT',sigl_art$DE_name), ]



sigl_art[grep('Stock disponible utilisable' , sigl_art$DE_name), 'type'] <- 'stock'
sigl_art[grep('Quantité consommée' , sigl_art$DE_name), 'type'] <- 'consommation'
sigl_art[grep('Quantité perdue' , sigl_art$DE_name), 'type'] <- 'perte'
sigl_art[grep('Jours rupture de stock' , sigl_art$DE_name), 'type'] <- 'rupture'
sigl_art[grep('AZT\\+3TC\\+NVP 60 mg\\+30mg\\+50mg Sirop' , sigl_art$DE_name), 'line'] <- 'azt_3tc_nvp_sirop'
sigl_art[grep('AZT\\+3TC 300mg\\+150mg\\+Cès' , sigl_art$DE_name), 'line'] <- 'azt_3tc_c'
sigl_art[grep('AZT\\+3TC\\+NVP 300mg\\+150mg\\+200mg Cès' , sigl_art$DE_name), 'line'] <- 'azt_3tc_nvp_c'
sigl_art[grep('ABC\\+DDI\\+LPV' , sigl_art$DE_name), 'line'] <- 'abc_ddi_lpv_c'
table(sigl_art$line)
sigl_art$DE_name[sigl_art$line == ""]


sigl_art$line_indicator <- paste(sigl_art$line, sigl_art$type, sep = '-')


library(dplyr)
library(tidyr)

sigl_art %>%  group_by(., period, type) %>% summarize(val=mean(value))

library(ggplot2)


sigl_art_any <- sigl_art[sigl_art$type == 'stock',] %>% group_by(orgUnit) %>% summarize(sum(value))


sigl_art_any <- sigl_art_any[sigl_art_any$`sum(value)` >0 ,]

sigl_art_sample <- sigl_art[sigl_art$orgUnit %in% sigl_art_any$orgUnit, ]

sample <- sigl_art_sample[sigl_art_sample$orgUnit %in% sample(sigl_art_sample$orgUnit, 49) & sigl_art_sample$line !=  'azt_3tc_nvp_sirop',  ]

sample$rupture <- FALSE
sample$rupture[sample$type == 'rupture' & sample$value > 0] <- TRUE

rupture <- subset(sample, rupture == TRUE , select =  c('orgUnit', 'period','rupture','line'))

sample <- sample[sample$type == 'stock', c('line_indicator', 'orgUnit', 'value', 'period','line')]

sample <- merge(sample, rupture , by = c('orgUnit','period','line'), all=TRUE)

sample$rupture[(sample$rupture == TRUE) & !is.na(sample$rupture)] <- sample$value[(sample$rupture == TRUE) & 
                                                                                         !is.na(sample$rupture)]

periods <- unique(pnls$period)
orgUnits <- unique(sample$orgUnit)

exp <- data.frame('period'= rep(periods, length(orgUnits)),
                  'orgUnit' = sort(rep(orgUnits, length(periods)))
                  )

exp$line <- 'abc_ddi_lpv_c'
exp2 <- exp3 <- exp
exp2$line <- 'azt_3tc_c'
exp3$line <- 'azt_3tc_nvp_c'

exp <- rbind(exp, exp2, exp3)

to_plot <- merge(sample, exp , by = c('orgUnit','period', 'line'), all=TRUE)
to_plot$month <- as.Date(zoo::as.yearmon(paste0(substr(to_plot$period, 1, 4), '-', substr(to_plot$period, 5,6))))

ggplot(to_plot) +
  geom_line(aes(x=month, y=value, colour=line)) + 
  geom_point(aes(x=month, y=value, colour=line), shape= 'x', size=2) +
  facet_wrap(~orgUnit, scales = 'free_y') + 
  geom_point(aes(x = month, y = rupture, colour = line, shape = line), alpha=.5 , size = 4)


## Observed CMM


### SIGL

sigl_conso <- sigl_art[sigl_art$type == 'consommation',]

sigl_conso %>% group_by(orgUnit, line) %>% summarize(mean(value))

### PNLS
pnls_art_lab <- merge(pnls_art, art_data_elements)



### CORDAID



## Cross plot with stocks

## Risk evaluation
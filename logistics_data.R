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
sigl_art[grep('AZT+3TC+NVP 60 mg+30mg+50mg Sirop' , sigl_art$DE_name), 'line'] <- 'azt_3tc_nvp_sirop'
sigl_art[grep("ABC+DDI+LPV" , sigl_art$DE_name), 'line'] <- 'abc_ddi_lpv_c'

sigl_art$DE_name[is.na(sigl_art$line)]


sigl_art$line_indicator <- paste(sigl_art$line, sigl_art$type, sep = '-')


library(dplyr)
library(tidyr)

sigl_art %>%  group_by(., period, type) %>% summarize(val=mean(value))


sample <- sigl_art[sigl_art$orgUnit %in% sample(sigl_art$orgUnit, 16) , c('stock') ]

sample <- subset(sample, select= c(DE_name, orgUnit, value, period))

spread(sample, key=DE_name , value=value)

library(ggplot2)
ggplot(sample) +
  geom_line(aes(x=period, y=C2 12.2 AZT+3TC 300mg+150mg+Cès - Stock disponible utilisable, colour=orgUnit))


data_art_2 <- group_by(data_art, orgUnit, type) 

data_art_2 <-  summarize(data_art_2, val=mean(value))


print(data.frame(data_art_2[400:440,]))

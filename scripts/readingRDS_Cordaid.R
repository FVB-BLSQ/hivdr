
#######################################################################################
# HIVDR project in collaboration with Bluesquare                                      #
# DRC data                                                                            #
#######################################################################################
# Last update: 10/08/2018                                                             #
# Author: Jenny Chung (external working for J&J GPH R&D DMP)                          #
#######################################################################################


###################################
# reading in RDS files of Cordaid #
###################################

fileName = 'data/bluesquare/Cordaid/Cordaid_TRAITEMENTS.rds'
Cordaid_treatment <- readRDS(fileName)
Cordaid_treatment$dataElement <- as.character(Cordaid_treatment$dataElement)
Cordaid_treatment$orgUnit <- as.character(Cordaid_treatment$orgUnit)
Cordaid_treatment$categoryOptionCombo <- as.character(Cordaid_treatment$categoryOptionCombo)
Cordaid_treatment$attributeOptionCombo <- as.character(Cordaid_treatment$attributeOptionCombo)
Cordaid_treatment$storedBy <- as.character(Cordaid_treatment$storedBy)
Cordaid_treatment$created <- as.character(Cordaid_treatment$created)
Cordaid_treatment$lastUpdated <- as.character(Cordaid_treatment$lastUpdated)
Cordaid_treatment$followUp <- as.character(Cordaid_treatment$followUp)
Cordaid_treatment$period <- as.character(Cordaid_treatment$period)


fileName = 'data/bluesquare/Cordaid/OU_metadata.rds'
OU_metadata_C <- readRDS(fileName)


fileName = 'data/bluesquare/Cordaid/OU_metadata_flat.rds'
OU_metadata_flat_C <- readRDS(fileName)


fileName = 'data/bluesquare/Cordaid/OU_metadata_DSinfo.rds'
OU_metadata_DSinfo_C <- readRDS(fileName)


fileName = 'data/bluesquare/Cordaid/DS_metadata.rds'
DS_metadata_C <- readRDS(fileName)


fileName = 'data/bluesquare/Cordaid/DEG_metadata.rds'
DEG_metadata_C <- readRDS(fileName)


fileName = 'data/bluesquare/Cordaid/CC_metadata.rds'
CC_metadata_C <- readRDS(fileName)



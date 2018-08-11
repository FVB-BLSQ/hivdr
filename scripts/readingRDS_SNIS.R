
#######################################################################################
# HIVDR project in collaboration with Bluesquare                                      #
# DRC data                                                                            #
#######################################################################################
# Last update: 10/08/2018                                                             #
# Author: Jenny Chung (external working for J&J GPH R&D DMP)                          #
#######################################################################################


################################
# reading in RDS files of SNIS #
################################

fileName = 'data/bluesquare/SNIS/PNLS/PNLS.rds'
PNLS <- readRDS(fileName)
PNLS$dataElement <- as.character(PNLS$dataElement)
PNLS$orgUnit <- as.character(PNLS$orgUnit)
PNLS$categoryOptionCombo <- as.character(PNLS$categoryOptionCombo)
PNLS$attributeOptionCombo <- as.character(PNLS$attributeOptionCombo)
PNLS$storedBy <- as.character(PNLS$storedBy)
PNLS$created <- as.character(PNLS$created)
PNLS$lastUpdated <- as.character(PNLS$lastUpdated)
PNLS$followUp <- as.character(PNLS$followUp)
PNLS$period <- as.character(PNLS$period)


fileName = 'data/bluesquare/SNIS/PNLS/OU_metadata_DSinfo.rds'
OU_metadata_DSinfo <- readRDS(fileName)


fileName = 'data/bluesquare/SNIS/PNLS/OU_metadata.rds'
OU_metadata <- readRDS(fileName)


fileName = 'data/bluesquare/SNIS/PNLS/DS_metadata.rds'
DS_metadata <- readRDS(fileName)


fileName = 'data/bluesquare/SNIS/PNLS/DEG_metadata.rds'
DEG_metadata <- readRDS(fileName)


fileName = 'data/bluesquare/SNIS/PNLS/CC_metadata.rds'
CC_metadata <- readRDS(fileName)


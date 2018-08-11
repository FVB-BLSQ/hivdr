
#######################################################################################
# HIVDR project in collaboration with Bluesquare                                      #
# DRC data                                                                            #
#######################################################################################
# Last update: 10/08/2018                                                             #
# Author: Jenny Chung (external working for J&J GPH R&D DMP)                          #
# Note: reading from the PNLS.RDS                                                     #
#######################################################################################

#######################################################################################
#                                                                                     #
#                      SNIS - PNLS - patients number on regimens                      #    
#                                                                                     #
#######################################################################################


#############
# data files # ----
##############
#source('functions/meanSexTotaux_RDS_f.R')
#source('scripts/readingRDS_SNIS.R')


#######################################
# selecting data elements of interest # ----
#######################################
DEOfInterest <- c(
  ## patients on regimen
  "a51J2lybALi", "lttU7PiCAqg", "Ua57G6vbmMq", "prnCi6GwYzL", "izhw4QcB5sc", "X6ROVclhfqF", "bCOQC3E2Apk", "X2nhX2N5B0B", "KDOLhTayXgY",
  "WTfY6gHxkKe", "ANTg88cSB09", "uIvFJy1ZSMC", "aozs6mB8T8n", "hHahC0WTLlX", "IAYSAoUetMU", "wLBRFksBtou", "KJ7qFDiAftt", "fiEARR6fRnJ",
  "hKZX17adANF", "pidcOIMv6Er", "UvUWHtK9yWm", "fdc1v0PSUZe", "RYbycrshxki", "Q71tylkcq2P")
PNLS2 <- PNLS[which(PNLS$dataElement %in% DEOfInterest),]


############################
# exploring - first glance # ----
############################
PNLS3 <- merge(PNLS2, DS_metadata, by.x = "dataElement", by.y = "DE_id")
rm(PNLS, PNLS2)
PNLS3 <- merge(PNLS3, OU_metadata_DSinfo, by.x = c("orgUnit", "DS_name", "DS_id"), by.y = c("OU_id", "DS_name", "DS_id"), all.x = TRUE)
PNLS3 <- merge(PNLS3, CC_metadata, by.x = c("categoryOptionCombo", "categoryCombo.id"), by.y = c("CatComboOpt_id", "CatCombo_id"), all.x = TRUE)


## selecting relevant columns
#############################
irrelevantCols <- c("X", "storedBy", "created", "lastUpdated", "followUp", "domainType", "zeroIsSignificant",
                    "parent.parent.parent.parent.name", "parent.parent.parent.parent.id")
PNLS4 <- PNLS3[, -which(names(PNLS3) %in% irrelevantCols)]

PNLS4 <- PNLS4[!is.na(PNLS4$OU_level),]
PNLS4$Year <- substr(PNLS4$period, 1, 4)
PNLS4$Month <- substr(PNLS4$period, 5, 6)
PNLS4$CatOpt_name.1 <- ifelse(is.na(PNLS4$CatOpt_name.1), "All", PNLS4$CatOpt_name.1)


## transforming variables
#########################
### age categories
### Note: difficult to categorise "< 25 ans", "Moins de 25 ans "!
PNLS4$ageCategory <- sub("25 ans et plus|25 et 49 ans |25 et 49 ans|50 ans et plus |15 et 19 ans |15 et 19 ans|20 et 24 ans |25 ans et plus |15 et 24 ans |25 - 49 ans|20 - 24 ans|15 - 19 ans |50 ans et plus|20 et 24 ans", 
                         ">=15y", PNLS4$CatOpt_name.2)
PNLS4$ageCategory <- sub("Moins de 15 ans |Moins de 14 ans |10 et 14 ans |10 et 14 ans|5 et 9 ans |5 et 9 ans|Moins d'un an |1 et 4 ans |0 – 14 ans|1 – 4  ans |5 – 9 ans|10 - 14 ans |< 1 an|1 et 4 ans|Moins d'un an", 
                         "<15y", PNLS4$ageCategory)


## Calculations on facility level
#################################
### 1) grouping by gender
#####
PNLS5 <- PNLS4 %>%
  group_by(orgUnit, period, DE_name, ageCategory, CatOpt_name.1) %>%
  mutate(numPatPerGender = sum(value, na.rm = TRUE))
PNLS5$numPatPerGender <- ifelse(is.na(PNLS5$value), NA, PNLS5$numPatPerGender)

    ### making a orgUnit_meta file
    relevantCols <- c("orgUnit", "OU_name", "parent.name", "parent.parent.name", "parent.parent.parent.name")
    orgUnitMetadata_DF <- unique(PNLS5[,which(names(PNLS5) %in% relevantCols)])
    #####

### 2) grouping by age category
#####
PNLS5_b <- PNLS5 %>%
  select(orgUnit, period, DE_name, ageCategory, CatOpt_name.1, numPatPerGender) %>%
  distinct() %>%
  group_by(orgUnit, period, DE_name, ageCategory) %>%
  mutate(numPatPerAgeCat = sum(numPatPerGender, na.rm = TRUE))
PNLS5_b$numPatPerAgeCat <- ifelse(is.na(PNLS5_b$numPatPerGender), NA, PNLS5_b$numPatPerAgeCat)

### 3) grouping by data element
#####
PNLS5_c <- PNLS5_b %>%
  select(orgUnit, period, DE_name, ageCategory, numPatPerAgeCat) %>%
  distinct() %>%
  group_by(orgUnit, period, DE_name) %>%
  mutate(numPatPerDEName = sum(numPatPerAgeCat, na.rm = TRUE))
PNLS5_c$numPatPerDEName <- ifelse(is.na(PNLS5_c$numPatPerAgeCat), NA, PNLS5_c$numPatPerDEName)

### 4) selecting the relevant variables and the grouped value for further steps
#####
PNLS6 <- PNLS5_c %>%
  select(orgUnit, period, DE_name, numPatPerDEName) %>%
  distinct()


## 'Non-Sex' vs 'Sex' columns
#############################
### reading in extra information about the regimens
#####
fileName = "data/bluesquare/SNIS/PNLS/PNLS-DRUG_drug variables_c20180810_JC.csv"
drugsInfo <- read.csv(fileName, header = T, sep = ";", stringsAsFactors = F, encoding = "UTF-8")
drugsInfo$X <- NULL
drugsInfo$PNLS.DRUG_adj <- sub("_sex_Totaux|_Totaux", "", drugsInfo$PNLS.DRUG)
drugsInfo <- drugsInfo %>%
  filter(PNLS.DRUG_RDS != "") %>%
  group_by(PNLS.DRUG_adj) %>%
  mutate(numVar = n()) 


relevantCols <- c("PNLS.DRUG_RDS", "PNLS.DRUG_group", "drugType", "drugFormulation", "backbone_drugClass", "backbone_drug", 
                  "nonBackbone_drugClass", "nonBackbone_drug", "numVar")
drugsInfo <- drugsInfo[, which(names(drugsInfo) %in% relevantCols)]
drugsInfo$PNLS.DRUG_RDS_adj <- paste0(drugsInfo$backbone_drug, "_", drugsInfo$nonBackbone_drug)


### adding extra information to the data
#####
PNLS7 <- merge(PNLS6, drugsInfo, by.x = "DE_name", by.y = "PNLS.DRUG_RDS", all.x = TRUE)


### to investigate whether a regimen has >1 value, and transformation from long to wide format
#####
df_name = PNLS7
drugs_type = "triploDrug"
number_var = 2
meanSexTotaux_RDS_f(dfName = df_name,
                    numberVar = number_var,
                    drugsType = drugs_type)
drugs3_triplo2var <- temp4
rm(df_name, temp4)
drugs3_triplo2var$SD <- as.numeric(drugs3_triplo2var$SD)

df_name = PNLS7
drugs_type = "triploDrug"
number_var = 1
meanSexTotaux_RDS_f(dfName = df_name,
                    numberVar = number_var,
                    drugsType = drugs_type)
drugs3_triplo1var <- temp4

drugs3_triplo <- rbind.fill(drugs3_triplo1var, drugs3_triplo2var)
drugs3_triplo <- merge(drugs3_triplo ,orgUnitMetadata_DF, by = "orgUnit", all.x = TRUE)


###################################################################################
#                          calculations and visualisations                        #
###################################################################################
### Note: there are no indications for new cases, old cases and NA


## makng one value from the 'Totaux' and 'sexTotaux' variables according the following rules ----
############################################################################################
drugs3_triplo$custom <- ifelse(is.na(drugs3_triplo$Totaux), drugs3_triplo$sexTotaux, NA)
drugs3_triplo$custom2 <- ifelse((is.na(drugs3_triplo$custom) & is.na(drugs3_triplo$sexTotaux)), drugs3_triplo$Totaux, drugs3_triplo$custom)
drugs3_triplo$custom3 <- ifelse((is.na(drugs3_triplo$custom2) & drugs3_triplo$Totaux == 0), 
                                drugs3_triplo$sexTotaux, drugs3_triplo$custom2)
drugs3_triplo$custom4 <- ifelse((is.na(drugs3_triplo$custom3) & drugs3_triplo$sexTotaux == 0), 
                                drugs3_triplo$Totaux, drugs3_triplo$custom3)
drugs3_triplo$custom5 <- ifelse((is.na(drugs3_triplo$custom4) & drugs3_triplo$Totaux == drugs3_triplo$sexTotaux), 
                                drugs3_triplo$Totaux, drugs3_triplo$custom4)
drugs3_triplo$custom6 <- ifelse((is.na(drugs3_triplo$custom5) & drugs3_triplo$Totaux != drugs3_triplo$sexTotaux), 
                                apply(drugs3_triplo[, c("Totaux","sexTotaux")], 1, max),
                                drugs3_triplo$custom5)
drugs3_triplo$sexTotaux_Totaux <- drugs3_triplo$custom6
customCols <- which(grepl("*custom*", names(drugs3_triplo)) == TRUE)
drugs3_triplo <- drugs3_triplo[,-customCols]

SNIS_PNLS_numPat <- drugs3_triplo
SNIS_PNLS_numPat$dataSource <- "SNIS_PNLS"
rm(list=setdiff(ls(), c("Cordaid_treatment5", "SNIS_PNLS_numPat")))



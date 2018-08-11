
#######################################################################################
# HIVDR project in collaboration with Bluesquare                                      #
# DRC data                                                                            #
#######################################################################################
# Last update: 10/08/2018                                                             #
# Author: Jenny Chung (external working for J&J GPH R&D DMP)                          #
# NOte: reading from the PNLS.RDS and Cordaid_TRAITMENTS.RDS                          #
#######################################################################################

#######################################################################################
#                                                                                     #
#                      SNIS - PNLS - patients number on regimens                      #  
#                  Cordaid - "4. VIH-SOINS TRAITEMENTS ET SOUTIENS"                   #
#                                                                                     #
#######################################################################################

##################
# scripts needed #
##################
# 1) libraries.R
# 2) readingRDS_SNIS.R
# 3) SNIS_PNLSdrug_patCount_fromRDS_explore.R
# 4) readingRDS_Cordaid.R
# 5) Cordaid_VIH-SOINS_patCount_explore.R
# 6) numPatOnReg_PNLSAndCordaid.R


###################################################
# reading in extra information about the regimens #
###################################################
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

drugsInfo <- drugsInfo[, c("PNLS.DRUG_RDS", "PNLS.DRUG_RDS_adj")]
drugsInfo$PNLS.DRUG_RDS <- sub("PNLS-DRUG-", "", drugsInfo$PNLS.DRUG_RDS)
drugsInfo2 <- drugsInfo[-which(grepl("*sex*", drugsInfo$PNLS.DRUG_RDS)),]


################################
# preparing a DE_metadata file #
################################
relevantCols <- c("orgUnit", "OU_name", "parent.name", "parent.parent.name", "parent.parent.parent.name")
uniqueOUMetadata_DF <- unique(SNIS_PNLS_numPat[, which(names(SNIS_PNLS_numPat) %in% relevantCols)])


##########################
# merging 2 data sources #
##########################
## SNIS-PNLS
############
relevantCols <- c("orgUnit", "period", "PNLS.DRUG_RDS_adj", "sexTotaux_Totaux")   #"dataSource"
SNIS_PNLS_numPat2 <- SNIS_PNLS_numPat[, which(names(SNIS_PNLS_numPat) %in% relevantCols)]


## Cordaid
##########
relevantCols <- c("orgUnit", "period", "DE_name", "numPatPerDEName")
Cordaid_treatment6 <- Cordaid_treatment5[, which(names(Cordaid_treatment5) %in% relevantCols)]
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "TDF + 3TC + LPV/r", "TDF_3TC_LPVr", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "ABC + 3TC + NVP", "ABC_3TC_NVP", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "TDF + FTC + NVP", "TDF_FTC_NVP", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "TDF+FTC+LPV+rt", "TDF_FTC_LPVr", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "AZT+3TC+ EFV", "AZT_3TC_EFV", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "AZT + 3TC + LPV/r", "AZT_3TC_LPVr", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "ABC + 3TC + EFV", "ABC_3TC_EFV", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "AZT+3TC+NVP", "AZT_3TC_NVP", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "ABC + 3TC + LPV/r", "ABC_3TC_LPVr", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "TDF+3TC+EFV", "TDF_3TC_EFV", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "TDF+3TC+NVP", "TDF_3TC_NVP", Cordaid_treatment6$DE_name)
Cordaid_treatment6$DE_name <- ifelse(Cordaid_treatment6$DE_name == "TDF+ FTC + EFV", "TDF_FTC_EFV", Cordaid_treatment6$DE_name)


## SNIS-PNLS + Cordaid
######################
numPat_2DS <- merge(SNIS_PNLS_numPat2, Cordaid_treatment6, by.x = c("orgUnit", "period", "PNLS.DRUG_RDS_adj"), 
                    by.y = c("orgUnit", "period", "DE_name"), all = TRUE)


### adding the data source
#####
numPat_2DS$dataSource <- ifelse(is.na(numPat_2DS$sexTotaux_Totaux) & !is.na(numPat_2DS$numPatPerDEName), "Cordaid",
                                ifelse(!is.na(numPat_2DS$sexTotaux_Totaux) & is.na(numPat_2DS$numPatPerDEName), "SNIS-PNLS",
                                       ifelse(!is.na(numPat_2DS$sexTotaux_Totaux) & !is.na(numPat_2DS$numPatPerDEName), "SNIS-PNLS, Cordaid",
                                              "NA")))


### making 1 value from mulitple data sources
#####
numPat_2DS$numPatPerDEName_2s <- ifelse(numPat_2DS$dataSource == "Cordaid", numPat_2DS$numPatPerDEName,
                                        ifelse(numPat_2DS$dataSource == "SNIS-PNLS", numPat_2DS$sexTotaux_Totaux, 
                                               NA))
numPat_2DS$numPatPerDEName_2s <- ifelse((is.na(numPat_2DS$numPatPerDEName_2s) & numPat_2DS$sexTotaux_Totaux != 0 & numPat_2DS$numPatPerDEName == 0), 
                                        numPat_2DS$sexTotaux_Totaux,
                                        ifelse((is.na(numPat_2DS$numPatPerDEName_2s) & numPat_2DS$sexTotaux_Totaux == 0 & numPat_2DS$numPatPerDEName != 0),
                                               numPat_2DS$numPatPerDEName,
                                               ifelse((is.na(numPat_2DS$numPatPerDEName_2s) & numPat_2DS$sexTotaux_Totaux != 0 & numPat_2DS$numPatPerDEName != 0), 
                                                      apply(numPat_2DS[, c("sexTotaux_Totaux","numPatPerDEName")],1,mean),
                                                      ifelse((is.na(numPat_2DS$numPatPerDEName_2s) & numPat_2DS$sexTotaux_Totaux == 0 & numPat_2DS$numPatPerDEName == 0),
                                                             0,numPat_2DS$numPatPerDEName_2s))))
numPat_2DS$numPatPerDEName_2s_SD <- ifelse(numPat_2DS$dataSource == "Cordaid", 0,
                                           ifelse(numPat_2DS$dataSource == "SNIS-PNLS", 0,
                                                  ifelse(numPat_2DS$dataSource == "SNIS-PNLS, Cordaid", apply(numPat_2DS[, c("sexTotaux_Totaux","numPatPerDEName")],1,sd),
                                                         NA)))
numPat_2DS$numPatPerDEName_2s_sdLB <- round(numPat_2DS$numPatPerDEName_2s - numPat_2DS$numPatPerDEName_2s_SD, 0)
numPat_2DS$numPatPerDEName_2s_sdUB <- round(numPat_2DS$numPatPerDEName_2s + numPat_2DS$numPatPerDEName_2s_SD, 0)
numPat_2DS$numPatPerDEName_2s_SEM <- numPat_2DS$numPatPerDEName_2s_SD/(2^0.5)
numPat_2DS$numPatPerDEName_2s_semLB <- round(numPat_2DS$numPatPerDEName_2s - numPat_2DS$numPatPerDEName_2s_SEM, 0)
numPat_2DS$numPatPerDEName_2s_semUB <- round(numPat_2DS$numPatPerDEName_2s + numPat_2DS$numPatPerDEName_2s_SEM, 0)


### typo correction
#####
#### orgUnit "PelcP75y8sa" had reported 30525 patients on the regimen TDF_FTC_EFV in the month January 2017. As the number is large, it is considered as a typo.
#### The reported number is change into 3052.5
numPat_2DS[which(numPat_2DS$orgUnit == "PelcP75y8sa" & numPat_2DS$period == "201701"& numPat_2DS$PNLS.DRUG_RDS_adj == "TDF_FTC_EFV"),]$numPatPerDEName_2s <- 3052.5


### adding orgUnit metadata
#####
numPat_2DS <- merge(numPat_2DS, uniqueOUMetadata_DF, by = "orgUnit", all.x = TRUE)


###############################################################################################################################################################


####################################################################################
#  preparing for visualisations: values from multiple data sources of a time point #
####################################################################################

## subsetting orgUnits having data from both data sources
#########################################################
orgUnit_2DS <- numPat_2DS[which(numPat_2DS$dataSource == "SNIS-PNLS, Cordaid"),]
orgUnit_2DS <- as.data.frame(unique(orgUnit_2DS$orgUnit), stringsAsFactors = F)
colnames(orgUnit_2DS)[1] <- "orgUnit"
orgUnit_2DS$orgUnit <- as.character(orgUnit_2DS$orgUnit)

numPat_2DS_orgUnit2DS <- numPat_2DS[which(numPat_2DS$orgUnit %in% orgUnit_2DS$orgUnit),]
numPat_2DS_orgUnit2DS$concordant <- ifelse(numPat_2DS_orgUnit2DS$sexTotaux_Totaux == numPat_2DS_orgUnit2DS$numPatPerDEName, "concordant", "not concordant")


## preparing for visualisations
###############################
m = "TDF_3TC_EFV"
tempDF <- subset(numPat_2DS_orgUnit2DS, numPat_2DS_orgUnit2DS$PNLS.DRUG_RDS_adj == m)

orgUnit_temp <- unique(tempDF[which(tempDF$concordant == "not concordant"),]$orgUnit)
tempDF2 <- tempDF[which(tempDF$orgUnit %in% orgUnit_temp),]

tempDF3 <- subset(tempDF2, tempDF2$concordant == "not concordant")
tempDF4 <- tempDF3 %>%
  select(orgUnit, PNLS.DRUG_RDS_adj, period, sexTotaux_Totaux, numPatPerDEName) %>%
  gather(key = dataSource, value = numPatPerDEName_2s, -orgUnit, -PNLS.DRUG_RDS_adj, -period)
tempDF4$dataSource <- ifelse(tempDF4$dataSource == "sexTotaux_Totaux", "SNIS-PNLS",
                             ifelse(tempDF4$dataSource == "numPatPerDEName", "Cordaid",
                                    "NA"))
tempDF5 <- rbind.fill(tempDF2, tempDF4)


###############################################################################################################################################################


##################
# visualisations #
##################

## country level ----
################
visDF <- numPat_2DS %>%
  group_by(period, PNLS.DRUG_RDS_adj) %>%
  mutate(numPatPerDEName_2s_agg = sum(numPatPerDEName_2s, na.rm = TRUE),
         numPatPerDEName_2s_semLB_agg = sum(numPatPerDEName_2s_semLB, na.rm = TRUE),
         numPatPerDEName_2s_semUB_agg = sum(numPatPerDEName_2s_semUB, na.rm = TRUE))
visDF2 <- visDF %>%
  select(period, PNLS.DRUG_RDS_adj, numPatPerDEName_2s_agg, numPatPerDEName_2s_semLB_agg, numPatPerDEName_2s_semUB_agg) %>%
  distinct()

p <- ggplot(visDF2, aes(x=period, y=numPatPerDEName_2s_agg)) +
  geom_bar(stat = "identity", alpha = 0.7) +   
  geom_errorbar(aes(ymin=numPatPerDEName_2s_semLB_agg, ymax=numPatPerDEName_2s_semUB_agg), colour = "black", width=.1) +
  facet_wrap(~factor(PNLS.DRUG_RDS_adj, levels= c("ABC_3TC_NVP", "ABC_3TC_EFV", "ABC_3TC_LPVr",
                                                  "AZT_3TC_NVP", "AZT_3TC_EFV", "AZT_3TC_LPVr",
                                                  "TDF_3TC_NVP", "TDF_3TC_EFV", "TDF_3TC_LPVr",
                                                  "TDF_FTC_NVP", "TDF_FTC_EFV", "TDF_FTC_LPVr",
                                                  "Autres (à préciser)")), 
             ncol = 3, scales = "free_y") + 
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1, size=6)) +
  labs(title=paste0("DRC: total number of patients on regimens (SNIS-PNLS & Cordaid)")) +
  xlab("Time (year-month)") +
  ylab("Number of patients") +
  theme(legend.position="top")
windows(record = T, 15,10)
p


## province level ----
################
visDF <- numPat_2DS %>%
  group_by(period, PNLS.DRUG_RDS_adj, parent.parent.parent.name) %>%
  mutate(numPatPerDEName_2s_agg = sum(numPatPerDEName_2s, na.rm = TRUE),
         numPatPerDEName_2s_semLB_agg = sum(numPatPerDEName_2s_semLB, na.rm = TRUE),
         numPatPerDEName_2s_semUB_agg = sum(numPatPerDEName_2s_semUB, na.rm = TRUE))
visDF2 <- visDF %>%
  select(period, parent.parent.parent.name, PNLS.DRUG_RDS_adj, numPatPerDEName_2s_agg, numPatPerDEName_2s_semLB_agg, numPatPerDEName_2s_semUB_agg) %>%
  distinct()

### making a data frame with unique province names
provinceDF <- as.data.frame(unique(visDF2$parent.parent.parent.name))
colnames(provinceDF)[1] <- "province"
provinceDF$index <- row.names(provinceDF)

#*# test: i = "kn Kinshasa Province"
for (i in unique(visDF2$parent.parent.parent.name)){
  temp <- subset(visDF2, visDF2$parent.parent.parent.name == i)
  
  p <- ggplot(temp, aes(x=period, y=numPatPerDEName_2s_agg)) +
    geom_bar(stat = "identity", alpha = 0.7) +   #fill = "#619CFF", 
    geom_errorbar(aes(ymin=numPatPerDEName_2s_semLB_agg, ymax=numPatPerDEName_2s_semUB_agg), colour = "black", width=.1) +
    facet_wrap(~factor(PNLS.DRUG_RDS_adj, levels= c("ABC_3TC_NVP", "ABC_3TC_EFV", "ABC_3TC_LPVr",
                                                    "AZT_3TC_NVP", "AZT_3TC_EFV", "AZT_3TC_LPVr",
                                                    "TDF_3TC_NVP", "TDF_3TC_EFV", "TDF_3TC_LPVr",
                                                    "TDF_FTC_NVP", "TDF_FTC_EFV", "TDF_FTC_LPVr",
                                                    "Autres (à préciser)")), 
               ncol = 3, scales = "free_y") + 
    theme_bw() +
    theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1, size=6)) +
    labs(title=paste0("DRC - province '", i, "' : total number of patients on regimens (SNIS-PNLS & Cordaid)")) +
    xlab("Time (year-month)") +
    ylab("Number of patients") +
    theme(legend.position="top")
  assign(paste0("p_", setdiff(provinceDF[provinceDF$province == i,]$index, NA)), p, envir=globalenv())   
  message("p_", setdiff(provinceDF[provinceDF$province == i,]$index, NA), " is created in the global environment.") 
  
}
windows(record = T, 15,10)
p_1   #Sud Ubangi
p_2   #Kinshasa
p_3   #Kasai Central
p_4   #Kwilu
p_5   #nothing (NA)
p_6   #Haut Katanga
p_7   #Lualaba
p_8   #Sud Kivu
p_9   #Bas Uele
p_10   #Kwango 
p_11   #Tanganyika
p_12   #Kasai Oriental
p_13   #Maindombe
p_14   #Maniema
p_15   #Kongo Central
p_16   #Sankuru
p_17   #Lomami
p_18   #Mongala
p_19   #Nord Ubangi


## facility level ----
#################
facilityOfInterest <- unique(numPat_2DS$orgUnit)[1:20]
facilityOfInterest <- unique(tempDF5$orgUnit)[1:20]
visDF <- numPat_2DS[which(numPat_2DS$orgUnit %in% facilityOfInterest),]

regimen = "TDF_3TC_EFV"
p <- ggplot(visDF[visDF$PNLS.DRUG_RDS_adj == regimen,], aes(x=period, y=numPatPerDEName_2s, colour = dataSource)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=numPatPerDEName_2s_semLB, ymax=numPatPerDEName_2s_semUB), colour = "black", width=.1) +
  facet_wrap(~orgUnit, ncol = 5, scales = "free_y") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1, size=6)) +
  labs(title=paste0("DRC - facility level : total number of patients on regimens (SNIS-PNLS & Cordaid)")) +
  xlab("Time (year-month)") +
  ylab("Number of patients") +
  theme(legend.position="top")
windows(record = T, 15,10)
p




#######################################################################################
# HIVDR project in collaboration with Bluesquare                                      #
# DRC data                                                                            #
#######################################################################################
# Last update: 10/08/2018                                                             #
# Author: Jenny Chung (external working for J&J GPH R&D DMP)                          #
#######################################################################################

#######################################################################################
#                                                                                     #
#               Cordaid - "4. VIH-SOINS TRAITEMENTS ET SOUTIENS"                      #    
#                                                                                     #
#######################################################################################


##############
# data files # ----
##############
#source('scripts/readingRDS_Cordaid.R')


#############
# exploring # ----
#############
Cordaid_treatment2 <- merge(Cordaid_treatment, DS_metadata_C, by.x = "dataElement", by.y = "DE_id")
Cordaid_treatment2 <- merge(Cordaid_treatment2, OU_metadata_DSinfo_C, by.x = c("orgUnit", "DS_name", "DS_id"), by.y = c("OU_id", "DS_name", "DS_id"))
Cordaid_treatment2 <- merge(Cordaid_treatment2, CC_metadata_C, by.x = c("categoryOptionCombo", "categoryCombo.id"), by.y = c("CatComboOpt_id", "CatCombo_id"))


## selecting relevant columns
#############################
relevantElements <- c("TDF + 3TC + LPV/r", "ABC + 3TC + NVP", "TDF + FTC + NVP", "TDF+FTC+LPV+rt", "Autres (à préciser)", "AZT+3TC+ EFV",
                      "AZT + 3TC + LPV/r", "ABC + 3TC + EFV", "AZT+3TC+NVP", "ABC + 3TC + LPV/r", "TDF+3TC+EFV", "TDF+3TC+NVP",
                      "TDF+ FTC + EFV")
Cordaid_treatment3 <- Cordaid_treatment2[which(Cordaid_treatment2$DE_name %in% relevantElements),]
irrelevantCols <- c("X", "storedBy", "created", "lastUpdated", "followUp", "domainType", "zeroIsSignificant",
                    "parent.parent.parent.parent.name", "parent.parent.parent.parent.id")
Cordaid_treatment3 <- Cordaid_treatment3[, -which(names(Cordaid_treatment3) %in% irrelevantCols)]
Cordaid_treatment3$Year <- substr(Cordaid_treatment3$period, 1, 4)
Cordaid_treatment3$Month <- substr(Cordaid_treatment3$period, 5, 6)
Cordaid_treatment3$CatOpt_name.2 <- ifelse(is.na(Cordaid_treatment3$CatOpt_name.2), "All", Cordaid_treatment3$CatOpt_name.2)


## transforming variables
#########################
### age categories
### Note: difficult to categorise "< 25 ans", "Moins de 25 ans "!
Cordaid_treatment3$ageCategory <- sub("25 ans et plus|25 et 49 ans |50 ans et plus |50 ans et plus|15 et 19 ans |20 et 24 ans |25 ans et plus |15 et 24 ans |25 - 49 ans|20 - 24 ans|15 - 19 ans |50 ans et plus|25 - 49 ans", 
                                      ">=15y", Cordaid_treatment3$CatOpt_name.1)
Cordaid_treatment3$ageCategory <- sub("Moins de 15 ans |Moins de 14 ans |10 et 14 ans |5 et 9 ans |Moins d'un an |1 et 4 ans |0 – 14 ans|1 – 4  ans |5 – 9 ans|10 - 14 ans |< 1 an", 
                                      "<15y", Cordaid_treatment3$ageCategory)
Cordaid_treatment3$ageCategory <- ifelse(grepl("*9 ans*", Cordaid_treatment3$ageCategory), 
                                         "<15y", Cordaid_treatment3$ageCategory)


## Calculations on facility level
#################################
Cordaid_treatment4 <- Cordaid_treatment3 %>%
  group_by(orgUnit, period, DE_name, ageCategory, CatOpt_name.2) %>%
  mutate(numPatPerGender = sum(value, na.rm = TRUE))
Cordaid_treatment4$numPatPerGender <- ifelse(is.na(Cordaid_treatment4$value), NA, Cordaid_treatment4$numPatPerGender)

Cordaid_treatment4_b <- Cordaid_treatment4 %>%
  select(orgUnit, period, DE_name, ageCategory, CatOpt_name.2, numPatPerGender) %>%
  distinct() %>%
  group_by(orgUnit, period, DE_name, ageCategory) %>%
  mutate(numPatPerAgeCat = sum(numPatPerGender, na.rm = TRUE))
Cordaid_treatment4_b$numPatPerAgeCat <- ifelse(is.na(Cordaid_treatment4_b$numPatPerGender), NA, Cordaid_treatment4_b$numPatPerAgeCat)

Cordaid_treatment4_c <- Cordaid_treatment4_b %>%
  select(orgUnit, period, DE_name, ageCategory, numPatPerAgeCat) %>%
  distinct() %>%
  group_by(orgUnit, period, DE_name) %>%
  mutate(numPatPerDEName = sum(numPatPerAgeCat, na.rm = TRUE))
Cordaid_treatment4_c$numPatPerDEName <- ifelse(is.na(Cordaid_treatment4_c$numPatPerAgeCat), NA, Cordaid_treatment4_c$numPatPerDEName)

Cordaid_treatment5 <- Cordaid_treatment4_c %>%
  select(orgUnit, period, DE_name, numPatPerDEName) %>%
  distinct()

Cordaid_treatment5$dataSource <- "Cordaid_traitments"
rm(list=setdiff(ls(), c("Cordaid_treatment5", "SNIS_PNLS_numPat")))


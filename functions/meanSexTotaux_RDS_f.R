

#######################################################################################
# HIVDR project in collaboration with Bluesquare                                      #
# DRC data                                                                            #
#######################################################################################
# Last update: 10/08/2018                                                             #
# Author: Jenny Chung (external working for J&J GPH R&D DMP)                          #
# Note: reading from the PNLS.RDS                                                     #
#######################################################################################


#################################################################
# function to calculate/obtain max, mean, sd, meanLB, meanUB of #
# the 'sex_Totaux' and the 'Totaux' variables of a regimen      #
#################################################################

# # test
# df_name = PNLS7
# drugs_type = "triploDrug"
# number_var = 1
# #*# test: i = "ABC_+_3TC_+_EFV"   #2 variables
# #*# test: i = "TDF_3TC_EFV"   #numVar = 1
# dfName = df_name
# drugsType = drugs_type
# numberVar = number_var
# 
# meanSexTotaux_f(dfName = df_name,
#                 drugsType = drugs_type,
#                 numberVar = number_var)


meanSexTotaux_RDS_f <- function(dfName, drugsType, numberVar){
  temp <- subset(dfName, dfName$numVar == numberVar)
  temp <- subset(temp, temp$drugType == drugsType)
  
  if(numberVar == 2){
    subsetList <- list()
    for(i in unique(temp$PNLS.DRUG_RDS_adj)){
      temp1 <- subset(temp, temp$PNLS.DRUG_RDS_adj == i)
      message("For the regimen ", i, " there are ", nrow(as.data.frame(unique(temp1$PNLS.DRUG_group))), " variables.")
      
      temp1$PNLS.DRUG_group <- NULL
      temp2 <- temp1 %>%
        spread(key = DE_name, value = numPatPerDEName)
      
      regCol <- which(grepl("*TDF*|*3TC*|*NVP*|*ABC*|*FTC*|*LPV*|*Autres*|*AZT*|*EFV*", names(temp2)) == TRUE)
      sexTotCol <- which(grepl("*sex*", names(temp2)) == TRUE)
      index <- which(!(regCol %in% sexTotCol))
      totCol <- regCol[index]
      temp2$missingN <- rowSums(is.na(temp2[, c(sexTotCol, totCol)]))
      
      missingN_df <- data.frame(missingN = unique(temp2$missingN), 
                                unnec = c(1:length(unique(temp2$missingN))))
      missingN_df <- missingN_df[order(missingN_df[,"missingN"]),]
      missingN_df$index <- as.numeric(rownames(missingN_df))
      missingN_df$unnec <- NULL
      
      #*# test: j = 2
      ## if the number of missing values is 0, then ...   
      ## if the number of missing values is  1, then ...
      ## if the number of missing values is 2, then ...
      subsetList2 <- list()
      for(j in unique(missingN_df$index)){
        temp3 <- subset(temp2, temp2$missingN == missingN_df$missingN[j])
        
        if(0 %in% missingN_df$missingN[j]){
          ### taking the maximum value of the "_sex_Totaux" and "_Totaux" variables
          temp3$new <- apply(temp3[, c(sexTotCol, totCol)], 1, max)
          newCol <- which(names(temp3) %in% "new")
          colnames(temp3)[newCol] <- paste0("Max")
          
          ### calculating the mean
          temp3$new <- apply(temp3[, c(sexTotCol, totCol)], 1, mean, na.rm = TRUE)
          newCol <- which(names(temp3) %in% "new")
          colnames(temp3)[newCol] <- paste0("Mean")
          
          ### calculating the sum
          temp3$new <- rowSums(temp3[, c(sexTotCol, totCol)], na.rm = TRUE)
          newCol <- which(names(temp3) %in% "new")
          colnames(temp3)[newCol] <- paste0("Sum")
          
          ### calculating the standard deviation
          temp3$new <- apply(temp3[, c(sexTotCol, totCol)], 1, sd, na.rm = TRUE)
          newCol <- which(names(temp3) %in% "new")
          colnames(temp3)[newCol] <- paste0("SD")
          
          ### calculating the standard deviation
          meanCol <- which(grepl("*Mean", names(temp3)) == TRUE)
          sdCol <- which(grepl("*SD", names(temp3)) == TRUE)
          temp3$new <- temp3[, meanCol] - temp3[, sdCol]
          newCol <- which(names(temp3) %in% "new")
          colnames(temp3)[newCol] <- paste0("MeanLB")
          temp3$new <- temp3[, meanCol] + temp3[, sdCol]
          newCol <- which(names(temp3) %in% "new")
          colnames(temp3)[newCol] <- paste0("MeanUB")
          
          ### rounding after calculations
          LBCol <- which(grepl("*MeanLB", names(temp3)) == TRUE)
          UBCol <- which(grepl("*MeanUB", names(temp3)) == TRUE)
          temp3[, meanCol] <- round(temp3[, meanCol], 2)
          temp3[, sdCol] <- round(temp3[, sdCol], 2)
          temp3[, LBCol] <- round(temp3[, LBCol], 2)
          temp3[, UBCol] <- round(temp3[, UBCol], 2)
          
          ### renaming the sex_Totaux and Totaux variables
          colnames(temp3)[sexTotCol] <- paste0("sexTotaux")
          colnames(temp3)[totCol] <- paste0("Totaux")
          
          subsetList2[[j]] <- temp3
          
        }
        
        if(1 %in% missingN_df$missingN[j]){
          ### renaming the sex_Totaux and Totaux columns
          temp3[,"sexTotaux"] <- temp3[, sexTotCol]
          temp3[,"Totaux"] <- temp3[, totCol]
          
          ### taking the maximum value of the "_sex_Totaux" and "_Totaux" variables
          temp3$Max <- apply(temp3[, c(sexTotCol, totCol)], 1, max)
          temp3$Mean <- apply(temp3[, c(sexTotCol, totCol)], 1, mean, na.rm = TRUE)
          temp3$Sum <- rowSums(temp3[, c(sexTotCol, totCol)], na.rm = TRUE)
          temp3$SD <- ifelse(!is.na(temp3$Mean), 0, "NA")
          temp3$MeanLB <- temp3$Mean
          temp3$MeanUB <- temp3$Mean
          
          temp3[, sexTotCol] <- NULL
          temp3[, totCol] <- NULL
          
          subsetList2[[j]] <- temp3
          
        }
        
        if(2 %in% missingN_df$missingN[j]){
          subsetList2[[j]] <- temp3
        }
        
      }
      temp3 <- do.call(rbind.fill, subsetList2)
      subsetList[[i]] <- temp3
      rm(temp1, temp2, temp3, missingN_df, subsetList2, j)
      
    }
    temp4 <- do.call(rbind.fill, subsetList)
    rm(subsetList, i, temp)
    assign("temp4", temp4, envir=globalenv())
    
  }
  
  
  if(numberVar == 1){
    subsetList <- list()
    for(i in unique(temp$PNLS.DRUG_RDS_adj)){
      temp2 <- subset(temp, temp$PNLS.DRUG_RDS_adj == i)
      message("For the regimen ", i, " there are ", nrow(as.data.frame(unique(temp2$PNLS.DRUG_group))), " variables.")
      
      popGroup <- unique(temp2$PNLS.DRUG_group)
      temp2$PNLS.DRUG_group <- NULL
      temp3 <- temp2 %>%
        spread(key = DE_name, value = numPatPerDEName)
      
      ## renaming the sex_Totaux and Totaux columns
      if(popGroup == "sex"){
        sexOrTotCol <- which(grepl("*sex*", names(temp3)) == TRUE)
        colnames(temp3)[sexOrTotCol] <- paste0("sexTotaux")
      }
      
      if(popGroup == "total"){
        sexOrTotCol <- which(grepl("*TDF*|*3TC*|*NVP*|*ABC*|*FTC*|*LPV*|*Autres*|*AZT*|*EFV*", names(temp2)) == TRUE)
        colnames(temp3)[sexOrTotCol] <- paste0("Totaux")
      }
      
      ## taking the maximum value of the "_sex_Totaux" and "_Totaux" variables
      temp3$Max <- temp3[, sexOrTotCol]
      temp3$Mean <- temp3[, sexOrTotCol]
      temp3$Sum <- temp3[, sexOrTotCol]
      temp3$SD <- ifelse(!is.na(temp3[, sexOrTotCol]), 0, "NA")
      temp3$MeanLB <- temp3[, sexOrTotCol]
      temp3$MeanUB <- temp3[, sexOrTotCol]
      
      subsetList[[i]] <- temp3
      rm(temp2, temp3, i)
      
    }
    temp4 <- do.call(rbind.fill, subsetList)
    assign("temp4", temp4, envir=globalenv())
    
  }
  
  
}


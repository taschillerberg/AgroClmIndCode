# P6_RandomForest.R
# This R program will open the yield data and agroclimate indicies. The yield 
# data will be formated and saved into a list. The agroclimate indicies will
# also be saved into a list. Categorical random forest will be preformed on 
# the yields. 
#
#
# T. A. Schillerberg
#               Feb. 2021
#      Updated: Nov. 2021

# Local Computer
# setwd("")
# fileloc1 <- '/Research/AgroclimaticConditions/Data/'
# loc1 <- 'Iizumi2020/gdhy_v1.2_v1.3_20190128/'
# loc2 <- 'Global_AgroInd/Agro092021_mu/'
# loc3 <- 'Global_AgroInd/Agro102021_RF/'

# HPC 
fileloc1 <- '~/AgroClmInd/data/'
loc1 <- 'Iizumi2020/'
loc2 <- 'AgroclimateInd/Agro092021_mu/'
loc3 <- 'AgroclimateInd/Agro022022_RF/'

options(show.error.locations = TRUE)

# Variables to change ##########################################################
core <- 11  # Runtime:  2 hrs for one type and detrending method
type <- 'M' #'OLS' 'M' "MM' 'TS' for when calculating categorical yield
t <- 'quartile'
       
# Libraries ####################################################################
library(pracma)
library(randomForest)
library(pdp)
library(ROCR)

# library(tidyverse)
library(dplyr); library(forcats); library(tibble)
library(readr); library(stringr); library(tidyr)
library(purrr); library(magrittr)

# Functions ####################################################################
r_randomForest <- function(X, varis){
  #     This function will find the regional yield and agroclimate indices. Shaping 
  # them into a long format of [Index, yr, yield, AgroInd], before testing to see if
  # there is more or less than 30% of the AgroInd dat missing. If less than 30% a 
  # column of AgroInd dat is missing the individual rows will be removed. If more 
  # than 30% of the column (AgroInd) will be removed. Data will be split into testing
  # and training data, before being sent to Random Forest, the importance should be 
  # TRUE. The Partial Dependency Plot and ROC curve will be ploted and saved. Send 
  # back the importance for each region.
  # 
  # X which region contains row of lat lon in top right and bottom left corner
  # varis -> regs contains the information of the region
  # varis -> yr
  # varis -> crop c('maize', 'rice', 'soya', 'wheat')
  # varis -> datYield contains the quartile yield
  # varis -> datAgro List containing the agroclimat indicies
 
  # Packages Required ####
  
  # require(ggplot2)
  require(randomForest)
  require(pdp)
  require(ROCR)
  # require(tidyverse)
  require(dplyr); require(forcats); require(tibble)
  require(readr); require(stringr); require(tidyr)
  require(purrr); require(magrittr)

  # Variables needed #####
  key <- varis[['key']][X] # might need t check
  datAgro <-varis[["datAgro"]]
  file1 <- c(varis[["file1"]], varis[["file2"]])
  file2 <- varis[["file2"]]
  
  # Calculation of regional #####
  n <- which(colnames(varis[["mask"]]) == key)
  m <- which(varis[["mask"]][,n] == 1)
  
  # Random Forest Data Shaping ####
  # Maize
  name <- vector()
  for (j in file2){ name <- c(name, paste0(j, '_', varis[["crop"]][1]))}
  datRF <- unlist(matrix(varis[['MdatYield']][m, 5:ncol(varis[['MdatYield']])], ncol = 1, byrow = TRUE)) %>%
    cbind(unlist(matrix(datAgro[[file1[1]]][m, 5:ncol(datAgro[[file1[1]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[2]]][m, 5:ncol(datAgro[[file1[2]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[3]]][m, 5:ncol(datAgro[[file1[3]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[4]]][m, 5:ncol(datAgro[[file1[4]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[5]]][m, 5:ncol(datAgro[[file1[5]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[1]]] [m, 5:ncol(datAgro[[name[1]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[2]]] [m, 5:ncol(datAgro[[name[2]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[3]]] [m, 5:ncol(datAgro[[name[3]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[4]]] [m, 5:ncol(datAgro[[name[4]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[5]]] [m, 5:ncol(datAgro[[name[5]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[6]]] [m, 5:ncol(datAgro[[name[6]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[7]]] [m, 5:ncol(datAgro[[name[7]]])],  ncol = 1, byrow = TRUE)))
  
  colnames(datRF) <- c('yield', file1)
  datRF[datRF == -999] <- NA 
  datRF <- datRF[complete.cases(datRF[,1]),] 
  datRF <- data.frame(datRF)
  datRF[,1][datRF[,1] == '1'] <- 'failure'
  datRF[,1][datRF[,1] == '2'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '3'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '4'] <- 'nonfailure'
  
  # Need to remove points with NA and if >30% of predictor is NA
  drop <- array(dim=c(0,0))
  if (dim(datRF)[1] > 0){
    for (i in 2:dim(datRF)[2]){
      if ((sum(is.na(datRF[,i]))/dim(datRF)[1]) < 0.3){
        datRF <- datRF[complete.cases(datRF[,i]),]
      } else {
        drop <- c(drop,i)
      }
      if (i == dim(datRF)[2] & (length(drop) >= 1)){
        datRF <- datRF[,-drop]
      } else {
        # Do nothing
      }
      # print(dim(datRF))
    }
  }
  
  # Split into training and testing data
  Mmtry <- round(sqrt(dim(datRF)[2]),0)
  train <- sample(seq_len(nrow(datRF)), replace=FALSE, size = floor(0.70*nrow(datRF)))
  datRFtrain <- datRF[train,]
  datRFtest <- datRF[-train,]
  MdatYtrain <- as.factor(datRFtrain[,1]);         MdatYtest <- as.factor(datRFtest[,1])
  MdatXtrain <- datRFtrain[,2:ncol(datRFtrain)];   MdatXtest <- datRFtest[,2:ncol(datRFtest)]
  
  
  # Rice
  name <- vector()
  for (j in file2){ name <- c(name, paste0(j, '_', varis[['crop']][2]))}
  datRF <- unlist(matrix(varis[['RdatYield']][m, 5:ncol(varis[['RdatYield']])], ncol = 1, byrow = TRUE)) %>%
    cbind(unlist(matrix(datAgro[[file1[1]]][m, 5:ncol(datAgro[[file1[1]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[2]]][m, 5:ncol(datAgro[[file1[2]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[3]]][m, 5:ncol(datAgro[[file1[3]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[4]]][m, 5:ncol(datAgro[[file1[4]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[5]]][m, 5:ncol(datAgro[[file1[5]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[1]]] [m, 5:ncol(datAgro[[name[1]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[2]]] [m, 5:ncol(datAgro[[name[2]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[3]]] [m, 5:ncol(datAgro[[name[3]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[4]]] [m, 5:ncol(datAgro[[name[4]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[5]]] [m, 5:ncol(datAgro[[name[5]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[6]]] [m, 5:ncol(datAgro[[name[6]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[7]]] [m, 5:ncol(datAgro[[name[7]]])],  ncol = 1, byrow = TRUE)))
  
  colnames(datRF) <- c('yield', file1)
  datRF[datRF == -999] <- NA 
  datRF <- datRF[complete.cases(datRF[,1]),] 
  datRF <- data.frame(datRF)
  datRF[,1][datRF[,1] == '1'] <- 'failure'
  datRF[,1][datRF[,1] == '2'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '3'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '4'] <- 'nonfailure'
  
  # Need to remove points with NA and if >30% of predictor is NA
  drop <- array(dim=c(0,0))
  if (dim(datRF)[1] > 0){
    for (i in 2:dim(datRF)[2]){
      if ((sum(is.na(datRF[,i]))/dim(datRF)[1]) < 0.3){
        datRF <- datRF[complete.cases(datRF[,i]),]
      } else {
        drop <- c(drop,i)
      }
      if (i == dim(datRF)[2] & (length(drop) >= 1)){
        datRF <- datRF[,-drop]
      } else {
        # Do nothing
      }
      # print(dim(datRF))
    }
  }
  
  # Split into training and testing data
  Rmtry <- round(sqrt(dim(datRF)[2]),0)
  train <- sample(seq_len(nrow(datRF)), replace=FALSE, size = floor(0.70*nrow(datRF)))
  datRFtrain <- datRF[train,]
  datRFtest <- datRF[-train,]
  RdatYtrain <- as.factor(datRFtrain[,1]);         RdatYtest <- as.factor(datRFtest[,1])
  RdatXtrain <- datRFtrain[,2:ncol(datRFtrain)];   RdatXtest <- datRFtest[,2:ncol(datRFtest)]
  
  
  # Soya
  name <- vector()
  for (j in file2){ name <- c(name, paste0(j, '_', varis[['crop']][3]))}
  datRF <- unlist(matrix(varis[['SdatYield']][m, 5:ncol(varis[['SdatYield']])], ncol = 1, byrow = TRUE)) %>%
    cbind(unlist(matrix(datAgro[[file1[1]]][m, 5:ncol(datAgro[[file1[1]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[2]]][m, 5:ncol(datAgro[[file1[2]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[3]]][m, 5:ncol(datAgro[[file1[3]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[4]]][m, 5:ncol(datAgro[[file1[4]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[5]]][m, 5:ncol(datAgro[[file1[5]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[1]]] [m, 5:ncol(datAgro[[name[1]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[2]]] [m, 5:ncol(datAgro[[name[2]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[3]]] [m, 5:ncol(datAgro[[name[3]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[4]]] [m, 5:ncol(datAgro[[name[4]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[5]]] [m, 5:ncol(datAgro[[name[5]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[6]]] [m, 5:ncol(datAgro[[name[6]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[7]]] [m, 5:ncol(datAgro[[name[7]]])],  ncol = 1, byrow = TRUE)))
  
  colnames(datRF) <- c('yield', file1)
  datRF[datRF == -999] <- NA 
  datRF <- datRF[complete.cases(datRF[,1]),] 
  datRF <- data.frame(datRF)
  datRF[,1][datRF[,1] == '1'] <- 'failure'
  datRF[,1][datRF[,1] == '2'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '3'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '4'] <- 'nonfailure'
  
  # Need to remove points with NA and if >30% of predictor is NA
  drop <- array(dim=c(0,0))
  if (dim(datRF)[1] > 0){
    for (i in 2:dim(datRF)[2]){
      if ((sum(is.na(datRF[,i]))/dim(datRF)[1]) < 0.3){
        datRF <- datRF[complete.cases(datRF[,i]),]
      } else {
        drop <- c(drop,i)
      }
      if (i == dim(datRF)[2] & (length(drop) >= 1)){
        datRF <- datRF[,-drop]
      } else {
        # Do nothing
      }
      # print(dim(datRF))
    }
  }
  
  # Split into training and testing data
  Smtry <- round(sqrt(dim(datRF)[2]),0)
  train <- sample(seq_len(nrow(datRF)), replace=FALSE, size = floor(0.70*nrow(datRF)))
  datRFtrain <- datRF[train,]
  datRFtest <- datRF[-train,]
  SdatYtrain <- as.factor(datRFtrain[,1]);         SdatYtest <- as.factor(datRFtest[,1])
  SdatXtrain <- datRFtrain[,2:ncol(datRFtrain)];   SdatXtest <- datRFtest[,2:ncol(datRFtest)]
  
  
  # Wheat
  name <- vector()
  for (j in file2){ name <- c(name, paste0(j, '_', varis[['crop']][4]))}
  datRF <- unlist(matrix(varis[['WdatYield']][m, 5:ncol(varis[['WdatYield']])], ncol = 1, byrow = TRUE)) %>%
    cbind(unlist(matrix(datAgro[[file1[1]]][m, 5:ncol(datAgro[[file1[1]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[2]]][m, 5:ncol(datAgro[[file1[2]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[3]]][m, 5:ncol(datAgro[[file1[3]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[4]]][m, 5:ncol(datAgro[[file1[4]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[5]]][m, 5:ncol(datAgro[[file1[5]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[1]]] [m, 5:ncol(datAgro[[name[1]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[2]]] [m, 5:ncol(datAgro[[name[2]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[3]]] [m, 5:ncol(datAgro[[name[3]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[4]]] [m, 5:ncol(datAgro[[name[4]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[5]]] [m, 5:ncol(datAgro[[name[5]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[6]]] [m, 5:ncol(datAgro[[name[6]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[7]]] [m, 5:ncol(datAgro[[name[7]]])],  ncol = 1, byrow = TRUE))) 
  
  colnames(datRF) <- c('yield', file1)
  datRF[datRF == -999] <- NA 
  datRF <- datRF[complete.cases(datRF[,1]),] 
  datRF <- data.frame(datRF)
  datRF[,1][datRF[,1] == '1'] <- 'failure'
  datRF[,1][datRF[,1] == '2'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '3'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '4'] <- 'nonfailure'
  
  # Need to remove points with NA and if >30% of predictor is NA
  drop <- array(dim=c(0,0))
  if (dim(datRF)[1] > 0){
    for (i in 2:dim(datRF)[2]){
      if ((sum(is.na(datRF[,i]))/dim(datRF)[1]) < 0.3){
        datRF <- datRF[complete.cases(datRF[,i]),]
      } else {
        drop <- c(drop,i)
      }
      if (i == dim(datRF)[2] & (length(drop) >= 1)){
        datRF <- datRF[,-drop]
      } else {
        # Do nothing
      }
      # print(dim(datRF))
    }
  }
  
  # Split into training and testing data
  Wmtry <- round(sqrt(dim(datRF)[2]),0)
  train <- sample(seq_len(nrow(datRF)), replace=FALSE, size = floor(0.70*nrow(datRF)))
  datRFtrain <- datRF[train,]
  datRFtest <- datRF[-train,]
  WdatYtrain <- as.factor(datRFtrain[,1]);         WdatYtest <- as.factor(datRFtest[,1])
  WdatXtrain <- datRFtrain[,2:ncol(datRFtrain)];   WdatXtest <- datRFtest[,2:ncol(datRFtest)]
  
  
  rm(m, i, name)
  
  # Random Forest with Random Forest package ####
  if (dim(MdatXtrain)[1] > 0){
    MRFdat <- randomForest::randomForest(x= MdatXtrain, y= MdatYtrain, xtest= MdatXtest, ytest= MdatYtest, 
                                         mtry = Mmtry, proximity = FALSE, ntree = 650, importance = TRUE, 
                                         na.action = na.omit, votes = TRUE, keep.forest = TRUE)
    
    Mimp <- importance(MRFdat) %>% 
      data.frame() %>% 
      rownames_to_column('AgroInd') %>% 
      tibble("Crop"='maize', "Region" = key)
  } else {
    MRFdat <- NA
    Mimp <- tibble("AgroInd"=NA, "failure"=NA, "nonfailure"=NA, 
                   "MeanDecreaseAccuracy"=NA, "MeanDecreaseGini"=NA, 
                   "Crop"='maize', "Region" = key)
  }
  if (dim(RdatXtrain)[1] > 0){
    RRFdat <- randomForest::randomForest(x= RdatXtrain, y= RdatYtrain, xtest= RdatXtest, ytest= RdatYtest, 
                                         mtry = Rmtry, proximity = FALSE, ntree = 650, importance = TRUE, 
                                         na.action = na.omit, votes = TRUE, keep.forest = TRUE)

    Rimp <- importance(RRFdat) %>% 
      data.frame() %>% 
      rownames_to_column('AgroInd') %>% 
      tibble("Crop"='rice', "Region" = key)

    
  } else {
    RRFdat <- NA
    Rimp <- tibble("AgroInd"=NA, "failure"=NA, "nonfailure"=NA, 
                   "MeanDecreaseAccuracy"=NA, "MeanDecreaseGini"=NA, 
                   "Crop"='rice', "Region" = key)
  }  
  if (dim(SdatXtrain)[1] > 0){
    SRFdat <- randomForest::randomForest(x= SdatXtrain, y= SdatYtrain, xtest= SdatXtest, ytest= SdatYtest, 
                                         mtry = Smtry, proximity = FALSE, ntree = 650, importance = TRUE, 
                                         na.action = na.omit, votes = TRUE, keep.forest = TRUE)
    Simp <- importance(SRFdat) %>% 
      data.frame() %>% 
      rownames_to_column('AgroInd') %>% 
      tibble("Crop"='soya', "Region" = key)
  } else {
    SRFdat <- NA
    Simp <- tibble("AgroInd"=NA, "failure"=NA, "nonfailure"=NA, 
                   "MeanDecreaseAccuracy"=NA, "MeanDecreaseGini"=NA, 
                   "Crop"='soya', "Region" = key)
  }
  if (dim(WdatXtrain)[1] > 0){
    WRFdat <- randomForest::randomForest(x= WdatXtrain, y= WdatYtrain, xtest= WdatXtest, ytest= WdatYtest, 
                                         mtry = Wmtry, proximity = FALSE, ntree = 650, importance = TRUE, 
                                         na.action = na.omit, votes = TRUE, keep.forest = TRUE)
    Wimp <- importance(WRFdat) %>% 
      data.frame() %>% 
      rownames_to_column('AgroInd') %>% 
      tibble("Crop"='wheat', "Region" = key)
  } else {
    WRFdat <- NA
    Wimp <- tibble("AgroInd"=NA, "failure"=NA, "nonfailure"=NA, 
                   "MeanDecreaseAccuracy"=NA, "MeanDecreaseGini"=NA, 
                   "Crop"='wheat', "Region" = key)
  }
  
  
  # Partial Dependancy Plots ####
  # References: https://bgreenwell.github.io/pdp/articles/pdp.html 
  # https://cran.r-project.org/web/packages/pdp/pdp.pdf
  # https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots
  
  # maize
  if(is.na(MRFdat) != TRUE & is.na(Mimp$AgroInd)[1] != TRUE){
    m <- which(Mimp$AgroInd == file1[1])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[1], plot = TRUE,
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df1a <- p$dat;                     colnames(df1a) <- c('X', 'yhat')
      df1a$AgroClmInd <- file1[1];       df1a$Crop <- 'maize'
    } else { 
      df1a <- data.frame(matrix(NA,ncol = 2)); colnames(df1a) <- c('X','yhat')
      df1a$AgroClmInd <- file1[1];             df1a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[2])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[2], plot = TRUE,
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df2a <- p$dat;                     colnames(df2a) <- c('X', 'yhat')
      df2a$AgroClmInd <- file1[2];       df2a$Crop <- 'maize'
    } else { 
      df2a <- data.frame(matrix(NA,ncol = 2)); colnames(df2a) <- c('X','yhat')
      df2a$AgroClmInd <- file1[2];             df2a$Crop <- 'maize'
      }
    m <- which(Mimp$AgroInd == file1[3])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[3], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df3a <- p$dat;                     colnames(df3a) <- c('X', 'yhat')
      df3a$AgroClmInd <- file1[3];       df3a$Crop <- 'maize'
    } else { 
      df3a <- data.frame(matrix(NA,ncol = 2)); colnames(df3a) <- c('X','yhat')
      df3a$AgroClmInd <- file1[3];             df3a$Crop <- 'maize'
      }
    m <- which(Mimp$AgroInd == file1[4])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[4], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df4a <- p$dat;                     colnames(df4a) <- c('X', 'yhat')
      df4a$AgroClmInd <- file1[4];       df4a$Crop <- 'maize'
    } else { 
      df4a <- data.frame(matrix(NA,ncol = 2)); colnames(df4a) <- c('X','yhat')
      df4a$AgroClmInd <- file1[4];             df4a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[5])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[5], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df5a <- p$dat;                     colnames(df5a) <- c('X', 'yhat')
      df5a$AgroClmInd <- file1[5];       df5a$Crop <- 'maize'
    } else { 
      df5a <- data.frame(matrix(NA,ncol = 2)); colnames(df5a) <- c('X','yhat')
      df5a$AgroClmInd <- file1[5];             df5a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[6])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[6], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df6a <- p$dat;                     colnames(df6a) <- c('X', 'yhat')
      df6a$AgroClmInd <- file1[6];       df6a$Crop <- 'maize'
    } else { 
      df6a <- data.frame(matrix(NA,ncol = 2)); colnames(df6a) <- c('X','yhat')
      df6a$AgroClmInd <- file1[6];             df6a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[7])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[7], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df7a <- p$dat;                     colnames(df7a) <- c('X', 'yhat')
      df7a$AgroClmInd <- file1[7];       df7a$Crop <- 'maize'
    } else { 
      df7a <- data.frame(matrix(NA,ncol = 2)); colnames(df7a) <- c('X','yhat')
      df7a$AgroClmInd <- file1[7];             df7a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[8])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[8], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df8a <- p$dat;                     colnames(df8a) <- c('X', 'yhat')
      df8a$AgroClmInd <- file1[8];       df8a$Crop <- 'maize'
    } else { 
      df8a <- data.frame(matrix(NA,ncol = 2)); colnames(df8a) <- c('X','yhat')
      df8a$AgroClmInd <- file1[8];             df8a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[9])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[9], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df9a <- p$dat;                     colnames(df9a) <- c('X', 'yhat')
      df9a$AgroClmInd <- file1[9];       df9a$Crop <- 'maize'
    } else { 
      df9a <- data.frame(matrix(NA,ncol = 2)); colnames(df9a) <- c('X','yhat')
      df9a$AgroClmInd <- file1[9];             df9a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[10])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[10], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df10a <- p$dat;                     colnames(df10a) <- c('X', 'yhat')
      df10a$AgroClmInd <- file1[10];     df10a$Crop <- 'maize'
    } else { 
      df10a <- data.frame(matrix(NA,ncol = 2)); colnames(df10a) <- c('X','yhat')
      df10a$AgroClmInd <- file1[10];            df10a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[11])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[11], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df11a <- p$dat;                     colnames(df11a) <- c('X', 'yhat')
      df11a$AgroClmInd <- file1[11];      df11a$Crop <- 'maize'
    } else { 
      df11a <- data.frame(matrix(NA,ncol = 2)); colnames(df11a) <- c('X','yhat')
      df11a$AgroClmInd <- file1[11];            df11a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[12])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[12], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df12a <- p$dat;                     colnames(df12a) <- c('X', 'yhat')
      df12a$AgroClmInd <- file1[12];      df12a$Crop <- 'maize'
    } else { 
      df12a <- data.frame(matrix(NA,ncol = 2)); colnames(df12a) <- c('X','yhat')
      df12a$AgroClmInd <- file1[12];            df12a$Crop <- 'maize'
    }
  } else {   
    df1a <- data.frame(matrix(NA,ncol = 2));     colnames(df1a) <- c('X','yhat')
    df1a$AgroClmInd <- file1[1];                 df1a$Crop <- 'maize'
    df2a <- data.frame(matrix(NA,ncol = 2));     colnames(df2a) <- c('X','yhat')
    df2a$AgroClmInd <- file1[2];                 df2a$Crop <- 'maize'
    df3a <- data.frame(matrix(NA,ncol = 2));     colnames(df3a) <- c('X','yhat')
    df3a$AgroClmInd <- file1[3];                 df3a$Crop <- 'maize'
    df4a <- data.frame(matrix(NA,ncol = 2));     colnames(df4a) <- c('X','yhat')
    df4a$AgroClmInd <- file1[4];                 df4a$Crop <- 'maize'
    df5a <- data.frame(matrix(NA,ncol = 2));     colnames(df5a) <- c('X','yhat')
    df5a$AgroClmInd <- file1[5];                 df5a$Crop <- 'maize'
    df6a <- data.frame(matrix(NA,ncol = 2));     colnames(df6a) <- c('X','yhat')
    df6a$AgroClmInd <- file1[6];                 df6a$Crop <- 'maize'
    df7a <- data.frame(matrix(NA,ncol = 2));     colnames(df7a) <- c('X','yhat')
    df7a$AgroClmInd <- file1[7];                 df7a$Crop <- 'maize'
    df8a <- data.frame(matrix(NA,ncol = 2));     colnames(df8a) <- c('X','yhat')
    df8a$AgroClmInd <- file1[8];                 df8a$Crop <- 'maize'
    df9a <- data.frame(matrix(NA,ncol = 2));     colnames(df9a) <- c('X','yhat')
    df9a$AgroClmInd <- file1[9];                 df9a$Crop <- 'maize'
    df10a <- data.frame(matrix(NA,ncol = 2));    colnames(df10a) <- c('X','yhat')
    df10a$AgroClmInd <- file1[10];               df10a$Crop <- 'maize'
    df11a <- data.frame(matrix(NA,ncol = 2));    colnames(df11a) <- c('X','yhat')
    df11a$AgroClmInd <- file1[11];               df11a$Crop <- 'maize'
    df12a <- data.frame(matrix(NA,ncol = 2));    colnames(df12a) <- c('X','yhat')
    df12a$AgroClmInd <- file1[12];               df12a$Crop <- 'maize'
  }
  PDPReg <- rbind(df1a, df2a, df3a, df4a,  df5a,  df6a, 
                  df7a, df8a, df9a, df10a, df11a, df12a) 
  
  #rice #
  if(is.na(RRFdat) != TRUE & is.na(Rimp$AgroInd)[1] != TRUE){
    m <- which(Rimp$AgroInd == file1[1])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[1], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df1b <- p$dat;                     colnames(df1b) <- c('X', 'yhat')
      df1b$AgroClmInd <- file1[1];       df1b$Crop <- 'rice'
    } else { 
      df1b <- data.frame(matrix(NA,ncol = 2)); colnames(df1b) <- c('X','yhat')
      df1b$AgroClmInd <- file1[1];             df1b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[2])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[2], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df2b <- p$dat;                     colnames(df2b) <- c('X', 'yhat')
      df2b$AgroClmInd <- file1[2];       df2b$Crop <- 'rice'
    } else { 
      df2b <- data.frame(matrix(NA,ncol = 2)); colnames(df2b) <- c('X','yhat')
      df2b$AgroClmInd <- file1[2];             df2b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[3])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[3], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df3b <- p$dat;                     colnames(df3b) <- c('X', 'yhat')
      df3b$AgroClmInd <- file1[3];       df3b$Crop <- 'rice'
    } else { 
      df3b <- data.frame(matrix(NA,ncol = 2)); colnames(df3b) <- c('X','yhat')
      df3b$AgroClmInd <- file1[3];             df3b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[4])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[4], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df4b <- p$dat;                     colnames(df4b) <- c('X', 'yhat')
      df4b$AgroClmInd <- file1[4];       df4b$Crop <- 'rice'
    } else { 
      df4b <- data.frame(matrix(NA,ncol = 2)); colnames(df4b) <- c('X','yhat')
      df4b$AgroClmInd <- file1[4];             df4b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[5])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[5], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df5b <- p$dat;                     colnames(df5b) <- c('X', 'yhat')
      df5b$AgroClmInd <- file1[5];       df5b$Crop <- 'rice'
    } else { 
      df5b <- data.frame(matrix(NA,ncol = 2)); colnames(df5b) <- c('X','yhat')
      df5b$AgroClmInd <- file1[5];             df5b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[6])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[6], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df6b <- p$dat;                     colnames(df6b) <- c('X', 'yhat')
      df6b$AgroClmInd <- file1[6];       df6b$Crop <- 'rice'
    } else { 
      df6b <- data.frame(matrix(NA,ncol = 2)); colnames(df6b) <- c('X','yhat')
      df6b$AgroClmInd <- file1[6];             df6b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[7])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[7], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df7b <- p$dat;                     colnames(df7b) <- c('X', 'yhat')
      df7b$AgroClmInd <- file1[7];       df7b$Crop <- 'rice'
    } else { 
      df7b <- data.frame(matrix(NA,ncol = 2)); colnames(df7b) <- c('X','yhat')
      df7b$AgroClmInd <- file1[7];             df7b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[8])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[8], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df8b <- p$dat;                     colnames(df8b) <- c('X', 'yhat')
      df8b$AgroClmInd <- file1[8];       df8b$Crop <- 'rice'
    } else { 
      df8b <- data.frame(matrix(NA,ncol = 2)); colnames(df8b) <- c('X','yhat')
      df8b$AgroClmInd <- file1[8];             df8b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[9])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[9], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df9b <- p$dat;                     colnames(df9b) <- c('X', 'yhat')
      df9b$AgroClmInd <- file1[9];       df9b$Crop <- 'rice'
    } else { 
      df9b <- data.frame(matrix(NA,ncol = 2)); colnames(df9b) <- c('X','yhat')
      df9b$AgroClmInd <- file1[9];             df9b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[10])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[10], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df10b <- p$dat;                     colnames(df10b) <- c('X', 'yhat')
      df10b$AgroClmInd <- file1[10];      df10b$Crop <- 'rice'
    } else { 
      df10b <- data.frame(matrix(NA,ncol = 2)); colnames(df10b) <- c('X','yhat')
      df10b$AgroClmInd <- file1[10];            df10b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[11])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[11], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df11b <- p$dat;                     colnames(df11b) <- c('X', 'yhat')
      df11b$AgroClmInd <- file1[11];      df11b$Crop <- 'rice'
    } else { 
      df11b <- data.frame(matrix(NA,ncol = 2)); colnames(df11b) <- c('X','yhat')
      df11b$AgroClmInd <- file1[11];            df11b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[12])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[12], plot = TRUE,   
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df12b <- p$dat;                     colnames(df12b) <- c('X', 'yhat')
      df12b$AgroClmInd <- file1[12];      df12b$Crop <- 'rice'
    } else { 
      df12b <- data.frame(matrix(NA,ncol = 2)); colnames(df12b) <- c('X','yhat')
      df12b$AgroClmInd <- file1[12];            df12b$Crop <- 'rice'
    }
  } else {   
    df1b <- data.frame(matrix(NA,ncol = 2));     colnames(df1b) <- c('X','yhat')
    df1b$AgroClmInd <- file1[1];                 df1b$Crop <- 'rice'
    df2b <- data.frame(matrix(NA,ncol = 2));     colnames(df2b) <- c('X','yhat')
    df2b$AgroClmInd <- file1[2];                 df2b$Crop <- 'rice'
    df3b <- data.frame(matrix(NA,ncol = 2));     colnames(df3b) <- c('X','yhat')
    df3b$AgroClmInd <- file1[3];                 df3b$Crop <- 'rice'
    df4b <- data.frame(matrix(NA,ncol = 2));     colnames(df4b) <- c('X','yhat')
    df4b$AgroClmInd <- file1[4];                 df4b$Crop <- 'rice'
    df5b <- data.frame(matrix(NA,ncol = 2));     colnames(df5b) <- c('X','yhat')
    df5b$AgroClmInd <- file1[5];                 df5b$Crop <- 'rice'
    df6b <- data.frame(matrix(NA,ncol = 2));     colnames(df6b) <- c('X','yhat')
    df6b$AgroClmInd <- file1[6];                 df6b$Crop <- 'rice'
    df7b <- data.frame(matrix(NA,ncol = 2));     colnames(df7b) <- c('X','yhat')
    df7b$AgroClmInd <- file1[7];                 df7b$Crop <- 'rice'
    df8b <- data.frame(matrix(NA,ncol = 2));     colnames(df8b) <- c('X','yhat')
    df8b$AgroClmInd <- file1[8];                 df8b$Crop <- 'rice'
    df9b <- data.frame(matrix(NA,ncol = 2));     colnames(df9b) <- c('X','yhat')
    df9b$AgroClmInd <- file1[9];                 df9b$Crop <- 'rice'
    df10b <- data.frame(matrix(NA,ncol = 2));    colnames(df10b) <- c('X','yhat')
    df10b$AgroClmInd <- file1[10];               df10b$Crop <- 'rice'
    df11b <- data.frame(matrix(NA,ncol = 2));    colnames(df11b) <- c('X','yhat')
    df11b$AgroClmInd <- file1[11];               df11b$Crop <- 'rice'
    df12b <- data.frame(matrix(NA,ncol = 2));    colnames(df12b) <- c('X','yhat')
    df12b$AgroClmInd <- file1[12];               df12b$Crop <- 'rice'
  }
  PDPReg <- rbind(PDPReg, df1b, df2b, df3b, df4b,  df5b,  df6b, 
                          df7b, df8b, df9b, df10b, df11b, df12b)
  
  #soya
  if(is.na(SRFdat) != TRUE & is.na(Simp$AgroInd)[1] != TRUE){
    m <- which(Simp$AgroInd == file1[1])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[1], plot = TRUE,   
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df1c <- p$dat;                     colnames(df1c) <- c('X', 'yhat')
      df1c$AgroClmInd <- file1[1];       df1c$Crop <- 'soya'
    } else { 
      df1c <- data.frame(matrix(NA,ncol = 2)); colnames(df1c) <- c('X','yhat')
      df1c$AgroClmInd <- file1[1];             df1c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[2])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[2], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df2c <- p$dat;                     colnames(df2c) <- c('X', 'yhat')
      df2c$AgroClmInd <- file1[2];       df2c$Crop <- 'soya'
    } else { 
      df2c <- data.frame(matrix(NA,ncol = 2)); colnames(df2c) <- c('X','yhat')
      df2c$AgroClmInd <- file1[2];             df2c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[3])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[3], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df3c <- p$dat;                     colnames(df3c) <- c('X', 'yhat')
      df3c$AgroClmInd <- file1[3];       df3c$Crop <- 'soya'
    } else { 
      df3c <- data.frame(matrix(NA,ncol = 2)); colnames(df3c) <- c('X','yhat')
      df3c$AgroClmInd <- file1[3];             df3c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[4])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[4], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df4c <- p$dat;                     colnames(df4c) <- c('X', 'yhat')
      df4c$AgroClmInd <- file1[4];       df4c$Crop <- 'soya'
    } else { 
      df4c <- data.frame(matrix(NA,ncol = 2)); colnames(df4c) <- c('X','yhat')
      df4c$AgroClmInd <- file1[4];             df4c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[5])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[5], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df5c <- p$dat;                     colnames(df5c) <- c('X', 'yhat')
      df5c$AgroClmInd <- file1[5];       df5c$Crop <- 'soya'
    } else { 
      df5c <- data.frame(matrix(NA,ncol = 2)); colnames(df5c) <- c('X','yhat')
      df5c$AgroClmInd <- file1[5];             df5c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[6])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[6], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df6c <- p$dat;                     colnames(df6c) <- c('X', 'yhat')
      df6c$AgroClmInd <- file1[6];       df6c$Crop <- 'soya'
    } else { 
      df6c <- data.frame(matrix(NA,ncol = 2)); colnames(df6c) <- c('X','yhat')
      df6c$AgroClmInd <- file1[6];             df6c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[7])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[7], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df7c <- p$dat;                     colnames(df7c) <- c('X', 'yhat')
      df7c$AgroClmInd <- file1[7];       df7c$Crop <- 'soya'
    } else { 
      df7c <- data.frame(matrix(NA,ncol = 2)); colnames(df7c) <- c('X','yhat')
      df7c$AgroClmInd <- file1[7];             df7c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[8])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[8], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df8c <- p$dat;                     colnames(df8c) <- c('X', 'yhat')
      df8c$AgroClmInd <- file1[8];       df8c$Crop <- 'soya'
    } else { 
      df8c <- data.frame(matrix(NA,ncol = 2)); colnames(df8c) <- c('X','yhat')
      df8c$AgroClmInd <- file1[8];             df8c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[9])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[9], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df9c <- p$dat;                     colnames(df9c) <- c('X', 'yhat')
      df9c$AgroClmInd <- file1[9];       df9c$Crop <- 'soya'
    } else { 
      df9c <- data.frame(matrix(NA,ncol = 2)); colnames(df9c) <- c('X','yhat')
      df9c$AgroClmInd <- file1[9];             df9c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[10])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[10], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df10c <- p$dat;                     colnames(df10c) <- c('X', 'yhat')
      df10c$AgroClmInd <- file1[10];      df10c$Crop <- 'soya'
    } else { 
      df10c <- data.frame(matrix(NA,ncol = 2)); colnames(df10c) <- c('X','yhat')
      df10c$AgroClmInd <- file1[10];            df10c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[11])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[11], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df11c <- p$dat;                     colnames(df11c) <- c('X', 'yhat')
      df11c$AgroClmInd <- file1[11];      df11c$Crop <- 'soya'
    } else { 
      df11c <- data.frame(matrix(NA,ncol = 2)); colnames(df11c) <- c('X','yhat')
      df11c$AgroClmInd <- file1[11];            df11c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[12])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[12], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df12c <- p$dat;                     colnames(df12c) <- c('X', 'yhat')
      df12c$AgroClmInd <- file1[12];      df12c$Crop <- 'soya'
    } else { 
      df12c <- data.frame(matrix(NA,ncol = 2)); colnames(df12c) <- c('X','yhat')
      df12c$AgroClmInd <- file1[12];            df12c$Crop <- 'soya'
    }
  } else {   
    df1c <- data.frame(matrix(NA,ncol = 2));     colnames(df1c) <- c('X','yhat')
    df1c$AgroClmInd <- file1[1];                 df1c$Crop <- 'soya'
    df2c <- data.frame(matrix(NA,ncol = 2));     colnames(df2c) <- c('X','yhat')
    df2c$AgroClmInd <- file1[2];                 df2c$Crop <- 'soya'
    df3c <- data.frame(matrix(NA,ncol = 2));     colnames(df3c) <- c('X','yhat')
    df3c$AgroClmInd <- file1[3];                 df3c$Crop <- 'soya'
    df4c <- data.frame(matrix(NA,ncol = 2));     colnames(df4c) <- c('X','yhat')
    df4c$AgroClmInd <- file1[4];                 df4c$Crop <- 'soya'
    df5c <- data.frame(matrix(NA,ncol = 2));     colnames(df5c) <- c('X','yhat')
    df5c$AgroClmInd <- file1[5];                 df5c$Crop <- 'soya'
    df6c <- data.frame(matrix(NA,ncol = 2));     colnames(df6c) <- c('X','yhat')
    df6c$AgroClmInd <- file1[6];                 df6c$Crop <- 'soya'
    df7c <- data.frame(matrix(NA,ncol = 2));     colnames(df7c) <- c('X','yhat')
    df7c$AgroClmInd <- file1[7];                 df7c$Crop <- 'soya'
    df8c <- data.frame(matrix(NA,ncol = 2));     colnames(df8c) <- c('X','yhat')
    df8c$AgroClmInd <- file1[8];                 df8c$Crop <- 'soya'
    df9c <- data.frame(matrix(NA,ncol = 2));     colnames(df9c) <- c('X','yhat')
    df9c$AgroClmInd <- file1[9];                 df9c$Crop <- 'soya'
    df10c <- data.frame(matrix(NA,ncol = 2));    colnames(df10c) <- c('X','yhat')
    df10c$AgroClmInd <- file1[10];               df10c$Crop <- 'soya'
    df11c <- data.frame(matrix(NA,ncol = 2));    colnames(df11c) <- c('X','yhat')
    df11c$AgroClmInd <- file1[11];               df11c$Crop <- 'soya'
    df12c <- data.frame(matrix(NA,ncol = 2));    colnames(df12c) <- c('X','yhat')
    df12c$AgroClmInd <- file1[12];               df12c$Crop <- 'soya'
  }
  PDPReg <- rbind(PDPReg, df1c, df2c, df3c, df4c,  df5c,  df6c, 
                  df7c, df8c, df9c, df10c, df11c, df12c)
  
  #wheat
  if(is.na(WRFdat) != TRUE & is.na(Wimp$AgroInd)[1] != TRUE){
    m <- which(Wimp$AgroInd == file1[1])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[1], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df1d <- p$dat;                     colnames(df1d) <- c('X', 'yhat')
      df1d$AgroClmInd <- file1[1];       df1d$Crop <- 'wheat'
    } else { 
      df1d <- data.frame(matrix(NA,ncol = 2)); colnames(df1d) <- c('X','yhat')
      df1d$AgroClmInd <- file1[1];             df1d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[2])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[2], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df2d <- p$dat;                     colnames(df2d) <- c('X', 'yhat')
      df2d$AgroClmInd <- file1[2];       df2d$Crop <- 'wheat'
    } else { 
      df2d <- data.frame(matrix(NA,ncol = 2)); colnames(df2d) <- c('X','yhat')
      df2d$AgroClmInd <- file1[2];             df2d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[3])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[3], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df3d <- p$dat;                     colnames(df3d) <- c('X', 'yhat')
      df3d$AgroClmInd <- file1[3];       df3d$Crop <- 'wheat'
    } else { 
      df3d <- data.frame(matrix(NA,ncol = 2)); colnames(df3d) <- c('X','yhat')
      df3d$AgroClmInd <- file1[3];             df3d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[4])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[4], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df4d <- p$dat;                     colnames(df4d) <- c('X', 'yhat')
      df4d$AgroClmInd <- file1[4];       df4d$Crop <- 'wheat'
    } else { 
      df4d <- data.frame(matrix(NA,ncol = 2)); colnames(df4d) <- c('X','yhat')
      df4d$AgroClmInd <- file1[4];             df4d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[5])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[5], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df5d <- p$dat;                     colnames(df5d) <- c('X', 'yhat')
      df5d$AgroClmInd <- file1[5];       df5d$Crop <- 'wheat'
    } else { 
      df5d <- data.frame(matrix(NA,ncol = 2)); colnames(df5d) <- c('X','yhat')
      df5d$AgroClmInd <- file1[5];             df5d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[6])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[6], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df6d <- p$dat;                     colnames(df6d) <- c('X', 'yhat')
      df6d$AgroClmInd <- file1[6];       df6d$Crop <- 'wheat'
    } else { 
      df6d <- data.frame(matrix(NA,ncol = 2)); colnames(df6d) <- c('X','yhat')
      df6d$AgroClmInd <- file1[6];             df6d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[7])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[7], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df7d <- p$dat;                     colnames(df7d) <- c('X', 'yhat')
      df7d$AgroClmInd <- file1[7];       df7d$Crop <- 'wheat'
    } else { 
      df7d <- data.frame(matrix(NA,ncol = 2)); colnames(df7d) <- c('X','yhat')
      df7d$AgroClmInd <- file1[7];             df7d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[8])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[8], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df8d <- p$dat;                     colnames(df8d) <- c('X', 'yhat')
      df8d$AgroClmInd <- file1[8];       df8d$Crop <- 'wheat'
    } else { 
      df8d <- data.frame(matrix(NA,ncol = 2)); colnames(df8d) <- c('X','yhat')
      df8d$AgroClmInd <- file1[8];             df8d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[9])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[9], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df9d <- p$dat;                     colnames(df9d) <- c('X', 'yhat')
      df9d$AgroClmInd <- file1[9];       df9d$Crop <- 'wheat'
    } else { 
      df9d <- data.frame(matrix(NA,ncol = 2)); colnames(df9d) <- c('X','yhat')
      df9d$AgroClmInd <- file1[9];             df9d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[10])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[10], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df10d <- p$dat;                     colnames(df10d) <- c('X', 'yhat')
      df10d$AgroClmInd <- file1[10];      df10d$Crop <- 'wheat'
    } else { 
      df10d <- data.frame(matrix(NA,ncol = 2)); colnames(df10d) <- c('X','yhat')
      df10d$AgroClmInd <- file1[10];            df10d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[11])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[11], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df11d <- p$dat;                     colnames(df11d) <- c('X', 'yhat')
      df11d$AgroClmInd <- file1[11];      df11d$Crop <- 'wheat'
    } else { 
      df11d <- data.frame(matrix(NA,ncol = 2)); colnames(df11d) <- c('X','yhat')
      df11d$AgroClmInd <- file1[11];            df11d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[12])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[12], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df12d <- p$dat;                     colnames(df12d) <- c('X', 'yhat')
      df12d$AgroClmInd <- file1[12];      df12d$Crop <- 'wheat'
    } else { 
      df12d <- data.frame(matrix(NA,ncol = 2)); colnames(df12d) <- c('X','yhat')
      df12d$AgroClmInd <- file1[12];            df12d$Crop <- 'wheat'
    }
  } else {   
    df1d <- data.frame(matrix(NA,ncol = 2));     colnames(df1d) <- c('X','yhat')
    df1d$AgroClmInd <- file1[1];                 df1d$Crop <- 'wheat'
    df2d <- data.frame(matrix(NA,ncol = 2));     colnames(df2d) <- c('X','yhat')
    df2d$AgroClmInd <- file1[2];                 df2d$Crop <- 'wheat'
    df3d <- data.frame(matrix(NA,ncol = 2));     colnames(df3d) <- c('X','yhat')
    df3d$AgroClmInd <- file1[3];                 df3d$Crop <- 'wheat'
    df4d <- data.frame(matrix(NA,ncol = 2));     colnames(df4d) <- c('X','yhat')
    df4d$AgroClmInd <- file1[4];                 df4d$Crop <- 'wheat'
    df5d <- data.frame(matrix(NA,ncol = 2));     colnames(df5d) <- c('X','yhat')
    df5d$AgroClmInd <- file1[5];                 df5d$Crop <- 'wheat'
    df6d <- data.frame(matrix(NA,ncol = 2));     colnames(df6d) <- c('X','yhat')
    df6d$AgroClmInd <- file1[6];                 df6d$Crop <- 'wheat'
    df7d <- data.frame(matrix(NA,ncol = 2));     colnames(df7d) <- c('X','yhat')
    df7d$AgroClmInd <- file1[7];                 df7d$Crop <- 'wheat'
    df8d <- data.frame(matrix(NA,ncol = 2));     colnames(df8d) <- c('X','yhat')
    df8d$AgroClmInd <- file1[8];                 df8d$Crop <- 'wheat'
    df9d <- data.frame(matrix(NA,ncol = 2));     colnames(df9d) <- c('X','yhat')
    df9d$AgroClmInd <- file1[9];                 df9d$Crop <- 'wheat'
    df10d <- data.frame(matrix(NA,ncol = 2));    colnames(df10d) <- c('X','yhat')
    df10d$AgroClmInd <- file1[10];               df10d$Crop <- 'wheat'
    df11d <- data.frame(matrix(NA,ncol = 2));    colnames(df11d) <- c('X','yhat')
    df11d$AgroClmInd <- file1[11];               df11d$Crop <- 'wheat'
    df12d <- data.frame(matrix(NA,ncol = 2));    colnames(df12d) <- c('X','yhat')
    df12d$AgroClmInd <- file1[12];               df12d$Crop <- 'wheat'
  }
  PDPReg <- rbind(PDPReg, df1d, df2d, df3d, df4d,  df5d,  df6d, 
                  df7d, df8d, df9d, df10d, df11d, df12d)
  
  rm(df1a, df1b, df1c, df1d,     df2a, df2b, df2c, df2d,     df3a, df3b, df3c, df3d,
     df4a, df4b, df4c, df4d,     df5a, df5b, df5c, df5d,     df6a, df6b, df6c, df6d,
     df7a, df7b, df7c, df7d,     df8a, df8b, df8c, df8d,     df9a, df9b, df9c, df9d,
     df10a, df10b, df10c, df10d, df11a, df11b, df11c, df11d, df12a, df12b, df12c, df12d)
  
  # ROC ####
  # references: https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/ 
  # https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/
  # https://en.wikipedia.org/wiki/Receiver_operating_characteristic 
  
  dat <- tibble()
  AUCCrop <- tibble() 
  #colnames(AUCCrop) <- c('AUC', 'Crop', 'Region')
  
  # Maize
  if (is.na(MRFdat) == FALSE){
    pred4ROC <- predict(MRFdat, MdatXtest, type= 'prob') %>% data.frame()
    # Define which observations belong to class
    true_values <- ifelse(MdatYtest=='failure',1,0)
    # Assess the performance of classifier for class[i]
    pred <- ROCR::prediction(pred4ROC$failure,true_values)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    x <- tibble(unlist(perf@x.values), unlist(perf@y.values), 'maize') 
    colnames(x) <- c(perf@x.name, perf@y.name,'Crop')
    dat <- rbind(dat, x)
    
    auc.perf <- ROCR::performance(pred, measure = "auc")
    x <- tibble(unlist(auc.perf@y.values),'maize',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  } else {
    x <- tibble("False positive rate" = NA,  
                "True positive rate" = NA, 
                'Crop' ='maize')
    dat <- rbind(dat, x)
    x <- tibble(NA,'maize',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  }
  
  
  # Rice
  if (is.na(RRFdat) == FALSE){
    pred4ROC <- predict(RRFdat, RdatXtest, type= 'prob') %>% data.frame()
    # Define which observations belong to class
    true_values <- ifelse(RdatYtest=='failure',1,0)
    # Assess the performance of classifier for class[i]
    pred <- ROCR::prediction(pred4ROC$failure,true_values)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    x <- tibble(unlist(perf@x.values), unlist(perf@y.values), 'rice') 
    colnames(x) <- c(perf@x.name, perf@y.name,'Crop')
    dat <- rbind(dat, x)
    auc.perf <- ROCR::performance(pred, measure = "auc")
    x <- tibble(unlist(auc.perf@y.values),'rice',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  } else {
    x <- tibble("False positive rate" = NA,  
                "True positive rate" = NA, 
                'Crop' ='rice')
    dat <- rbind(dat, x)
    x <- tibble(NA,'rice',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  }
  
    # Soya
  if (is.na(SRFdat) == FALSE){
    pred4ROC <- predict(SRFdat, SdatXtest, type= 'prob') %>% data.frame()
    # Define which observations belong to class
    true_values <- ifelse(SdatYtest=='failure',1,0)
    # Assess the performance of classifier for class[i]
    pred <- ROCR::prediction(pred4ROC$failure,true_values)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    x <- tibble(unlist(perf@x.values), unlist(perf@y.values), 'soya') 
    colnames(x) <- c(perf@x.name, perf@y.name,'Crop')
    dat <- rbind(dat, x)
    auc.perf <- ROCR::performance(pred, measure = "auc")
    x <- tibble(unlist(auc.perf@y.values),'soya',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
    
  } else {
    x <- tibble("False positive rate" = NA,  
                "True positive rate" = NA, 
                'Crop' ='soya')
    dat <- rbind(dat, x)
    x <- tibble(NA,'soya',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  }
  
  # Wheat
  if (is.na(WRFdat) == FALSE){
    pred4ROC <- predict(WRFdat, WdatXtest, type= 'prob') %>% data.frame()
    # Define which observations belong to class
    true_values <- ifelse(WdatYtest=='failure',1,0)
    # Assess the performance of classifier for class[i]
    pred <- ROCR::prediction(pred4ROC$failure,true_values)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    x <- tibble(unlist(perf@x.values), unlist(perf@y.values), 'wheat') 
    colnames(x) <- c(perf@x.name, perf@y.name,'Crop')
    dat <- rbind(dat, x)
    auc.perf <- ROCR::performance(pred, measure = "auc")
    x <- tibble(unlist(auc.perf@y.values),'wheat',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
    
  } else {
    x <- tibble("False positive rate" = NA,  
                "True positive rate" = NA, 
                'Crop' ='wheat')
    dat <- rbind(dat, x)
    x <- tibble(NA,'wheat',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  }
  # Forming dataframes to save ####
  RFList <- list()
  
  dat$Region <- key
  name <- paste0('r_',key,'_ROCR')
  RFList[[name]] <- dat
  name <- paste0('r_',key,'_AUC')
  RFList[[name]] <- AUCCrop  
  name <- paste0('r_',key,'_Imp')
  RFList[[name]] <- rbind(Mimp, Rimp, Simp, Wimp) %>%
    arrange(Crop, desc(MeanDecreaseGini)) 
  PDPReg$Region <- key
  name <- paste0('r_',key,'_PDPR')
  RFList[[name]] <- PDPReg
  
  rm(pred4ROC,true_values, pred, perf, auc.perf, x)
  
  return(RFList)
}
g_randomForest <- function(X, varis){
  #     This function will find the regional yield and agroclimate indices. Shaping 
  # them into a long format of [Index, yr, yield, AgroInd], before testing to see if
  # there is more or less than 30% of the AgroInd dat missing. If less than 30% a 
  # column of AgroInd dat is missing the individual rows will be removed. If more 
  # than 30% of the column (AgroInd) will be removed. Data will be split into testing
  # and training data, before being sent to Random Forest, the importance should be 
  # TRUE. The Partial Dependency Plot and ROC curve will be ploted and saved. Send 
  # back the importance for each region.
  
  # Packages Required ####
  require(ggplot2)
  require(randomForest)
  require(pdp)
  require(ROCR)
  # require(tidyverse)
  require(dplyr); require(forcats); require(tibble)
  require(readr); require(stringr); require(tidyr)
  require(purrr); require(magrittr)
  
  # Variables needed #####
  key <- varis[['key']]
  datAgro <-varis[["datAgro"]]
  file1 <- c(varis[["file1"]], varis[["file2"]])
  file2 <- varis[["file2"]]
  m <- varis[['mask']]
  
  # Random Forest Data Shaping ####
  # Maize
  name <- vector()
  for (j in file2){ name <- c(name, paste0(j, '_', varis[['crop']][1]))}
  
  datRF <- unlist(matrix(varis[['MdatYield']][m, 5:ncol(varis[['MdatYield']])], ncol = 1, byrow = TRUE)) %>%
    cbind(unlist(matrix(datAgro[[file1[1]]][m, 5:ncol(datAgro[[file1[1]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[2]]][m, 5:ncol(datAgro[[file1[2]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[3]]][m, 5:ncol(datAgro[[file1[3]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[4]]][m, 5:ncol(datAgro[[file1[4]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[5]]][m, 5:ncol(datAgro[[file1[5]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[1]]] [m, 5:ncol(datAgro[[name[1]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[2]]] [m, 5:ncol(datAgro[[name[2]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[3]]] [m, 5:ncol(datAgro[[name[3]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[4]]] [m, 5:ncol(datAgro[[name[4]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[5]]] [m, 5:ncol(datAgro[[name[5]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[6]]] [m, 5:ncol(datAgro[[name[6]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[7]]] [m, 5:ncol(datAgro[[name[7]]])],  ncol = 1, byrow = TRUE)))
  
  colnames(datRF) <- c('yield', file1)
  datRF[datRF == -999] <- NA 
  datRF <- datRF[complete.cases(datRF[,1]),] 
  datRF <- data.frame(datRF)
  datRF[,1][datRF[,1] == '1'] <- 'failure'
  datRF[,1][datRF[,1] == '2'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '3'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '4'] <- 'nonfailure'
  
  # Need to remove points with NA and if >30% of predictor is NA
  drop <- array(dim=c(0,0))
  if (dim(datRF)[1] > 0){
    for (i in 2:dim(datRF)[2]){
      if ((sum(is.na(datRF[,i]))/dim(datRF)[1]) < 0.3){
        datRF <- datRF[complete.cases(datRF[,i]),]
      } else {
        drop <- c(drop,i)
      }
      if (i == dim(datRF)[2] & (length(drop) >= 1)){
        datRF <- datRF[,-drop]
      } else {
        # Do nothing
      }
      # print(dim(datRF))
    }
  }
  
  # Split into training and testing data
  Mmtry <- round(sqrt(dim(datRF)[2]),0)
  train <- sample(seq_len(nrow(datRF)), replace=FALSE, size = floor(0.70*nrow(datRF)))
  datRFtrain <- datRF[train,]
  datRFtest <- datRF[-train,]
  MdatYtrain <- as.factor(datRFtrain[,1]);         MdatYtest <- as.factor(datRFtest[,1])
  MdatXtrain <- datRFtrain[,2:ncol(datRFtrain)];   MdatXtest <- datRFtest[,2:ncol(datRFtest)]
  
  
  # Rice
  name <- vector()
  for (j in file2){ name <- c(name, paste0(j, '_', varis[['crop']][2]))}
  datRF <- unlist(matrix(varis[['RdatYield']][m, 5:ncol(varis[['RdatYield']])], ncol = 1, byrow = TRUE)) %>%
    cbind(unlist(matrix(datAgro[[file1[1]]][m, 5:ncol(datAgro[[file1[1]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[2]]][m, 5:ncol(datAgro[[file1[2]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[3]]][m, 5:ncol(datAgro[[file1[3]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[4]]][m, 5:ncol(datAgro[[file1[4]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[5]]][m, 5:ncol(datAgro[[file1[5]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[1]]] [m, 5:ncol(datAgro[[name[1]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[2]]] [m, 5:ncol(datAgro[[name[2]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[3]]] [m, 5:ncol(datAgro[[name[3]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[4]]] [m, 5:ncol(datAgro[[name[4]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[5]]] [m, 5:ncol(datAgro[[name[5]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[6]]] [m, 5:ncol(datAgro[[name[6]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[7]]] [m, 5:ncol(datAgro[[name[7]]])],  ncol = 1, byrow = TRUE)))
  
  colnames(datRF) <- c('yield', file1)
  datRF[datRF == -999] <- NA 
  datRF <- datRF[complete.cases(datRF[,1]),] 
  datRF <- data.frame(datRF)
  datRF[,1][datRF[,1] == '1'] <- 'failure'
  datRF[,1][datRF[,1] == '2'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '3'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '4'] <- 'nonfailure'
  
  # Need to remove points with NA and if >30% of predictor is NA
  drop <- array(dim=c(0,0))
  if (dim(datRF)[1] > 0){
    for (i in 2:dim(datRF)[2]){
      if ((sum(is.na(datRF[,i]))/dim(datRF)[1]) < 0.3){
        datRF <- datRF[complete.cases(datRF[,i]),]
      } else {
        drop <- c(drop,i)
      }
      if (i == dim(datRF)[2] & (length(drop) >= 1)){
        datRF <- datRF[,-drop]
      } else {
        # Do nothing
      }
      # print(dim(datRF))
    }
  }
  
  # Split into training and testing data
  Rmtry <- round(sqrt(dim(datRF)[2]),0)
  train <- sample(seq_len(nrow(datRF)), replace=FALSE, size = floor(0.70*nrow(datRF)))
  datRFtrain <- datRF[train,]
  datRFtest <- datRF[-train,]
  RdatYtrain <- as.factor(datRFtrain[,1]);         RdatYtest <- as.factor(datRFtest[,1])
  RdatXtrain <- datRFtrain[,2:ncol(datRFtrain)];   RdatXtest <- datRFtest[,2:ncol(datRFtest)]
  
  
  # Soya
  name <- vector()
  for (j in file2){ name <- c(name, paste0(j, '_', varis[['crop']][3]))}
  datRF <- unlist(matrix(varis[['SdatYield']][m, 5:ncol(varis[['SdatYield']])], ncol = 1, byrow = TRUE)) %>%
    cbind(unlist(matrix(datAgro[[file1[1]]][m, 5:ncol(datAgro[[file1[1]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[2]]][m, 5:ncol(datAgro[[file1[2]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[3]]][m, 5:ncol(datAgro[[file1[3]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[4]]][m, 5:ncol(datAgro[[file1[4]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[5]]][m, 5:ncol(datAgro[[file1[5]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[1]]] [m, 5:ncol(datAgro[[name[1]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[2]]] [m, 5:ncol(datAgro[[name[2]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[3]]] [m, 5:ncol(datAgro[[name[3]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[4]]] [m, 5:ncol(datAgro[[name[4]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[5]]] [m, 5:ncol(datAgro[[name[5]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[6]]] [m, 5:ncol(datAgro[[name[6]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[7]]] [m, 5:ncol(datAgro[[name[7]]])],  ncol = 1, byrow = TRUE)))
  
  colnames(datRF) <- c('yield', file1)
  datRF[datRF == -999] <- NA 
  datRF <- datRF[complete.cases(datRF[,1]),] 
  datRF <- data.frame(datRF)
  datRF[,1][datRF[,1] == '1'] <- 'failure'
  datRF[,1][datRF[,1] == '2'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '3'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '4'] <- 'nonfailure'
  
  # Need to remove points with NA and if >30% of predictor is NA
  drop <- array(dim=c(0,0))
  if (dim(datRF)[1] > 0){
    for (i in 2:dim(datRF)[2]){
      if ((sum(is.na(datRF[,i]))/dim(datRF)[1]) < 0.3){
        datRF <- datRF[complete.cases(datRF[,i]),]
      } else {
        drop <- c(drop,i)
      }
      if (i == dim(datRF)[2] & (length(drop) >= 1)){
        datRF <- datRF[,-drop]
      } else {
        # Do nothing
      }
      # print(dim(datRF))
    }
  }
  
  # Split into training and testing data
  Smtry <- round(sqrt(dim(datRF)[2]),0)
  train <- sample(seq_len(nrow(datRF)), replace=FALSE, size = floor(0.70*nrow(datRF)))
  datRFtrain <- datRF[train,]
  datRFtest <- datRF[-train,]
  SdatYtrain <- as.factor(datRFtrain[,1]);         SdatYtest <- as.factor(datRFtest[,1])
  SdatXtrain <- datRFtrain[,2:ncol(datRFtrain)];   SdatXtest <- datRFtest[,2:ncol(datRFtest)]
  
  
  # Wheat
  name <- vector()
  for (j in file2){ name <- c(name, paste0(j, '_', varis[['crop']][4]))}
  datRF <- unlist(matrix(varis[['WdatYield']][m, 5:ncol(varis[['WdatYield']])], ncol = 1, byrow = TRUE)) %>%
    cbind(unlist(matrix(datAgro[[file1[1]]][m, 5:ncol(datAgro[[file1[1]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[2]]][m, 5:ncol(datAgro[[file1[2]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[3]]][m, 5:ncol(datAgro[[file1[3]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[4]]][m, 5:ncol(datAgro[[file1[4]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[file1[5]]][m, 5:ncol(datAgro[[file1[5]]])], ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[1]]] [m, 5:ncol(datAgro[[name[1]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[2]]] [m, 5:ncol(datAgro[[name[2]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[3]]] [m, 5:ncol(datAgro[[name[3]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[4]]] [m, 5:ncol(datAgro[[name[4]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[5]]] [m, 5:ncol(datAgro[[name[5]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[6]]] [m, 5:ncol(datAgro[[name[6]]])],  ncol = 1, byrow = TRUE))) %>%
    cbind(unlist(matrix(datAgro[[name[7]]] [m, 5:ncol(datAgro[[name[7]]])],  ncol = 1, byrow = TRUE))) 
  
  colnames(datRF) <- c('yield', file1)
  datRF[datRF == -999] <- NA 
  datRF <- datRF[complete.cases(datRF[,1]),] 
  datRF <- data.frame(datRF)
  datRF[,1][datRF[,1] == '1'] <- 'failure'
  datRF[,1][datRF[,1] == '2'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '3'] <- 'nonfailure'
  datRF[,1][datRF[,1] == '4'] <- 'nonfailure'
  
  # Need to remove points with NA and if >30% of predictor is NA
  drop <- array(dim=c(0,0))
  if (dim(datRF)[1] > 0){
    for (i in 2:dim(datRF)[2]){
      if ((sum(is.na(datRF[,i]))/dim(datRF)[1]) < 0.3){
        datRF <- datRF[complete.cases(datRF[,i]),]
      } else {
        drop <- c(drop,i)
      }
      if (i == dim(datRF)[2] & (length(drop) >= 1)){
        datRF <- datRF[,-drop]
      } else {
        # Do nothing
      }
      # print(dim(datRF))
    }
  }
  
  # Split into training and testing data
  Wmtry <- round(sqrt(dim(datRF)[2]),0)
  train <- sample(seq_len(nrow(datRF)), replace=FALSE, size = floor(0.70*nrow(datRF)))
  datRFtrain <- datRF[train,]
  datRFtest <- datRF[-train,]
  WdatYtrain <- as.factor(datRFtrain[,1]);         WdatYtest <- as.factor(datRFtest[,1])
  WdatXtrain <- datRFtrain[,2:ncol(datRFtrain)];   WdatXtest <- datRFtest[,2:ncol(datRFtest)]
  
  
  rm(m, i, dat, name)
  
  # Random Forest with Random Forest package ####
  if (dim(MdatXtrain)[1] > 0){
    MRFdat <- randomForest::randomForest(x= MdatXtrain, y= MdatYtrain, xtest= MdatXtest, ytest= MdatYtest, 
                                         mtry = Mmtry, proximity = FALSE, ntree = 650, importance = TRUE, 
                                         na.action = na.omit, votes = TRUE, keep.forest = TRUE)
    
    Mimp <- importance(MRFdat) %>% 
      data.frame() %>% 
      rownames_to_column('AgroInd') %>% 
      tibble("Crop"='maize', "Region" = key)
  } else {
    MRFdat <- NA
    Mimp <- tibble("AgroInd"=NA, "failure"=NA, "nonfailure"=NA, 
                   "MeanDecreaseAccuracy"=NA, "MeanDecreaseGini"=NA, 
                   "Crop"='maize', "Region" = key)
  }
  if (dim(RdatXtrain)[1] > 0){
    RRFdat <- randomForest::randomForest(x= RdatXtrain, y= RdatYtrain, xtest= RdatXtest, ytest= RdatYtest, 
                                         mtry = Rmtry, proximity = FALSE, ntree = 650, importance = TRUE, 
                                         na.action = na.omit, votes = TRUE, keep.forest = TRUE)
    
    Rimp <- importance(RRFdat) %>% 
      data.frame() %>% 
      rownames_to_column('AgroInd') %>% 
      tibble("Crop"='rice', "Region" = key)
    
    
  } else {
    RRFdat <- NA
    Rimp <- tibble("AgroInd"=NA, "failure"=NA, "nonfailure"=NA, 
                   "MeanDecreaseAccuracy"=NA, "MeanDecreaseGini"=NA, 
                   "Crop"='rice', "Region" = key)
  }  
  if (dim(SdatXtrain)[1] > 0){
    SRFdat <- randomForest::randomForest(x= SdatXtrain, y= SdatYtrain, xtest= SdatXtest, ytest= SdatYtest, 
                                         mtry = Smtry, proximity = FALSE, ntree = 650, importance = TRUE, 
                                         na.action = na.omit, votes = TRUE, keep.forest = TRUE)
    Simp <- importance(SRFdat) %>% 
      data.frame() %>% 
      rownames_to_column('AgroInd') %>% 
      tibble("Crop"='soya', "Region" = key)
  } else {
    SRFdat <- NA
    Simp <- tibble("AgroInd"=NA, "failure"=NA, "nonfailure"=NA, 
                   "MeanDecreaseAccuracy"=NA, "MeanDecreaseGini"=NA, 
                   "Crop"='soya', "Region" = key)
  }
  if (dim(WdatXtrain)[1] > 0){
    WRFdat <- randomForest::randomForest(x= WdatXtrain, y= WdatYtrain, xtest= WdatXtest, ytest= WdatYtest, 
                                         mtry = Wmtry, proximity = FALSE, ntree = 650, importance = TRUE, 
                                         na.action = na.omit, votes = TRUE, keep.forest = TRUE)
    Wimp <- importance(WRFdat) %>% 
      data.frame() %>% 
      rownames_to_column('AgroInd') %>% 
      tibble("Crop"='wheat', "Region" = key)
  } else {
    WRFdat <- NA
    Wimp <- tibble("AgroInd"=NA, "failure"=NA, "nonfailure"=NA, 
                   "MeanDecreaseAccuracy"=NA, "MeanDecreaseGini"=NA, 
                   "Crop"='wheat', "Region" = key)
  }
  
  
  # Partial Dependancy Plots ####
  # References: https://bgreenwell.github.io/pdp/articles/pdp.html 
  # https://cran.r-project.org/web/packages/pdp/pdp.pdf
  # https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots
  
  # maize
  if(is.na(MRFdat) != TRUE & is.na(Mimp$AgroInd)[1] != TRUE){
    m <- which(Mimp$AgroInd == file1[1])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[1], plot = TRUE,
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df1a <- p$dat;                     colnames(df1a) <- c('X', 'yhat')
      df1a$AgroClmInd <- file1[1];       df1a$Crop <- 'maize'
    } else { 
      df1a <- data.frame(matrix(NA,ncol = 2)); colnames(df1a) <- c('X','yhat')
      df1a$AgroClmInd <- file1[1];             df1a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[2])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[2], plot = TRUE,
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df2a <- p$dat;                     colnames(df2a) <- c('X', 'yhat')
      df2a$AgroClmInd <- file1[2];       df2a$Crop <- 'maize'
    } else { 
      df2a <- data.frame(matrix(NA,ncol = 2)); colnames(df2a) <- c('X','yhat')
      df2a$AgroClmInd <- file1[2];             df2a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[3])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[3], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df3a <- p$dat;                     colnames(df3a) <- c('X', 'yhat')
      df3a$AgroClmInd <- file1[3];       df3a$Crop <- 'maize'
    } else { 
      df3a <- data.frame(matrix(NA,ncol = 2)); colnames(df3a) <- c('X','yhat')
      df3a$AgroClmInd <- file1[3];             df3a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[4])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[4], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df4a <- p$dat;                     colnames(df4a) <- c('X', 'yhat')
      df4a$AgroClmInd <- file1[4];       df4a$Crop <- 'maize'
    } else { 
      df4a <- data.frame(matrix(NA,ncol = 2)); colnames(df4a) <- c('X','yhat')
      df4a$AgroClmInd <- file1[4];             df4a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[5])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[5], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df5a <- p$dat;                     colnames(df5a) <- c('X', 'yhat')
      df5a$AgroClmInd <- file1[5];       df5a$Crop <- 'maize'
    } else { 
      df5a <- data.frame(matrix(NA,ncol = 2)); colnames(df5a) <- c('X','yhat')
      df5a$AgroClmInd <- file1[5];             df5a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[6])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[6], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df6a <- p$dat;                     colnames(df6a) <- c('X', 'yhat')
      df6a$AgroClmInd <- file1[6];       df6a$Crop <- 'maize'
    } else { 
      df6a <- data.frame(matrix(NA,ncol = 2)); colnames(df6a) <- c('X','yhat')
      df6a$AgroClmInd <- file1[6];             df6a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[7])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[7], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df7a <- p$dat;                     colnames(df7a) <- c('X', 'yhat')
      df7a$AgroClmInd <- file1[7];       df7a$Crop <- 'maize'
    } else { 
      df7a <- data.frame(matrix(NA,ncol = 2)); colnames(df7a) <- c('X','yhat')
      df7a$AgroClmInd <- file1[7];             df7a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[8])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[8], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df8a <- p$dat;                     colnames(df8a) <- c('X', 'yhat')
      df8a$AgroClmInd <- file1[8];       df8a$Crop <- 'maize'
    } else { 
      df8a <- data.frame(matrix(NA,ncol = 2)); colnames(df8a) <- c('X','yhat')
      df8a$AgroClmInd <- file1[8];             df8a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[9])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[9], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df9a <- p$dat;                     colnames(df9a) <- c('X', 'yhat')
      df9a$AgroClmInd <- file1[9];       df9a$Crop <- 'maize'
    } else { 
      df9a <- data.frame(matrix(NA,ncol = 2)); colnames(df9a) <- c('X','yhat')
      df9a$AgroClmInd <- file1[9];             df9a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[10])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[10], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df10a <- p$dat;                     colnames(df10a) <- c('X', 'yhat')
      df10a$AgroClmInd <- file1[10];     df10a$Crop <- 'maize'
    } else { 
      df10a <- data.frame(matrix(NA,ncol = 2)); colnames(df10a) <- c('X','yhat')
      df10a$AgroClmInd <- file1[10];            df10a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[11])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[11], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df11a <- p$dat;                     colnames(df11a) <- c('X', 'yhat')
      df11a$AgroClmInd <- file1[11];      df11a$Crop <- 'maize'
    } else { 
      df11a <- data.frame(matrix(NA,ncol = 2)); colnames(df11a) <- c('X','yhat')
      df11a$AgroClmInd <- file1[11];            df11a$Crop <- 'maize'
    }
    m <- which(Mimp$AgroInd == file1[12])
    if (length(m) != 0){
      p <- pdp::partial(MRFdat, train= MdatXtrain, pred.var=file1[12], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df12a <- p$dat;                     colnames(df12a) <- c('X', 'yhat')
      df12a$AgroClmInd <- file1[12];      df12a$Crop <- 'maize'
    } else { 
      df12a <- data.frame(matrix(NA,ncol = 2)); colnames(df12a) <- c('X','yhat')
      df12a$AgroClmInd <- file1[12];            df12a$Crop <- 'maize'
    }
  } else {   
    df1a <- data.frame(matrix(NA,ncol = 2));     colnames(df1a) <- c('X','yhat')
    df1a$AgroClmInd <- file1[1];                 df1a$Crop <- 'maize'
    df2a <- data.frame(matrix(NA,ncol = 2));     colnames(df2a) <- c('X','yhat')
    df2a$AgroClmInd <- file1[2];                 df2a$Crop <- 'maize'
    df3a <- data.frame(matrix(NA,ncol = 2));     colnames(df3a) <- c('X','yhat')
    df3a$AgroClmInd <- file1[3];                 df3a$Crop <- 'maize'
    df4a <- data.frame(matrix(NA,ncol = 2));     colnames(df4a) <- c('X','yhat')
    df4a$AgroClmInd <- file1[4];                 df4a$Crop <- 'maize'
    df5a <- data.frame(matrix(NA,ncol = 2));     colnames(df5a) <- c('X','yhat')
    df5a$AgroClmInd <- file1[5];                 df5a$Crop <- 'maize'
    df6a <- data.frame(matrix(NA,ncol = 2));     colnames(df6a) <- c('X','yhat')
    df6a$AgroClmInd <- file1[6];                 df6a$Crop <- 'maize'
    df7a <- data.frame(matrix(NA,ncol = 2));     colnames(df7a) <- c('X','yhat')
    df7a$AgroClmInd <- file1[7];                 df7a$Crop <- 'maize'
    df8a <- data.frame(matrix(NA,ncol = 2));     colnames(df8a) <- c('X','yhat')
    df8a$AgroClmInd <- file1[8];                 df8a$Crop <- 'maize'
    df9a <- data.frame(matrix(NA,ncol = 2));     colnames(df9a) <- c('X','yhat')
    df9a$AgroClmInd <- file1[9];                 df9a$Crop <- 'maize'
    df10a <- data.frame(matrix(NA,ncol = 2));    colnames(df10a) <- c('X','yhat')
    df10a$AgroClmInd <- file1[10];               df10a$Crop <- 'maize'
    df11a <- data.frame(matrix(NA,ncol = 2));    colnames(df11a) <- c('X','yhat')
    df11a$AgroClmInd <- file1[11];               df11a$Crop <- 'maize'
    df12a <- data.frame(matrix(NA,ncol = 2));    colnames(df12a) <- c('X','yhat')
    df12a$AgroClmInd <- file1[12];               df12a$Crop <- 'maize'
  }
  PDPReg <- rbind(df1a, df2a, df3a, df4a,  df5a,  df6a, 
                  df7a, df8a, df9a, df10a, df11a, df12a) 
  
  #rice #
  if(is.na(RRFdat) != TRUE & is.na(Rimp$AgroInd)[1] != TRUE){
    m <- which(Rimp$AgroInd == file1[1])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[1], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df1b <- p$dat;                     colnames(df1b) <- c('X', 'yhat')
      df1b$AgroClmInd <- file1[1];       df1b$Crop <- 'rice'
    } else { 
      df1b <- data.frame(matrix(NA,ncol = 2)); colnames(df1b) <- c('X','yhat')
      df1b$AgroClmInd <- file1[1];             df1b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[2])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[2], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df2b <- p$dat;                     colnames(df2b) <- c('X', 'yhat')
      df2b$AgroClmInd <- file1[2];       df2b$Crop <- 'rice'
    } else { 
      df2b <- data.frame(matrix(NA,ncol = 2)); colnames(df2b) <- c('X','yhat')
      df2b$AgroClmInd <- file1[2];             df2b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[3])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[3], plot = TRUE, 
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df3b <- p$dat;                     colnames(df3b) <- c('X', 'yhat')
      df3b$AgroClmInd <- file1[3];       df3b$Crop <- 'rice'
    } else { 
      df3b <- data.frame(matrix(NA,ncol = 2)); colnames(df3b) <- c('X','yhat')
      df3b$AgroClmInd <- file1[3];             df3b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[4])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[4], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df4b <- p$dat;                     colnames(df4b) <- c('X', 'yhat')
      df4b$AgroClmInd <- file1[4];       df4b$Crop <- 'rice'
    } else { 
      df4b <- data.frame(matrix(NA,ncol = 2)); colnames(df4b) <- c('X','yhat')
      df4b$AgroClmInd <- file1[4];             df4b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[5])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[5], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df5b <- p$dat;                     colnames(df5b) <- c('X', 'yhat')
      df5b$AgroClmInd <- file1[5];       df5b$Crop <- 'rice'
    } else { 
      df5b <- data.frame(matrix(NA,ncol = 2)); colnames(df5b) <- c('X','yhat')
      df5b$AgroClmInd <- file1[5];             df5b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[6])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[6], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df6b <- p$dat;                     colnames(df6b) <- c('X', 'yhat')
      df6b$AgroClmInd <- file1[6];       df6b$Crop <- 'rice'
    } else { 
      df6b <- data.frame(matrix(NA,ncol = 2)); colnames(df6b) <- c('X','yhat')
      df6b$AgroClmInd <- file1[6];             df6b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[7])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[7], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df7b <- p$dat;                     colnames(df7b) <- c('X', 'yhat')
      df7b$AgroClmInd <- file1[7];       df7b$Crop <- 'rice'
    } else { 
      df7b <- data.frame(matrix(NA,ncol = 2)); colnames(df7b) <- c('X','yhat')
      df7b$AgroClmInd <- file1[7];             df7b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[8])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[8], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df8b <- p$dat;                     colnames(df8b) <- c('X', 'yhat')
      df8b$AgroClmInd <- file1[8];       df8b$Crop <- 'rice'
    } else { 
      df8b <- data.frame(matrix(NA,ncol = 2)); colnames(df8b) <- c('X','yhat')
      df8b$AgroClmInd <- file1[8];             df8b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[9])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[9], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df9b <- p$dat;                     colnames(df9b) <- c('X', 'yhat')
      df9b$AgroClmInd <- file1[9];       df9b$Crop <- 'rice'
    } else { 
      df9b <- data.frame(matrix(NA,ncol = 2)); colnames(df9b) <- c('X','yhat')
      df9b$AgroClmInd <- file1[9];             df9b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[10])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[10], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df10b <- p$dat;                     colnames(df10b) <- c('X', 'yhat')
      df10b$AgroClmInd <- file1[10];      df10b$Crop <- 'rice'
    } else { 
      df10b <- data.frame(matrix(NA,ncol = 2)); colnames(df10b) <- c('X','yhat')
      df10b$AgroClmInd <- file1[10];            df10b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[11])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[11], plot = TRUE,  
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df11b <- p$dat;                     colnames(df11b) <- c('X', 'yhat')
      df11b$AgroClmInd <- file1[11];      df11b$Crop <- 'rice'
    } else { 
      df11b <- data.frame(matrix(NA,ncol = 2)); colnames(df11b) <- c('X','yhat')
      df11b$AgroClmInd <- file1[11];            df11b$Crop <- 'rice'
    }
    m <- which(Rimp$AgroInd == file1[12])
    if (length(m) != 0){
      p <- pdp::partial(RRFdat, train= RdatXtrain, pred.var=file1[12], plot = TRUE,   
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df12b <- p$dat;                     colnames(df12b) <- c('X', 'yhat')
      df12b$AgroClmInd <- file1[12];      df12b$Crop <- 'rice'
    } else { 
      df12b <- data.frame(matrix(NA,ncol = 2)); colnames(df12b) <- c('X','yhat')
      df12b$AgroClmInd <- file1[12];            df12b$Crop <- 'rice'
    }
  } else {   
    df1b <- data.frame(matrix(NA,ncol = 2));     colnames(df1b) <- c('X','yhat')
    df1b$AgroClmInd <- file1[1];                 df1b$Crop <- 'rice'
    df2b <- data.frame(matrix(NA,ncol = 2));     colnames(df2b) <- c('X','yhat')
    df2b$AgroClmInd <- file1[2];                 df2b$Crop <- 'rice'
    df3b <- data.frame(matrix(NA,ncol = 2));     colnames(df3b) <- c('X','yhat')
    df3b$AgroClmInd <- file1[3];                 df3b$Crop <- 'rice'
    df4b <- data.frame(matrix(NA,ncol = 2));     colnames(df4b) <- c('X','yhat')
    df4b$AgroClmInd <- file1[4];                 df4b$Crop <- 'rice'
    df5b <- data.frame(matrix(NA,ncol = 2));     colnames(df5b) <- c('X','yhat')
    df5b$AgroClmInd <- file1[5];                 df5b$Crop <- 'rice'
    df6b <- data.frame(matrix(NA,ncol = 2));     colnames(df6b) <- c('X','yhat')
    df6b$AgroClmInd <- file1[6];                 df6b$Crop <- 'rice'
    df7b <- data.frame(matrix(NA,ncol = 2));     colnames(df7b) <- c('X','yhat')
    df7b$AgroClmInd <- file1[7];                 df7b$Crop <- 'rice'
    df8b <- data.frame(matrix(NA,ncol = 2));     colnames(df8b) <- c('X','yhat')
    df8b$AgroClmInd <- file1[8];                 df8b$Crop <- 'rice'
    df9b <- data.frame(matrix(NA,ncol = 2));     colnames(df9b) <- c('X','yhat')
    df9b$AgroClmInd <- file1[9];                 df9b$Crop <- 'rice'
    df10b <- data.frame(matrix(NA,ncol = 2));    colnames(df10b) <- c('X','yhat')
    df10b$AgroClmInd <- file1[10];               df10b$Crop <- 'rice'
    df11b <- data.frame(matrix(NA,ncol = 2));    colnames(df11b) <- c('X','yhat')
    df11b$AgroClmInd <- file1[11];               df11b$Crop <- 'rice'
    df12b <- data.frame(matrix(NA,ncol = 2));    colnames(df12b) <- c('X','yhat')
    df12b$AgroClmInd <- file1[12];               df12b$Crop <- 'rice'
  }
  PDPReg <- rbind(PDPReg, df1b, df2b, df3b, df4b,  df5b,  df6b, 
                  df7b, df8b, df9b, df10b, df11b, df12b)
  
  #soya
  if(is.na(SRFdat) != TRUE & is.na(Simp$AgroInd)[1] != TRUE){
    m <- which(Simp$AgroInd == file1[1])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[1], plot = TRUE,   
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df1c <- p$dat;                     colnames(df1c) <- c('X', 'yhat')
      df1c$AgroClmInd <- file1[1];       df1c$Crop <- 'soya'
    } else { 
      df1c <- data.frame(matrix(NA,ncol = 2)); colnames(df1c) <- c('X','yhat')
      df1c$AgroClmInd <- file1[1];             df1c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[2])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[2], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df2c <- p$dat;                     colnames(df2c) <- c('X', 'yhat')
      df2c$AgroClmInd <- file1[2];       df2c$Crop <- 'soya'
    } else { 
      df2c <- data.frame(matrix(NA,ncol = 2)); colnames(df2c) <- c('X','yhat')
      df2c$AgroClmInd <- file1[2];             df2c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[3])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[3], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df3c <- p$dat;                     colnames(df3c) <- c('X', 'yhat')
      df3c$AgroClmInd <- file1[3];       df3c$Crop <- 'soya'
    } else { 
      df3c <- data.frame(matrix(NA,ncol = 2)); colnames(df3c) <- c('X','yhat')
      df3c$AgroClmInd <- file1[3];             df3c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[4])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[4], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df4c <- p$dat;                     colnames(df4c) <- c('X', 'yhat')
      df4c$AgroClmInd <- file1[4];       df4c$Crop <- 'soya'
    } else { 
      df4c <- data.frame(matrix(NA,ncol = 2)); colnames(df4c) <- c('X','yhat')
      df4c$AgroClmInd <- file1[4];             df4c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[5])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[5], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df5c <- p$dat;                     colnames(df5c) <- c('X', 'yhat')
      df5c$AgroClmInd <- file1[5];       df5c$Crop <- 'soya'
    } else { 
      df5c <- data.frame(matrix(NA,ncol = 2)); colnames(df5c) <- c('X','yhat')
      df5c$AgroClmInd <- file1[5];             df5c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[6])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[6], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df6c <- p$dat;                     colnames(df6c) <- c('X', 'yhat')
      df6c$AgroClmInd <- file1[6];       df6c$Crop <- 'soya'
    } else { 
      df6c <- data.frame(matrix(NA,ncol = 2)); colnames(df6c) <- c('X','yhat')
      df6c$AgroClmInd <- file1[6];             df6c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[7])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[7], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df7c <- p$dat;                     colnames(df7c) <- c('X', 'yhat')
      df7c$AgroClmInd <- file1[7];       df7c$Crop <- 'soya'
    } else { 
      df7c <- data.frame(matrix(NA,ncol = 2)); colnames(df7c) <- c('X','yhat')
      df7c$AgroClmInd <- file1[7];             df7c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[8])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[8], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df8c <- p$dat;                     colnames(df8c) <- c('X', 'yhat')
      df8c$AgroClmInd <- file1[8];       df8c$Crop <- 'soya'
    } else { 
      df8c <- data.frame(matrix(NA,ncol = 2)); colnames(df8c) <- c('X','yhat')
      df8c$AgroClmInd <- file1[8];             df8c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[9])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[9], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df9c <- p$dat;                     colnames(df9c) <- c('X', 'yhat')
      df9c$AgroClmInd <- file1[9];       df9c$Crop <- 'soya'
    } else { 
      df9c <- data.frame(matrix(NA,ncol = 2)); colnames(df9c) <- c('X','yhat')
      df9c$AgroClmInd <- file1[9];             df9c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[10])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[10], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df10c <- p$dat;                     colnames(df10c) <- c('X', 'yhat')
      df10c$AgroClmInd <- file1[10];      df10c$Crop <- 'soya'
    } else { 
      df10c <- data.frame(matrix(NA,ncol = 2)); colnames(df10c) <- c('X','yhat')
      df10c$AgroClmInd <- file1[10];            df10c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[11])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[11], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df11c <- p$dat;                     colnames(df11c) <- c('X', 'yhat')
      df11c$AgroClmInd <- file1[11];      df11c$Crop <- 'soya'
    } else { 
      df11c <- data.frame(matrix(NA,ncol = 2)); colnames(df11c) <- c('X','yhat')
      df11c$AgroClmInd <- file1[11];            df11c$Crop <- 'soya'
    }
    m <- which(Simp$AgroInd == file1[12])
    if (length(m) != 0){
      p <- pdp::partial(SRFdat, train= SdatXtrain, pred.var=file1[12], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df12c <- p$dat;                     colnames(df12c) <- c('X', 'yhat')
      df12c$AgroClmInd <- file1[12];      df12c$Crop <- 'soya'
    } else { 
      df12c <- data.frame(matrix(NA,ncol = 2)); colnames(df12c) <- c('X','yhat')
      df12c$AgroClmInd <- file1[12];            df12c$Crop <- 'soya'
    }
  } else {   
    df1c <- data.frame(matrix(NA,ncol = 2));     colnames(df1c) <- c('X','yhat')
    df1c$AgroClmInd <- file1[1];                 df1c$Crop <- 'soya'
    df2c <- data.frame(matrix(NA,ncol = 2));     colnames(df2c) <- c('X','yhat')
    df2c$AgroClmInd <- file1[2];                 df2c$Crop <- 'soya'
    df3c <- data.frame(matrix(NA,ncol = 2));     colnames(df3c) <- c('X','yhat')
    df3c$AgroClmInd <- file1[3];                 df3c$Crop <- 'soya'
    df4c <- data.frame(matrix(NA,ncol = 2));     colnames(df4c) <- c('X','yhat')
    df4c$AgroClmInd <- file1[4];                 df4c$Crop <- 'soya'
    df5c <- data.frame(matrix(NA,ncol = 2));     colnames(df5c) <- c('X','yhat')
    df5c$AgroClmInd <- file1[5];                 df5c$Crop <- 'soya'
    df6c <- data.frame(matrix(NA,ncol = 2));     colnames(df6c) <- c('X','yhat')
    df6c$AgroClmInd <- file1[6];                 df6c$Crop <- 'soya'
    df7c <- data.frame(matrix(NA,ncol = 2));     colnames(df7c) <- c('X','yhat')
    df7c$AgroClmInd <- file1[7];                 df7c$Crop <- 'soya'
    df8c <- data.frame(matrix(NA,ncol = 2));     colnames(df8c) <- c('X','yhat')
    df8c$AgroClmInd <- file1[8];                 df8c$Crop <- 'soya'
    df9c <- data.frame(matrix(NA,ncol = 2));     colnames(df9c) <- c('X','yhat')
    df9c$AgroClmInd <- file1[9];                 df9c$Crop <- 'soya'
    df10c <- data.frame(matrix(NA,ncol = 2));    colnames(df10c) <- c('X','yhat')
    df10c$AgroClmInd <- file1[10];               df10c$Crop <- 'soya'
    df11c <- data.frame(matrix(NA,ncol = 2));    colnames(df11c) <- c('X','yhat')
    df11c$AgroClmInd <- file1[11];               df11c$Crop <- 'soya'
    df12c <- data.frame(matrix(NA,ncol = 2));    colnames(df12c) <- c('X','yhat')
    df12c$AgroClmInd <- file1[12];               df12c$Crop <- 'soya'
  }
  PDPReg <- rbind(PDPReg, df1c, df2c, df3c, df4c,  df5c,  df6c, 
                  df7c, df8c, df9c, df10c, df11c, df12c)
  
  #wheat
  if(is.na(WRFdat) != TRUE & is.na(Wimp$AgroInd)[1] != TRUE){
    m <- which(Wimp$AgroInd == file1[1])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[1], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df1d <- p$dat;                     colnames(df1d) <- c('X', 'yhat')
      df1d$AgroClmInd <- file1[1];       df1d$Crop <- 'wheat'
    } else { 
      df1d <- data.frame(matrix(NA,ncol = 2)); colnames(df1d) <- c('X','yhat')
      df1d$AgroClmInd <- file1[1];             df1d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[2])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[2], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df2d <- p$dat;                     colnames(df2d) <- c('X', 'yhat')
      df2d$AgroClmInd <- file1[2];       df2d$Crop <- 'wheat'
    } else { 
      df2d <- data.frame(matrix(NA,ncol = 2)); colnames(df2d) <- c('X','yhat')
      df2d$AgroClmInd <- file1[2];             df2d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[3])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[3], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df3d <- p$dat;                     colnames(df3d) <- c('X', 'yhat')
      df3d$AgroClmInd <- file1[3];       df3d$Crop <- 'wheat'
    } else { 
      df3d <- data.frame(matrix(NA,ncol = 2)); colnames(df3d) <- c('X','yhat')
      df3d$AgroClmInd <- file1[3];             df3d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[4])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[4], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df4d <- p$dat;                     colnames(df4d) <- c('X', 'yhat')
      df4d$AgroClmInd <- file1[4];       df4d$Crop <- 'wheat'
    } else { 
      df4d <- data.frame(matrix(NA,ncol = 2)); colnames(df4d) <- c('X','yhat')
      df4d$AgroClmInd <- file1[4];             df4d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[5])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[5], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df5d <- p$dat;                     colnames(df5d) <- c('X', 'yhat')
      df5d$AgroClmInd <- file1[5];       df5d$Crop <- 'wheat'
    } else { 
      df5d <- data.frame(matrix(NA,ncol = 2)); colnames(df5d) <- c('X','yhat')
      df5d$AgroClmInd <- file1[5];             df5d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[6])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[6], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df6d <- p$dat;                     colnames(df6d) <- c('X', 'yhat')
      df6d$AgroClmInd <- file1[6];       df6d$Crop <- 'wheat'
    } else { 
      df6d <- data.frame(matrix(NA,ncol = 2)); colnames(df6d) <- c('X','yhat')
      df6d$AgroClmInd <- file1[6];             df6d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[7])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[7], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df7d <- p$dat;                     colnames(df7d) <- c('X', 'yhat')
      df7d$AgroClmInd <- file1[7];       df7d$Crop <- 'wheat'
    } else { 
      df7d <- data.frame(matrix(NA,ncol = 2)); colnames(df7d) <- c('X','yhat')
      df7d$AgroClmInd <- file1[7];             df7d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[8])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[8], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df8d <- p$dat;                     colnames(df8d) <- c('X', 'yhat')
      df8d$AgroClmInd <- file1[8];       df8d$Crop <- 'wheat'
    } else { 
      df8d <- data.frame(matrix(NA,ncol = 2)); colnames(df8d) <- c('X','yhat')
      df8d$AgroClmInd <- file1[8];             df8d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[9])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[9], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df9d <- p$dat;                     colnames(df9d) <- c('X', 'yhat')
      df9d$AgroClmInd <- file1[9];       df9d$Crop <- 'wheat'
    } else { 
      df9d <- data.frame(matrix(NA,ncol = 2)); colnames(df9d) <- c('X','yhat')
      df9d$AgroClmInd <- file1[9];             df9d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[10])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[10], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df10d <- p$dat;                     colnames(df10d) <- c('X', 'yhat')
      df10d$AgroClmInd <- file1[10];      df10d$Crop <- 'wheat'
    } else { 
      df10d <- data.frame(matrix(NA,ncol = 2)); colnames(df10d) <- c('X','yhat')
      df10d$AgroClmInd <- file1[10];            df10d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[11])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[11], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df11d <- p$dat;                     colnames(df11d) <- c('X', 'yhat')
      df11d$AgroClmInd <- file1[11];      df11d$Crop <- 'wheat'
    } else { 
      df11d <- data.frame(matrix(NA,ncol = 2)); colnames(df11d) <- c('X','yhat')
      df11d$AgroClmInd <- file1[11];            df11d$Crop <- 'wheat'
    }
    m <- which(Wimp$AgroInd == file1[12])
    if (length(m) != 0){
      p <- pdp::partial(WRFdat, train= WdatXtrain, pred.var=file1[12], plot = TRUE,    
                        which.class= 'failure', plot.engine = 'ggplot2', prob = 'TRUE')
      df12d <- p$dat;                     colnames(df12d) <- c('X', 'yhat')
      df12d$AgroClmInd <- file1[12];      df12d$Crop <- 'wheat'
    } else { 
      df12d <- data.frame(matrix(NA,ncol = 2)); colnames(df12d) <- c('X','yhat')
      df12d$AgroClmInd <- file1[12];            df12d$Crop <- 'wheat'
    }
  } else {   
    df1d <- data.frame(matrix(NA,ncol = 2));     colnames(df1d) <- c('X','yhat')
    df1d$AgroClmInd <- file1[1];                 df1d$Crop <- 'wheat'
    df2d <- data.frame(matrix(NA,ncol = 2));     colnames(df2d) <- c('X','yhat')
    df2d$AgroClmInd <- file1[2];                 df2d$Crop <- 'wheat'
    df3d <- data.frame(matrix(NA,ncol = 2));     colnames(df3d) <- c('X','yhat')
    df3d$AgroClmInd <- file1[3];                 df3d$Crop <- 'wheat'
    df4d <- data.frame(matrix(NA,ncol = 2));     colnames(df4d) <- c('X','yhat')
    df4d$AgroClmInd <- file1[4];                 df4d$Crop <- 'wheat'
    df5d <- data.frame(matrix(NA,ncol = 2));     colnames(df5d) <- c('X','yhat')
    df5d$AgroClmInd <- file1[5];                 df5d$Crop <- 'wheat'
    df6d <- data.frame(matrix(NA,ncol = 2));     colnames(df6d) <- c('X','yhat')
    df6d$AgroClmInd <- file1[6];                 df6d$Crop <- 'wheat'
    df7d <- data.frame(matrix(NA,ncol = 2));     colnames(df7d) <- c('X','yhat')
    df7d$AgroClmInd <- file1[7];                 df7d$Crop <- 'wheat'
    df8d <- data.frame(matrix(NA,ncol = 2));     colnames(df8d) <- c('X','yhat')
    df8d$AgroClmInd <- file1[8];                 df8d$Crop <- 'wheat'
    df9d <- data.frame(matrix(NA,ncol = 2));     colnames(df9d) <- c('X','yhat')
    df9d$AgroClmInd <- file1[9];                 df9d$Crop <- 'wheat'
    df10d <- data.frame(matrix(NA,ncol = 2));    colnames(df10d) <- c('X','yhat')
    df10d$AgroClmInd <- file1[10];               df10d$Crop <- 'wheat'
    df11d <- data.frame(matrix(NA,ncol = 2));    colnames(df11d) <- c('X','yhat')
    df11d$AgroClmInd <- file1[11];               df11d$Crop <- 'wheat'
    df12d <- data.frame(matrix(NA,ncol = 2));    colnames(df12d) <- c('X','yhat')
    df12d$AgroClmInd <- file1[12];               df12d$Crop <- 'wheat'
  }
  PDPReg <- rbind(PDPReg, df1d, df2d, df3d, df4d,  df5d,  df6d, 
                  df7d, df8d, df9d, df10d, df11d, df12d)
  
  rm(df1a, df1b, df1c, df1d,     df2a, df2b, df2c, df2d,     df3a, df3b, df3c, df3d,
     df4a, df4b, df4c, df4d,     df5a, df5b, df5c, df5d,     df6a, df6b, df6c, df6d,
     df7a, df7b, df7c, df7d,     df8a, df8b, df8c, df8d,     df9a, df9b, df9c, df9d,
     df10a, df10b, df10c, df10d, df11a, df11b, df11c, df11d, df12a, df12b, df12c, df12d)
  # rm(myDat, myColors, p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,x)
  
  
  # ROC ####
  # references: https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/ 
  # https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/
  # https://en.wikipedia.org/wiki/Receiver_operating_characteristic 
  
  dat <- tibble()
  AUCCrop <- tibble() 
  #colnames(AUCCrop) <- c('AUC', 'Crop', 'Region')
  
  # Maize
  if (is.na(MRFdat) == FALSE){
    pred4ROC <- predict(MRFdat, MdatXtest, type= 'prob') %>% data.frame()
    # Define which observations belong to class
    true_values <- ifelse(MdatYtest=='failure',1,0)
    # Assess the performance of classifier for class[i]
    pred <- ROCR::prediction(pred4ROC$failure,true_values)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    x <- tibble(unlist(perf@x.values), unlist(perf@y.values), 'maize') 
    colnames(x) <- c(perf@x.name, perf@y.name,'Crop')
    dat <- rbind(dat, x)
    
    auc.perf <- ROCR::performance(pred, measure = "auc")
    x <- tibble(unlist(auc.perf@y.values),'maize',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  } else {
    x <- tibble("False positive rate" = NA,  
                "True positive rate" = NA, 
                'Crop' ='maize')
    dat <- rbind(dat, x)
    x <- tibble(NA,'maize',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  }
  
  
  # Rice
  if (is.na(RRFdat) == FALSE){
    pred4ROC <- predict(RRFdat, RdatXtest, type= 'prob') %>% data.frame()
    # Define which observations belong to class
    true_values <- ifelse(RdatYtest=='failure',1,0)
    # Assess the performance of classifier for class[i]
    pred <- ROCR::prediction(pred4ROC$failure,true_values)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    x <- tibble(unlist(perf@x.values), unlist(perf@y.values), 'rice') 
    colnames(x) <- c(perf@x.name, perf@y.name,'Crop')
    dat <- rbind(dat, x)
    auc.perf <- ROCR::performance(pred, measure = "auc")
    x <- tibble(unlist(auc.perf@y.values),'rice',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  } else {
    x <- tibble("False positive rate" = NA,  
                "True positive rate" = NA, 
                'Crop' ='rice')
    dat <- rbind(dat, x)
    x <- tibble(NA,'rice',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  }
  
  # Soya
  if (is.na(SRFdat) == FALSE){
    pred4ROC <- predict(SRFdat, SdatXtest, type= 'prob') %>% data.frame()
    # Define which observations belong to class
    true_values <- ifelse(SdatYtest=='failure',1,0)
    # Assess the performance of classifier for class[i]
    pred <- ROCR::prediction(pred4ROC$failure,true_values)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    x <- tibble(unlist(perf@x.values), unlist(perf@y.values), 'soya') 
    colnames(x) <- c(perf@x.name, perf@y.name,'Crop')
    dat <- rbind(dat, x)
    auc.perf <- ROCR::performance(pred, measure = "auc")
    x <- tibble(unlist(auc.perf@y.values),'soya',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
    
  } else {
    x <- tibble("False positive rate" = NA,  
                "True positive rate" = NA, 
                'Crop' ='soya')
    dat <- rbind(dat, x)
    x <- tibble(NA,'soya',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  }
  
  # Wheat
  if (is.na(WRFdat) == FALSE){
    pred4ROC <- predict(WRFdat, WdatXtest, type= 'prob') %>% data.frame()
    # Define which observations belong to class
    true_values <- ifelse(WdatYtest=='failure',1,0)
    # Assess the performance of classifier for class[i]
    pred <- ROCR::prediction(pred4ROC$failure,true_values)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    x <- tibble(unlist(perf@x.values), unlist(perf@y.values), 'wheat') 
    colnames(x) <- c(perf@x.name, perf@y.name,'Crop')
    dat <- rbind(dat, x)
    auc.perf <- ROCR::performance(pred, measure = "auc")
    x <- tibble(unlist(auc.perf@y.values),'wheat',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
    
  } else {
    x <- tibble("False positive rate" = NA,  
                "True positive rate" = NA, 
                'Crop' ='wheat')
    dat <- rbind(dat, x)
    x <- tibble(NA,'wheat',key)
    colnames(x) <- c('AUC', 'Crop', 'Region')
    AUCCrop <- rbind(AUCCrop, x)
  }
  
  # Forming dataframes to save ####
  RFList <- list()
  dat$Region <- key
  name <- paste0('r_',key,'_ROCR')
  RFList[[name]] <- dat
  name <- paste0('r_',key,'_AUC')
  RFList[[name]] <- AUCCrop  
  name <- paste0('r_',key,'_Imp')
  RFList[[name]] <- rbind(Mimp, Rimp, Simp, Wimp) %>%
    arrange(Crop, desc(MeanDecreaseGini)) 
  PDPReg$Region <- key
  name <- paste0('r_',key,'_PDPR')
  RFList[[name]] <- PDPReg
  
  rm(pred4ROC,true_values, pred, perf, auc.perf, X)
  
  return(RFList)
}

# Part I -- Yield and Agro Pre-Processing ######################################
#      This section will open existing yield files. Detrend the yield at each 
# gridpoint using a linear trend and determine the quantiles for each yeild 
# value next the yield will be converted into a categorical crop failure and 
# non-failure events. A failure event is defined as the lower / first quartile
# of crop yield for each grid point. All yield, ( quartile, categorical) will
# be saved into a list (YieldList).
#
# References: Iizumi and Sakai (2020)
T1a <- Sys.time()
print(paste0('Started opening files at: ', T1a))

# 1.1 Variables needed ##########
lat <- seq(-89.75, 89.75, by = 0.5) # y-axis
lon <- seq(-179.75, 179.75, by = 0.5) # x-axis
latlon <- expand.grid(lat, lon) %>%
  rename('lat' = 'Var1') %>%
  rename('lon' = 'Var2')

latID <- seq(1, length(lat), by = 1)
lonID <- seq(1, length(lon), by = 1)
latlonID <- expand.grid(latID, lonID) %>%
  rename('latID' = 'Var1') %>%
  rename('lonID' = 'Var2')

latlon <- cbind(latlon, latlonID)
yr <- 1982:2016 

crop <- c('maize', 'rice','soya', 'wheat')
YieldList <- list()

file1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp')
file2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')
AgroInd <- list()

rm(lat, lon, latID, lonID, latlonID)
# 1.2 Opening Yield ##########
for (i in 1:length(crop)){
  # Quartile (percentiles 25th and 75th)
  dat <- read_csv(paste(fileloc1, loc1, 'Yield_', crop[i],'_',type, '_quartile','.csv', sep = ''),
                  col_names = TRUE, cols(.default = col_double()))
  name <- paste(crop[i], '_quartile', sep='')
  YieldList[[name]] <- dat
  # # Decile (percentiles 10th and 90th)
  # dat <- read_csv(paste(fileloc1, loc1, 'Yield_', crop[i],'_',type, '_decile','.csv', sep = ''),
  #                 col_names = TRUE, cols(.default = col_double()))
  # name <- paste(crop[i], '_decile', sep='')
  # YieldList[[name]] <- dat
  # # Nickle (percentiles 5th and 95th)
  # dat <- read_csv(paste(fileloc1, loc1, 'Yield_', crop[i],'_',type, '_nickle','.csv', sep = ''),
  #                 col_names = TRUE, cols(.default = col_double()))
  # name <- paste(crop[i], '_nickle', sep='')
  # YieldList[[name]] <- dat
  # # SigmaOne 
  # dat <- read_csv(paste(fileloc1, loc1, 'Yield_', crop[i],'_',type, '_sigmaOne','.csv', sep = ''),
  #                 col_names = TRUE, cols(.default = col_double()))
  # name <- paste(crop[i], '_sigmaOne', sep='')
  # YieldList[[name]] <- dat
}

rm(i, dat, name)


# 1.3 AgroIndices opening ##########
# 1.3a Opening of non crop dependent #####
for (i in 1:length(file1)){
  dat <- read_csv(paste(fileloc1, loc2, file1[i], '.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double()))
  dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
  name <- file1[i]
  AgroInd[[name]] <- dat
}
# 1.3b Opening of crop dependent #####
for (i in 1:length(file2)){
  for(j in 1:length(crop)){
    dat <- read_csv(paste(fileloc1, loc2, file2[i],'_',crop[j], '.csv', sep=''),
                    col_names = TRUE, cols(.default = col_double()))
    dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
    name <-  paste(file2[i],"_",crop[j], sep='')
    AgroInd[[name]] <- dat
  }
}

rm(loc2, i, j, dat, name) #file1, file2
# 1.4 Open regions ##########
regionMask <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6RegionsMask.csv'),
                       col_names = TRUE)
regionKey <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6Regions.csv'),
                      col_names = TRUE)

T1b <- Sys.time()
print(paste0('Finished loading files at: ', T1b,
             '. Time elapsed:', T1b-T1a))

# Part II -- Regional Random Forest ############################################
#     This section will preprocess and shape variables for the random forest 
# analysis.
#
# References: https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
# https://bgreenwell.github.io/pdp/articles/pdp.html
T2a <- Sys.time()
print(paste0('Started Random Forest at: ', T2a))

# 2.1 Variables Needed ##########

myLevels <- c('Temperate','Frost Free', "Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
              "Oceania","South & Southeast Asia", "Sub-Saharah Africa", "Temperate South America", "Tropical South America",
              "West-Central Asia")
myLevelsAcr <- c('Temp','FrFree','CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA')
ID <- 3:(length(myLevelsAcr))

# 2.2 Random Forest with randomForest package ##########

variables <- list('file1' = file1,
                  'file2' = file2,
                  'mask' = regionMask,
                  'key' = myLevelsAcr,
                  'yr' = yr,
                  'fileloc1' = c(fileloc1, loc3),
                  'crop' = crop,
                  'MdatYield' = YieldList[[paste0('maize_',t)]],
                  'RdatYield' = YieldList[[paste0('rice_',t)]],
                  'SdatYield' = YieldList[[paste0('soya_',t)]],
                  'WdatYield' = YieldList[[paste0('wheat_',t)]],
                  'datAgro' = AgroInd)
dat <- parallel::mclapply(X = ID , FUN = r_randomForest,
                          varis = variables, mc.cores = core)

# 2.3 AUC ROC variables ##########

for (i in 1:(length(myLevelsAcr)[1]-2)){
  if (i == 1){
    AUC <- dat[[i]][[paste0('r_',myLevelsAcr[i+2],'_AUC')]]
    IMP <- dat[[i]][[paste0('r_',myLevelsAcr[i+2],'_Imp')]]
    ROC <- dat[[i]][[paste0('r_',myLevelsAcr[i+2],'_ROCR')]]
    PDP <- dat[[i]][[paste0('r_',myLevelsAcr[i+2],'_PDPR')]]
  } else {
    AUC <- rbind(AUC, dat[[i]][[paste0('r_',myLevelsAcr[i+2],'_AUC')]])
    IMP <- rbind(IMP, dat[[i]][[paste0('r_',myLevelsAcr[i+2],'_Imp')]])
    ROC <- rbind(ROC, dat[[i]][[paste0('r_',myLevelsAcr[i+2],'_ROCR')]])
    PDP <- rbind(PDP, dat[[i]][[paste0('r_',myLevelsAcr[i+2],'_PDPR')]])
  }
}

# 2.3 Saving variables ##########
rm(dat)

T2b <- Sys.time()
print(paste0('Finished Regional Random Forest at: ', T2b,
             '. Time elapsed:', T2b-T2a))
# Part II -- Global Random Forest ##############################################
#     This section will preprocess and shape variables for the random forest 
# analysis.
#
# References: https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
T3a <- Sys.time()
print(paste0('Started Global Random Forest at: ', T3a))

# 3.1 Variables Needed ##########

# 3.2 Temperate Region Random Forest with randomForest package ##########
T3b <- Sys.time()
print(paste0('Started Temperate Region Random Forest at: ', T3b))

mu <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
m <- which(mu >= 1)

variables <- list('file1' = file1,
                  'file2' = file2,
                  'mask' = m,
                  'key' = 'Temp',
                  'yr' = yr,
                  'fileloc1' = c(fileloc1, loc3),
                  'crop' = crop,
                  'MdatYield' = YieldList[[paste0('maize_',t)]],
                  'RdatYield' = YieldList[[paste0('rice_',t)]],
                  'SdatYield' = YieldList[[paste0('soya_',t)]],
                  'WdatYield' = YieldList[[paste0('wheat_',t)]],
                  'datAgro' = AgroInd)

datRFTemp <- parallel::mclapply(X = 1 , FUN = g_randomForest, 
                          varis = variables, mc.cores = 1)

T3c <- Sys.time()
print(paste0('Finished Temperate Region Random Forest at: ', T3c,
             '. Time elapsed:', T3c-T3b))

# 3.3 FrostFree Random Forest with randomForest package ##########
T3b <- Sys.time()
print(paste0('Started Frost Free Random Forest at: ', T3b))

m <- which(mu < 1)

variables <- list('file1' = file1,
                  'file2' = file2,
                  'mask' = m,
                  'key' = 'FrFree',
                  'yr' = yr,
                  'fileloc1' = c(fileloc1, loc3),
                  'crop' = crop,
                  'MdatYield' = YieldList[[paste0('maize_',t)]],
                  'RdatYield' = YieldList[[paste0('rice_',t)]],
                  'SdatYield' = YieldList[[paste0('soya_',t)]],
                  'WdatYield' = YieldList[[paste0('wheat_',t)]],
                  'datAgro' = AgroInd)

datRFFrFree <- parallel::mclapply(X = 1 , FUN = g_randomForest, 
                               varis = variables, mc.cores = 1)

T3c <- Sys.time()
print(paste0('Finished Frost Free Random Forest at: ', T3c,
             '. Time elapsed:', T3c-T3b))

# 3.4 AUC ROC variables ##########

AUC <- rbind(AUC, datRFTemp[[1]][[paste0('r_','Temp','_AUC')]])
IMP <- rbind(IMP, datRFTemp[[1]][[paste0('r_','Temp','_Imp')]])
ROC <- rbind(ROC, datRFTemp[[1]][[paste0('r_','Temp','_ROCR')]])
PDP <- rbind(PDP, datRFTemp[[1]][[paste0('r_','Temp','_PDPR')]])

AUC <- rbind(AUC, datRFFrFree[[1]][[paste0('r_','FrFree','_AUC')]])
IMP <- rbind(IMP, datRFFrFree [[1]][[paste0('r_','FrFree','_Imp')]])
ROC <- rbind(ROC, datRFFrFree [[1]][[paste0('r_','FrFree','_ROCR')]])
PDP <- rbind(PDP, datRFFrFree [[1]][[paste0('r_','FrFree','_PDPR')]])

# 3.5 Saving variables ##########

write.csv(AUC, file = paste0(fileloc1, loc3, 'AUCRegional','_RF_',type,'_',t, '.csv'), 
          row.names = FALSE)
write.csv(IMP, file = paste0(fileloc1, loc3, 'ImpRegional','_RF_',type,'_',t, '.csv'), 
          row.names = FALSE)
write.csv(ROC, file = paste0(fileloc1, loc3, 'ROCRegional','_RF_',type,'_',t, '.csv'), 
          row.names = FALSE)
write.csv(PDP, file = paste0(fileloc1, loc3, 'PDPRegional','_RF_',type,'_',t, '.csv'), 
          row.names = FALSE)

rm(dat)

T3b <- Sys.time()
print(paste0('Finished Global Random Forest at: ', T3b,
             '. Time elapsed:', T3b-T3a))
print(paste0('Finished Random Forest at: ', T3b,
             '. Time elapsed:', T3b-T1a))
# END
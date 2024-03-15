# P7_YieldAgroRelations.R
# This R program will open yield, agro indices, and random forest importance.
# The yield will be processed to determine catagory. A timeseries figure will 
# result display the percentage of cropland that experiences crop failure and 
# high yield years. An optional figure(s) will be constructed that will display 
# a regional timeseries of failure and high yield years. These figures are saved.
# From these figures regional analysis will determine wich years result in 
# failure and high yield. The RF importance will then be used to 
#
#
# T. A. Schillerberg
#               Feb. 2021
#      Updated: Mar. 2022

# Local Computer
setwd("")
fileloc1 <- '/Research/AgroclimaticConditions/Data/'

loc1 <- 'Iizumi2020/gdhy_v1.2_v1.3_20190128/'
loc2 <- 'Global_AgroInd/Agro092021_mu/'
loc3 <- 'Global_AgroInd/Agro092021_RF_OB/'
loc4 <- 'Global_AgroInd/Agro092021_Dist/'

# Hopper
# fileloc1 <- '~/AgroClmInd/data/'
# loc1 <- 'Iizumi2020/'
# loc2 <- 'AgroclimateInd/Agro092021_mu/'
# loc3 <- 'AgroclimateInd/Agro092021_RF/'
# loc4 <- 'AgroclimateInd/Agro092021_Dist/'

options(show.error.locations = TRUE)

# Variables to change ##########################################################
core = 1  # How many core will be ran? (1 or 11)
hpc <- FALSE
type <- 'M' #'OLS' 'M' "MM' 'TS' for when calculating categorical yield
per <- 'quartile'   # 'quartile'  'decile'  'nickle'   'sigmaOne'
# Time needed:         12hrs       5hrs      3hrs       6.5hrs

# Libraries ####################################################################
library(tidyverse)
library(dplyr)
library(viridis)
library(EnvStats)
library(gridExtra)
#library(beepr)
library(BRRR)

# Functions ####################################################################
get_legend <- function(p, position = NULL){
  # Reference: 
  if(is.null(p)) return(NULL)
  if(!is.null(position)){
    p <- p + theme(legend.position = position)
  }
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) leg <- tmp$grobs[[leg]]
  else leg <- NULL
  leg
}

as_ggplot <- function(x){
  # Reference: https://github.com/kassambara/ggpubr/blob/master/R/as_ggplot.R 
  cowplot::ggdraw() +
    cowplot::draw_grob(grid::grobTree(x))
}

CapStr <- function(y) {
  # reference: https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

cummCell <- function(X, varis){
  # This section will calculate the table needed to calculate the cummulative function

  # Variables Needed #####
  file1 = varis[['file1']]
  file2 = varis[['file2']]
  yr = varis[['yr']]
  crop = varis[['crop']]
  AgroInd <- varis[['AgroInd']]
  YieldList <- varis[['YieldList']]
  r <- varis[['regions']][X]
  mask <- varis[['mask']]
  print(r)
  
  RegFH <- array(NA, dim=c(0,6))
  colnames(RegFH) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  
  n <- which(colnames(mask) == r)
  m <- which(regionMask[,n] == 1)
  
  # Regional Filling #####
  yd_mR <- YieldList[['maize_per']][m, 5:ncol(YieldList[['maize_per']])]
  colnames(yd_mR) <- yr;    yd_mR <- gather(yd_mR, key=Year, value=Yield)
  yd_rR <- YieldList[['rice_per']] [m, 5:ncol(YieldList[['rice_per']])]
  colnames(yd_rR) <- yr;    yd_rR <- gather(yd_rR, key=Year, value=Yield)
  yd_sR <- YieldList[['soya_per']] [m, 5:ncol(YieldList[['soya_per']])]
  colnames(yd_sR) <- yr;    yd_sR <- gather(yd_sR, key=Year, value=Yield)
  yd_wR <- YieldList[['wheat_per']][m, 5:ncol(YieldList[['wheat_per']])]
  colnames(yd_wR) <- yr;    yd_wR <- gather(yd_wR, key=Year, value=Yield)
  
  agro_lsfR <- gather(AgroInd[[file1[1]]][m, 5:ncol(AgroInd[[file1[1]]])], key= Year, value=AgValue)
  agro_fffR <- gather(AgroInd[[file1[2]]][m, 5:ncol(AgroInd[[file1[2]]])], key= Year, value=AgValue)
  agro_cgsR <- gather(AgroInd[[file1[3]]][m, 5:ncol(AgroInd[[file1[3]]])], key= Year, value=AgValue)
  agro_afdR <- gather(AgroInd[[file1[4]]][m, 5:ncol(AgroInd[[file1[4]]])], key= Year, value=AgValue)
  agro_sfoR <- gather(AgroInd[[file1[5]]][m, 5:ncol(AgroInd[[file1[5]]])], key= Year, value=AgValue)
  
  name <- vector()
  # Maize
  for (j in file2){ name <- c(name, paste(j, '_', crop[1], sep=''))}
  Magro_gddR <- gather(AgroInd[[name[1]]][m, 5:ncol(AgroInd[[name[1]]])], key= Year, value=AgValue)
  Magro_hsR  <- gather(AgroInd[[name[2]]][m, 5:ncol(AgroInd[[name[2]]])], key= Year, value=AgValue)
  Magro_tpR  <- gather(AgroInd[[name[3]]][m, 5:ncol(AgroInd[[name[3]]])], key= Year, value=AgValue)
  Magro_ddR  <- gather(AgroInd[[name[4]]][m, 5:ncol(AgroInd[[name[4]]])], key= Year, value=AgValue)
  Magro_smpR <- gather(AgroInd[[name[5]]][m, 5:ncol(AgroInd[[name[5]]])], key= Year, value=AgValue)
  Magro_smmR <- gather(AgroInd[[name[6]]][m, 5:ncol(AgroInd[[name[6]]])], key= Year, value=AgValue)
  Magro_smhR <- gather(AgroInd[[name[7]]][m, 5:ncol(AgroInd[[name[7]]])], key= Year, value=AgValue)
  
  name <- vector()
  # Rice
  for (j in file2){ name <- c(name, paste(j, '_', crop[2], sep=''))}
  Ragro_gddR <- gather(AgroInd[[name[1]]][m, 5:ncol(AgroInd[[name[1]]])], key= Year, value=AgValue)
  Ragro_hsR  <- gather(AgroInd[[name[2]]][m, 5:ncol(AgroInd[[name[2]]])], key= Year, value=AgValue)
  Ragro_tpR  <- gather(AgroInd[[name[3]]][m, 5:ncol(AgroInd[[name[3]]])], key= Year, value=AgValue)
  Ragro_ddR  <- gather(AgroInd[[name[4]]][m, 5:ncol(AgroInd[[name[4]]])], key= Year, value=AgValue)
  Ragro_smpR <- gather(AgroInd[[name[5]]][m, 5:ncol(AgroInd[[name[5]]])], key= Year, value=AgValue)
  Ragro_smmR <- gather(AgroInd[[name[6]]][m, 5:ncol(AgroInd[[name[6]]])], key= Year, value=AgValue)
  Ragro_smhR <- gather(AgroInd[[name[7]]][m, 5:ncol(AgroInd[[name[7]]])], key= Year, value=AgValue)
  
  name <- vector()
  # Soya
  for (j in file2){ name <- c(name, paste(j, '_', crop[3], sep=''))}
  Sagro_gddR <- gather(AgroInd[[name[1]]][m, 5:ncol(AgroInd[[name[1]]])], key= Year, value=AgValue)
  Sagro_hsR  <- gather(AgroInd[[name[2]]][m, 5:ncol(AgroInd[[name[2]]])], key= Year, value=AgValue)
  Sagro_tpR  <- gather(AgroInd[[name[3]]][m, 5:ncol(AgroInd[[name[3]]])], key= Year, value=AgValue)
  Sagro_ddR  <- gather(AgroInd[[name[4]]][m, 5:ncol(AgroInd[[name[4]]])], key= Year, value=AgValue)
  Sagro_smpR <- gather(AgroInd[[name[5]]][m, 5:ncol(AgroInd[[name[5]]])], key= Year, value=AgValue)
  Sagro_smmR <- gather(AgroInd[[name[6]]][m, 5:ncol(AgroInd[[name[6]]])], key= Year, value=AgValue)
  Sagro_smhR <- gather(AgroInd[[name[7]]][m, 5:ncol(AgroInd[[name[7]]])], key= Year, value=AgValue)
  
  name <- vector()
  # Wheat
  for (j in file2){ name <- c(name, paste(j, '_', crop[4], sep=''))}
  Wagro_gddR <- gather(AgroInd[[name[1]]][m, 5:ncol(AgroInd[[name[1]]])], key= Year, value=AgValue)
  Wagro_hsR  <- gather(AgroInd[[name[2]]][m, 5:ncol(AgroInd[[name[2]]])], key= Year, value=AgValue)
  Wagro_tpR  <- gather(AgroInd[[name[3]]][m, 5:ncol(AgroInd[[name[3]]])], key= Year, value=AgValue)
  Wagro_ddR  <- gather(AgroInd[[name[4]]][m, 5:ncol(AgroInd[[name[4]]])], key= Year, value=AgValue)
  Wagro_smpR <- gather(AgroInd[[name[5]]][m, 5:ncol(AgroInd[[name[5]]])], key= Year, value=AgValue)
  Wagro_smmR <- gather(AgroInd[[name[6]]][m, 5:ncol(AgroInd[[name[6]]])], key= Year, value=AgValue)
  Wagro_smhR <- gather(AgroInd[[name[7]]][m, 5:ncol(AgroInd[[name[7]]])], key= Year, value=AgValue)
  
  rm(m)
  # Table Making #####
  # Maize
  m <- which(yd_mR$Yield == 1)
  if(length(m) == 0){
    dat <- array(NA, dim=c(0,6))
  } else {
    dat <- matrix(r, nrow=length(m), ncol=1) %>%
    cbind(yd_mR$Year[m],'maize','failure',file1[1], agro_lsfR$AgValue[m]) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file1[2], agro_fffR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file1[3], agro_cgsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file1[4], agro_afdR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file1[5], agro_sfoR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file2[1], Magro_gddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file2[2], Magro_hsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file2[3], Magro_tpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file2[4], Magro_ddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file2[5], Magro_smpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file2[6], Magro_smmR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','failure',file2[7], Magro_smhR$AgValue[m]))
  }
  colnames(dat) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  RegFH <- rbind(RegFH, dat)
  
  m <- which(yd_mR$Yield == 4)
  if(length(m) == 0){
    dat <- array(NA, dim=c(0,6))
  } else {
    dat <- matrix(r, nrow=length(m), ncol=1) %>%
    cbind(yd_mR$Year[m],'maize','high',file1[1], agro_lsfR$AgValue[m]) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file1[2], agro_fffR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file1[3], agro_cgsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file1[4], agro_afdR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file1[5], agro_sfoR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file2[1], Magro_gddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file2[2], Magro_hsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file2[3], Magro_tpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file2[4], Magro_ddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file2[5], Magro_smpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file2[6], Magro_smmR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_mR$Year[m],'maize','high',file2[7], Magro_smhR$AgValue[m]))
  }
  colnames(dat) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  RegFH <- rbind(RegFH, dat)
  
  # Rice
  m <- which(yd_rR$Yield == 1)
  if(length(m) == 0){
    dat <- array(NA, dim=c(0,6))
  } else {
    dat <- matrix(r, nrow=length(m), ncol=1) %>%
    cbind(yd_rR$Year[m],'rice','failure',file1[1], agro_lsfR$AgValue[m]) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file1[2], agro_fffR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file1[3], agro_cgsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file1[4], agro_afdR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file1[5], agro_sfoR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file2[1], Ragro_gddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file2[2], Ragro_hsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file2[3], Ragro_tpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file2[4], Ragro_ddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file2[5], Ragro_smpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file2[6], Ragro_smmR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','failure',file2[7], Ragro_smhR$AgValue[m]))
  }
  colnames(dat) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  RegFH <- rbind(RegFH, dat)
  
  m <- which(yd_rR$Yield == 4)
  if(length(m) == 0){
    dat <- array(NA, dim=c(0,6))
  } else {
    dat <- matrix(r, nrow=length(m), ncol=1) %>%
    cbind(yd_rR$Year[m],'rice','high',file1[1], agro_lsfR$AgValue[m]) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file1[2], agro_fffR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file1[3], agro_cgsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file1[4], agro_afdR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file1[5], agro_sfoR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file2[1], Ragro_gddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file2[2], Ragro_hsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file2[3], Ragro_tpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file2[4], Ragro_ddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file2[5], Ragro_smpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file2[6], Ragro_smmR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_rR$Year[m],'rice','high',file2[7], Ragro_smhR$AgValue[m]))
  }
  colnames(dat) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  RegFH <- rbind(RegFH, dat)
  
  # Soy
  m <- which(yd_sR$Yield == 1)
  if(length(m) == 0){
    dat <- array(NA, dim=c(0,6))
  } else {
    dat <- matrix(r, nrow=length(m), ncol=1) %>%
    cbind(yd_sR$Year[m],'soya','failure',file1[1], agro_lsfR$AgValue[m]) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file1[2], agro_fffR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file1[3], agro_cgsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file1[4], agro_afdR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file1[5], agro_sfoR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file2[1], Sagro_gddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file2[2], Sagro_hsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file2[3], Sagro_tpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file2[4], Sagro_ddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file2[5], Sagro_smpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file2[6], Sagro_smmR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','failure',file2[7], Sagro_smhR$AgValue[m]))
  }
  colnames(dat) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  RegFH <- rbind(RegFH, dat)
  
  m <- which(yd_sR$Yield == 4)
  if(length(m) == 0){
    dat <- array(NA, dim=c(0,6))
  } else {
    dat <- matrix(r, nrow=length(m), ncol=1) %>%
    cbind(yd_sR$Year[m],'soya','high',file1[1], agro_lsfR$AgValue[m]) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file1[2], agro_fffR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file1[3], agro_cgsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file1[4], agro_afdR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file1[5], agro_sfoR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file2[1], Sagro_gddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file2[2], Sagro_hsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file2[3], Sagro_tpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file2[4], Sagro_ddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file2[5], Sagro_smpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file2[6], Sagro_smmR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_sR$Year[m],'soya','high',file2[7], Sagro_smhR$AgValue[m]))
  }
  colnames(dat) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  RegFH <- rbind(RegFH, dat)
  
  # Wheat
  m <- which(yd_wR$Yield == 1)
  if(length(m) == 0){
    dat <- array(NA, dim=c(0,6))
  } else {
    dat <- matrix(r, nrow=length(m), ncol=1) %>%
    cbind(yd_wR$Year[m],'wheat','failure',file1[1], agro_lsfR$AgValue[m]) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file1[2], agro_fffR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file1[3], agro_cgsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file1[4], agro_afdR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file1[5], agro_sfoR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file2[1], Wagro_gddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file2[2], Wagro_hsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file2[3], Wagro_tpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file2[4], Wagro_ddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file2[5], Wagro_smpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file2[6], Wagro_smmR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','failure',file2[7], Wagro_smhR$AgValue[m]))
  }
  colnames(dat) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  RegFH <- rbind(RegFH, dat)
  
  m <- which(yd_wR$Yield == 4)
  if(length(m) == 0){
    dat <- array(NA, dim=c(0,6))
  } else {
    dat <- matrix(r, nrow=length(m), ncol=1) %>%
    cbind(yd_wR$Year[m],'wheat','high',file1[1], agro_lsfR$AgValue[m]) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file1[2], agro_fffR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file1[3], agro_cgsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file1[4], agro_afdR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file1[5], agro_sfoR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file2[1], Wagro_gddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file2[2], Wagro_hsR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file2[3], Wagro_tpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file2[4], Wagro_ddR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file2[5], Wagro_smpR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file2[6], Wagro_smmR$AgValue[m])) %>%
    rbind(matrix(r, nrow=length(m), ncol=1) %>%
            cbind(yd_wR$Year[m],'wheat','high',file2[7], Wagro_smhR$AgValue[m]))
  }
  colnames(dat) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  RegFH <- rbind(RegFH, dat)
  
  rm(n, m, j, dat, name, yd_mR, yd_rR, yd_sR, yd_wR,
     agro_lsfR,  agro_fffR,  agro_cgsR, agro_afdR, agro_sfoR, 
     Magro_gddR, Magro_hsR,  Magro_tpR, Magro_ddR, Magro_smpR, Magro_smmR, Magro_smhR,
     Ragro_gddR, Ragro_hsR,  Ragro_tpR, Ragro_ddR, Ragro_smpR, Ragro_smmR, Ragro_smhR,
     Sagro_gddR, Sagro_hsR,  Sagro_tpR, Sagro_ddR, Sagro_smpR, Sagro_smmR, Sagro_smhR,
     Wagro_gddR, Wagro_hsR,  Wagro_tpR, Wagro_ddR, Wagro_smpR, Wagro_smmR, Wagro_smhR)
  
  # Data Shapping ####
  RegFH <- as_tibble(RegFH)
  RegFH$Year <- as.numeric(RegFH$Year); RegFH$AgroValue <- as.numeric(RegFH$AgroValue)
  RegFH$AgroValue[RegFH$AgroValue == -999] <- NA
  RegFH <- na.omit(RegFH)
  
  return(RegFH)
}

# Part I -- Yield and Agro Pre-Processing ######################################
#      This section will open existing yield and agroclimate files. 
#
# References: Iizumi and Sakai (2020)
T1a <- Sys.time()
print(paste('Loading files started at: ', T1a, sep = ''))

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

# 1.2 Opening ##########
for (i in 1:length(crop)){
  # Opening of spefied percentile
  dat <- read_csv(paste0(fileloc1, loc1, 'Yield_', crop[i], '_', type,'_',per,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  name <- paste(crop[i], '_per', sep='')
  YieldList[[name]] <- dat
}

rm(loc1, i, dat, name)


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

# 1.4 Opening of Regions ##########

regionMask <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6RegionsMask.csv'),
                       col_names = TRUE)
regionKey <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6Regions.csv'),
                      col_names = TRUE)

# 1.5 Opening of RandomForest importance ##########
# ImpReg <- read_csv(paste0(fileloc1, loc3, 'ImpRegional_RF_',type,'_',per, '.csv'), col_names = TRUE)

T1b <- Sys.time()
print(paste0('Finished loading files at: ', T1b,
             '. Time elapsed:', T1b-T1a))

# Part II -- Frequency Time Series #############################################
#     This section will create a data frame and plot time series/frequency plots.
# One time series will be global with regional percentages. The other wil be a 
# time series that will be percentages regionally.
#
# References: https://github.com/EmilHvitfeldt/r-color-palettes 
T2a <- Sys.time()
print(paste('Frequency time series started at: ', T2a, sep = ''))

# 2.1 Variables Needed ##########
GlobEvents <- array(NA, dim= c(0,11))
subGlob <- array(NA, dim= c(0,11))
colnames(subGlob) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                       'HighCells', 'HighPer','HighRegPer', 'year', 'crop', 'region')

myColors <- c('Total'="#6B6B69",'Global'="#B7B8B4","Centeral America & Caribbean"="#DAA51B","East Asia"="#52BCA3",
              "Europe & Mediterranean"="#2F8AC4", "North America"="#CC3A8E", "Oceania"="#E58606",
              "Southeast Asia"="#99C945", "Sub-Saharah Africa"="#ED645A", "Temprate South America"="#764E9F",
              "Tropical South America"="#5D69B1", "West-Central Asia" = "#24796C")
myColorsAcr <- c('Total'="#6B6B69",'Global'="#B7B8B4","CAC"="#DAA51B","EAS"="#52BCA3",
                 "EUM"="#2F8AC4", "NAM"="#CC3A8E", "OCE"="#E58606",
                 "SEA"="#99C945", "SAF"="#ED645A", "TSA"="#764E9F",
                 "SAM"="#5D69B1", "WCA" = "#24796C")

myLevels <- c('Total','Global', "Centeral America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
              "Oceania","Southeast Asia", "Sub-Saharah Africa", "Temprate South America", "Tropical South America",
              "West-Central Asia")
myLevelsAcr <- c('Total','Global','CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA')

# 2.2 Data Frame of events ##########
# This part will create a data frame of crop failure and high yield events.
# Global Events
if (hpc == TRUE){
  for (i in crop){
    dat <- apply(X = YieldList[[paste0(i,'_per')]][,5:ncol(YieldList[[paste0(i,'_per')]])], 
                 MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE) + 
                   sum(x == '2', na.rm=TRUE) + sum(x == '3', na.rm=TRUE)+ sum(x == '4', na.rm=TRUE)) %>% 
      cbind(apply(X = YieldList[[paste0(i,'_per')]][,5:ncol(YieldList[[paste0(i,'_per')]])], 
                  MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE) + 
                    sum(x == '2', na.rm=TRUE) + sum(x == '3', na.rm=TRUE)+ sum(x == '4', na.rm=TRUE))) %>%
      cbind(apply(X = YieldList[[paste0(i,'_per')]][,5:ncol(YieldList[[paste0(i,'_per')]])], 
                  MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(apply(X = YieldList[[paste0(i,'_per')]][,5:ncol(YieldList[[paste0(i,'_per')]])], 
                  MARGIN = 2, FUN = function(x) sum(x == '4', na.rm=TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(yr, ncol = 1)) %>%
      cbind(matrix(i, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix('Total', nrow= length(yr), ncol = 1)) %>%
      as_tibble()
    GlobEvents <- rbind(GlobEvents, dat)
  }
  colnames(GlobEvents) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                            'HighCells', 'HighPer','HighRegPer', 'Year', 'Crop', 'Region')
  # Temperate & Tropical #####
  dat <- AgroInd[["AccFrostDays"]]
  mu <- apply(dat[,5:ncol(dat)], MARGIN = 1, FUN= mean, na.rm=TRUE)
  m <- which(mu >= 1)
  for (i in crop){
    dat <- apply(X = YieldList[[paste0(i,'_per')]][,5:ncol(YieldList[[paste0(i,'_per')]])], 
                 MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE) + 
                   sum(x == '2', na.rm=TRUE) + sum(x == '3', na.rm=TRUE)+ sum(x == '4', na.rm=TRUE)) %>%
      cbind(apply(X = YieldList[[paste0(i,'_per')]][m, 5:ncol(YieldList[[paste0(i,'_per')]])], 
                  MARGIN = 2, FUN = function(x) sum(is.na(x) == FALSE))) %>%
      cbind(apply(X = YieldList[[paste0(i,'_per')]][m, 5:ncol(YieldList[[paste0(i,'_per')]])],
                  MARGIN = 2, FUN = function(x) sum(x == 1, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(apply(X = YieldList[[paste0(i,'_per')]][m, 5:ncol(YieldList[[paste0(i,'_per')]])],
                  MARGIN = 2, FUN = function(x) sum(x == 4, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(yr, ncol = 1)) %>%
      cbind(matrix(i, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix('Temperate', nrow= length(yr), ncol = 1)) %>%
      as_tibble()
    colnames(dat) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                       'HighCells', 'HighPer','HighRegPer', 'Year', 'Crop', 'Region')
    GlobEvents <- rbind(GlobEvents, dat)
  }
  m <- which(mu < 1)
  for (i in crop){
    dat <- apply(X = YieldList[[paste0(i,'_per')]][,5:ncol(YieldList[[paste0(i,'_per')]])], 
                 MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE) + 
                   sum(x == '2', na.rm=TRUE) + sum(x == '3', na.rm=TRUE)+ sum(x == '4', na.rm=TRUE)) %>%
      cbind(apply(X = YieldList[[paste0(i,'_per')]][m, 5:ncol(YieldList[[paste0(i,'_per')]])], 
                  MARGIN = 2, FUN = function(x) sum(is.na(x) == FALSE))) %>%
      cbind(apply(X = YieldList[[paste0(i,'_per')]][m, 5:ncol(YieldList[[paste0(i,'_per')]])],
                  MARGIN = 2, FUN = function(x) sum(x == 1, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(apply(X = YieldList[[paste0(i,'_per')]][m, 5:ncol(YieldList[[paste0(i,'_per')]])],
                  MARGIN = 2, FUN = function(x) sum(x == 4, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(yr, ncol = 1)) %>%
      cbind(matrix(i, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix('Tropical', nrow= length(yr), ncol = 1)) %>%
      as_tibble()
    colnames(dat) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                       'HighCells', 'HighPer','HighRegPer', 'Year', 'Crop', 'Region')
    GlobEvents <- rbind(GlobEvents, dat)
  }
  
  # Regional Events #####
  for (r in 3:length(myLevelsAcr)){
    n <- which(colnames(regionMask) == myLevelsAcr[r])
    m <- which(regionMask[,n] == 1)
    
    # yd_mR <- matrix(NA, nrow = length(m), ncol = length(yr))
    # colnames(yd_mR) <- as.character(yr)
    # yd_rR <- yd_mR; yd_sR <- yd_mR; yd_wR <- yd_mR
    # 
    # yd_mR <- YieldList[['maize_per']][m, 5:ncol(YieldList[['maize_per']])]
    # yd_rR <- YieldList[['rice_per']] [m, 5:ncol(YieldList[['rice_per']])]
    # yd_sR <- YieldList[['soya_per']] [m, 5:ncol(YieldList[['soya_per']])]
    # yd_wR <- YieldList[['wheat_per']][m, 5:ncol(YieldList[['wheat_per']])]
    
    # Count the number of failure occurances #
    dat <- apply(X = YieldList[['maize_per']][,5:ncol(YieldList[['maize_per']])], 
                 MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE) + 
                   sum(x == '2', na.rm=TRUE) + sum(x == '3', na.rm=TRUE)+ sum(x == '4', na.rm=TRUE)) %>%
      cbind(apply(X = YieldList[['maize_per']][m, 5:ncol(YieldList[['maize_per']])], 
                  MARGIN = 2, FUN = function(x) sum(is.na(x) == FALSE))) %>%
      cbind(apply(X = YieldList[['maize_per']][m, 5:ncol(YieldList[['maize_per']])],
                  MARGIN = 2, FUN = function(x) sum(x == 1, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(apply(X = YieldList[['maize_per']][m, 5:ncol(YieldList[['maize_per']])],
                  MARGIN = 2, FUN = function(x) sum(x == 4, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(yr, ncol = 1)) %>%
      cbind(matrix('maize', nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(myLevelsAcr[r], nrow= length(yr), ncol = 1)) %>%
      as_tibble()
    colnames(dat) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                       'HighCells', 'HighPer','HighRegPer', 'Year', 'Crop', 'Region')
    GlobEvents <- rbind(GlobEvents, dat)
    
    dat <- apply(X = YieldList[['rice_per']][,5:ncol(YieldList[['rice_per']])], 
                 MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE) + 
                   sum(x == '2', na.rm=TRUE) + sum(x == '3', na.rm=TRUE)+ sum(x == '4', na.rm=TRUE)) %>%
      cbind(apply(X = YieldList[['rice_per']][m,5:ncol(YieldList[['rice_per']])], 
                  MARGIN = 2, FUN = function(x) sum(is.na(x) == FALSE))) %>%
      cbind(apply(X = YieldList[['rice_per']][m,5:ncol(YieldList[['rice_per']])], 
                  MARGIN = 2, FUN = function(x) sum(x == 1, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(apply(X = YieldList[['rice_per']][m,5:ncol(YieldList[['rice_per']])], 
                  MARGIN = 2, FUN = function(x) sum(x == 4, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(yr, ncol = 1)) %>%
      cbind(matrix('rice', nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(myLevelsAcr[r], nrow= length(yr), ncol = 1)) %>%
      as_tibble()
    colnames(dat) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                       'HighCells', 'HighPer','HighRegPer', 'Year', 'Crop', 'Region')
    GlobEvents <- rbind(GlobEvents, dat)
    
    dat <- apply(X = YieldList[['soya_per']][,5:ncol(YieldList[['soya_per']])], 
                 MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE) + 
                   sum(x == '2', na.rm=TRUE) + sum(x == '3', na.rm=TRUE)+ sum(x == '4', na.rm=TRUE)) %>%
      cbind(apply(X =YieldList[['soya_per']][m,5:ncol(YieldList[['soya_per']])],
                  MARGIN = 2, FUN = function(x) sum(is.na(x) == FALSE))) %>%
      cbind(apply(X = YieldList[['soya_per']][m,5:ncol(YieldList[['soya_per']])],
                  MARGIN = 2, FUN = function(x) sum(x == 1, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(apply(X = YieldList[['soya_per']][m,5:ncol(YieldList[['soya_per']])], 
                  MARGIN = 2, FUN = function(x) sum(x == 4, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(yr, ncol = 1)) %>%
      cbind(matrix('soya', nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(myLevelsAcr[r], nrow= length(yr), ncol = 1)) %>%
      as_tibble()
    colnames(dat) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                       'HighCells', 'HighPer','HighRegPer', 'Year', 'Crop', 'Region')
    GlobEvents <- rbind(GlobEvents, dat)
    
    dat <- apply(X = YieldList[['wheat_per']][,5:ncol(YieldList[['wheat_per']])], 
                 MARGIN = 2, FUN = function(x) sum(x == '1', na.rm=TRUE) + 
                   sum(x == '2', na.rm=TRUE) + sum(x == '3', na.rm=TRUE)+ sum(x == '4', na.rm=TRUE)) %>%
      cbind(apply(X = YieldList[['wheat_per']][m,5:ncol(YieldList[['wheat_per']])],
                  MARGIN = 2, FUN = function(x) sum(is.na(x) == FALSE))) %>%
      cbind(apply(X = YieldList[['wheat_per']][m,5:ncol(YieldList[['wheat_per']])],
                  MARGIN = 2, FUN = function(x) sum(x == 1, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(apply(X = YieldList[['wheat_per']][m,5:ncol(YieldList[['wheat_per']])],
                  MARGIN = 2, FUN = function(x) sum(x == 4, na.rm = TRUE))) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(NA, nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(yr, ncol = 1)) %>%
      cbind(matrix('wheat', nrow= length(yr), ncol = 1)) %>%
      cbind(matrix(myLevelsAcr[r], nrow= length(yr), ncol = 1)) %>%
      as_tibble()
    colnames(dat) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                       'HighCells', 'HighPer','HighRegPer', 'Year', 'Crop', 'Region')
    GlobEvents <- rbind(GlobEvents, dat)
  }
  # Find the cropland that is not included in the regions
  for (i in crop){
    for (j in yr){
      dat <- subset(GlobEvents, Crop == i) %>%
        subset(Region != 'Total') %>%
        subset(Region != 'Temperate') %>%
        subset(Region != 'Tropical') %>%
        subset(Year == j)
      sub <- subset(GlobEvents, Crop == i) %>%
        subset(Region == 'Total') %>%
        subset(Year == j)
      line <- dat$Cells[1] %>%
        cbind(as.numeric(sub$RegionalCells) - sum(as.numeric(dat$RegionalCells), na.rm=TRUE)) %>%
        cbind(as.numeric(sub$FailCells) - sum(as.numeric(dat$FailCells), na.rm=TRUE)) %>%
        cbind(0) %>%
        cbind(0) %>%
        cbind(as.numeric(sub$HighCells) - sum(as.numeric(dat$HighCells), na.rm=TRUE)) %>%
        cbind(0) %>%
        cbind(0) %>%
        cbind(j) %>%
        cbind(i) %>%
        cbind('Outside') %>%
        as_tibble()
      colnames(line) <- c('Cells', 'RegionalCells', 'FailCells','FailPer','FailRegPer',
                          'HighCells', 'HighPer','HighRegPer', 'Year', 'Crop', 'Region')
      subGlob <- rbind(subGlob, line)
    }
  }
  
  GlobEvents <- rbind(GlobEvents, subGlob)
  
  # Fill in percentages
  GlobEvents$FailPer <- round(as.numeric(GlobEvents$FailCells)/as.numeric(GlobEvents$Cells) * 100, digits= 2)
  GlobEvents$FailRegPer <- round(as.numeric(GlobEvents$FailCells)/as.numeric(GlobEvents$RegionalCells) * 100, digits= 2)
  GlobEvents$HighPer <- round(as.numeric(GlobEvents$HighCells)/as.numeric(GlobEvents$Cells) * 100, digits= 2)
  GlobEvents$HighRegPer <- round(as.numeric(GlobEvents$HighCells)/as.numeric(GlobEvents$RegionalCells) * 100, digits= 2)
  
  GlobEvents <- as_tibble(GlobEvents)
  write.csv(GlobEvents, file = paste0(fileloc1, loc4, 'GlobalFailureHighYieldEvents_',type,'_',per,'.csv'), 
            row.names = FALSE)
  
  rm(r, n, m, i, j, dat, subGlob, sub, line)
} else {
  print(paste0('HPC == TRUE   No analysis preformed and written in this section.'))
  GlobEvents <- read_csv(paste0(fileloc1, loc4, 'GlobalFailureHighYieldEvents_',type,'_',per, '.csv'), 
                         col_names = TRUE) 
}

T2b <- Sys.time()
print(paste0('Finished Frequency dataframe shaping at: ', T2b,
             '. Time elapsed:', T2b-T2a))

# 2.3 Plotting of Global Frequency ###########
if (hpc == FALSE){
  # 2.2a Failure Events #####
  # Maize
  dat <- subset(GlobEvents, Crop == 'maize') %>%
    subset(Region != 'Global') %>%
    subset(Region != 'Total')
  dat <- dat %>%
    mutate(Region = factor(Region, levels=myLevelsAcr)) %>%
    arrange(Region)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste("Maize Crop Failure",sep=''), x="Year",
         y="Percent of global cropland experiencing failure (%)", fill = "Region") +
    ylim(0,40) +
    geom_col(aes(y = FailPer), colour=NA, position = position_stack())
  ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_Maize', ".tiff", sep=''),
         width = 14, height = 8, dpi = 350)
  # ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_Maize_WithGlobal', ".tiff", sep=''),
  #        width = 14, height = 8, dpi = 350)
  
  # Rice
  dat <- subset(GlobEvents, Crop == 'rice') %>%
    subset(Region != 'Global') %>%
    subset(Region != 'Total')
  dat <- dat %>%
    mutate(Region = factor(Region, levels=myLevelsAcr)) %>%
    arrange(Region)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste("Rice Crop Failure",sep=''), x="Year", 
         y="Percent of global cropland experiencing failure (%)", fill = "Region") +
    ylim(0,30) +
    geom_col(aes(y = FailPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_Rice', ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  # ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_Rice_WithGlobal', ".tiff", sep=''),
  #        width = 14, height = 8, dpi = 350)
  
  # Soya
  dat <- subset(GlobEvents, Crop == 'soya') %>%
    subset(Region != 'Global') %>%
    subset(Region != 'Total')
  dat <- dat %>%
    mutate(Region = factor(Region, levels=myLevelsAcr)) %>%
    arrange(Region)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste("Soybean Crop Failure",sep=''), x="Year", 
         y="Percent of global cropland experiencing failure (%)", fill = "Region") +
    ylim(0,35) +
    geom_col(aes(y = FailPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_Soya', ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  # ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_Soya_WithGlobal', ".tiff", sep=''),
  #        width = 14, height = 8, dpi = 350)
  
  dat <- subset(GlobEvents, Crop == 'wheat') %>%
    subset(Region != 'Global') %>%
    subset(Region != 'Total')
  dat <- dat %>%
    mutate(Region = factor(Region, levels=myLevelsAcr)) %>%
    arrange(Region)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste("Wheat Crop Failure",sep=''), x="Year", 
         y="Percent of global cropland experiencing failure (%)", fill = "Region") +
    ylim(0,35) +
    geom_col(aes(y = FailPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_Wheat', ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  # ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_Wheat_WithGlobal', ".tiff", sep=''),
  #        width = 14, height = 8, dpi = 350)
  
  # 2.2b High Yield Events #####
  # Maize
  dat <- subset(GlobEvents, Crop == 'maize') %>%
    subset(Region != 'Global') %>%
    subset(Region != 'Total')
  dat <- dat %>%
    mutate(Region = factor(Region, levels=myLevelsAcr)) %>%
    arrange(Region)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste("Maize Crop High Yield",sep=''), x="Year", 
         y="Percent of global cropland experiencing high yield (%)", fill = "Region") +
    ylim(0,40) +
    geom_col(aes(y = HighPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_Maize', ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  # ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_Maize_WithGlobal', ".tiff", sep=''),
  #        width = 14, height = 8, dpi = 350)
  
  #Rice
  dat <- subset(GlobEvents, Crop == 'rice') %>%
    subset(Region != 'Global') %>%
    subset(Region != 'Total')
  dat <- dat %>%
    mutate(Region = factor(Region, levels=myLevelsAcr)) %>%
    arrange(Region)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste("Rice Crop High Yield",sep=''), x="Year", 
         y="Percent of global cropland experiencing high yield (%)", fill = "Region") +
    ylim(0,40) +
    geom_col(aes(y = HighPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_Rice', ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  # ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_Rice_WithGlobal', ".tiff", sep=''),
  #        width = 14, height = 8, dpi = 350)
  
  # Soya
  dat <- subset(GlobEvents, Crop == 'soya') %>%
    subset(Region != 'Global') %>%
    subset(Region != 'Total')
  dat <- dat %>%
    mutate(Region = factor(Region, levels=myLevelsAcr)) %>%
    arrange(Region)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste("Soybeans Crop High Yield",sep=''), x="Year", 
         y="Percent of global cropland experiencing high yield (%)", fill = "Region") +
    ylim(0,40) +
    geom_col(aes(y = HighPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_Soya', ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  # ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_Soya_WithGlobal', ".tiff", sep=''),
  #        width = 14, height = 8, dpi = 350)
  
  dat <- subset(GlobEvents, Crop == 'wheat') %>%
    subset(Region != 'Global') %>%
    subset(Region != 'Total')
  dat <- dat %>%
    mutate(Region = factor(Region, levels=myLevelsAcr)) %>%
    arrange(Region)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste("Wheat Crop High Yield",sep=''), x="Year", 
         y="Percent of global cropland experiencing high yield (%)", fill = "Region") +
    ylim(0,35) +
    geom_col(aes(y = HighPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_Wheat', ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  # ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_Wheat_WithGlobal', ".tiff", sep=''),
  #        width = 14, height = 8, dpi = 350)
  
  # 2.2c Regional Failure Events #####
  r <- myLevelsAcr[3]
  c <- crop[1]
  dat <- subset(GlobEvents, Crop == c) %>%
    subset(Region == r)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste(CapStr(c)," Crop Failure",sep=''), x="Year", 
         y="Percent of regional cropland experiencing failure (%)", fill = "Region") +
    ylim(0,100) +
    geom_col(aes(y = FailRegPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'FailDistPercentage_',type,'_',per,'_',c,'_',r, ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  
  # 2.2c Regional High Yield Events #####
  r <- myLevelsAcr[3]
  c <- crop[1]
  dat <- subset(GlobEvents, Crop == c) %>%
    subset(Region == r)
  ggplot(dat, aes(x = Year, fill=Region)) +
    theme_bw()+
    scale_fill_manual(values= myColorsAcr) +
    labs(title=paste(CapStr(c)," High Yields",sep=''), x="Year", 
         y="Percent of regional cropland experiencing high yields (%)", fill = "Region") +
    ylim(0,100) +
    geom_col(aes(y = FailRegPer), colour=NA, position = position_stack()) 
  ggsave(filename = paste(fileloc1, loc4,'HighDistPercentage_',type,'_',per,'_',c,'_',r, ".tiff", sep=''), 
         width = 14, height = 8, dpi = 350)
  
  rm(dat, r, c, myColors)
} else {
  print(paste0('HPC = TRUE  No plotting preformed'))
}

T2c <- Sys.time()
print(paste0('Finished Frequency timeseries ploting at: ', T2c,
             '. Time elapsed:', T2c-T2a))

# Part III -- Cummulative Function ############################################
#      This section will calculate the cumulative function for each region 
# based off of failure and high yield events. 
#
# References: 

# 3.1a Variables needed ##########
# RegFH <- array(NA, dim=c(0,8))
# colnames(RegFH) <- c('lat','lon','Region','Year','Crop','Quality','AgroInd','AgroValue')
T3a <- Sys.time()
print(paste('Regional Analysis for Cummulative function started at: ', T3a, sep = ''))
# 3.1b Regional Variables ##########

if (hpc == TRUE){
  ID <- 3:(length(myLevelsAcr))
  variables <- list('file1' = file1,
                    'file2' = file2,
                    'yr' = yr,
                    'crop' = crop,
                    'YieldList' = YieldList,
                    'AgroInd' = AgroInd,
                    'mask' = regionMask,
                    'regions' = myLevelsAcr[3:length(myLevelsAcr)])
  RegFH <- parallel::mclapply(X = ID, FUN= cummCell,
                              varis= variables, mc.cores = core)
  RegFH <- rbind(RegFH[[1]], RegFH[[2]]) %>%
    rbind(RegFH[[3]]) %>%
    rbind(RegFH[[4]]) %>%
    rbind(RegFH[[5]]) %>%
    rbind(RegFH[[6]]) %>%
    rbind(RegFH[[7]]) %>%
    rbind(RegFH[[8]]) %>%
    rbind(RegFH[[9]]) %>%
    rbind(RegFH[[10]]) 
  colnames(RegFH) <- c('Region','Year','Crop','Quality','AgroInd','AgroValue')
  write.csv(RegFH, file = paste0(fileloc1, loc4, 'RegionalFailHigh_AgroInd_Cumulative_',type, '_',per, '.csv'), 
            row.names = FALSE)
  print(paste0('hpc == TRUE  No plotting preformed'))
} else {
  RegFH <- read_csv(paste0(fileloc1, loc4, 'RegionalFailHigh_AgroInd_Cumulative_',type, '_',per, '.csv'), 
                    col_names = TRUE) 
  for (r in 3:length(myLevelsAcr)){
    # Function ####
    
    # Plotting ####
    for (c in 1:length(crop)){
      # P1 Last Spring Frost ####
      fdat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[1]) %>%
        subset(Quality == 'failure')
      agVar <- fdat$AgroInd
      if (dim(fdat)[1] == 0){
        p1 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[1], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(fdat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[1]) %>%
          subset(Quality == 'high')
        if (dim(dat)[1] == 0){
          p1 <- ggplot(NULL)+
            theme_bw() +
            labs(x= file1[1], y= 'Cumulative Probability')
        } else {
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
          p <- EnvStats::gofTest(x=fdat$AgroValue, y=dat$AgroValue, test= 'ks', discrete=TRUE)$p.value %>%
            round(4)
          dat <- rbind(f,h) %>% 
            as_tibble(.)
          dat$V1 <- as.factor(dat$V1)
          dat$V2 <- as.numeric(dat$V2)
          dat$V3 <- as.numeric(dat$V3)
          p1 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
            geom_path() +
            theme_bw() +
            scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
            # geom_text(x= (mean(dat$V2)+2*sd(dat$V2)), y= 0.25, label = p) +
            theme(legend.title = element_blank())+
            labs(x= agVar, y= 'Cumulative Probability') 
        }
      }
      
      # P2 First Fall Frost ####
      fdat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[2]) %>%
        subset(Quality == 'failure')
      agVar <- fdat$AgroInd
      if (dim(fdat)[1] == 0){
        p2 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[2], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(fdat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[2]) %>%
          subset(Quality == 'high')
        if (dim(dat)[1] == 0){
          p2 <- p2 <- ggplot(NULL)+
            theme_bw() +
            labs(x= file1[2], y= 'Cumulative Probability') 
        } else {
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
          p <- EnvStats::gofTest(x=fdat$AgroValue, y=dat$AgroValue, test= 'ks', discrete=TRUE)$p.value %>%
            round(4)
          dat <- rbind(f,h) %>% 
            as_tibble(.)
          dat$V1 <- as.factor(dat$V1)
          dat$V2 <- as.numeric(dat$V2)
          dat$V3 <- as.numeric(dat$V3)
          p2 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
            geom_path() +
            theme_bw() +
            scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
            # geom_text(x= (mean(dat$V2)+sd(dat$V2)), y= 0.25, label = p) +
            labs( x= agVar, y= 'Cumulative Probability') + 
            theme(legend.position = "NULL")
        }
      }
      
      # P3 Climatlological Growing Season ####
      fdat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[3]) %>%
        subset(Quality == 'failure')
      agVar <- fdat$AgroInd
      if (dim(fdat)[1] == 0){
        p3 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[3], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(fdat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[3]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        p <- EnvStats::gofTest(x=fdat$AgroValue, y=dat$AgroValue, test= 'ks', discrete=TRUE)$p.value %>%
          round(4)
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p3 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          # geom_text(x= (mean(dat$V2)+2*sd(dat$V2)), y= 0.25, label = p) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P4 Accumulated Frost Days ####
      fdat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[4]) %>%
        subset(Quality == 'failure')
      agVar <- fdat$AgroInd
      if (dim(fdat)[1] == 0){
        p4 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[4], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(fdat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[4]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        p <- EnvStats::gofTest(x=fdat$AgroValue, y=dat$AgroValue, test= 'ks', discrete=TRUE)$p.value %>%
          round(4)
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p4 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P5 Start Field Operations ####
      dat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[5]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p5 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[5], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[5]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p5 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P6 Growing Degree Day ####
      dat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[1]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p6 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[1], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365') 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[1]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC')
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p6 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P7 Heat Stress ####
      dat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[2]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p7 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[2], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[2]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p7 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P8 Total Precipitation ####
      dat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[3]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p8 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[3], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365') 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[3]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC')
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p8 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs( x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P9 Dry Days ####
      dat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[4]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p9 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[4], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[4]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p9 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P10 SM Planting ####
      dat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[5]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p10 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[5], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[5]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p10 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P11 SM MidSeason ####
      dat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[6]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p11 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[3], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[6]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p11 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P12 SM Harvest ####
      dat <- subset(RegFH, Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[7]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p12 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[7], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[7]) %>%
          subset(Quality == 'high')
        h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
        h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
          cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
          cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p12 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs( x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      # Plotting ####
      myLegend <- p1
      myLegend <- get_legend(myLegend, position = 'bottom') %>% as_ggplot()
      p1 <- p1 +
        theme(legend.position = "NULL")
      x <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, p12, ncol = 3, nrow = 4,
                        top = paste0(myLevels[r], ' ', crop[c]))
      x <- grid.arrange(x, myLegend, nrow=2, heights=c(12,1))
      ggsave(x, filename = paste0(fileloc1, loc4, 'CumulativeProbability_',type,'_', per,'_reg', myLevelsAcr[r],'_',crop[c],'.tiff'), 
             width = 11, height = 12, dpi=350)
      rm(dat,agVar,f,h, p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, myLegend, x)
    } # End c Crop for plotting
  } # End Regional r
  rm(r,c)
  skrrrahh(48)
}

T3b <- Sys.time()
print(paste0('Finished Regional Analysis for Cummulative function at: ', T3b,
            '. Time elapsed:', T3b-T3a))

# 3.2a Variables needed ##########
if(hpc == FALSE){
  GlobEvents <- read_csv(paste0(fileloc1, loc4, 'GlobalFailureHighYieldEvents_',type,'_',per, '.csv'), 
                         col_names = TRUE) 
  RegFH <- read_csv(paste0(fileloc1, loc4, 'RegionalFailHigh_AgroInd_Cumulative_',type,'_',per, '.csv'), 
                    col_names = TRUE) 
} 

thresh <- 30

# 3.2b Regional with Treshold ##########
T3a <- Sys.time()
print(paste('Regional Analysis Threshold for Cummulative function started at: ', T3a, sep = ''))

if(hpc == FALSE){
  GlobEvents <- GlobEvents %>%
    add_column(FailYr = 0, HighYr = 0)
  GlobEvents$FailYr[GlobEvents$FailRegPer >= thresh] <- 1
  GlobEvents$HighYr[GlobEvents$HighRegPer >= thresh] <- 1
  
  for (r in 3:length(myLevelsAcr)){
    for (c in 1:length(crop)){
      dat <- subset(GlobEvents, Region == myLevelsAcr[r]) %>%
        subset(Crop == crop[c])
      fail <- dat$Year[which(dat$FailYr == 1)]
      high <- dat$Year[which(dat$HighYr == 1)]
      
      # Plotting ####
      # P1 Last Spring Frost ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[1]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p1 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[1], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[1]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p1 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          theme(legend.title = element_blank())+
          labs(x= agVar, y= 'Cumulative Probability') 
      }
      
      # P2 First Fall Frost ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[2]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p2 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[2], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[2]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p2 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P3 Climatological Growing Season ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[3]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p3 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[3], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[3]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p3 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P4 Accumulated Frost Days ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[4]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p4 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[4], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[4]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p4 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P5 Start Field Operations ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file1[5]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p5 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file1[5], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file1[5]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p5 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P6 Growing Degree Days ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[1]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p6 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[1], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365') 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[1]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC')
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p6 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P7 Heat Stress ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[2]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p7 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[2], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[2]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p7 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P8 Total Precipitation ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[3]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p8 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[3], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365') 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[3]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC')
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p8 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs( x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P9 Dry Days ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[4]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p9 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[4], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[4]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p9 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P10 SM Planting ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[5]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p10 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[5], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[5]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p10 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P11 SM MidSeason ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[6]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p11 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[6], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[6]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p11 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs(x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      
      # P12 SM Harvest ####
      dat <- subset(RegFH, Year %in% fail) %>%
        subset(Crop == crop[c]) %>%
        subset(Region == myLevelsAcr[r]) %>%
        subset(AgroInd == file2[7]) %>%
        subset(Quality == 'failure')
      agVar <- dat$AgroInd
      if (dim(dat)[1] == 0){
        p12 <- ggplot(NULL)+
          theme_bw() +
          labs(x= file2[7], y= 'Cumulative Probability') 
      } else {
        f <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#D8B365', discrete = TRUE) 
        f <- matrix('failure', nrow=length(f$Order.Statistics), ncol=1) %>%
          cbind(matrix(f$Order.Statistics, nrow= length(f$Order.Statistics), ncol=1)) %>%
          cbind(matrix(f$Cumulative.Probabilities, nrow= length(f$Cumulative.Probabilities), ncol=1))
        dat <- subset(RegFH, Year %in% high) %>%
          subset(Crop == crop[c]) %>%
          subset(Region == myLevelsAcr[r]) %>%
          subset(AgroInd == file2[7]) %>%
          subset(Quality == 'high')
        if(dim(dat)[1] == 0){
          h <- matrix(NA, ncol = 3)
        } else{
          h <- EnvStats::ecdfPlot(dat$AgroValue, ecdf.col= '#5AB4AC', discrete = TRUE)
          h <- matrix('high', nrow=length(h$Order.Statistics), ncol=1) %>%
            cbind(matrix(h$Order.Statistics, nrow= length(h$Order.Statistics), ncol=1)) %>%
            cbind(matrix(h$Cumulative.Probabilities, nrow= length(h$Cumulative.Probabilities), ncol=1))
        }
        dat <- rbind(f,h) %>% 
          as_tibble(.)
        dat$V1 <- as.factor(dat$V1)
        dat$V2 <- as.numeric(dat$V2)
        dat$V3 <- as.numeric(dat$V3)
        p12 <- ggplot(dat, aes(x= V2, y= V3, color=V1)) +
          geom_path() +
          theme_bw() +
          scale_color_manual(values = c('#D8B365','#5AB4AC'), labels = c('Failure', 'High Yield')) +
          labs( x= agVar, y= 'Cumulative Probability') + 
          theme(legend.position = "NULL")
      }
      # All ####
      myLegend <- p1
      myLegend <- get_legend(myLegend, position = 'bottom') %>% as_ggplot()
      p1 <- p1 +
        theme(legend.position = "NULL")
      x <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, p12, ncol = 3, nrow = 4,
                        top = paste0(myLevels[r], ' ', crop[c]))
      x <- grid.arrange(x, myLegend, nrow=2, heights=c(12,1))
      ggsave(x, filename = paste0(fileloc1, loc4, 'CumulativeProbabilityThresh_',type,'_', per,'_reg', myLevelsAcr[r],'_',crop[c],'.tiff'), 
             width = 11, height = 12, dpi=350)
      rm(dat,agVar,f,h, p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, myLegend, x)
    } # End c Crop for plotting
  } # End Regional r
  # beep(sound = 2, espr = NULL)
  skrrrahh(48)
} else{
  print(paste0('HPC = TRUE No plotting'))
}

T3b <- Sys.time()
print(paste0('Finished Regional Analysis Threshold for Cummulative function at: ', T3b,
             '. Time elapsed:', T3b-T3a))
# END #


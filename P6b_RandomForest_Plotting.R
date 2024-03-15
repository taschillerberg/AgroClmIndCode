# P6a_RandomForest_Plot.R
# This R program will open the output from P6_RandomForest.R and plot the 
# partical dependency plots 
#
#
# T. A. Schillerberg
#               Feb. 2021
#      Updated: Nov. 2021

# Local Computer
setwd("/Research/AgroclimaticConditions/Code")
fileloc1 <- '/Research/AgroclimaticConditions/Data/'

loc1 <- 'Global_AgroInd/Agro102021_RF/'
loc2 <- 'Global_AgroInd/Agro092021_mu/'

# HPC
# fileloc1 <- '~/AgroClmInd/data/'
# loc1 <- 'AgroclimateInd/Agro12021_RF/'

# Variables to change ##########################################################
type <- 'M'
t <- 'quartile'
# Libraries ####################################################################
library(tidyverse)
library(viridis)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggradar) #spider chart ggplot
library(cowplot)

# Functions ####################################################################

get_legend <- function(p, position = NULL){
  
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

multiplot <- function(..., plotlist=NULL, file, cols, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols),
                     byrow = TRUE)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Part I -- Ploting Partial Dependency Plots ###################################
#
# 

# 1.1 Variables needed ##########
crop <- c('maize', 'rice','soya', 'wheat')

file1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp')
file2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')

myColors <- c('maize' = '#1B9E77', 'rice' = '#D95F02', 
              'soya'= '#7570B3','wheat'='#E7298A')
MyColors <- c('Maize' = '#1B9E77', 'Rice' = '#D95F02', 
              'Soybeans'= '#7570B3','Wheat'='#E7298A')
AgroInd <- list()
alpha = 0.05

myLevels <- c('Temperate','Frost Free', "Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
              "Oceania","South & Southeast Asia", "Sub-Saharah Africa", "Temprate South America", "Tropical South America",
              "West-Central Asia")
myLevelsAcr <- c('Temp','FrFree','CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA')

# 1.2 Opening files ##########
regionMask <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6RegionsMask.csv'),
                       col_names = TRUE)
regionKey <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6Regions.csv'),
                      col_names = TRUE)

PDPReg <- read_csv(paste0(fileloc1, loc1, 'PDPRegional_RF_',type,'_',t, '.csv'), col_names = TRUE)
PDPRegCV <- read_csv(paste0(fileloc1, loc1, 'PDPRegional_RF_CV_', type, '_',t,'.csv'),
                       col_names = TRUE)

# regionKey <- rowid_to_column(regionKey)
# PDPReg <- read_csv(paste0(fileloc1, loc1, 'PDPRegional_RF_',type,'_',t, '.csv'), col_names = TRUE) 
# PDPRegBoot <- read_csv(paste0(fileloc1, loc1, 'RF_Bootstrap/PDPRegional_RF_bootstrap_', type, '_',t,'.csv'),
#                        col_names = TRUE)

# ExF1.2a Opening of non-crop dependent #
for (i in 1:length(file1)){
  dat <- read_csv(paste(fileloc1, loc2, file1[i], '.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double()))
  dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
  name <- file1[i]
  AgroInd[[name]] <- dat
}
# ExF1.2b Opening of crop dependent #
for (i in 1:length(file2)){
  for(j in 1:length(crop)){
    dat <- read_csv(paste(fileloc1, loc2, file2[i],'_',crop[j], '.csv', sep=''),
                    col_names = TRUE, cols(.default = col_double()))
    dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
    name <-  paste(file2[i],"_",crop[j], sep='')
    AgroInd[[name]] <- dat
  }
}

rm(i, j, dat, name) #file1, file2

# 1.3 Calculating Mu and Minimum##########
datMu <- matrix(NA, ncol = 5)

# yrs <- 1982:2016
# 1.3.a Mu Non-corp dependent #####
for (a in 1:length(file1)){
  dat <- AgroInd[[file1[a]]]
  
  for (i in 1:length(myLevelsAcr)){
    if (i == 1){
      x <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
      m <- which(x >= 1)
    } else if (i == 2){
      x <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
      m <- which(x < 1)
    } else {
      n <- which(colnames(regionMask) == myLevelsAcr[i])
      m <- which(regionMask[,n] == 1)
    } 
    datR <- dat[m,]
    datR[datR == -999] <- NA
    datR <- matrix(datR[,5:ncol(datR)], ncol = 1) %>%
      unlist()
    mu <- matrix(myLevelsAcr[i]) %>%
      cbind(file1[a]) %>%
      cbind('Mu') %>%
      cbind(mean(datR, na.rm = TRUE) %>%
              round(digits = 2)) %>%
      cbind('maize')
    datMu <- rbind(datMu, mu)
  }
}
# 1.3.b Mu Crop dependent #####
for (a in 1:length(file2)){
  for (c in 1:length(crop)){
    agro <- paste0(file2[a],'_',crop[c])
    dat <- AgroInd[[agro]]
    for (i in 1:length(myLevelsAcr)){
      if (i == 1){
        x <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
        m <- which(x >= 1)
      } else if (i == 2){
        x <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
        m <- which(x < 1)
      } else {
        n <- which(colnames(regionMask) == myLevelsAcr[i])
        m <- which(regionMask[,n] == 1)
      } 
      datR <- dat[m,]
      datR[datR == -999] <- NA
      datR <- matrix(datR[,5:ncol(datR)], ncol = 1) %>%
        unlist()
      mu <- matrix(myLevelsAcr[i]) %>%
        cbind(file1[a]) %>%
        cbind('Mu') %>%
        cbind(mean(datR, na.rm = TRUE) %>%
                round(digits = 2)) %>%
        cbind(crop[c])
      datMu <- rbind(datMu, mu)
    }
  }
}

# 1.3.c Min Probability Non-crop & crop dependent #####
for (i in myLevelsAcr){
  for (c in crop){
    for (a in c(file1, file2)){
      dat <- subset(PDPReg, Region == i) %>%
        subset(Crop == c) %>%
        subset(AgroClmInd == a)
      m <- which(dat$yhat == min(dat$yhat, na.rm = TRUE))
      if (length(m) == 0){
        minx <- matrix(i) %>%
          cbind(a) %>%
          cbind('Min') %>%
          cbind(NA) %>%
          cbind(c)
      } else {
        minx <- matrix(i) %>%
        cbind(a) %>%
        cbind('Min') %>%
        cbind(dat$X[m]) %>%
        cbind(c)
      }
      datMu <- rbind(datMu, minx)
    }
  }
}

colnames(datMu) <- c("Region","Agro", 'Method','X','Crop') 
datMu <- as_tibble(datMu[2:nrow(datMu),])
datMu$X<- as.numeric(datMu$X)

rm(dat, minx, mu, a, c, i, m)
# 1.4 Plotting ##########
file1 <- c(file1, file2)
myOrderX <- c('SpFrost', "FallFrost", "GrowSeason", "FrostDays", "StFieldOp", "GDD",
              "HeatStress", "Precip",   "Dry Days", "FieldCondP", "FieldCondM", "FieldCondH")
yhat <- 0.2
alpha = 0.05
df <- 500-1
t.score = qt(p=alpha/2, df=df,lower.tail=FALSE)
method <- 'Min'

for (i in 1:length(myLevelsAcr)){
  PDP <- subset(PDPReg, Region == myLevelsAcr[i])
  PDPCV <- subset(PDPRegCV, Region == myLevelsAcr[i])
  methodValue <- subset(datMu, Region == myLevelsAcr[i]) %>%
    subset(Method == method)
  
  # p1 ####
  dat <- subset(PDP, AgroClmInd == file1[1])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[1])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[1])
  dat <- cbind(dat, yhat) %>% as_tibble() 

    p1 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[1], y="", color = "Legend")
  
  # p2 ####
  dat <- subset(PDP, AgroClmInd == file1[2])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[2])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  
  dat <- subset(methodValue, Agro == file1[2])
  dat <- cbind(dat, yhat) %>% as_tibble() 

  p2 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[2], y="", color = "Legend")
  
  # p3 ####
  dat <- subset(PDP, AgroClmInd == file1[3])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[3])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[3])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p3 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[3], y="", color = "Legend")
  
  # p4 ####
  dat <- subset(PDP, AgroClmInd == file1[4])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[4])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[4])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p4 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[4], y="", color = "Legend")
  
  # p5 ####
  dat <- subset(PDP, AgroClmInd == file1[5])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[5])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[5])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p5 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[5], y="", color = "Legend")
  
  # p6 ####
  dat <- subset(PDP, AgroClmInd == file1[6])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[6])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[6])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p6 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[6], y="", color = "Legend")
  
  # p7 ####
  dat <- subset(PDP, AgroClmInd == file1[7])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[7])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[7])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p7 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[7], y="", color = "Legend")
  
  # p8 ####
  dat <- subset(PDP, AgroClmInd == file1[8])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[8])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[8])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p8 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[8], y="", color = "Legend")
  
  # p9 ####
  dat <- subset(PDP, AgroClmInd == file1[9])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[9])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[9])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p9 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[9], y="", color = "Legend")
  
  # p10 ####
  dat <- subset(PDP, AgroClmInd == file1[10])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[10])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[10])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p10 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[10], y="", color = "Legend")
  
  # p11 ####
  dat <- subset(PDP, AgroClmInd == file1[11])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[11])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[11])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p11 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[11], y="", color = "Legend")
  
  # p12 ####
  dat <- subset(PDP, AgroClmInd == file1[12])
  datM <- subset(dat, Crop == 'maize') 
  datR <- subset(dat, Crop == 'rice')
  datS <- subset(dat, Crop == 'soya')
  datW <- subset(dat, Crop == 'wheat')
  datCV <- subset(PDPCV, AgroClmInd == file1[12])
  dat <- subset(datCV, Crop == 'maize')
  for (j in 1:dim(datM)[1]){
    datB <- subset(dat, X == datM$X[j])$yhat
    datM$SE[j] <- sd(datB)/sqrt(length(datB))
    datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
    datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
  }
  dat <- subset(datCV, Crop == 'rice')
  for (j in 1:dim(datR)[1]){
    datB <- subset(dat, X == datR$X[j])$yhat
    datR$SE[j] <- sd(datB)/sqrt(length(datB))
    datR$yhatMin[j] <- datR$yhat[j] - t.score * datR$SE[j]
    datR$yhatMax[j] <- datR$yhat[j] + t.score * datR$SE[j]
  }
  dat <- subset(datCV, Crop == 'soya')
  for (j in 1:dim(datS)[1]){
    datB <- subset(dat, X == datS$X[j])$yhat
    datS$SE[j] <- sd(datB)/sqrt(length(datB))
    datS$yhatMin[j] <- datS$yhat[j] - t.score * datS$SE[j]
    datS$yhatMax[j] <- datS$yhat[j] + t.score * datS$SE[j]
  }
  dat <- subset(datCV, Crop == 'wheat')
  for (j in 1:dim(datW)[1]){
    datB <- subset(dat, X == datW$X[j])$yhat
    datW$SE[j] <- sd(datB)/sqrt(length(datB))
    datW$yhatMin[j] <- datW$yhat[j] - t.score * datW$SE[j]
    datW$yhatMax[j] <- datW$yhat[j] + t.score * datW$SE[j]
  }
  dat <- subset(methodValue, Agro == file1[12])
  dat <- cbind(dat, yhat) %>% as_tibble() 
  
  p12 <- ggplot(NULL, aes(x= X, y= yhat))+ 
    theme_light() + ylab("") +
    geom_point(data=dat, color= myColors) +
    geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#1B9E77', alpha = 0.5) +
    geom_ribbon(data = na.omit(datR), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#D95F02', alpha = 0.5) +
    geom_ribbon(data = na.omit(datS), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#7570B3', alpha = 0.5) +
    geom_ribbon(data = na.omit(datW), aes(x= X, ymin=yhatMin, ymax=yhatMax),
                fill= '#E7298A', alpha = 0.5) +
    geom_line(data= na.omit(datM), color= '#1B9E77', size= 1) +
    geom_path(data= na.omit(datR), color= '#D95F02', size= 1) +
    geom_path(data= na.omit(datS), color= '#7570B3', size= 1) +
    geom_path(data= na.omit(datW), color= '#E7298A', size= 1) +
    labs(x=myOrderX[12], y="", color = "Legend")
  
  # Final Plot ####
  myDat <- data.frame(col1= 1:4,
                      col2=1:4,
                      col3= c("Maize", "Rice", "Soybeans", "Wheat"))
  colnames(myDat) <- c('X','Y', 'Crop')
  myColors <- c('Maize' = '#1B9E77', 'Rice' = '#D95F02', 
                'Soybeans'= '#7570B3','Wheat'='#E7298A')
  myLegend <- ggplot(myDat)+
    geom_line(aes(x=X, y=Y, color=Crop))+
    theme_light() +
    theme(legend.title = element_blank())+
    guides(color = guide_legend(override.aes = list(size = 5))) +
    scale_color_manual(values= myColors)
  myLegend <- get_legend(myLegend, position = 'bottom') %>% as_ggplot()
  
  # Plot together
  x <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol = 3, nrow = 4,
                    top = paste0("Partial Dependency Plots for ", myLevels[i]))
  x <- grid.arrange(x, myLegend, ncol = 1, heights = c(12, 0.6))
  
  ggsave(x, filename = paste0(fileloc1,loc1, 'pdpRF_',type,'_',t,'_', myLevelsAcr[i], '.tiff'), 
         width = 11, height = 12, dpi=350)
}

rm(datMu,yhat,alpha,df,t.score,method,
   i,PDP,PDPCV,dat,datM,datR,datS,datW,datCV,j,
   p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,myLegend,x)
# Part II -- Ploting ROC Curves and AUC #########################################
#
# References: 

# 2.1 Variables needed ##########
myColorCrops <- c('maize' = '#1B9E77', 'rice' = '#D95F02', 
              'soya'= '#7570B3','wheat'='#E7298A')

myColors <- c("Central America & Caribbean"="#DAA51B","East Asia"="#52BCA3",
              "Europe & Mediterranean"="#2F8AC4", "North America"="#CC3A8E", "Oceania"="#E58606",
              "South & Southeast Asia"="#99C945", "Sub-Saharah Africa"="#ED645A", "Temprate South America"="#764E9F",
              "Tropical South America"="#5D69B1", "West-Central Asia" = "#24796C", "Temprate"="#4d4e4f", "Frost Free"="#adb4b8")
myColorsAcr <- c("CAC"="#DAA51B","EAS"="#52BCA3",
                 "EUM"="#2F8AC4", "NAM"="#CC3A8E", "OCE"="#E58606",
                 "SEA"="#99C945", "SAF"="#ED645A", "TSA"="#764E9F",
                 "SAM"="#5D69B1", "WCA" = "#24796C", "Temp"="#4d4e4f", "FrFree"="#adb4b8")

myLevels <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
              "Oceania","Southeast Asia", "Sub-Saharah Africa", "Temprate South America", "Tropical South America",
              "West-Central Asia", 'Temperate','Frost Free')
myLevels2 <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", 'Frost Free', "North America",
               "Oceania","Sub-Saharah Africa","Tropical South America","Southeast Asia",'Temperate',
               "Temprate South America","West-Central Asia")
myLevelsAcr <- c('CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA', 'Temp','FrFree')

# 2.2 Variables Needed ##########
ROCReg <- read_csv(paste0(fileloc1, loc1, 'ROCRegional_RF_',type,'_',t, '.csv'), 
                   col_names = TRUE) 
AUCReg <- read_csv(paste0(fileloc1, loc1, 'AUCRegional_RF_',type,'_',t, '.csv'),
                   col_names = TRUE)
myplots <- list()

# 2.2 Plotting by Region ##########

for (i in 1:length(myLevelsAcr)){
  AUC <- subset(AUCReg, Region == myLevelsAcr[i])
  ROC <- subset(ROCReg, Region == myLevelsAcr[i])
  message(i)
  
  myplots[[i]] <- local({
    i <- i
    p <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`))+ 
      geom_path(data= na.omit(ROC), aes(color = Crop), size= 1) +
      labs(title = myLevels[i]) +
      scale_color_manual(values= myColorCrops) +
      geom_text(x = 0.75, y = 0.40, aes(color = AUC$Crop[1]),
                label = round(AUC$AUC[1],4)) + 
      geom_text(x = 0.75, y = 0.30, aes(color = AUC$Crop[2]),
                label = round(AUC$AUC[2],4)) + 
      geom_text(x = 0.75, y = 0.20, aes(color = AUC$Crop[3]),
                label = round(AUC$AUC[3],4)) + 
      geom_text(x = 0.75, y = 0.10, aes(color = AUC$Crop[4]),
                label = round(AUC$AUC[4],4)) + 
      geom_abline(intercept = 0, slope= 1, color = 'grey50') +
      theme_light() + 
      theme(legend.position = "NULL")
    print(p)
  })
}

myDat <- data.frame(col1= 1:4,
                    col2=1:4,
                    col3= c("Maize", "Rice", "Soybeans", "Wheat"))
colnames(myDat) <- c('X','Y', 'Crop')
i <- i+1
myplots[[i]] <- local({
  i <- i
  p <- ggplot(myDat)+
    geom_line(aes(x=X, y=Y, color=Crop))+
    theme_light() +
    theme(legend.title = element_blank())+
    guides(color = guide_legend(override.aes = list(size = 5))) +
    scale_color_manual(values= MyColors)
  p <- get_legend(p) %>% as_ggplot()
  print(p)
})
# Plot the ROC plot and save to location

ggsave(multiplot(plotlist = myplots, cols = 4), 
       filename = paste0(fileloc1,loc1, "ROC_RF_",type,'_',t,'.tiff'), 
       width = 11, height = 12, dpi=350)

# 2.3 Plotting by Crop ##########
#     Ploting is by crop
AUC <- subset(AUCReg, Crop == crop[1])
ROC <- subset(ROCReg, Crop == crop[1])
p1 <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`))+ 
  geom_path(data= na.omit(ROC), aes(color = Region), size= 1) +
  labs(title = 'Maize') +
  scale_color_manual(values= myColorsAcr)  +
  geom_abline(intercept = 0, slope= 1, color = 'grey50') +
  theme_light()
  # theme(legend.position = "NULL")

AUC <- subset(AUCReg, Crop == crop[2])
ROC <- subset(ROCReg, Crop == crop[2])
p2 <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`))+ 
  geom_path(data= na.omit(ROC), aes(color = Region), size= 1) +
  labs(title = 'Rice') +
  scale_color_manual(values= myColorsAcr)  +
  geom_abline(intercept = 0, slope= 1, color = 'grey50') +
  theme_light() +
  theme(legend.position = "NULL")

AUC <- subset(AUCReg, Crop == crop[3])
ROC <- subset(ROCReg, Crop == crop[3])
p3 <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`))+ 
  geom_path(data= na.omit(ROC), aes(color = Region), size= 1) +
  labs(title = 'Soybean') +
  scale_color_manual(values= myColorsAcr)  +
  geom_abline(intercept = 0, slope= 1, color = 'grey50') +
  theme_light() +
  theme(legend.position = "NULL")

AUC <- subset(AUCReg, Crop == crop[4])
ROC <- subset(ROCReg, Crop == crop[4])
p4 <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`))+ 
  geom_path(data= na.omit(ROC), aes(color = Region), size= 1) +
  labs(title = 'Wheat') +
  scale_color_manual(values= myColorsAcr)  +
  geom_abline(intercept = 0, slope= 1, color = 'grey50') +
  theme_light() +
  theme(legend.position = "NULL")

mylegend <- get_legend(p1) %>% as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

x <- grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)
x <- grid.arrange(x, mylegend, ncol = 2,
                 top = paste0("ROC Curve"),
                 widths = c(8,2))

ggsave(x, filename = paste0(fileloc1,loc1, "ROC_RF_Compiled",type,'_',t,'.tiff'), 
       width = 14, height = 12, dpi=350)

# 2.3 AUC Table ##########
AUC <- AUCReg %>% spread(Crop, AUC) 
AUC <- arrange(AUC, Region)
AUC$Region <- myLevels2
# Need to change the row names to be spelled out instead of Ancronyms may need to check when Temprate and Frost Free are added
write.csv(AUC, file = paste0(fileloc1, loc1, 'AUCRegional_RF_',type,'_',t, '_wide.csv'), 
          row.names = FALSE)
rm(myplots,AUC, ROC,p,i,myDat, p1,p2,p3,p4,x)
# Part III -- Importance Ploting ###############################################
#
#
#

# 3.1 Variables needed ##########
crop <- c('maize','rice','soya','wheat')
myColors <- c("Central America & Caribbean"="#DAA51B","East Asia"="#52BCA3",
              "Europe & Mediterranean"="#2F8AC4", "North America"="#CC3A8E", "Oceania"="#E58606",
              "South & Southeast Asia"="#99C945", "Sub-Saharah Africa"="#ED645A", "Temprate South America"="#764E9F",
              "Tropical South America"="#5D69B1", "West-Central Asia" = "#24796C", "Temprate"="#4d4e4f", "Frost Free"="#adb4b8")
myColorsAcr <- c("CAC"="#DAA51B","EAS"="#52BCA3",
                 "EUM"="#2F8AC4", "NAM"="#CC3A8E", "OCE"="#E58606",
                 "SEA"="#99C945", "SAF"="#ED645A", "TSA"="#764E9F",
                 "SAM"="#5D69B1", "WCA" = "#24796C", "Temp"="#4d4e4f", "FrFree"="#adb4b8")
myLevels <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
              "Oceania","Southeast Asia", "Sub-Saharah Africa", "Temprate South America", "Tropical South America",
              "West-Central Asia", 'Temperate','Frost Free')
myLevelsLong <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
              "Oceania","South & Southeast Asia", "Sub-Sahara Africa", "Temperate South America", "Tropical South America",
              "West-Central Asia",'Temperate','Tropics')
myLevelsShortB <- c("C America & Caribb.", "E Asia", "Europe & Mediterr.", "N America",
                   "Oceania","S & SE Asia", "Sub-Sahara Africa", "Temp S America", "Trop S America",
                   "W-C Asia",'**Temperate**','**Tropics**')
myLevelsAcr <- c('CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA', 'Temp','FrFree')

file1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp','GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')
myOrderX <- c('Last Spring Frost', "First Fall Frost", 
              "Climatlogical Growing Season", "Accumulated Frost Days", "Start of Field Opperations",
              "Growing Degree Days", "Heat Stress Days", "Total Precipitation", 
              "Dry Days", "Planting Field Conditions", "Mid-Season Field Conditions", 
              "Harvest Field Conditions")
myOrderXAcr <- c('LSF', "FFF", 
              "CGS", "AFD", "SFO",
              "GGD", "HSD", "TP", 
              "DD", "PFC", "MFC", 
              "HFC")
myOrderXShortTest <- c("LastSpringFrost"="SpFrost", "FirstFallFrost"="FallFrost", 
                       "ClimGrowingSeason"="GrowSeason","AccFrostDays"="FrostDays",
                       "StartFieldOp"="StFieldOp","GrowDegDay"="GDD","HeatStress"="HeatStress",
                       "TotPrecip"="Precip", "DryDay"="DryDays", "SMPlanting"="FieldCondP", 
                       "SMMidSeason"="FieldCondM","SMHarvestSeason"="FieldCondH")
myOrderXShort <- c('SpFrost', "FallFrost", "GrowSeason", "FrostDays", "StFieldOp", "GDD",
                   "HeatStress", "Precip",   "Dry Days", "FieldCondP", "FieldCondM", "FieldCondH")

# 3.2 Opening files ###########
ImpReg <- read_csv(paste0(fileloc1, loc1, 'ImpRegional_RF_',type,'_',t, '.csv'), 
                   col_names = TRUE)
ImpRegCV <- read_csv(paste0(fileloc1, loc1, 'ImpRegional_RF_CV_',type,'_',t, '.csv'), 
                     col_names = TRUE)

# 3.3 Plotting Bargraphs ##########
  # Plotting GINI #####
  dat <- subset(ImpReg, Crop == crop[1]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseGini, .fun='sum'))
  p1 <- ggplot(na.omit(dat), aes(x=MeanDecreaseGini, fill=Region)) +
    labs(title = 'Maize', y='', x = 'Mean Decrease Gini') +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light() +
    theme(legend.position = "NULL")
  
  dat <- subset(ImpReg, Crop == crop[2]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseGini, .fun='sum'))
  p2 <- ggplot(na.omit(dat), aes(x=MeanDecreaseGini, fill=Region)) +
    labs(title = 'Rice', y='', x = 'Mean Decrease Gini') +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light() +
    theme(legend.position = "NULL")
  
  dat <- subset(ImpReg, Crop == crop[3]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseGini, .fun='sum'))
  p3 <- ggplot(na.omit(dat), aes(x=MeanDecreaseGini, fill=Region)) +
    labs(title = 'Soybeans', y='', x = 'Mean Decrease Gini') +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light() +
    theme(legend.position = "NULL")
  
  dat <- subset(ImpReg, Crop == crop[4]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseGini, .fun='sum'))
  p4 <- ggplot(na.omit(dat), aes(x=MeanDecreaseGini, fill=Region)) +
    labs(title = 'Wheat', y='', x = 'Mean Decrease Gini') +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light() +
    theme(legend.position = "NULL")
  
  # Legend
  dat <- subset(ImpReg, Crop == crop[1]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseGini, .fun='sum'))
  myLegend <- ggplot(dat, aes(x=MeanDecreaseGini, fill=Region)) +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light()
  myLegend <- get_legend(myLegend) %>% as_ggplot()
  
  # Plot the partical dependancy plot and save to location
  x <- gridExtra::grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)
  x <- gridExtra::grid.arrange(x, myLegend, ncol = 2, widths = c(11, 2), 
                               top = paste0("Variable Importance"))
  ggsave(x, filename = paste0(fileloc1,loc1, "Importance_RF_barGINI_",type,'_',t, '.tiff'), 
         width = 12, height = 6, dpi=350)
  
  # Plotting Accuracy #####
  dat <- subset(ImpReg, Crop == crop[1]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseAccuracy, .fun='sum'))
  p1 <- ggplot(na.omit(dat), aes(x=MeanDecreaseAccuracy, fill=Region)) +
    labs(title = 'Maize', y='', x = 'Mean Decrease Accuracy') +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light() +
    theme(legend.position = "NULL")
  
  dat <- subset(ImpReg, Crop == crop[2]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseAccuracy, .fun='sum'))
  p2 <- ggplot(na.omit(dat), aes(x=MeanDecreaseAccuracy, fill=Region)) +
    labs(title = 'Rice', y='', x = 'Mean Decrease Accuracy') +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light() +
    theme(legend.position = "NULL")
  
  dat <- subset(ImpReg, Crop == crop[3]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseAccuracy, .fun='sum'))
  p3 <- ggplot(na.omit(dat), aes(x=MeanDecreaseAccuracy, fill=Region)) +
    labs(title = 'Soybeans', y='', x = 'Mean Decrease Accuracy') +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light() +
    theme(legend.position = "NULL")
  
  dat <- subset(ImpReg, Crop == crop[4]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseAccuracy, .fun='sum'))
  p4 <- ggplot(na.omit(dat), aes(x=MeanDecreaseAccuracy, fill=Region)) +
    labs(title = 'Wheat', y='', x = 'Mean Decrease Accuracy') +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light() +
    theme(legend.position = "NULL")
  
  # Legend
  dat <- subset(ImpReg, Crop == crop[1]) 
  dat$AgroInd <- as.factor(dat$AgroInd)
  dat <- mutate(dat, AgroInd = fct_reorder(AgroInd, MeanDecreaseAccuracy, .fun='sum'))
  myLegend <- ggplot(dat, aes(x=MeanDecreaseAccuracy, fill=Region)) +
    geom_col(aes(y = AgroInd), orientation = "y", position = position_stack(), 
             colour = NA) +
    scale_fill_manual(values= myColorsAcr) +
    theme_light()
  myLegend <- get_legend(myLegend) %>% as_ggplot()
  
  # Plot the partical dependancy plot and save to location
  x <- gridExtra::grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)
  x <- gridExtra::grid.arrange(x, myLegend, ncol = 2, widths = c(11, 2), 
                               top = paste0("Variable Importance"))
  ggsave(x, filename = paste0(fileloc1,loc1, "Importance_RF_barAcc_",type,'_',t, '.tiff'), 
         width = 12, height = 6, dpi=350)

# 3.4. Plotting Heat Map ##########
  ImpReg <- read_csv(paste0(fileloc1, loc1, 'ImpRegional_RF_',type,'_',t, '.csv'), 
                     col_names = TRUE)
  ImpRegNew <- array()
  # Plotting #####
  for (i in 1:length(crop)){
    for (j in 1:length(myLevelsAcr)){
      dat <- subset(ImpReg, Crop == crop[i]) %>%
        subset(Region == myLevelsAcr[j]) %>%
        arrange(desc(MeanDecreaseAccuracy))
      dat <- add_column(dat, Imp = 1:nrow(dat))
      ImpRegNew <- rbind(ImpRegNew, dat)
    }
  }
  
  # Continous #####
  myOrderY <- myLevelsAcr
  ImpRegNew <- ImpRegNew %>%
    mutate(AgroInd = factor(AgroInd, levels = rev(file1))) %>% 
    mutate(Region = factor(Region, levels = myOrderY)) %>%
    arrange(AgroInd, Region)
  
  dat <- subset(ImpRegNew, Crop == crop[1]) 
  p1 <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
    geom_tile() + 
    scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
    labs(title= "Maize", y="", x="") +
    # guides(fill=guide_legend(title="Importance")) +
    theme_light() +
    scale_x_discrete(guide = guide_axis(angle = 45), labels = myOrderY) +
    scale_y_discrete(labels = rev(myOrderXAcr)) + 
    guides(fill=guide_legend(title="Importance")) +
    theme(legend.position = "NULL")
  
  dat <- subset(ImpRegNew, Crop == crop[2]) 
  p2 <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
    geom_tile() + 
    scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
    labs(title= "Rice", y="", x="") +
    # guides(fill=guide_legend(title="Importance")) +
    theme_light() +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_discrete(labels = rev(myOrderXAcr)) + 
    guides(fill=guide_legend(title="Importance"))+
    theme(legend.position = "NULL")
  
  dat <- subset(ImpRegNew, Crop == crop[3]) 
  p3 <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
    geom_tile() + 
    scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
    labs(title= "Soybean", y="", x="") +
    # guides(fill=guide_legend(title="Importance")) +
    theme_light() +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_discrete(labels = rev(myOrderXAcr)) + 
    guides(fill=guide_legend(title="Importance"))+
    theme(legend.position = "NULL")
  
  dat <- subset(ImpRegNew, Crop == crop[4]) 
  p4 <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
    geom_tile() + 
    scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
    labs(title= "Wheat", y="", x="") +
    # guides(fill=guide_legend(title="Importance")) +
    theme_light() +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_discrete(labels = rev(myOrderXAcr)) + 
    guides(fill=guide_legend(title="Importance"))+
    theme(legend.position = "NULL")
  
  # Legend
  dat <- subset(ImpRegNew, Crop == crop[1]) 
  myLegend <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
    geom_tile() + 
    scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
    theme_light() +
    guides(fill=guide_legend(title="Importance"))
  myLegend <- get_legend(myLegend) %>% as_ggplot()
  
  x <- gridExtra::grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)
  x <- gridExtra::grid.arrange(x, myLegend, ncol = 2, widths = c(11, 2), 
                               top = paste0("Variable Importance"))
  
  ggsave(x, filename = paste0(fileloc1,loc1, "Importance_HeatMap_RF_",type,'_',t, '.tiff'), 
         width = 9, height = 6, dpi=350)
# 3.4. Plotting Spider Plot ##########
crop <- c('maize','rice','soya','wheat')
Crop <- c('Maize','Rice','Soya','Wheat')
datImp <- array()

for (i in 1:length(myLevelsAcr)){

  # Maize ####
    dat <- subset(ImpReg, Region == myLevelsAcr[i]) %>%
      subset(Crop == 'maize') %>%
      #arrange(desc(MeanDecreaseAccuracy))
      arrange(MeanDecreaseAccuracy)
    dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
      arrange(factor(AgroInd, levels = file1))
    datImp <- tibble(dat$Imp) %>% t()
    first <- datImp
    colnames(datImp) <- dat$AgroInd
    if (length(dat$AgroInd) == 10){
      start <-3
    }else {start <-1}
    colnames(datImp) <- myOrderXShort[start:length(myOrderXShort)]
    # datImp <- rbind(rep(dim(datImp)[2],dim(datImp)[2]) , rep(1,dim(datImp)[2]) , datImp)

    datCV <- subset(ImpRegCV, Region == myLevelsAcr[i]) %>%
      subset(Crop == 'maize')
    # Order by importance reformat table
    for (j in 1:500){
      dat <- subset(datCV, CValid == j) %>%
        #arrange(desc(MeanDecreaseAccuracy))
        arrange(MeanDecreaseAccuracy)
      dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
        arrange(factor(AgroInd, levels = file1))
      dat <- tibble(dat$Imp) %>% t()
      datImp <- rbind(datImp, dat)
    }
    datImp <- datImp[-1,] # Removes the first row which is the main run
    rownames(datImp) <- 1:dim(datImp)[1]
    mu <- apply(datImp[,], MARGIN = 2, mean, na.rm=TRUE)
    datImp <- rbind(datImp, first)
    datImp <- rbind(datImp, mu)
    datImp <- as_tibble(datImp, rownames = 'Group')
    colorsImp <- c(rep(rgb(0,0,0,0.1),(dim(datImp)[1]-1)),'#f54242')
    # colorsImp <- c(rep(rgb(0,0,0,0.1),(dim(datImp)[1]-2)),'#f54242','#4242f5')
    
    p1 <- ggradar(data.frame(datImp),
                  values.radar = c(dim(datImp)[2]-1,(dim(datImp)[2]-1)*0.5,1),
                  grid.min=1,  grid.mid= (dim(datImp)[2]/2), grid.max=(dim(datImp)[2]-1),
                  gridline.mid.colour='grey', background.circle.colour='white',
                  group.line.width = 1, 
                  group.point.size = 0,
                  group.colours = colorsImp,
                  legend.position = 'none',
                  grid.label.size = 5,
                  axis.label.size = 2) 
      # labs(title = "Maize")
    
  # Rice ####
    dat <- subset(ImpReg, Region == myLevelsAcr[i]) %>%
      subset(Crop == 'rice') %>%
      #arrange(desc(MeanDecreaseAccuracy))
      arrange(MeanDecreaseAccuracy)
    dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
      arrange(factor(AgroInd, levels = file1))
    datImp <- tibble(dat$Imp) %>% t()
    first <- datImp
    colnames(datImp) <- dat$AgroInd
    if (length(dat$AgroInd) == 10){
      start <-3
    }else {start <-1}
    colnames(datImp) <- myOrderXShortTest[start:length(myOrderXShort)]
    # datImp <- rbind(rep(dim(datImp)[2],dim(datImp)[2]) , rep(1,dim(datImp)[2]) , datImp)
    
    datCV <- subset(ImpRegCV, Region == myLevelsAcr[i]) %>%
      subset(Crop == 'rice')
    # Order by importance reformat table
    for (j in 1:500){
      dat <- subset(datCV, CValid == j) %>%
        #arrange(desc(MeanDecreaseAccuracy))
        arrange(MeanDecreaseAccuracy)
      dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
        arrange(factor(AgroInd, levels = file1))
      dat <- tibble(dat$Imp) %>% t()
      datImp <- rbind(datImp, dat)
    }
    datImp <- datImp[-1,] # Removes the first row which is the main run
    rownames(datImp) <- 1:dim(datImp)[1]
    mu <- apply(datImp[,], MARGIN = 2, mean, na.rm=TRUE)
    datImp <- rbind(datImp, first)
    datImp <- rbind(datImp, mu)
    datImp <- as_tibble(datImp, rownames = 'Group')
    colorsImp <- c(rep(rgb(0,0,0,0.1),(dim(datImp)[1]-1)),'#f54242')
    # colorsImp <- c(rep(rgb(0,0,0,0.1),(dim(datImp)[1]-2)),'#f54242','#4242f5')
    
    p2 <- ggradar(data.frame(datImp),
                  values.radar = c(dim(datImp)[2]-1,(dim(datImp)[2]-1)*0.5,1),
                  grid.min=1,  grid.mid= (dim(datImp)[2]/2), grid.max=(dim(datImp)[2]-1),
                  gridline.mid.colour='grey', background.circle.colour='white',
                  group.line.width = 1, 
                  group.point.size = 0,
                  group.colours = colorsImp,
                  legend.position = 'none',
                  grid.label.size = 5,
                  axis.label.size = 2) 
      # labs(title = "Rice")
    
  # Soya ####
    dat <- subset(ImpReg, Region == myLevelsAcr[i]) %>%
      subset(Crop == 'soya') %>%
      #arrange(desc(MeanDecreaseAccuracy))
      arrange(MeanDecreaseAccuracy)
    dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
      arrange(factor(AgroInd, levels = file1))
    datImp <- tibble(dat$Imp) %>% t()
    first <- datImp
    colnames(datImp) <- dat$AgroInd
    if (length(dat$AgroInd) == 10){
      start <-3
      colnames(datImp) <- myOrderXShort[start:length(myOrderXShort)]
    }else if (length(dat$AgroInd) == 1){
      colnames(datImp) <- dat$AgroInd
    }else {
      start <-1
      colnames(datImp) <- myOrderXShort[start:length(myOrderXShort)]}
   
    # datImp <- rbind(rep(dim(datImp)[2],dim(datImp)[2]) , rep(1,dim(datImp)[2]) , datImp)
    if (dim(dat)[1] > 1){
      datCV <- subset(ImpRegCV, Region == myLevelsAcr[i]) %>%
        subset(Crop == 'soya')
      # Order by importance reformat table
      for (j in 1:500){
        dat <- subset(datCV, CValid == j) %>%
          #arrange(desc(MeanDecreaseAccuracy))
          arrange(MeanDecreaseAccuracy)
        dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
          arrange(factor(AgroInd, levels = file1))
        dat <- tibble(dat$Imp) %>% t()
        datImp <- rbind(datImp, dat)
      }
      datImp <- datImp[-1,] # Removes the first row which is the main run
      rownames(datImp) <- 1:dim(datImp)[1]
      mu <- apply(datImp[,], MARGIN = 2, mean, na.rm=TRUE)
      datImp <- rbind(datImp, first)
      datImp <- rbind(datImp, mu)
      datImp <- as_tibble(datImp, rownames = 'Group')
      colorsImp <- c(rep(rgb(0,0,0,0.1),(dim(datImp)[1]-1)),'#f54242')
      # colorsImp <- c(rep(rgb(0,0,0,0.1),(dim(datImp)[1]-2)),'#f54242','#4242f5')
      
      p3 <- ggradar(data.frame(datImp),
                    values.radar = c(dim(datImp)[2]-1,(dim(datImp)[2]-1)*0.5,1),
                    grid.min=1,  grid.mid= (dim(datImp)[2]/2), grid.max=(dim(datImp)[2]-1),
                    gridline.mid.colour='grey', background.circle.colour='white',
                    group.line.width = 1, 
                    group.point.size = 0,
                    group.colours = colorsImp,
                    legend.position = 'none',
                    grid.label.size = 5,
                    axis.label.size = 2)
        # labs(title = "Soy")
    } else {
      p3 <- NULL
    }
    
    
  # Wheat ####
    dat <- subset(ImpReg, Region == myLevelsAcr[i]) %>%
      subset(Crop == 'wheat') %>%
      #arrange(desc(MeanDecreaseAccuracy))
      arrange(MeanDecreaseAccuracy)
    dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
      arrange(factor(AgroInd, levels = file1))
    datImp <- tibble(dat$Imp) %>% t()
    first <- datImp
    colnames(datImp) <- dat$AgroInd
    if (length(dat$AgroInd) == 10){
      start <-3
    }else {start <-1}
    colnames(datImp) <- myOrderXShort[start:length(myOrderXShort)]
    # datImp <- rbind(rep(dim(datImp)[2],dim(datImp)[2]) , rep(1,dim(datImp)[2]) , datImp)
    
    datCV <- subset(ImpRegCV, Region == myLevelsAcr[i]) %>%
      subset(Crop == 'wheat')
    # Order by importance reformat table
    for (j in 1:500){
      dat <- subset(datCV, CValid == j) %>%
        #arrange(desc(MeanDecreaseAccuracy))
        arrange(MeanDecreaseAccuracy)
      dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
        arrange(factor(AgroInd, levels = file1))
      dat <- tibble(dat$Imp) %>% t()
      datImp <- rbind(datImp, dat)
    }
    datImp <- datImp[-1,] # Removes the first row which is the main run
    rownames(datImp) <- 1:dim(datImp)[1]
    mu <- apply(datImp[,], MARGIN = 2, mean, na.rm=TRUE)
    datImp <- rbind(datImp, first)
    datImp <- rbind(datImp, mu)
    datImp <- as_tibble(datImp, rownames = 'Group')
    colorsImp <- c(rep(rgb(0,0,0,0.1),(dim(datImp)[1]-1)),'#f54242')
    # colorsImp <- c(rep(rgb(0,0,0,0.1),(dim(datImp)[1]-2)),'#f54242','#4242f5')
    
    p4 <- ggradar(data.frame(datImp),
                  values.radar = c(dim(datImp)[2]-1,(dim(datImp)[2]-1)*0.5,1),
                  grid.min=1,  grid.mid= (dim(datImp)[2]/2), grid.max=(dim(datImp)[2]-1),
                  gridline.mid.colour='grey', background.circle.colour='white',
                  group.line.width = 1, 
                  group.point.size = 0,
                  group.colours = colorsImp,
                  legend.position = 'none',
                  grid.label.size = 5,
                  axis.label.size = 2)
      # labs(title = "Wheat")
    
  # Final Plot ####
    myTitle <- ggdraw() +
      draw_label(paste0(myLevelsLong[i]),
                 fontface = "bold",
                 angle = 0,
                 size=14)
    F1 <- plot_grid(p1, p2,
                   p3, p4,
                   ncol = 2,
                   labels = c('A','B','C','D')
                   )
    F1 <- plot_grid(myTitle,
                    F1,
                    ncol = 1,
                    rel_heights = c(0.05,1))
    ggsave(F1, filename = paste0(fileloc1, loc1, "Importance_Spider_RF_",myLevelsAcr[i],'_',type,'_',t, '_mu.jpg'),
           width = 8, height = 8, dpi = 350, bg = 'white')
}

  
# END #

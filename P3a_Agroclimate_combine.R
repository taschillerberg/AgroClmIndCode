# P3a_Agroclimate_Combine.R
# This R program will open output of global agroclimate indices from the 
# P3_Global_Agroclimate.R file and combine them into a single file for each 
# indices and end its program by writing the file. 
#
# T. A. Schillerberg
#               Jun. 2020
#      Updated: Aug. 2021

# Local Computer
# Time: ~15 min
setwd("/Research/AgroclimaticConditions/Code")
fileloc1 <- '/Research/AgroclimaticConditions/Data/Global_AgroInd/'
fileloc2 <- '/Research/AgroclimaticConditions/Data/'
Adate <- 'Agro092021'


# fileloc1 <- '~/AgroClmInd/data/'
# fileloc2 <- '~/AgroClmInd/data/AgroclimateInd/'

options(show.error.locations = TRUE)

# Libraries ####################################################################
library(tidyverse)
library(ggplot2)

# Functions ####################################################################
#NA

# PART I  -- Combine ###########################################################
#     This section will load in the agroclimate files, combine, and output
#
# References: 

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

days <- read_csv(paste(fileloc2, 'NASAPower/Days_from_1-1-1981.csv', sep=''),
                col_names = TRUE) %>%
  column_to_rownames('Year')
# 'dayStartEnd'

yr <- 1981:2018
file <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
          'StartFieldOp')
file2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting', 
           'SMMidSeason','SMHarvestSeason')

crop <- c('maize', 'rice', 'soya', 'wheat') # ,'wwheat')

units <- c('day of year','day of year','days', 'days','day of year', 
           'days', 'days','days', 'days','days', 'days','days', 'days', #Growing Deg Days & Heat Stress Days
           'mm','mm','mm','mm', 'days', 'days', 'days', 'days', #Total Precipitation & Dry Days
           'days', 'days', 'days', 'days','days', 'days', 'days', 'days', #SMPlanting & SMMidSeason
           'days', 'days', 'days', 'days') # SMHarvestSeason

rm(latID, lonID, latlonID)

T1 <- Sys.time()
print(paste('Variables loaded at: ', T1, sep=''))

Agro_mu <- cbind(latlon, matrix(data=NA, nrow= dim(latlon)[1], 
                                ncol=(length(file)+length(file2)*length(crop))))


# 1.2 Read in Agroclimate  ##########
# (not crop dependent)

for (i in 1:length(file)){
  dat1 <- read_csv(paste(fileloc1, Adate,'/', file[i], '_1.csv', sep = ''), 
                   col_names = TRUE, cols(.default = col_double()))
  latlon <- dat1[,1:4]
  dat1 <- dat1[, -which(names(dat1) %in% c('1991','1992'))]
  
  dat2 <- read_csv(paste(fileloc1,Adate,'/', file[i], '_2.csv', sep = ''), 
                   col_names = TRUE, cols(.default = col_double()))
  dat2 <- dat2[, -which(names(dat2) %in% c('latID','lonID','1990','2000','2001'))]
  
  dat3 <- read_csv(paste(fileloc1,Adate,'/', file[i], '_3.csv', sep = ''), 
                   col_names = TRUE, cols(.default = col_double()))
  dat3 <- dat3[, -which(names(dat3) %in% c('latID','lonID','1999','2009','2010'))]
  
  dat4 <- read_csv(paste(fileloc1,Adate,'/', file[i], '_4.csv', sep = ''), 
                   col_names = TRUE, cols(.default = col_double()))
  dat4 <- dat4[, -which(names(dat4) %in% c('latID','lonID','2008'))]
  
  dat <- left_join(dat1, dat2, by = c('lat','lon'), keep = FALSE) %>%
    left_join(dat3, by = c('lat','lon'), keep = FALSE) %>%
    left_join(dat4, by = c('lat','lon'), keep = FALSE)
  
  if (file[i] == 'LastSpringFrost'| file[i] == 'FirstFallFrost' | file[i] == 'StartFieldOp'){
    for (j in yr){
      dat5 <- dat
      m <- which(colnames(dat) == j)
      n <- which(rownames(days) == j)
      dat[,m] <- dat[,m] - days$first_day[n]
      dat[,m][dat5[,m] == -999] <- -999
    }
  }
  
  write.csv(dat, file = paste0(fileloc1,Adate,'_mu/', file[i], '.csv'), row.names = FALSE)
  
  dat[,6:(ncol(dat)-1)][dat[,6:(ncol(dat)-1)] == -999] <- NA
  Agro_mu[,(4+i)] <- apply(X = dat[,6:(ncol(dat)-1)], MARGIN =1, FUN = mean, na.rm = TRUE) %>%
    round(digits = 0)
}

T2 <- Sys.time()
C <- T2 - T1
print(paste('Finished combining and writing non-crop dependent variables at: ',
            T2, '. Time elapased: ', C, sep=''))

# 1.3 Read in Agroclimate  ##########
# (crop dependent)
counter <- 0
file2a <- character(length = (length(file2)*length(crop)))

for (i in file2){
  for (j in crop){
    counter = counter + 1
    
    dat1 <- read_csv(paste(fileloc1,Adate,'/', i, '_', j, '_1.csv', sep = ''),
                     col_names = TRUE, cols(.default = col_double()))
    latlon <- dat1[,1:4]
    dat1 <- dat1[, -which(names(dat1) %in% c('1991','1992'))]

    dat2 <- read_csv(paste(fileloc1,Adate,'/', i, '_', j, '_2.csv', sep = ''),
                     col_names = TRUE, cols(.default = col_double()))
    dat2 <- dat2[, -which(names(dat2) %in% c('latID','lonID','1990','2000','2001'))]

    dat3 <- read_csv(paste(fileloc1,Adate,'/', i, '_', j, '_3.csv', sep = ''),
                     col_names = TRUE, cols(.default = col_double()))
    dat3 <- dat3[, -which(names(dat3) %in% c('latID','lonID','1999','2009','2010'))]

    dat4 <- read_csv(paste(fileloc1,Adate,'/', i, '_', j, '_4.csv', sep = ''),
                     col_names = TRUE, cols(.default = col_double()))
    dat4 <- dat4[, -which(names(dat4) %in% c('latID','lonID','2008'))]

    dat <- left_join(dat1, dat2, by = c('lat','lon'), keep = FALSE) %>%
      left_join(dat3, by = c('lat','lon'), keep = FALSE) %>%
      left_join(dat4, by = c('lat','lon'), keep = FALSE)

    write.csv(dat, file = paste(fileloc1,Adate,'_mu/', i,'_',j, '.csv', sep=''), row.names = FALSE)

    dat[,6:(ncol(dat)-1)][dat[,6:(ncol(dat)-1)] == -999] <- NA
    Agro_mu[,(9 + counter)] <- apply(X = dat[,6:(ncol(dat)-1)], MARGIN =1, FUN = mean, na.rm = TRUE)
    
    file2a[counter] <- paste(i,'_',j,sep='')
  } # end j of crops
} # end i of file2

colnames(Agro_mu) <- c(colnames(latlon), file, file2a)

T2 <- Sys.time()
C <- T2 - T1
print(paste('Finished combining and writing crop dependent variables at: ',
            T2, '. Time elapased: ', C, sep=''))

# PART II -- Plot ##############################################################
#     This section will plot the mean of the agroclimate indicies. 
#
# References: 

# 2.1 Variables Needed ##########

# world map 
baseData <- map_data('world')

# 2.2 Ploting ##########
for (i in 5:length(Agro_mu)){
  obs <- Agro_mu[,i]
  df_obs <- data.frame(latlon$lat, latlon$lon, obs)
  
  # If the y-axis needs flipped
  # df_obs$lat <- df_obs$lat * -1
  colnames(df_obs) <- c('lat', 'lon', 'obs')
  limits <- round(range(df_obs$obs, na.rm = TRUE))
  
  # ploting and saving
  p1_obs <- ggplot(data=df_obs, aes(x=lon,y=lat, fill=obs)) +   theme_bw() +
    labs(title=paste("Mean value of ", colnames(Agro_mu)[i], sep=""), x="", y="")+ 
    theme(plot.title = element_text(hjust = 0.5, size=15)) +
    geom_tile() + 
    # scale_fill_discrete(na.value="white", limits=limits)+
    scale_fill_gradient(low="blue", high="yellow", na.value="white", limits=limits) +
    #scale_colour_gradientn(name = "",colours = terrain.colors(10), 
    #                       limits=limits ,na.value="white") + 
    geom_polygon(data=baseData, aes(x=long, y=lat, group=group), 
                 colour="black", fill="white", alpha=0) +
    coord_fixed(ratio=1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE)+ 
    theme(legend.position="bottom",legend.title = element_blank())+ 
    theme(legend.key.height  = unit(0.5, "cm"), 
          legend.direction="horizontal",legend.text = element_text(size = 15))+
    theme(plot.margin=margin(t=0,unit="cm"))+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=10,face="bold"))+
    theme(legend.key.size = unit(1.5, "cm"))
  plot(p1_obs)
  
  # saving
  ggsave(filename = paste0(fileloc1, Adate, '_mu/' ,'Mu_', colnames(Agro_mu)[i], ".tiff"), width = 8, height = 5, dpi = 300)
  
}


T2 <- Sys.time()
C <- T2 - T1
print(paste('Finished plotting mean agroclimate variables at: ',
            T2, '. Time elapased: ', C, sep=''))

# END

# P2_DataConversion.R
# This R program will convert datafiles into a csv latlon datafiles and outputed 
# by year. The first part will import, reorganize, and output NASAPower datafile
# which are in a ASSIS format (or something) they were downloaded from the NASA
# website. Temperatures will be corrected into C and precipitation changed into 
# the appropriate format. The second part will load in GLEAM v3.3a (nc) data
# convert and write the files. 
#
# T. A. Schillerberg
#               Aug. 2020
#      Updated: Dec. 2020

# Local
setwd("")
fileloc1 <- '/Research/AgroclimaticConditions/Data/NASAPower/'
fileloc2 <- '/Research/AgroclimaticConditions/Data/GLEAM_v3.3a/SMsurf/'
# HPC
fileloc1 <- '~/AgroClmInd/data/NASAPower/'
fileloc2 <- '~/AgroClmInd/data/GLEAM/SMsurf/'

options(show.error.locations = TRUE)

# Libraries ####################################################################
library(raster)
library(sp)
library(ggplot2)
library(ncdf4)
library(tidyverse)

# Functions ####################################################################
pt_latlon <- function(datX, latlonID){
  #     This function will return the dataframe value for 0.5 deg spacing to a 
  # 2D latlon dataframe
  # latlonID is in the form lat,lon,latID,lonID
  
  # lat <- seq(-89.75, 89.75, by = 0.5) # y-axis
  # lon <- seq(-179.75, 179.75, by = 0.5) # x-axis
  # latlon <- expand.grid(lat, lon) %>%
  #   rename('lat' = 'Var1') %>%
  #   rename('lon' = 'Var2')
  datX[latlonID[3], latlonID[4]]
}
diy <- function(year){
  #     This function will determine how many days are in a given year
  365 + (year %% 4 == 0) - (year %% 100 == 0) + (year %% 400 == 0)
}
KtoC <- function(K){
  #     This function will convert a temperature in Kelvin to a temperature in 
  # Celcius. 
  round((K - 273.15),2)
}
Pm2toPmm <- function(P){
  #     This function will convert daily precipitation from (kg m^-2 s^-1) to
  # (mm d^-1) 
  # (kg m^-2 s^-1) * (m^3/997kg) * (86400s/day)
  #
  round(((P*60*60*24*1000)/997),2)
}

# PART I -- Conversion of NASAPower ############################################
#     This section will read in the raw NASAPower datafiles and create a csv in 
# format latlonID. First maximum temperature, then minimum, and finally 
# precipitation. 
#
# References: power.larc.nasa.gov 

# . 1.1 Variables needed -------------------------------------------------------
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
rm(latID, lonID, latlonID)

yr <- 1981:2019
loc <- 'RawFiles/'

dayStEn <- read_csv(paste(fileloc1, 'Days_from_1-1-1981.csv', sep=''),
                    col_names = TRUE) %>%
  column_to_rownames(var='Year')

# . 1.2 Maximum temperature ----------------------------------------------------
#     Converting maximum temperature from K to C. Takes about 10 minutes on 
# Apple laptop to process one year.
for (i in 1:length(yr)){
  dat <- read_csv(paste(fileloc1, loc, 'power_801_daily_t2m_max_lst_', yr[i],
                        '.csv', sep = ''), skip = 16, col_names=TRUE,
                  cols(.default = col_guess())) %>%
    column_to_rownames(var = 'T2M_MAX.lon')
  dat[dat == -999] <- NA
  # Check to make sure that all time, lat, lon exist in the data file if not
  # print out an error of the year, lat, lon
  latdays <- expand.grid(lat, dayStEn$first_day[i]:dayStEn$last_day[i])
  for (j in 1:dim(latdays)[1]){
    row <- rownames(dat[j,])
    t <- parse_number(sapply(strsplit(row, '='),'[[',2))
    l <- parse_number(sapply(strsplit(row, '='),'[[',3))
    if (t != latdays[j,2]){
      print(paste('error in t = ', latdays[j,2], ' l = ', latdays[j,1], sep=''))
      break
    } else if (l != latdays[j,1]){
      print(paste('error in l = ', latdays[j,1], ' t = ', latdays[j,2], sep=''))
      break
    } else {}
  } # end j for loop -- testing

  # Change the units
  dat <- sapply(dat, KtoC)

  # Make into long format dataframe
  days <- dayStEn$first_day[i]:dayStEn$last_day[i]
  dat_latlon <- latlon %>%
    cbind(matrix(data = -999, nrow = dim(latlon)[1],
                 ncol = length(as.character(days))))
  colnames(dat_latlon) <- c(colnames(latlon),
                            as.character(dayStEn$first_day[i]:
                                           dayStEn$last_day[i]))
  for (j in 1:length(days)){
    dat_latlon[,(4+j)] <- matrix(data = dat[((j-1)*length(lat)+1):(j*length(lat)),],
                   ncol = 1, byrow=FALSE)
  }

  write.csv(dat_latlon, file = paste(fileloc1, '202012/',
                                     'POWER_World_Daily_T2M_MAX_',yr[i],'.csv',
                                     sep=''), row.names = FALSE)
  print(paste('Finished writing POWER_World_Daily_T2M_MAX_', yr[i], '.csv',sep=''))
}

rm(dat, days, dat_latlon, latdays, row, t, l, i, j )

# . 1.3 Minimum temperature ----------------------------------------------------
for (i in 1:length(yr)){
  dat <- read_csv(paste(fileloc1, loc, 'power_801_daily_t2m_min_lst_', yr[i],
                        '.csv', sep = ''), skip = 16, col_names=TRUE,
                  cols(.default = col_guess())) %>%
    column_to_rownames(var = 'T2M_MIN.lon')
  dat[dat == -999] <- NA
  # Check to make sure that all time, lat, lon exist in the data file if not
  # print out an error of the year, lat, lon
  latdays <- expand.grid(lat, dayStEn$first_day[i]:dayStEn$last_day[i])
  for (j in 1:dim(latdays)[1]){
    row <- rownames(dat[j,])
    t <- parse_number(sapply(strsplit(row, '='),'[[',2))
    l <- parse_number(sapply(strsplit(row, '='),'[[',3))
    if (t != latdays[j,2]){
      print(paste('error in t = ', latdays[j,2], ' l = ', latdays[j,1], sep=''))
      break
    } else if (l != latdays[j,1]){
      print(paste('error in l = ', latdays[j,1], ' t = ', latdays[j,2], sep=''))
      break
    } else {}
  } # end j for loop -- testing

  # Change the units
  dat <- sapply(dat, KtoC)

  # Make into long format dataframe
  days <- dayStEn$first_day[i]:dayStEn$last_day[i]
  dat_latlon <- latlon %>%
    cbind(matrix(data = -999, nrow = dim(latlon)[1],
                 ncol = length(as.character(days))))
  colnames(dat_latlon) <- c(colnames(latlon),
                            as.character(dayStEn$first_day[i]:
                                           dayStEn$last_day[i]))
  for (j in 1:length(days)){
    dat_latlon[,(4+j)] <- matrix(data = dat[((j-1)*length(lat)+1):(j*length(lat)),],
                                 ncol = 1, byrow=FALSE)
  }

  write.csv(dat_latlon, file = paste(fileloc1, '202012/',
                                     'POWER_World_Daily_T2M_MIN_',yr[i],'.csv',
                                     sep=''), row.names = FALSE)
  print(paste('Finished writing POWER_World_Daily_T2M_MIN_', yr[i], '.csv',sep=''))
}
# Start 7:43
rm(dat, days, dat_latlon, latdays, row, t, l, i, j )

# . 1.4 Precipitation ----------------------------------------------------------
for (i in 1:length(yr)){
  dat <- read_csv(paste(fileloc1, loc, 'power_801_daily_prectot_lst_', yr[i], 
                        '.csv', sep = ''), skip = 16, col_names=TRUE, 
                  cols(.default = col_guess())) %>%
    column_to_rownames(var = 'PRECTOT.lon')
  dat[dat == -999] <- NA
  # Check to make sure that all time, lat, lon exist in the data file if not 
  # print out an error of the year, lat, lon
  latdays <- expand.grid(lat, dayStEn$first_day[i]:dayStEn$last_day[i])
  for (j in 1:dim(latdays)[1]){
    row <- rownames(dat[j,])
    t <- parse_number(sapply(strsplit(row, '='),'[[',2))
    l <- parse_number(sapply(strsplit(row, '='),'[[',3))
    if (t != latdays[j,2]){
      print(paste('error in t = ', latdays[j,2], ' l = ', latdays[j,1], sep=''))
      break
    } else if (l != latdays[j,1]){
      print(paste('error in l = ', latdays[j,1], ' t = ', latdays[j,2], sep=''))
      break
    } else {}
  } # end j for loop -- testing 
  
  # Change the units
  dat <- sapply(dat, Pm2toPmm)
  
  # Make into long format dataframe
  days <- dayStEn$first_day[i]:dayStEn$last_day[i]
  dat_latlon <- latlon %>%
    cbind(matrix(data = -999, nrow = dim(latlon)[1], 
                 ncol = length(as.character(days))))
  colnames(dat_latlon) <- c(colnames(latlon), 
                            as.character(dayStEn$first_day[i]:
                                           dayStEn$last_day[i]))
  for (j in 1:length(days)){
    dat_latlon[,(4+j)] <- matrix(data = dat[((j-1)*length(lat)+1):(j*length(lat)),], 
                                 ncol = 1, byrow=FALSE)
  }
  
  write.csv(dat_latlon, file = paste(fileloc1, '202012/', 
                                     'POWER_World_Daily_PRECTOT_',yr[i],'.csv',
                                     sep=''), row.names = FALSE)
  print(paste('Finished writing POWER_World_Daily_PRECTOT_', yr[i], '.csv',sep=''))
}

rm(dat, days, dat_latlon, latdays, row, t, l, i, j )


# PART II -- Conversion of GLEAM (nc) ##########################################
#     This section will read in the raw nc files of GLEAM v3.3a. Units m3/m3
#
# References: Martens et al. (2017)

# . 2.1 Variables needed -------------------------------------------------------
yr <- 2016:2018
loc <- 'NN_r0.5_latlon/'
# . 2.2 Opening the files ------------------------------------------------------
# takes about 10 minutes a file on hpc

for (i in 1:length(yr)){
  dat_nc <- ncdf4::nc_open(paste(fileloc2, loc, 'NN_SMsurf_',yr[i], '_GLEAM_v3.3a.nc', sep=''))
  lat_nc <- ncdf4::ncvar_get(dat_nc, 'lat')
  lon_nc <- ncdf4::ncvar_get(dat_nc, 'lon')
  time_nc <- ncdf4::ncvar_get(dat_nc, 'time')
  dat <- ncdf4::ncvar_get(dat_nc, 'SMsurf')
  fillvalue <- ncdf4::ncatt_get(dat_nc, 'SMsurf', '_FillValue')
  dat[dat == fillvalue$value] <- NA

  dimnames(dat) <- list(lon_nc, lat_nc, time_nc)
  dat <- aperm(dat, perm=c(2,1,3))
  dat <- dat[order(as.numeric(row.names(dat))),,]

  #check to make sure that the number of days match
  if(diy(yr[i]) != length(time_nc)){
    print(paste('error in number of days when GLEAM year = ', yr[i], sep=''))
    break
  } else {
    print(yr[i])
  }
  m <- which(rownames(dayStEn) == yr[i])
  days <- dayStEn$first_day[m]:dayStEn$last_day[m]
  dat_latlon <- latlon %>%
    cbind(matrix(data = -999, nrow = dim(latlon)[1],
                 ncol = length(as.character(days))))
  colnames(dat_latlon) <- c(colnames(latlon),
                            as.character(dayStEn$first_day[m]:
                                           dayStEn$last_day[m]))
  # Convert into a 2D array
  for (j in 1:length(time_nc)){
    dat1 <- dat[,,j]
    dat_latlon[,(4+j)] <- apply(X=latlon, MARGIN=1, FUN=pt_latlon, datX=dat1)
  }

  write.csv(dat_latlon, file = paste(fileloc2,loc, '202303/',
                                     'NN_SMsurf_',yr[i], '_GLEAM_v3.3a.csv',
                                     sep=''), row.names = FALSE)
  print(paste('Finished writing NN_SMsurf_',yr[i], '_GLEAM_v3.3a.csv',sep=''))
}

# END ##########################################################################
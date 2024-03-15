# P3_Global_Agroclimate.R
# This R program will open temperature, precipitation, and soil moisture data. 
# File (mask) will also be opened that contains if there is cropland (maize, 
# rice, soya, wheat), (Crop_season) planting and harvesting dates (start, range, average, and 
# end) of each main season crop and winter wheat. 12 agroclimate indicies will 
# will then be calculated at a global scale according to the cropland. 
#
# Becasue of memory issues related to HPC this program was run at 4 
# (year) intervals. The R Script P3a_Agroclimate_combine.R 
#
# T. A. Schillerberg
#               Jun. 2020
#      Updated: Feb. 2022


# Local Computer
# setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/AgroclimaticConditions/Code")
# fileloc1 <- '/Research/AgroclimaticConditions/Data/'
# fileloc2 <- '/Research/AgroclimaticConditions/Data/NASAPower/'
# fileloc3 <- '/Research/AgroclimaticConditions/Data/GLEAM_v3.3a/SMsurf/NN_r0.5_latlon/'
# fileloc4 <- fileloc1

# HPC
fileloc1 <- '~/AgroClmInd/data/'
fileloc2 <- '~/AgroClmInd/data/NASAPower/'
fileloc3 <- '~/AgroClmInd/data/GLEAM/SMsurf/'
fileloc4 <- '~/AgroClmInd/data/AgroclimateInd/Agro022022'

options(show.error.locations = TRUE)

# Variables to change ##########################################################
yr <- 1981:1992 # 1981:1992 1990:2001 1999:2010 2008:2018
time <- 1:12 # 1:12 10:21 19:30 28:38
core <- 19
part <- '1_2022' # 1 2 3 4

# Libraries ####################################################################
library(dplyr); library(forcats); library(tibble)
library(readr); library(stringr); library(tidyr)
library(purrr); library(magrittr)
# library(tidyverse)

Ta <- Sys.time()
print(paste('Variables loaded at: ', Ta, sep=''))
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

Last_Spring_Frost <- function(X, varis){
  #     This function will compute the day of the year that the last spring 
  # frost occurs. In the northern hemisphere this is July 15 -> Jan 15 
  # (backwards), in the southern hemisphere this is Jan 15 -> July 15 (backwards). 
  #
  # X <- ID <- row number
  # varis <- list of variables: latlon, days, Tmin
  
  # variables needed ####
  ID <- X
  July15 <- 195; Jan15 <- 14;  Feb28 <- 58
  TminPt <- varis[["Tmin"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  
  # calculation ####
  if (varis[["mask"]][ID] == 1) {
    # The data point is cropland
    Agro <- array(-999, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Test the hemispheres
      if (latlonPt$lat >= 0){ 
        # Northern Hemisphere
        start <- varis[["days"]]$first_day[i] + leap + July15 - 1
        end <- varis[["days"]]$first_day[i] + Jan15
      } else if (latlonPt$lat <= 0){
        # Southern Hemisphere
        if (i == 1){
          start <- varis[["days"]]$first_day[i] + Jan15 - 1
          end <- varis[["days"]]$first_day[i]
        } else {
          start <- varis[["days"]]$first_day[i] + Jan15 - 1
          end <- varis[["days"]]$first_day[i-1] + leapb + July15
        }
      } else {
        # ERROR in Hemispheres
        print(paste('Error at point ID ', ID, ' when calculating the last spring frost'))
        break
      } # End testing hemispheres 
      
      # Calculating
      for (j in seq(start,end, by= -1)){
        m <- which(colnames(TminPt) == j)
        if (TminPt[m] <= 0){
          Agro[i] <- as.integer(colnames(TminPt[m]))
          break
        }
      } # End j loop of days
      
    } # End i loop of years
    
  } else if (varis[["mask"]][ID] == 0 ){
    # The data point is non-cropland or ocean
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
}

First_Fall_Frost <- function(X, varis){
  #     This function will compute the day of the year that the first fall 
  # frost occurs. In the northern hemisphere this is July 15 -> Jan 15 
  # (forwards), in the southern hemisphere this is Jan 15 -> July 15 (forwards). 
  #
  # X <- ID <- row number
  # varis <- list of variables: latlon, days, Tmin
  
  # variables needed ####
  ID <- X
  July15 <- 195; Jan15 <- 14;  Feb28 <- 58
  TminPt <- varis[["Tmin"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  
  # calculation ####
  if (varis[["mask"]][ID] == 1) {
    # The data point is cropland
    Agro <- array(-999, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Test the hemispheres
      if (latlonPt$lat >= 0){ 
        # Northern Hemisphere
        start <-  varis[["days"]]$first_day[i] + leap + July15
        if (i == length(varis[["days"]]$Year)){
          end <- varis[["days"]]$last_day[i]
        } else {
          end <- varis[["days"]]$first_day[i+1] + Jan15 - 1
        }
      } else if (latlonPt$lat <= 0){
        # Southern Hemisphere
        start <- varis[["days"]]$first_day[i] + Jan15
        end <- varis[["days"]]$first_day[i] + leap + July15 - 1
      } else {
        # ERROR in Hemispheres
        print(paste('Error at point ID ', ID, ' when calculating the first fall frost'))
        break
      } # End testing hemispheres 
      
      for (j in seq(start,end, by= 1)){
        m <- which(colnames(TminPt) == j)
        if (TminPt[m] <= 0){
          Agro[i] <- as.integer(colnames(TminPt[m]))
          break
        }
      } # End j loop of days
      
    } # End i loop of years
    
  } else if (varis[["mask"]][ID] == 0){
    # The data point is non-cropland or ocean
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
}

Clm_Growing_Season_Length <- function(X, varis){
  #     This function will compute the climatological growing season. Which is 
  # from the last spring frost (i) until the first fall frost (i) for both 
  # hemispheres. When there is no last spring frost or first fall frost 
  # (cropland value is -999) the start/end of the season will be used.
  #
  # X <- ID <- row number
  # varis <- list of variables: latlon, days, Tmin
  
  # variables needed ####
  ID <- X
  July15 <- 195; Jan15 <- 14;  Feb28 <- 58
  TminPt <- varis[["Tmin"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  LSFrostPt <- varis[["LastSpringFrost"]][ID,-c(1:4)]
  FFFrostPt <- varis[["FirstFallFrost"]][ID,-c(1:4)]
  
  # calculation ####
  
  if (varis[["mask"]][ID] == 1){
    # The data point is cropland
    Agro <- array(0, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Test the hemispheres
      if (latlonPt$lat >= 0){ 
        # Northern Hemisphere
        if (LSFrostPt[i] == -999){
          start <- varis[["days"]]$first_day[i] + Jan15
        } else { 
          # LSFrostPt[i] != -999
          start <- LSFrostPt[i]
        } 
        if (FFFrostPt[i] != -999) {
          end <- FFFrostPt[i]
        } else if (FFFrostPt[i] == -999 & i == length(varis[["days"]]$Year)){
          end <- varis[["days"]]$last_day[i]
        } else {
          end <- varis[["days"]]$first_day[i+1] + Jan15 -1
        }
        
      } else if (latlonPt$lat <= 0){
        # Southern Hemisphere
        if (LSFrostPt[i] == -999 & i == 1) {
          start <- varis[["days"]]$first_day[i]
        } else if (LSFrostPt[i] == -999) {
          start <- varis[["days"]]$first_day[i - 1] + leapb + July15
        } else {
          # LSFrostPt[i] != -999
          start <- LSFrostPt[i]
        } 
        if (FFFrostPt[i] == -999){
          end <- varis[["days"]]$first_day[i] + leap + July15 - 1
        } else {
          # FFFrostPt[i] != -999
          end <- FFFrostPt[i]
        }
      } else {
        # ERROR in Hemispheres
        print(paste('Error at point ID ', ID, ' when calculating the clm growing season'))
        break
      } # End testing hemispheres
      
      # Calculating number of days
      if(as.integer(start) - 0 == as.integer(end)){
        Agro[i] <- diy(varis[["days"]]$Year[i])
      } else if (as.integer(start) - 1 == as.integer(end)) {
        Agro[i] <- diy(varis[["days"]]$Year[i])
      } else if (as.integer(start) - 2 == as.integer(end)) {
        Agro[i] <- diy(varis[["days"]]$Year[i])
      } else if (as.integer(start) < as.integer(end)) {
        Agro[i] <- as.integer(end) - as.integer(start)
        # Agro[i] <- length(as.integer(start):as.integer(end))
      } else {
        print(paste('Error at point ID ', ID, ' when calculating the Agro[i] climatological growing season'))
      }
       
    } # End i loop of years
    
  } else if (varis[["mask"]][ID] == 0){
    # The data point is non-cropland or ocean
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
  
}

Accumulated_Frost_Days <- function(X, varis){
  #     This function will compute the accumulated frost days between the first 
  # fall frost (i-1) and last spring frost (i). A frost will be calculated if 
  # the minimum temperature is less than or equal to 0 degrees C. This value 
  # will be 0 for regions that do not have frost days.
  #
  # X <- ID <- row number
  # varis <- list of variables: latlon, days, Tmin
  
  # variables needed ####
  ID <- X
  July15 <- 195; Jan15 <- 14
  TminPt <- varis[["Tmin"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  LSFrostPt <- varis[["LastSpringFrost"]][ID,-c(1:4)]
  FFFrostPt <- varis[["FirstFallFrost"]][ID,-c(1:4)]
  
  # calculation ####
  if (varis[["mask"]][ID] == 1){
    # The data point is cropland
    Agro <- array(0, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Test the hemispheres
      if (latlonPt$lat >= 0){ 
        # Northern Hemisphere
        if (i == 1){
          start <- varis[["days"]]$first_day[i]
        } else if (FFFrostPt[i-1] == -999){
          start <- varis[["days"]]$first_day[i-1] + leapb +July15
        } else {
          # FFFrost[i-1] != -999
          start <- FFFrostPt[i-1]
        }
        if (LSFrostPt[i] == -999){
          end <- varis[["days"]]$first_day[i] + leap + July15 -1
        } else {
          # LSFrostPt[i] != -999
          end <- LSFrostPt[i]
        }
        
      } else if (latlonPt$lat <= 0){
        # Southern Hemisphere
        if (i == 1){
          start <- varis[["days"]]$first_day[i]
        } else if (FFFrostPt[i-1] == -999){
          start <- varis[["days"]]$first_day[i] + Jan15
        } else {
          # FFFrostPt[i-1] != -999
          start <- FFFrostPt[i-1]
        }
        if (LSFrostPt[i] == -999){
          end <- varis[["days"]]$first_day[i] + Jan15 -1
        } else {
          # LSFrostPt[i] != -999
          end <- LSFrostPt[i]
        }
      } else {
        # ERROR in Hemispheres
        print(paste('Error at point ID ', ID, ' when calculating the accumulated Frost days'))
        break
      } # End testing hemispheres
      
      
      for (j in as.integer(start):as.integer(end)){
        m <- which(colnames(TminPt) == j)
        if (TminPt[m] <= 0){
          Agro[i] <- Agro[i] + 1
        }
      }

    } # End i loop of years
    
  } else if (varis[["mask"]][ID] == 0){
    # The data point is non-cropland
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
}

Start_Field_Opp <- function(X, varis){
  #     This function will compute the start of field operations. Which will be 
  # determined when the cumulative average of temperature from July 15/Jan15 is 
  # more than 200 deg C. 
  #
  # X <- ID <- row number
  # varis <- list of variables: latlon, days, Tmin
  
  # variables needed ####
  ID <- X
  July15 <- 195; Jan15 <- 14
  TminPt <- varis[["Tmin"]][ID,]
  TmaxPt <- varis[["Tmax"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  
  # calculation ####
  if (varis[["mask"]][ID] == 1){
    # The data point is cropland
    Agro <- array(-999, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Test the hemispheres
      if (latlonPt$lat >= 0){ 
        # Northern Hemisphere
        end <- varis[["days"]]$first_day[i] + leap + July15 - 1
        start <- varis[["days"]]$first_day[i]
      } else if (latlonPt$lat <= 0){
        # Southern Hemisphere
        if (i == 1){
          end <- varis[["days"]]$first_day[i] + Jan15 - 1
          start <- varis[["days"]]$first_day[i] 
        } else {
          end <- varis[["days"]]$first_day[i] + Jan15 - 1
          start <- varis[["days"]]$first_day[i-1] + leapb + July15
        }
      } else {
        # ERROR in Hemispheres
        print(paste('Error at point ID ', ID, ' when calculating the last spring frost'))
        break
      } # End testing hemispheres 
      
      # Calculating
      Tsum <- 0
      for (j in seq(start,end, by= 1)){
        m <- which(colnames(TmaxPt) == j)
        n <- which(colnames(TminPt) == j)
        Tave <- (as.integer(TmaxPt[m]) + as.integer(TminPt[m]))/2
        Tsum <- Tsum + Tave
        if (Tsum > 200){
          Agro[i] <- as.integer(colnames(TmaxPt[m]))
          break
        }
      } # End j loop of days
      
      # if (Agro[i] == -999){
      #   # Do Nothing
      # } else if (yr[i] == 1981){
      #   # Do Nothing
      # } else if (i == 1 & Agro[i] <= varis[["days"]]$first_day[i]){
      #   Agro[i] <- diy(yr[i]-1) - abs(varis[["days"]]$first_day[i] - Agro[i])
      # } else if (i == 1 & Agro[i] >= varis[["days"]]$first_day[i]) {
      #   Agro[i] <- Agro[i] - varis[["days"]]$first_day[i]
      # } else if (i > 1 & Agro[i] <= varis[["days"]]$first_day[i]){
      #   Agro[i] <- Agro[i] - varis[["days"]]$first_day[i-1]
      # } else if (i > 1 & Agro[i] >= varis[["days"]]$first_day[i]) {
      #   Agro[i] <- Agro[i] - varis[["days"]]$first_day[i]
      # } else {
      #   print(paste('Error at point ID ', ID, ' when calculating the day of the year start of field operations'))
      # }
      
    } # End i loop of years
    
  } else if (varis[["mask"]][ID] == 0){
    # The data point is non-cropland
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
}

Growing_Deg_Day <- function(X, varis, crop, season){
  #     This function will compute the number of growing degree days for each 
  # crop. The function will start with the start of the planting season end with 
  # the end of the harvest season.
  #
  # X <- ID <- row number
  # varis <- list of variables: mask, latlon, days, Tmax, Tmin, Crop Season Values, 
  #         last spring frost, first fall frost
  # crop <- 1 (maize), 2 (rice), 3 (soya), 4 (wheat)
  # variables <- list('mask'=Crop_season[["mask"]]$cropland,
  #                   'latlon' = latlon,
  #                   'days'=Crop_season[["dayStartEnd"]],
  #                   'Tmax'=Tmax,
  #                   'Tmin'=Tmin,
  #                   'MaizeSea' = Crop_season[["maize"]],
  #                   'RiceSea' = Crop_season[["rice"]],
  #                   'SoyaSea' = Crop_season[["soya"]],
  #                   'WheatSea' = Crop_season[["wheat"]])
  #
  # References: Zhu and Troy (2018)
  
  # variables needed #####
  ID <- X
  July15 <- 195; Jan15 <- 14; Feb28 <- 58
  TmaxPt <- varis[["Tmax"]][ID,]
  TminPt <- varis[["Tmin"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  
  if (crop == 1){
    cropPt <- varis[["MaizeSea"]][ID,]
    maskPt <- varis[["mask"]]$MaizeDate[ID]
    Tup <- 30     # Upper limits P:30
    Tbase <- 10   # Lower Limits P:10
  } else if (crop == 2){
    cropPt <- varis[["RiceSea"]][ID,]
    maskPt <- varis[["mask"]]$RiceDate[ID]
    Tup <- 30     # P: 40
    Tbase <- 10   # P: 10
  } else if (crop == 3){
    cropPt <- varis[["SoyaSea"]][ID,]
    maskPt <- varis[["mask"]]$SoyaDate[ID]
    Tup <- 30    # P: 30
    Tbase <- 10  # P: 10
  } else if (crop == 4){
    cropPt <- varis[["WheatSea"]][ID,]
    maskPt <- varis[["mask"]]$WheatDate[ID]
    Tup <- 25    # P: 30
    Tbase <- 5.5 # P: 0
  } else if (crop == 5){
    cropPt <- varis[["WWheatSea"]][ID,]
    maskPt <- varis[["mask"]]$WWheatDate[ID]
    Tup <- 25    # P: 30
    Tbase <- 1   # P: 0
  }else {
    print('Unreconized crop type or no crop type given. Please specify the crop type.')
  }
  
  # Season
  if (season == 1){
    startPt <- cropPt$plant.start
    endPt <- cropPt$plant.end
  } else if (season == 2){
    if (is.na(cropPt$plant)){
      startPt <- 0
    } else if (cropPt$plant == 0){
      startPt <- (cropPt$plant.start + cropPt$plant.end)/2
    } else {startPt <- cropPt$plant}
    if (is.na(cropPt$harvest)){
      endPt <- 0
    } else if (cropPt$harvest == 0){
      endPt <- (cropPt$harvest.start + cropPt$harvest.end)/2
    } else {endPt <- cropPt$harvest}
  } else if (season == 3){
    startPt <- cropPt$harvest.start
    endPt <- cropPt$harvest.end
  } else if (season == 4) {
    startPt <- cropPt$plant.start
    endPt <- cropPt$harvest.end
  } else { 
    print("No season given")
    startPt <- 0
    endPt <-1}
  
  # calculation ####
  if (maskPt == 1){
    # The data point is cropland
    Agro <- array(0, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Start of season ####
      if (i == 1 & startPt < endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc1'))
        }
      } else if (i == 1 & startPt > endPt){
        start <- varis[["days"]]$first_day[i]
      } else if (i > 1 & startPt < endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc2'))
        }
      } else if (i > 1 & startPt > endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i-1] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i-1] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc3'))
        }
      } else {
        print(paste('Error at point ID ', ID, ' when calculating starting dates. loc4'))
        break
      } # End testing starting date
      
      # End of the season ####
      if (i < length(varis[["days"]]$Year) & startPt < endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc1'))
        }
      } else if (i < length(varis[["days"]]$Year) & startPt > endPt) {
        if (endPt < Feb28){ #Assuming theat planting date is from the previous year
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc2'))
        }
      } else if (i == length(varis[["days"]]$Year) & startPt < endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc3'))
        }
      } else if (i == length(varis[["days"]]$Year) & startPt > endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc3'))
        }
      } else {
        print(paste('Error at point ID ', ID, ' when calculating ending dates. loc4', yr[i]))
      }
      
      # Calculation of daily growing degree days ####
      for (j in seq(start, end, by= 1)){
        m <- which(colnames(TmaxPt) == j)
        
        if (TmaxPt[m] <= Tup & TmaxPt[m] >= Tbase){
          Tmax <- as.integer(TmaxPt[m])
        } else if (TmaxPt[m] >= Tup){
          Tmax <- Tup
        } else if (TmaxPt[m] <= Tbase){
          Tmax <- Tbase
        } else { 
          print(paste("error in Tmax calculation of GDD. when ID is ",ID))
        }
        if (TminPt[m] <= Tup & TminPt[m] >= Tbase){
          Tmin <- as.integer(TminPt[m])
        } else if (TminPt[m] >= Tup){
          Tmin <- Tup
        } else if (TminPt[m] <= Tbase){
          Tmin <- Tbase
        } else { 
          print(paste("error in Tmin calculation of GDD. when ID is ",ID))
        }
        
        GDD <- round((((Tmax + Tmin)/2) - Tbase), digits = 0)
        if (GDD <= 0){
          # Do noting
        } else if (GDD > 0){
          Agro[i] <- Agro[i] + GDD
        }
      } # End j loop of days
      
    } # End i loop of years
    
  } else if (maskPt == 0){
    # The data point is non-cropland or ocean
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
  # rm(ID, varis, July15, Jan15, Feb28, TminPt, TmaxPt, latlonPt, cropPt, maskPt, startPt, endPt, i, j , m, Tup, Tbase, GDD, Agro)
}

Heat_Stress_Day <- function(X, varis, crop, season){
  #     This function will compute the number of heat stress days for each crop.
  # The function will start with the start of the planting season end with the 
  # end of the harvest season.
  #
  # X <- ID <- row number
  # varis <- list of variables: mask, latlon, days, Tmax, Tmin, Crop Season Values, 
  #         last spring frost, first fall frost
  # crop <- 1 (maize), 2 (rice), 3 (soya), 4 (wheat)
  # variables <- list('mask'=Crop_season[["mask"]]$cropland,
  #                   'latlon' = latlon,
  #                   'days'=Crop_season[["dayStartEnd"]],
  #                   'Tmax'=Tmax,
  #                   'Tmin'=Tmin,
  #                   'MaizeSea' = Crop_season[["maize"]],
  #                   'RiceSea' = Crop_season[["rice"]],
  #                   'SoyaSea' = Crop_season[["soya"]],
  #                   'WheatSea' = Crop_season[["wheat"]])
  #
  # References: Sanchez et al. (2014) Jackson et al., (2021) Hatfield et al., (2008, 2011)
  
  # variables needed ####
  ID <- X
  July15 <- 195; Jan15 <- 14; Feb28 <- 58
  TmaxPt <- varis[["Tmax"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  
  if (crop == 1){
    cropPt <- varis[["MaizeSea"]][ID,]
    maskPt <- varis[["mask"]]$MaizeDate[ID]
    Tstress <- 42  # Whole plant Tmax
    # Lethal Tmax = 46C        Grain filling 36.0C
    # Sanchez et al. 2014
  } else if (crop == 2){
    cropPt <- varis[["RiceSea"]][ID,]
    maskPt <- varis[["mask"]]$RiceDate[ID]
    Tstress <- 35.4 # Whole plant Tmax
    # Leathal Tmax = 42.9C    Grain filling Tmax 31.3C 
    # Sanchez et al. 2014
  } else if (crop == 3){
    cropPt <- varis[["SoyaSea"]][ID,]
    maskPt <- varis[["mask"]]$SoyaDate[ID]
    Tstress <- 39.4 # Whole plant Tmax
  } else if (crop == 4){
    cropPt <- varis[["WheatSea"]][ID,]
    maskPt <- varis[["mask"]]$WheatDate[ID]
    Tstress <- 28.7 # Whole plant Tmax
    
  } else if (crop == 5){
    cropPt <- varis[["WWheatSea"]][ID,]
    maskPt <- varis[["mask"]]$WWheatDate[ID]
    Tstress <- 28.5 # Whole plant Tmax
  }else {
    print('Unreconized crop type or no crop type given. Please specify the crop type.')
  }
  
  # Season
  if (season == 1){
    startPt <- cropPt$plant.start
    endPt <- cropPt$plant.end
  } else if (season == 2){
    if (is.na(cropPt$plant)){
      startPt <- 0
    } else if (cropPt$plant == 0){
      startPt <- (cropPt$plant.start + cropPt$plant.end)/2
    } else {startPt <- cropPt$plant}
    if (is.na(cropPt$harvest)){
      endPt <- 0
    } else if (cropPt$harvest == 0){
      endPt <- (cropPt$harvest.start + cropPt$harvest.end)/2
    } else {endPt <- cropPt$harvest}
  } else if (season == 3){
    startPt <- cropPt$harvest.start
    endPt <- cropPt$harvest.end
  } else if (season == 4) {
    startPt <- cropPt$plant.start
    endPt <- cropPt$harvest.end
  } else { 
    print("No season given")
    startPt <- 0
    endPt <-1}
  
  # calculation ####
  if (maskPt == 1){
    # The data point is cropland
    Agro <- array(0, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Start of season
      if (i == 1 & startPt < endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc1'))
        }
      } else if (i == 1 & startPt > endPt){
        start <- varis[["days"]]$first_day[i]
      } else if (i > 1 & startPt < endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc2'))
        }
      } else if (i > 1 & startPt > endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i-1] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i-1] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc3'))
        }
      } else {
        print(paste('Error at point ID ', ID, ' when calculating starting dates. loc4'))
        break
      } # End testing starting date
      
      # End of the season
      if (i < length(varis[["days"]]$Year) & startPt < endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc1'))
        }
      } else if (i < length(varis[["days"]]$Year) & startPt > endPt) {
        if (endPt < Feb28){ #Assuming theat planting date is from the previous year
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc2'))
        }
      } else if (i == length(varis[["days"]]$Year) & startPt < endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc3'))
        }
      } else if (i == length(varis[["days"]]$Year) & startPt > endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc3'))
        }
      } else {
        print(paste('Error at point ID ', ID, ' when calculating ending dates. loc4', yr[i]))
      }
      
      # Calculation of daily heat stress days
      for (j in seq(start, end, by= 1)){
        m <- which(colnames(TmaxPt) == j)
        
        if (TmaxPt[m] >= Tstress){
          Agro[i] <- Agro[i] + 1
        } else if (TmaxPt[m] < Tstress){
        } else { 
          print(paste("error in Tmax calculation of heat Stress. when ID is ",ID))
        }
        
      } # End j loop of days
      
    } # End i loop of years
    
  } else if (maskPt == 0){
    # The data point is non-cropland or ocean
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
}

Precip_Dry_Day <- function(X, varis, crop, type, season){
  #     This function will calculate the total precipitation and dry days during
  # the crop growing season. For the day to be a wet day the precipitation must 
  # be equal to or more than 1mm and a dry day is when precipitation is less 
  # than 1mm. 
  #
  # X <- ID <- row number
  # varis <- list of variables: mask, latlon, days, Precip, Crop Season Values, 
  #         last spring frost, first fall frost
  # crop <- 1 (maize), 2 (rice), 3 (soya), 4 (wheat)
  # type <- 1 total precipitation, 2 count of dry days in the season
  # variables <- list('mask'=Crop_season[["mask"]]$cropland,
  #                   'latlon' = latlon,
  #                   'days'=Crop_season[["dayStartEnd"]],
  #                   'Precip'=Precip,
  #                   'MaizeSea' = Crop_season[["maize"]],
  #                   'RiceSea' = Crop_season[["rice"]],
  #                   'SoyaSea' = Crop_season[["soya"]],
  #                   'WheatSea' = Crop_season[["wheat"]])
  
  # variables needed ####
  ID <- X
  July15 <- 195; Jan15 <- 14; Feb28 <- 58
  PrecipPt <- varis[["Precip"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  
  if (crop == 1){
    cropPt <- varis[["MaizeSea"]][ID,]
    maskPt <- varis[["mask"]]$MaizeDate[ID]
  } else if (crop == 2){
    cropPt <- varis[["RiceSea"]][ID,]
    maskPt <- varis[["mask"]]$RiceDate[ID]
  } else if (crop == 3){
    cropPt <- varis[["SoyaSea"]][ID,]
    maskPt <- varis[["mask"]]$SoyaDate[ID]
  } else if (crop == 4){
    cropPt <- varis[["WheatSea"]][ID,]
    maskPt <- varis[["mask"]]$WheatDate[ID]
  } else if (crop == 5){
    cropPt <- varis[["WWheatSea"]][ID,]
    maskPt <- varis[["mask"]]$WWheatDate[ID]
  } else {
    print('Unreconized crop type or no crop type given. Please specify the crop type.')
  }
  
  # Season
  if (season == 1){
    startPt <- cropPt$plant.start
    endPt <- cropPt$plant.end
  } else if (season == 2){
    if (is.na(cropPt$plant)){
      startPt <- 0
    } else if (cropPt$plant == 0){
      startPt <- (cropPt$plant.start + cropPt$plant.end)/2
    } else {startPt <- cropPt$plant}
    if (is.na(cropPt$harvest)){
      endPt <- 0
    } else if (cropPt$harvest == 0){
      endPt <- (cropPt$harvest.start + cropPt$harvest.end)/2
    } else {endPt <- cropPt$harvest}
  } else if (season == 3){
    startPt <- cropPt$harvest.start
    endPt <- cropPt$harvest.end
  } else if (season == 4) {
    startPt <- cropPt$plant.start
    endPt <- cropPt$harvest.end
  } else { 
    print("No season given")
    startPt <- 0
    endPt <-1}
  
  # calculation ####
  if (maskPt == 1){
    # The data point is cropland
    AgroTP <- array(0, dim=length(varis[["days"]]$Year))
    AgroDD <- array(0, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Start of season
      if (i == 1 & startPt < endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc1'))
        }
      } else if (i == 1 & startPt > endPt){
        start <- varis[["days"]]$first_day[i]
      } else if (i > 1 & startPt < endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc2'))
        }
      } else if (i > 1 & startPt > endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i-1] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i-1] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc3'))
        }
      } else {
        print(paste('Error at point ID ', ID, ' when calculating starting dates. loc4'))
        break
      } # End testing starting date
      
      # End of the season
      if (i < length(varis[["days"]]$Year) & startPt < endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc1'))
        }
      } else if (i < length(varis[["days"]]$Year) & startPt > endPt) {
        if (endPt < Feb28){ #Assuming theat planting date is from the previous year
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc2'))
        }
      } else if (i == length(varis[["days"]]$Year) & startPt < endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc3'))
        }
      } else if (i == length(varis[["days"]]$Year) & startPt > endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc3'))
        }
      } else {
        print(paste('Error at point ID ', ID, ' when calculating ending dates. loc4', yr[i]))
      }
      
      # Calculation of Precipitation Days
      for (j in seq(start,end, by= 1)){
        m <- which(colnames(PrecipPt) == j)
        if (PrecipPt[m] >= 1){
          AgroTP[i] = AgroTP[i] + as.integer(PrecipPt[m])
        } else {
          AgroDD[i] <- AgroDD[i] + 1
        }
      } # End j loop of days
      
    } # End i loop of years
    
    if (type == 1){
      Agro <- AgroTP
    } else if (type == 2){
      Agro <- AgroDD
    } else {
      print('No type given')
    }
    
  } else if (maskPt == 0){
    # The data point is non-cropland or ocean
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
}

Moisture_Field_Cond <- function(X, varis, crop, season, type){
  #     This function will calculate the number of days where the soil-water
  # content in the (surface) is between 10%-70% of maximum soil water-holding 
  # capacity and precipitation for day (i) is <= 1mm and the preceding day (i-1) 
  # is <= 5mm. Giving the number of workable days.
  #
  # X <- ID <- row number
  # varis <- list of variables: mask, latlon, days, Precip, Crop Season Values, 
  #         last spring frost, first fall frost
  # crop <- 1 (maize), 2 (rice), 3 (soya), 4 (wheat)
  # season <- 1 planting, 2 growing, 3 harvest
  # type <- 1 sum of days, 2 percentage of days in season
  # variables <- list('mask'=Crop_season[["mask"]],
  #                   'latlon' = latlon,
  #                   'days'=Crop_season[["dayStartEnd"]],
  #                   'Precip'=Precip,
  #                   'SMoist'=SMoist,
  #                   'MaizeSea' = Crop_season[["maize"]],
  #                   'RiceSea' = Crop_season[["rice"]],
  #                   'SoyaSea' = Crop_season[["soya"]],
  #                   'WheatSea' = Crop_season[["wheat"]])
  
  # variables needed ####
  ID <- X
  July15 <- 195; Jan15 <- 14; Feb28 <- 58
  PrecipPt <- varis[["Precip"]][ID,]
  latlonPt <- varis[["latlon"]][ID,]
  SmPt <- varis[["SMoist"]][ID,]
  
  if (crop == 1){
    cropPt <- varis[["MaizeSea"]][ID,]
    maskPt <- varis[["mask"]]$MaizeDate[ID]
  } else if (crop == 2){
    cropPt <- varis[["RiceSea"]][ID,]
    maskPt <- varis[["mask"]]$RiceDate[ID]
  } else if (crop == 3){
    cropPt <- varis[["SoyaSea"]][ID,]
    maskPt <- varis[["mask"]]$SoyaDate[ID]
  } else if (crop == 4){
    cropPt <- varis[["WheatSea"]][ID,]
    maskPt <- varis[["mask"]]$WheatDate[ID]
  } else if (crop == 5){
    cropPt <- varis[["WWheatSea"]][ID,]
    maskPt <- varis[["mask"]]$WWheatDate[ID]
  } else {
    print('Unreconized crop type or no crop type given. Please specify the crop type.')
  }
  
  # Season
  if (season == 1){
    startPt <- cropPt$plant.start
    endPt <- cropPt$plant.end
  } else if (season == 2){
    if (is.na(cropPt$plant)){
      startPt <- 0
    } else if (cropPt$plant == 0){
      startPt <- (cropPt$plant.start + cropPt$plant.end)/2
    } else {startPt <- cropPt$plant}
    if (is.na(cropPt$harvest)){
      endPt <- 0
    } else if (cropPt$harvest == 0){
      endPt <- (cropPt$harvest.start + cropPt$harvest.end)/2
    } else {endPt <- cropPt$harvest}
  } else if (season == 3){
    startPt <- cropPt$harvest.start
    endPt <- cropPt$harvest.end
  } else if (season == 4) {
    startPt <- cropPt$plant.start
    endPt <- cropPt$harvest.end
  } else { 
    print("No season given")
    startPt <- 0
    endPt <-1}
  
  # browser()
  
  # calculation ####
  if (maskPt == 1){
    # print(ID)
    # The data point is cropland
    Agro <- array(0, dim=length(varis[["days"]]$Year))
    
    for (i in 1:length(varis[["days"]]$Year)){
      # print(i)
      # Determine if there is a leap day
      if(diy(varis[["days"]]$Year[i]) == 365){leap <- 0} else {leap <- 1}
      if(diy(varis[["days"]]$Year[i]-1) == 365){leapb <- 0} else {leapb <- 1}
      if(diy(varis[["days"]]$Year[i]+1) == 365){leapf <- 0} else {leapf <- 1}
      
      # Start of season
      if (i == 1 & startPt < endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc1'))
        }
      } else if (i == 1 & startPt > endPt){
        start <- varis[["days"]]$first_day[i]
      } else if (i > 1 & startPt < endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc2'))
        }
      } else if (i > 1 & startPt > endPt){
        if (startPt < Feb28){
          start <- varis[["days"]]$first_day[i-1] + startPt - 1
        } else if (startPt > Feb28){
          start <- varis[["days"]]$first_day[i-1] + leap + startPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating starting dates. loc3'))
        }
      } else {
        print(paste('Error at point ID ', ID, ' when calculating starting dates. loc4'))
        break
      } # End testing starting date
      
      # End of the season
      if (i < length(varis[["days"]]$Year) & startPt < endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc1'))
        }
      } else if (i < length(varis[["days"]]$Year) & startPt > endPt) {
        if (endPt < Feb28){ #Assuming theat planting date is from the previous year
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc2'))
        }
      } else if (i == length(varis[["days"]]$Year) & startPt < endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc3'))
        }
      } else if (i == length(varis[["days"]]$Year) & startPt > endPt){
        if (endPt < Feb28){
          end <- varis[["days"]]$first_day[i] + endPt - 1
        } else if (endPt > Feb28){
          end <- varis[["days"]]$first_day[i] + leap + endPt - 1
        } else {
          print(paste('Error at point ID ', ID, ' when calculating ending dates for GDD. loc3'))
        }
      } else {
        print(paste('Error at point ID ', ID, ' when calculating ending dates. loc4', yr[i]))
      }

      # Calculation of daily soil moisture
      Psum <- 0
      for (j in seq(start, end, by= 1)){
        # print(j)
        m <- which(colnames(SmPt) == j)[1]
        if (SmPt[m] >= 0.10 & SmPt[m] <= 0.70){
          if (PrecipPt[m] <= 1 & PrecipPt[m-1] <= 5){
            Psum = Psum + 1
          } else {}
        } else {
          # do nothing
        }
      } # End j loop of days
      
      if (type == 1){
        Agro[i] <- Psum
      } else if (type == 2){
        Agro[i] <- Psum / length(seq(start, end, by= 1))
      } else {
        print("No type given.")
      }
      
    } # End i loop of years
    
  } else if (maskPt == 0){
    # The data point is non-cropland or ocean
    Agro <- array(NA, dim=length(varis[["days"]]$Year))
  } else {
    print(paste('error at point ID ', ID, ' when calculating the mask'))
  }
  Agro
  # rm(ID, varis, July15, Jan15, Feb28, PrecipPt, latlonPt, SmPt, cropPt, maskPt, startPt, endPt, i, j , m, Psum, Agro)
}
 
Tb <- Sys.time()
C <- Tb - Ta
print(paste('Functions finished loading at: ', Tb, '. Time elapsed: ', C, sep=''))
# PART I  -- Mask and Season ###################################################
#     This section will load in the mask and the planting/harvest dates and
# convert them into a list. 
#
# References: Monfreda et al. (2008), Sacks et al. (2010)

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
RowID <- rowid_to_column(latlon)

Crop_season <- list()
crop <- c('maize', 'rice','soya', 'wheat') # , 'wwheat')
crop2 <- c('Maize.crop', 'Rice.crop','Soybeans.crop', 'Wheat.crop', 
          'Wheat.Winter.crop')
season <- c('plant', 'plant.start', 'plant.end','plant.range', 
            'harvest', 'harvest.start','harvest.end','harvest.range')

testing = FALSE #Will auto change

rm(latID, lonID, latlonID)

# 1.2 Mask read in ##########
loc <- 'EarthStat/'
file <- 'EarthStat_Sacks_Mask.csv'
dat <- read_csv(paste(fileloc1, loc, file, sep = ''), col_names = TRUE,
                cols(.default = col_double()))
name <- 'mask'
Crop_season[[name]] <- dat

dat <- read_csv(paste(fileloc2, 'Days_from_1-1-1981.csv', sep=''),
                    col_names = TRUE) 
dat <- dat[time,]
name <- 'dayStartEnd'
Crop_season[[name]] <- dat

rm(loc, file, dat, name)

# 1.3 Season read in ##########
loc <- 'Sacks2010/'
for (i in 1:length(crop2)){
  dat <- read_csv(paste(fileloc1, loc, 'Growth_Season_', crop2[i], '.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double()))
  name <- crop2[i]
  Crop_season[[name]] <- dat
}

T1 <- Sys.time()
C <- T1 - Tb
print(paste('Part 1 finished at: ', T1, '. Time elapsed: ', C, sep=''))
rm(loc, i, dat, name, Tb)

### Testing points #
# testing <- TRUE
# testingPts <- read_csv(paste(fileloc1, 'EarthStat/', 'TestingPoints.csv', sep = ''),
#                        col_names = TRUE,)
# testingPts$point <- testingPts$point - 1
# latlon <- latlon[testingPts$point,]
# RowID <- rowid_to_column(testingPts)
# Crop_season[["mask"]] <- Crop_season[["mask"]][testingPts$point,]
# Crop_season[["maize"]] <- Crop_season[["maize"]][testingPts$point,]
# Crop_season[["rice"]] <- Crop_season[["rice"]][testingPts$point,]
# Crop_season[["soya"]] <- Crop_season[["soya"]][testingPts$point,]
# Crop_season[["wheat"]] <- Crop_season[["wheat"]][testingPts$point,]
# Crop_season[["winterWheat"]] <- Crop_season[["winterWheat"]][testingPts$point,]
###



# PART II  -- Temperature Agroclimate ##########################################
#     This section will load in the temperature (max and min - deg C) variables.
# It will then calculate the related temperature dependent Agroclimate indices.
# After the indices have been calculated a file for each index will be written.
# Before the next section it will remove the variables not needed. 
#
# References: 

# 2.1 Variables needed ##########
loc <- '202012/'
Tmax <- latlon; Tmin <- Tmax
AgroInd <- list()

# 2.2 Load in the temperature variables ##########

for (i in yr){
  dat <- read_csv(paste(fileloc2, loc, 'POWER_World_Daily_T2M_MAX_',i,'.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double())) %>%
    dplyr::select(c(-lat,-lon,-latID,-lonID))
  if (testing == TRUE){
    dat <- dat[testingPts$point,]
  } else {dat <- dat}
  Tmax <- cbind(Tmax, dat)
  
  dat <- read_csv(paste(fileloc2, loc, 'POWER_World_Daily_T2M_MIN_',i,'.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double())) %>%
    dplyr::select(c(-lat,-lon,-latID,-lonID))
  if (testing == TRUE){
    dat <- dat[testingPts$point,]
  } else {dat <- dat}
  Tmin <- cbind(Tmin, dat)
}

T2 <- Sys.time()
C <- T2 - T1
print(paste('Finished reading in temperature variables at: ', T2, '. Time elapsed: ', C, sep=''))
rm(T1, dat, i)

# 2.3 Last Spring Frost ##########
T2a <- Sys.time()
print(paste('Last Spring Frost calculation started at: ', T2a, sep = ''))

var <- 'LastSpringFrost'
variables <- list('mask'=Crop_season[["mask"]]$CroplandDate,
                  'latlon' = latlon,
                  'days'=Crop_season[["dayStartEnd"]],
                  'Tmin'=Tmin)
dat <- parallel::mclapply(X = RowID$rowid, FUN = Last_Spring_Frost, 
                          varis = variables, mc.cores = core) %>%
  unlist() %>%
  matrix(ncol = length(yr), byrow = TRUE)
colnames(dat) <- yr
dat <- cbind(latlon, dat)
write.csv(dat, file = paste(fileloc4, var, '_', part, '.csv', sep = ''), 
          row.names = FALSE)
name <- var
AgroInd[[name]] <- dat

T2b <- Sys.time()
C <- T2b - T2a
print(paste('Last Spring Frost calculation compleded and written at: ',
            T2b, '. Time elapsed: ', C, sep=''))

# 2.4 First Fall Frost ##########
T2a <- Sys.time()
print(paste('First Fall Frost calculation started at: ', T2a, sep = ''))

var <- 'FirstFallFrost'
dat <- parallel::mclapply(X = RowID$rowid, FUN = First_Fall_Frost, 
                          varis = variables, mc.cores = core) %>%
  unlist() %>%
  matrix(ncol = length(yr), byrow = TRUE)
colnames(dat) <- yr
dat <- cbind(latlon, dat)
write.csv(dat, file = paste(fileloc4, var, '_', part, '.csv', sep = ''), 
          row.names = FALSE)
name <- var
AgroInd[[name]] <- dat

T2b <- Sys.time()
C <- T2b - T2a
print(paste('First Fall Frost calculation compleded and written at: ',
            T2b, '. Time elapsed: ', C, sep=''))

# 2.5 Climatological Growing Season ##########
T2a <- Sys.time()
print(paste('Climatological Growing Season calculation started at: ', T2a, sep = ''))

var <- 'ClimGrowingSeason'
variables <- list('mask'=Crop_season[["mask"]]$CroplandDate,
                  'latlon' = latlon,
                  'days'=Crop_season[["dayStartEnd"]],
                  'Tmin'=Tmin,
                  'LastSpringFrost'= AgroInd[["LastSpringFrost"]],
                  'FirstFallFrost' = AgroInd[["FirstFallFrost"]])
dat <- parallel::mclapply(X = RowID$rowid, FUN = Clm_Growing_Season_Length, 
                          varis = variables, mc.cores = core) %>%
  unlist() %>%
  matrix(ncol = length(yr), byrow = TRUE)
colnames(dat) <- yr
dat <- cbind(latlon, dat)
write.csv(dat, file = paste(fileloc4, var, '_', part, '.csv', sep = ''), 
          row.names = FALSE)
name <- var
AgroInd[[name]] <- dat

T2b <- Sys.time()
C <- T2b - T2a
print(paste('Climatological Growing Season calculation compleded and written at: ',
            T2b, '. Time elapsed: ', C, sep=''))

# 2.6 Accumulated Frost Days ##########
T2a <- Sys.time()
print(paste('Accumulated Frost Days calculation started at: ', T2a, sep = ''))

var <- 'AccFrostDays'
dat <- parallel::mclapply(X = RowID$rowid, FUN = Accumulated_Frost_Days, 
                          varis = variables, mc.cores = core) %>%
  unlist() %>%
  matrix(ncol = length(yr), byrow = TRUE)
colnames(dat) <- yr
dat <- cbind(latlon, dat)
write.csv(dat, file = paste(fileloc4, var, '_', part, '.csv', sep = ''), 
          row.names = FALSE)
name <- var
AgroInd[[name]] <- dat

T2b <- Sys.time()
C <- T2b - T2a
print(paste('Accumulated Frost Days calculation compleded and written at: ',
            T2b, '. Time elapsed: ', C, sep=''))

# 2.7 Start of Field Operations ##########
T2a <- Sys.time()
print(paste('Start of Field Operations calculation started at: ', T2a, sep = ''))

var <- 'StartFieldOp'
variables <- list('mask'=Crop_season[["mask"]]$CroplandDate,
                  'latlon' = latlon,
                  'days'=Crop_season[["dayStartEnd"]],
                  'Tmin'=Tmin,
                  'Tmax'=Tmax,
                  'LastSpringFrost'= AgroInd[["LastSpringFrost"]],
                  'FirstFallFrost' = AgroInd[["FirstFallFrost"]])
dat <- parallel::mclapply(X = RowID$rowid, FUN = Start_Field_Opp, 
                          varis = variables, mc.cores = core) %>%
  unlist() %>%
  matrix(ncol = length(yr), byrow = TRUE)
colnames(dat) <- yr
dat <- cbind(latlon, dat)
write.csv(dat, file = paste(fileloc4, var, '_', part, '.csv', sep = ''), 
          row.names = FALSE)
name <- var
AgroInd[[name]] <- dat

T2b <- Sys.time()
C <- T2b - T2a
print(paste('Start of field operations calculation compleded and written at: ',
            T2b, '. Time elapsed: ', C, sep=''))

# 2.8 Growing Degree Days ##########
T2a <- Sys.time()
print(paste('Growing Degree Days calculation started at: ', T2a, sep = ''))

var <- 'GrowDegDay'
variables <- list('mask'=Crop_season[["mask"]],
                  'latlon' = latlon,
                  'days'=Crop_season[["dayStartEnd"]],
                  'Tmin'=Tmin,
                  'Tmax'=Tmax,
                  'MaizeSea' = Crop_season[[crop2[1]]],
                  'RiceSea' = Crop_season[[crop2[2]]],
                  'SoyaSea' = Crop_season[[crop2[3]]],
                  'WheatSea' = Crop_season[[crop2[4]]],
                  'WWheatSea' = Crop_season[[crop2[5]]])
for (i in 1:length(crop)){
  dat <- parallel::mclapply(X = RowID$rowid, FUN = Growing_Deg_Day, 
                            varis = variables, crop = i, season = 4, 
                            mc.cores = core) %>%
    unlist() %>%
    matrix(ncol = length(yr), byrow = TRUE)
  colnames(dat) <- yr
  dat <- cbind(latlon, dat)
  write.csv(dat, file = paste(fileloc4, var,'_',crop[i], '_', part, '.csv', sep = ''), 
            row.names = FALSE)
  name <- paste(var, '_' ,crop[i], sep = '')
  AgroInd[[name]] <- dat
}

T2b <- Sys.time()
C <- T2b - T2a
print(paste('Growing Degree Day calculation compleded and written at: ',
            T2b, '. Time elapsed: ', C, sep=''))

# 2.9 Heat Stress Days ##########
T2a <- Sys.time()
print(paste('Heat Stress Day calculation started at: ', T2a, sep = ''))

var <- 'HeatStress'
variables <- list('mask'=Crop_season[["mask"]],
                  'latlon' = latlon,
                  'days'=Crop_season[["dayStartEnd"]],
                  'Tmin'=Tmin,
                  'Tmax'=Tmax,
                  'MaizeSea' = Crop_season[[crop2[1]]],
                  'RiceSea' = Crop_season[[crop2[2]]],
                  'SoyaSea' = Crop_season[[crop2[3]]],
                  'WheatSea' = Crop_season[[crop2[4]]],
                  'WWheatSea' = Crop_season[[crop2[5]]])
for (i in 1:length(crop)){
  dat <- parallel::mclapply(X = RowID$rowid, FUN = Heat_Stress_Day, 
                            varis = variables, crop = i, season = 4,
                            mc.cores = core) %>%
    unlist() %>%
    matrix(ncol = length(yr), byrow = TRUE)
  colnames(dat) <- yr
  dat <- cbind(latlon, dat)
  write.csv(dat, file = paste(fileloc4, var,'_',crop[i], '_', part, '.csv', sep = ''), 
            row.names = FALSE)
  name <- paste(var, '_', crop[i], sep = '')
  AgroInd[[name]] <- dat
}

T2b <- Sys.time()
C <- T2b - T2a
print(paste('Heat Stress Day calculation compleded and written at: ',
            T2b, '. Time elapsed: ', C, sep=''))
# 2.10 Final ##########

T2b <- Sys.time()
C <- T2b - T2
print(paste('Part 2 finished at: ', T2b, '. Time ellapsed: ',C, sep=''))
rm(dat, Tmin, Tmax)

# PART III --  Precipitation Agroclimate #######################################
#     This section will load in the precipitation (mm) variables.
# It will then calculate the related temperature dependent Agroclimate indices.
# After the indices have been calculated a file for each index will be written.
# Before the next section it will remove the variables not needed. 
#
# References: 

# 3.1 Variables needed ##########
loc <- '202012/'
Precip <- latlon

# 3.2 Load in the precipitation variable ##########
for (i in yr){
  dat <- read_csv(paste(fileloc2, loc, 'POWER_World_Daily_PRECTOT_',i,'.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double())) %>%
    dplyr::select(c(-lat,-lon,-latID,-lonID))
  if (testing == TRUE){
    dat <- dat[testingPts$point,]
  } else {dat <- dat}
  Precip <- cbind(Precip, dat)
}

T3 <- Sys.time()
C <- T3 - T2b
print(paste('Finished reading in precipitation variables at: ', T3, '. Time elapsed: ', C, sep=''))
rm(T2, dat, i)

# 3.3 Total season precipitation ##########
T3a <- Sys.time()
print(paste('Growing Season precipitation calculation started at: ', T3a, sep = ''))

var <- 'TotPrecip'
variables <- list('mask'=Crop_season[["mask"]],
                  'latlon' = latlon,
                  'days'=Crop_season[["dayStartEnd"]],
                  'Precip'=Precip,
                  'MaizeSea' = Crop_season[[crop2[1]]],
                  'RiceSea' = Crop_season[[crop2[2]]],
                  'SoyaSea' = Crop_season[[crop2[3]]],
                  'WheatSea' = Crop_season[[crop2[4]]],
                  'WWheatSea' = Crop_season[[crop2[5]]])
for (i in 1:length(crop)){
  dat <- parallel::mclapply(X = RowID$rowid, FUN = Precip_Dry_Day, 
                            varis = variables, crop = i, type = 1, season = 4,  
                            mc.cores = core) %>%
    unlist() %>%
    matrix(ncol = length(yr), byrow = TRUE)
  colnames(dat) <- yr
  dat <- cbind(latlon, dat)
  write.csv(dat, file = paste(fileloc4, var,'_',crop[i], '_', part, '.csv', sep = ''), 
            row.names = FALSE)
  name <- paste(var, '_' ,crop[i], sep = '')
  AgroInd[[name]] <- dat
}

T3b <- Sys.time()
C <- T3b - T3a 
print(paste('Growing season precipitation calculation compleded and written at: ',
            T3b, '. Time elapsed: ', C, sep=''))

# 3.4 Season Dry Days ##########
T3a <- Sys.time()
print(paste('Growing Season dry days calculation started at: ', T3a, sep = ''))

var <- 'DryDay'
variables <- list('mask'=Crop_season[["mask"]],
                  'latlon' = latlon,
                  'days'=Crop_season[["dayStartEnd"]],
                  'Precip'=Precip,
                  'MaizeSea' = Crop_season[[crop2[1]]],
                  'RiceSea' = Crop_season[[crop2[2]]],
                  'SoyaSea' = Crop_season[[crop2[3]]],
                  'WheatSea' = Crop_season[[crop2[4]]],
                  'WWheatSea' = Crop_season[[crop2[5]]])
for (i in 1:length(crop)){
  dat <- parallel::mclapply(X = RowID$rowid, FUN = Precip_Dry_Day, 
                            varis = variables, crop = i, type = 2, season = 4,  
                            mc.cores = core) %>%
    unlist() %>%
    matrix(ncol = length(yr), byrow = TRUE)
  colnames(dat) <- yr
  dat <- cbind(latlon, dat)
  write.csv(dat, file = paste(fileloc4, var,'_',crop[i], '_', part, '.csv', sep = ''), 
            row.names = FALSE)
  name <- paste(var, '_' ,crop[i], sep = '')
  AgroInd[[name]] <- dat
}

T3b <- Sys.time()
C <- T3b - T3a
print(paste('Growing season dry days calculation compleded and written at: ',
            T3b, '. Time elapsed: ', C, sep=''))
# 3.5 Final ##########

T3b <- Sys.time()
C <- T3b - T3
print(paste('Part 2 finished at: ', T3b, '. Time ellapsed: ',C, sep=''))
rm(dat)

# PART VI --  Soil Moisture Agroclimate ########################################
#     This section will load in the soil moisture (m3/m3) variable.
# It will then calculate the related temperature dependent Agroclimate indices.
# After the indices have been calculated a file for each index will be written.
# Before the next section it will remove the variables not needed.  
#
# References:
# 4.1 Variables needed ##########
loc <- '202012/'
SMoist <- latlon

# 4.2 Load in the soil moisture variable ##########
for (i in 1:length(yr)){
  dat <- read_csv(paste(fileloc3, loc, 'NN_SMsurf_',yr[i], '_GLEAM_v3.3a.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double())) %>%
    dplyr::select(c(-lat,-lon,-latID,-lonID))
  if (testing == TRUE){
    dat <- dat[testingPts$point,]
  } else {dat <- dat}
  SMoist <- cbind(Precip, dat)
}

T4 <- Sys.time()
C <- T4 - T3b
print(paste('Finished reading in soil moisture variables at: ', T4, '. Time elapsed: ', C, sep=''))
rm( dat, i)

# 4.3 Field Conditions for planting ##########
T4a <- Sys.time()
print(paste('Planting soil conditions calculation started at: ', T3a, sep = ''))

var <- 'SMPlanting'
variables <- list('mask'=Crop_season[["mask"]],
                  'latlon' = latlon,
                  'days'=Crop_season[["dayStartEnd"]],
                  'Precip'=Precip,
                  'SMoist'=SMoist,
                  'MaizeSea' = Crop_season[[crop2[1]]],
                  'RiceSea' = Crop_season[[crop2[2]]],
                  'SoyaSea' = Crop_season[[crop2[3]]],
                  'WheatSea' = Crop_season[[crop2[4]]],
                  'WWheatSea' = Crop_season[[crop2[5]]])
for (i in 1:length(crop)){
  print(i)
  dat <- parallel::mclapply(X = RowID$rowid, FUN = Moisture_Field_Cond, 
                            varis = variables, crop = i, season = 1, type = 1, 
                            mc.cores = core) %>%
    unlist() %>%
    matrix(ncol = length(yr), byrow = TRUE)
  colnames(dat) <- yr
  dat <- cbind(latlon, dat)
  write.csv(dat, file = paste(fileloc4, var,'_',crop[i], '_', part, '.csv', sep = ''), 
            row.names = FALSE)
  name <- paste(var, '_' ,crop[i], sep = '')
  AgroInd[[name]] <- dat
}

T4b <- Sys.time()
C <- T4b - T4a
print(paste('Planting soil conditions calculation compleded and written at: ',
            T4b, '. Time elapsed: ', C, sep=''))

# 4.4 Field Conditions for mid season ##########
T4a <- Sys.time()
print(paste('Mid-Season soil conditions calculation started at: ', T4a, sep = ''))

var <- 'SMMidSeason'
for (i in 1:length(crop)){
  print(i)
  dat <- parallel::mclapply(X = RowID$rowid, FUN = Moisture_Field_Cond, 
                            varis = variables, crop = i, season = 2, type = 1,
                            mc.cores = core) %>%
    unlist() %>%
    matrix(ncol = length(yr), byrow = TRUE)
  colnames(dat) <- yr
  dat <- cbind(latlon, dat)
  write.csv(dat, file = paste(fileloc4, var,'_',crop[i], '_', part, '.csv', sep = ''), 
            row.names = FALSE)
  name <- paste(var, '_' ,crop[i], sep = '')
  AgroInd[[name]] <- dat
}

T4b <- Sys.time()
C <- T4b - T4a
print(paste('Mid-Season soil conditions calculation compleded and written at: ',
            T4b, '. Time elapsed: ', C, sep=''))
# 4.5 Field Conditions for harvest ##########
T4a <- Sys.time()
print(paste('Harvesting soil conditions calculation started at: ', T3a, sep = ''))

var <- 'SMHarvestSeason'
for (i in 1:length(crop)){
  print(i)
  dat <- parallel::mclapply(X = RowID$rowid, FUN = Moisture_Field_Cond, 
                            varis = variables, crop = i, season = 3, type = 1, 
                            mc.cores = core) %>%
    unlist() %>%
    matrix(ncol = length(yr), byrow = TRUE)
  colnames(dat) <- yr
  dat <- cbind(latlon, dat)
  write.csv(dat, file = paste(fileloc4, var,'_',crop[i], '_', part, '.csv', sep = ''), 
            row.names = FALSE)
  name <- paste(var, '_' ,crop[i], sep = '')
  AgroInd[[name]] <- dat
}

T4b <- Sys.time()
C <- T4b - T4a
print(paste('Harvesting soil conditions calculation compleded and written at: ',
            T4b, '. Time elapsed: ', C, sep=''))

# 4.6 Final ##########
T4b <- Sys.time()
C <- T4b - Ta
print(paste('Program compleded and written at: ',
            T4b, '. Time elapsed: ', C, sep=''))

#END

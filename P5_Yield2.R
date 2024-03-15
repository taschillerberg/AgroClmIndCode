# P5_Yield.R
# This R program will open convert nc yield files to a csv file. It will also 
# detrend the yield using four different methods. The methods will be compared
# using the coefficients and residuals. The standarized yield will be calculated
# and plotted for the specified method. This section (1.5) could be put into a 
# loop so that all methods would be plotted at once. The final section will 
# calculate the quartiles, value for each quartile, and the categoral 
# (failure v. nonfailure) for each grid cell.
#
#
# T. A. Schillerberg
#               Jun. 2020
#      Updated: May. 2021

# Local Computer
# setwd("")
# fileloc1 <- '/Research/AgroclimaticConditions/Data/'
# loc1 <- 'Iizumi2020/gdhy_v1.2_v1.3_20190128/'
# loc2 <- 'Global_AgroInd/Agro12021_mu/'
# loc3 <- 'Global_AgroInd/Agro12021_yield/'

# Hopper
fileloc1 <- '~/AgroClmInd/data/'
loc1 <- 'Iizumi2020/'
loc2 <- 'AgroclimateInd/Agro12021_mu/'
loc3 <- 'AgroclimateInd/Agro12021_yield/'

options(show.error.locations = TRUE)

# Variables to change ##########################################################
yieldExist <- TRUE
core <- 19   # How many core will be ran? (1 or 19)
hpc <- TRUE # Is this script running on a hpc?
#     Takes 1.5 hrs on hopper when part 1.3 is just preprocessing variables.
cat <- FALSE # Should the categorical yield failure/non-fail be calculated?
type <- 'M' #'OLS' 'M' "MM' 'TS' for when calculating categorical yield

# Libraries ####################################################################
library(stats)
library(MASS)
library(zyp)
library(tidyverse)
library(ncdf4)

# Functions ####################################################################
yd_detrend_OLS <- function(X, varis){
  #     This function will detrend the crop yield using the Ordinary Least Squares 
  # Method (OLS). The OLS method is very sensitive to outliers at the beginning and 
  # ends of the time series (Finger, 2013; Conradt et al., 2014). If less than 30% 
  # of the data is missing, the data will be detrended. If greater than 30% of the 
  # data is missing the function will return a sequence of NAs. NAs will be added 
  # back into the residuals. The function will return the residuals.
  
  # Packages required #####
  require(stats)
  
  # Variables needed #####
  dat <- varis[['datYd']][X,5:ncol(varis[['datYd']])]
  yr <- varis[['yr']]
  count <- 0
  
  # Calculation #####
  if((sum(is.na(dat))/length(dat)) > 0.3){ #NA Remove
    res <- array(data = NA, dim=dim(dat)) %>% 
      as_tibble()
    coe <- c(NA, NA)
  } else { 
    res <- array(data = NA, dim=dim(dat)) %>% 
      as_tibble()
    datY <- cbind(t(dat), yr) %>%
      na.omit()
    
    # Detrend the Y values
    fun <- lm(datY[,1]~datY[,2])
    resi <- fun$residuals
    coe <- fun$coefficients
    
    # Fill the NA
    for (i in 1:length(dat)){
      if(is.na(dat[i]) == FALSE){
        res[i] <- resi[i + count]
      } else if (is.na(dat[i]) == TRUE){
        count <- count + 1
        res[i] <- NA
      }
    } # End NA fill
  } # End NA remove
  
  sdRes <- res / sd(res, na.rm=TRUE)
  
  OLS <- cbind(t(coe), res, sdRes)
  colnames(OLS) <- c('Intercept', 'Slope', yr, paste0('Sd', yr))
  return(OLS)
  rm(dat, res, datY, fun, resi, coef, sdRes)
}

yd_detrend_M <- function(X, varis){
  #     This function will detrend the crop yield using the M-estimator. The M-estimator
  # is robust against vertical outliers but not horizontal outliers (Finger 2013). If 
  # less than 30%   # of the data is missing, the data will be detrended. If greater 
  # than 30% of the data is missing the function will return a sequence of NAs. NAs will
  # be added back into the residuals. The function will return the residuals.
  
  # Packages required #####
  require(MASS)
  
  # Variables needed #####
  dat <- varis[['datYd']][X,5:ncol(varis[['datYd']])]
  yr <- varis[['yr']]
  count <- 0
  
  # Calculation #####
  if((sum(is.na(dat))/length(dat)) > 0.3){ #NA Remove
    res <- array(data = NA, dim=dim(dat)) %>% 
      as_tibble()
    coe <- c(NA, NA)
  } else { 
    res <- array(data = NA, dim=dim(dat)) %>% 
      as_tibble()
    datY <- cbind(t(dat), yr) %>%
      na.omit()
    
    # Detrend the Y values
    fun <- MASS::rlm(datY[,1]~datY[,2], method="M")
    resi <- fun$residuals
    coe <- fun$coefficients
    
    # Fill the NA
    for (i in 1:length(dat)){
      if(is.na(dat[i]) == FALSE){
        res[i] <- resi[i + count]
      } else if (is.na(dat[i]) == TRUE){
        count <- count + 1
        res[i] <- NA
      }
    } # End NA fill
  } # End NA remove
  
  sdRes <- res / sd(res, na.rm=TRUE)
  
  M <- cbind(t(coe), res, sdRes)
  colnames(M) <- c('Intercept', 'Slope', yr, paste0('Sd', yr))
  return(M)
  rm(dat, res, datY, fun, resi, coef, sdRes)
}

yd_detrend_MM <- function(X, varis){
  #     This function will detrend the crop yield using the M-estimator. The MM-estimator
  # resist against outler contamination of up to 50% (Finger, 2013). If less than 30% 
  # of the data is missing, the data will be detrended. If greater than 30% of the 
  # data is missing the function will return a sequence of NAs. NAs will be added 
  # back into the residuals. The function will return the residuals.
  
  # Packages required #####
  require(MASS)
  
  # Variables needed #####
  dat <- varis[['datYd']][X,5:ncol(varis[['datYd']])]
  yr <- varis[['yr']]
  count <- 0
  
  # Calculation #####
  if((sum(is.na(dat))/length(dat)) > 0.3){ #NA Remove
    res <- array(data = NA, dim=dim(dat)) %>% 
      as_tibble()
    coe <- c(NA, NA)
  } else { 
    res <- array(data = NA, dim=dim(dat)) %>% 
      as_tibble()
    datY <- cbind(t(dat), yr) %>%
      na.omit()
    
    # Detrend the Y values
    fun <- MASS::rlm(datY[,1]~datY[,2], method="MM")
    resi <- fun$residuals
    coe <- fun$coefficients
    
    # Fill the NA
    for (i in 1:length(dat)){
      if(is.na(dat[i]) == FALSE){
        res[i] <- resi[i + count]
      } else if (is.na(dat[i]) == TRUE){
        count <- count + 1
        res[i] <- NA
      }
    } # End NA fill
  } # End NA remove
  
  sdRes <- res / sd(res, na.rm=TRUE)
  
  MM <- cbind(t(coe), res, sdRes)
  colnames(MM) <- c('Intercept', 'Slope', yr, paste0('Sd', yr))
  return(MM)
  rm(dat, res, datY, fun, resi, coef, sdRes)
}

yd_detrend_TS <- function(X, varis){
  #     This function will detrend the crop yield using the Theil-Sen (TS) estimator. 
  # The TS-estimator is a nonparametric regression technique. Efficient if the 
  # residuals follow outlier-free and homoscedastic distributions, handles 
  # heteroscedastic error terms better (Finger 2013). is robust against vertical 
  # outliers but not horizontal outliers. If less than 30%of the data is missing,
  # the data will be detrended. If greater than 30% of the  data is missing the 
  # function will return a sequence of NAs. NAs will be added back into the residuals.
  # The function will return the residuals.
  
  # Packages required #####
  require(zyp)
  
  # Variables needed #####
  dat <- varis[['datYd']][X,5:ncol(varis[['datYd']])]
  yr <- varis[['yr']]
  count <- 0
  
  # Calculation #####
  if((sum(is.na(dat))/length(dat)) > 0.3){ #NA Remove
    res <- array(data = NA, dim=dim(dat)) %>% 
      as_tibble()
    coe <- c(NA, NA)
  } else { 
    res <- array(data = NA, dim=dim(dat)) %>% 
      as_tibble()
    datY <- cbind(t(dat), yr) %>%
      na.omit()
    
    # Detrend the Y values
    # fun <- zyp::zyp.sen(V1 ~ yr, as.data.frame(datY))
    fun <- mblm::mblm(V1 ~ yr, as.data.frame(datY))
    resi <- fun$residuals
    coe <- fun$coefficients
    
    # Fill the NA
    for (i in 1:length(dat)){
      if(is.na(dat[i]) == FALSE){
        res[i] <- resi[i + count]
      } else if (is.na(dat[i]) == TRUE){
        count <- count + 1
        res[i] <- NA
      }
    } # End NA fill
  } # End NA remove
  
  sdRes <- res / sd(res, na.rm=TRUE)
  
  TS <- cbind(t(coe), res, sdRes)
  colnames(TS) <- c('Intercept', 'Slope', yr, paste0('Sd', yr))
  return(TS)
  rm(dat, res, datY, fun, resi, coef, sdRes)
}

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

yd_quartile <- function(datX){
  #     This function will find the quartile of the (yield) data and return 
  # which quartile each value is in.
  
  q <- quantile(datX, na.rm = TRUE)
  datX2 <- datX
  datX2[datX <= q[2]] <- 1
  datX2[datX >= q[4]] <- 4
  datX2[datX <= q[4] & datX >= q[3]] <- 3
  datX2[datX <= q[3] & datX >= q[2]] <- 2

  return(datX2)
}

yd_decile <- function(datX){
  #     This function will find the lower and upper decile of the (yield) data.
  # The function will return is the yield is in the lower decile, upper decile,
  # or the middle deciles below the mean (2) or above the mean (3)
  
  q <- quantile(datX, probs=c(0.1, 0.5, 0.9), na.rm = TRUE)
  datX2 <- datX
  datX2[datX <= q[2]] <- 1
  datX2[datX >= q[3]] <- 4
  datX2[datX <= q[3] & datX >= q[3]] <- 3
  datX2[datX <= q[3] & datX >= q[1]] <- 2
  
  return(datX2)
}

yd_nickle <- function(datX){
  #     This function will find the lower and upper decile of the (yield) data.
  # The function will return is the yield is in the lower decile, upper decile,
  # or the middle deciles below the mean (2) or above the mean (3)
  
  q <- quantile(datX, probs=c(0.05, 0.5, 0.95), na.rm = TRUE)
  datX2 <- datX
  datX2[datX <= q[2]] <- 1
  datX2[datX >= q[3]] <- 4
  datX2[datX <= q[3] & datX >= q[3]] <- 3
  datX2[datX <= q[3] & datX >= q[1]] <- 2
  
  return(datX2)
}

yd_sigmaOne <- function(datX){
  #     This function will find the lower and upper decile of the (yield) data.
  # The function will return is the yield is in the lower decile, upper decile,
  # or the middle deciles below the mean (2) or above the mean (3)
  
  sd <- sd(datX, na.rm = TRUE)
  mu <- mean(datX, na.rm = TRUE)
  q <- c((mu - sd), mu, (mu + sd))
  
  datX2 <- datX
  datX2[datX <= q[2]] <- 1
  datX2[datX >= q[3]] <- 4
  datX2[datX <= q[3] & datX >= q[3]] <- 3
  datX2[datX <= q[3] & datX >= q[1]] <- 2
  
  return(datX2)
}

yd_quartile_level <- function(datX){
  #     This function will find the quartile of the (yield) data and return 
  # which quartile each value is in.
  
  q <- quantile(datX, na.rm = TRUE)
  # datX2 <- datX
  # datX2[datX <= q[2]] <- 1
  # datX2[datX >= q[4]] <- 4
  # datX2[datX <= q[4] & datX >= q[3]] <- 3
  # datX2[datX <= q[3] & datX >= q[2]] <- 2
  
  return(q)
}

category <- function(x){
  #     This function will convert an element from a quartile (1,2,3,4) format to 
  # a categorical format of failure (1) and non-failure (2,3,4). If the value is not
  # in the quartile format the value will be treated as a NA. 
  
  y <- x
  for (i in 1:length(x)){
    if (is.na(x[i])){
      y[i] <- NA
    } else if (x[i] == 1){
      y[i] <- 'failure'
    } else if (x[i] != 1){
      y[i] <- 'non_failure'
    } else {
      print(x)
      y[i] <- NA
    }
  }
  return(y)
}

# Part I -- Yield Pre-Processing ###############################################
#      This section will open the yield (nc) and process it into one array for 
# crop following the data structure of NASA POWER and GLEAM soil moisture 
# variables. After the files have been opened and converted a csv format of the 
# crop yield files will be written to preserve time in future calculations. The 
# yield will be detrended, seperated into quartiles, and converted into 
# categorical failure and non-failure crop events. 
#
# References: Iizumi and Sakai (2020)

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
yr <- 1982:2016 

crop2 <- c('maize_major','rice_major','soybean','wheat')
crop <- c('maize', 'rice','soya', 'wheat')

YieldList <- list()
nclon <- c(seq(0.25, 179.75, by = 0.5), seq(-179.75, -0.25, by = 0.5))

rm(latID, lonID, latlonID)
# 1.2 Open regions ##########
regions <- read_csv(paste0(fileloc1,'EarthStat/', 'BreadbasketRegions.csv'), 
                    col_names = TRUE) 

# 1.3 Open Yield files ##########
T1a <- Sys.time()
print(paste0('Started opening Yield files at: ', T1a))

if (yieldExist == FALSE){
  for(i in 1:length(crop2)){
    dat <- latlon %>%
      cbind(array(dat = -999, dim = c(dim(latlon)[1], length(yr)))) %>%
      as_tibble()
    for (j in 1:length(yr)){
      ncData <- ncdf4::nc_open(paste0(
        fileloc1, loc1, crop2[i], '/', 'yield_', yr[j],'.nc4'))
      # print(ncData)
      #     Get the variables
      ncData_yield <- ncdf4::ncvar_get(ncData, 'var')
      ncData_lat <- ncdf4::ncvar_get(ncData, 'lat')
      ncData_lon <- ncdf4::ncvar_get(ncData, 'lon') #lon[lon > 180] <- lon[]-180
      #     Change the missing data to NA
      fillvalue <- ncdf4::ncatt_get(ncData, 'var','_Fillvalue')
      ncData_yield[ncData_yield == fillvalue$value] <- -999
      ncdf4::nc_close(ncData)
      #     Testing plotting
      ncData_r <- raster::raster(t(ncData_yield), xmn=min(nclon), xmx=max(nclon),
                                 ymn=min(ncData_lat), ymx=max(ncData_lat),
                                 crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
      # ncData_r <- raster::flip(ncData_r, direction='y')
      # plot(ncData_r)
      
      ncData_r <- as_tibble(matrix(ncData_r, ncol = length(ncData_lon), byrow=TRUE),
                            .name_repair = 'minimal')
      colnames(ncData_r) <- as.character(nclon) 
      ncData_r <- dplyr::relocate(ncData_r, as.character(lon)) %>% 
        as.matrix()
      dat[,(4+j)] <- apply(X = latlon, MARGIN = 1, FUN = pt_latlon, datX = ncData_r)
    }
    colnames(dat) <- c(colnames(latlon), paste0('y', yr))
    dat[dat == -999] <- NA
    name <- crop[i]
    YieldList[[name]] <- dat
    write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop2[i], '.csv'), 
              row.names = FALSE)
  }
  rm(i, j, fillvalue, ncData, ncData_r, ncData_yield, 
     ncData_lat, ncData_lon, nclon, dat)
} else  {
  # (yieldExist == TRUE)  # Yield data has been opened and preprocessed 
  # Data needs to be opened
  for (i in 1:length(crop2)){
    dat <- read_csv(paste0(fileloc1, loc1, 'Yield_', crop2[i], '.csv'),
                  col_names = TRUE, cols(.default = col_double()))
    name <- crop[i]
    YieldList[[name]] <- dat
  }
  rm(i, dat, name)
}  

rm(yieldExist, crop2)
T1b <- Sys.time()
print(paste0('Finished loading yield files at: ', T1b,
             '. Time elapsed:', T1b-T1a))

# 1.4 Detrend ##########
#     Detrend the yield at each gridpoint using an Ordinary Least Squares (OLS), M-,
# MM-, and TS- estimators. The mean, SD, min, max, median absolute deviation for the 
# detrended (residuals) yield. 
T1b <- Sys.time()
print(paste0('Started Detrending yield files at: ', T1b))

if (hpc == TRUE) {
  for (i in 1:length(crop)){
    variables <- list('datYd' = YieldList[[crop[i]]],
                      'yr' = yr)
    dat <- parallel::mclapply(X = RowID$rowid, FUN = yd_detrend_OLS,
                              varis = variables, mc.cores = core) %>%
      unlist() %>%
      matrix(ncol = (length(yr)+2+length(yr)), byrow = TRUE)
    colnames(dat) <- c('Intercept', 'Slope', yr, paste0("Sd",yr))
    dat <- cbind(latlon, dat) %>% 
      as_tibble()
    name <- paste0(crop[i], '_detrend_OLS')
    YieldList[[name]] <- dat
    write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop[i], '_detrend_OLS', '.csv'),
              row.names = FALSE)
    
    # M-estimater
    dat <- parallel::mclapply(X = RowID$rowid, FUN = yd_detrend_M,
                              varis = variables, mc.cores = core) %>%
      unlist() %>%
      matrix(ncol = (length(yr)+2+length(yr)), byrow = TRUE)
    colnames(dat) <- c('Intercept', 'Slope', yr, paste0("Sd",yr))
    dat <- cbind(latlon, dat) %>% 
      as_tibble()
    name <- paste0(crop[i], '_detrend_M')
    YieldList[[name]] <- dat
    write.csv(dat, file = paste(fileloc1, loc1, 'Yield_', crop[i], '_detrend_M', '.csv', sep = ''),
              row.names = FALSE)
    
    # MM-estimater
    dat <- parallel::mclapply(X = RowID$rowid, FUN = yd_detrend_MM,
                              varis = variables, mc.cores = core) %>%
      unlist() %>%
      matrix(ncol = (length(yr)+2+length(yr)), byrow = TRUE)
    colnames(dat) <- c('Intercept', 'Slope', yr, paste0("Sd",yr))
    dat <- cbind(latlon, dat) %>% 
      as_tibble()
    name <- paste0(crop[i], '_detrend_MM')
    YieldList[[name]] <- dat
    write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop[i], '_detrend_MM', '.csv'),
              row.names = FALSE)
    
    # Theil-Sen estimater
    dat <- parallel::mclapply(X = RowID$rowid, FUN = yd_detrend_TS,
                              varis = variables, mc.cores = core) %>%
      unlist() %>%
      matrix(ncol = (length(yr)+2+length(yr)), byrow = TRUE)
    colnames(dat) <- c('Intercept', 'Slope', yr, paste0("Sd",yr))
    dat <- cbind(latlon, dat) %>% 
      as_tibble()
    name <- paste0(crop[i], '_detrend_TS')
    YieldList[[name]] <- dat
    write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop[i], '_detrend_TS', '.csv'),
              row.names = FALSE)
    
  }
} else {
  # open files
  for (i in 1:length(crop)){
    dat <- read_csv(paste0(fileloc1, loc1, 'Yield_', crop[i], '_detrend_OLS', '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
    name <- paste0(crop[i], '_detrend_OLS')
    YieldList[[name]] <- dat
    
    dat <- read_csv(paste0(fileloc1, loc1, 'Yield_', crop[i], '_detrend_M', '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
    name <- paste0(crop[i], '_detrend_M')
    YieldList[[name]] <- dat
    
    dat <- read_csv(paste0(fileloc1, loc1, 'Yield_', crop[i], '_detrend_MM', '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
    name <- paste0(crop[i], '_detrend_MM')
    YieldList[[name]] <- dat
    
    dat <- read_csv(paste0(fileloc1, loc1, 'Yield_', crop[i], '_detrend_TS', '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
    name <- paste0(crop[i], '_detrend_TS')
    YieldList[[name]] <- dat
  }
  rm(dat, name, i)
}

rm(dat, i, name, variables)
T1c <- Sys.time()
print(paste0('Finished detrending yield files at: ', T1c,
             '. Time elapsed:', T1c-T1b))

# 1.5 Testing Detrending methods ##########
#     Will make a table to compare the different methods used for detrending
T1b <- Sys.time()
print(paste0('Started testing detrended yield files at: ', T1b))

for (i in 1:length(crop)){
  dat <- matrix(ncol= 4, nrow= 10)
  colnames(dat) <- c("OLS","M","MM","TS")
  rownames(dat) <- c("CoefMin","CoefMean", "CoefMax","CoefSd","CoefMAD",
                     "ResiMin","ResiMean", "ResiMax","ResifSd","ResiMAD")
  
  # OLS
  dat[1,1] <- round(min(YieldList[[paste0(crop[i], '_detrend_OLS')]]$Slope, na.rm= TRUE),2)
  dat[2,1] <- round(mean(YieldList[[paste0(crop[i], '_detrend_OLS')]]$Slope, na.rm= TRUE),2)
  dat[3,1] <- round(max(YieldList[[paste0(crop[i], '_detrend_OLS')]]$Slope, na.rm= TRUE),2)
  dat[4,1] <- round(sd(YieldList[[paste0(crop[i], '_detrend_OLS')]]$Slope, na.rm= TRUE),2)
  dat[5,1] <- round(mad(YieldList[[paste0(crop[i], '_detrend_OLS')]]$Slope, 
                        center=median(YieldList[[paste0(crop[i], '_detrend_OLS')]]$Slope, na.rm=TRUE), 
                        na.rm= TRUE),2)
  x <- unlist(YieldList[[paste0(crop[i], '_detrend_OLS')]][,7:41]) %>%
    matrix(ncol = 1) %>% na.omit()
  dat[6,1] <- round(min(x),2)
  dat[7,1] <- round(mean(x),2)
  dat[8,1] <- round(max(x),2)
  dat[9,1] <- round(sd(x),2)
  dat[10,1] <- round(mad(x, center = median(x)),2)
  
  # M
  dat[1,2] <- round(min(YieldList[[paste0(crop[i], '_detrend_M')]]$Slope, na.rm= TRUE),2)
  dat[2,2] <- round(mean(YieldList[[paste0(crop[i], '_detrend_M')]]$Slope, na.rm= TRUE),2)
  dat[3,2] <- round(max(YieldList[[paste0(crop[i], '_detrend_M')]]$Slope, na.rm= TRUE),2)
  dat[4,2] <- round(sd(YieldList[[paste0(crop[i], '_detrend_M')]]$Slope, na.rm= TRUE),2)
  dat[5,2] <- round(mad(YieldList[[paste0(crop[i], '_detrend_M')]]$Slope, 
                        center=median(YieldList[[paste0(crop[i], '_detrend_M')]]$Slope, na.rm=TRUE), 
                        na.rm= TRUE),2)
  x <- unlist(YieldList[[paste0(crop[i], '_detrend_M')]][,7:41]) %>%
    matrix(ncol = 1) %>% na.omit()
  dat[6,2] <- round(min(x),2)
  dat[7,2] <- round(mean(x),2)
  dat[8,2] <- round(max(x),2)
  dat[9,2] <- round(sd(x),2)
  dat[10,2] <- round(mad(x, center = median(x)),2)
  
  # MM
  dat[1,3] <- round(min(YieldList[[paste0(crop[i], '_detrend_MM')]]$Slope, na.rm= TRUE),2)
  dat[2,3] <- round(mean(YieldList[[paste0(crop[i], '_detrend_MM')]]$Slope, na.rm= TRUE),2)
  dat[3,3] <- round(max(YieldList[[paste0(crop[i], '_detrend_MM')]]$Slope, na.rm= TRUE),2)
  dat[4,3] <- round(sd(YieldList[[paste0(crop[i], '_detrend_MM')]]$Slope, na.rm= TRUE),2)
  dat[5,3] <- round(mad(YieldList[[paste0(crop[i], '_detrend_MM')]]$Slope, 
                        center=median(YieldList[[paste0(crop[i], '_detrend_MM')]]$Slope, na.rm=TRUE), 
                        na.rm= TRUE),2)
  x <- unlist(YieldList[[paste0(crop[i], '_detrend_MM')]][,7:41]) %>%
    matrix(ncol = 1) %>% na.omit()
  dat[6,3] <- round(min(x),2)
  dat[7,3] <- round(mean(x),2)
  dat[8,3] <- round(max(x),2)
  dat[9,3] <- round(sd(x),2)
  dat[10,3] <- round(mad(x, center = median(x)),2)
  
  # TS
  dat[1,4] <- round(min(YieldList[[paste0(crop[i], '_detrend_TS')]]$Slope, na.rm= TRUE),2)
  dat[2,4] <- round(mean(YieldList[[paste0(crop[i], '_detrend_TS')]]$Slope, na.rm= TRUE),2)
  dat[3,4] <- round(max(YieldList[[paste0(crop[i], '_detrend_TS')]]$Slope, na.rm= TRUE),2)
  dat[4,4] <- round(sd(YieldList[[paste0(crop[i], '_detrend_TS')]]$Slope, na.rm= TRUE),2)
  dat[5,4] <- round(mad(YieldList[[paste0(crop[i], '_detrend_TS')]]$Slope, 
                        center=median(YieldList[[paste0(crop[i], '_detrend_TS')]]$Slope, na.rm=TRUE), 
                        na.rm= TRUE),2)
  x <- unlist(YieldList[[paste0(crop[i], '_detrend_MM')]][,7:41]) %>%
    matrix(ncol = 1) %>% na.omit()
  dat[6,4] <- round(min(x),2)
  dat[7,4] <- round(mean(x),2)
  dat[8,4] <- round(max(x),2)
  dat[9,4] <- round(sd(x),2)
  dat[10,4] <- round(mad(x, center = median(x)),2)
  
  write.csv(dat, file = paste(fileloc1, loc1, 'Yield_', crop[i], '_detrend_CoeficientTest', '.csv', sep = ''),
            row.names = TRUE)
}

rm(x, dat, i)
rm(dat, i, name, variables)
T1c <- Sys.time()
print(paste0('Finished testing detrended yield files at: ', T1c,
             '. Time elapsed:', T1c-T1b))

# 1.6 Plotting standardize Yield ##########
T1b <- Sys.time()
print(paste0('Started plotting standardized yield files at: ', T1b))

if (hpc == FALSE) {
  myColors <- c('maize' = '#1B9E77', 'rice' = '#D95F02', 
                'soya'= '#7570B3','wheat'='#E7298A')
  for (t in c('OLS','M','MM','TS')){
    # Global
    dat <- YieldList[[paste0(crop[1], '_detrend_', t)]][,42:76] %>%
      rbind(YieldList[[paste0(crop[2], '_detrend_', t)]][,42:76]) %>%
      rbind(YieldList[[paste0(crop[3], '_detrend_', t)]][,42:76]) %>%
      rbind(YieldList[[paste0(crop[4], '_detrend_', t)]][,42:76]) %>%
      cbind(matrix(crop[1], ncol = dim(latlon)[1], nrow = 1) %>%
              cbind(matrix(crop[2], ncol = dim(latlon)[1], nrow = 1)) %>%
              cbind(matrix(crop[3], ncol = dim(latlon)[1], nrow = 1)) %>%
              cbind(matrix(crop[4], ncol = dim(latlon)[1], nrow = 1)) %>%
              t()) %>%
      na.omit()
    colnames(dat) <- c(yr, 'Crop')
    dat <- pivot_longer(dat, cols = as.character(1982:2016), names_to= "Year", values_to="Yield" )
    p0 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title='Global',
           x = 'Standardized Yield') +
      theme(legend.position = 'bottom')
    
    # Regional
    datReg <- array(NA, dim=c(0,37))
    for (r in 1:dim(regions)[1]){
      latR <- seq(regions$BLlat[r], regions$TRlat[r], by = 0.5) # y-axis
      lonR <- seq(regions$BLlon[r], regions$TRlon[r], by = 0.5) # x-axis
      latlonR <- tibble(expand.grid(latR, lonR)) %>% 
        rename('lat' = 'Var1') %>% rename('lon' = 'Var2')
      
      dat <- matrix(NA, nrow = dim(latlonR)[1], ncol = length(yr))
      colnames(dat) <- as.character(yr)
      yd_mR <- cbind(latlonR, dat)
      yd_rR <- yd_mR; yd_sR <- yd_mR; yd_wR <- yd_mR
      
      for (i in 1:length(lonR)){
        m <- which(YieldList[[paste0(crop[1], '_detrend_', t)]]$lat == latR[1] & 
                     YieldList[[paste0(crop[1], '_detrend_', t)]]$lon == lonR[i])
        yd_mR[((i-1)*length(latR)+1):(i*length(latR)),3:ncol(yd_mR)] <-
          YieldList[[paste0(crop[1], '_detrend_', t)]][m:(m+length(latR)-1),42:76]
        yd_rR[((i-1)*length(latR)+1):(i*length(latR)),3:ncol(yd_rR)] <-
          YieldList[[paste0(crop[2], '_detrend_', t)]][m:(m+length(latR)-1),42:76]
        yd_sR[((i-1)*length(latR)+1):(i*length(latR)),3:ncol(yd_sR)] <-
          YieldList[[paste0(crop[3], '_detrend_', t)]][m:(m+length(latR)-1),42:76]
        yd_wR[((i-1)*length(latR)+1):(i*length(latR)),3:ncol(yd_wR)] <-
          YieldList[[paste0(crop[4], '_detrend_', t)]][m:(m+length(latR)-1),42:76]
      } # End i regional filling
      
      dat <- yd_mR[,3:ncol(yd_mR)] %>%
        rbind(yd_rR[,3:ncol(yd_rR)]) %>%
        rbind(yd_sR[,3:ncol(yd_sR)]) %>%
        rbind(yd_wR[,3:ncol(yd_wR)]) %>%
        cbind(matrix(crop[1], ncol = dim(yd_mR)[1], nrow = 1) %>%
                cbind(matrix(crop[2], ncol = dim(yd_mR)[1], nrow = 1)) %>%
                cbind(matrix(crop[3], ncol = dim(yd_mR)[1], nrow = 1)) %>%
                cbind(matrix(crop[4], ncol = dim(yd_mR)[1], nrow = 1)) %>%
                t()) %>%
        cbind(matrix(regions$Region[r], ncol = 1))
      datReg <- rbind(datReg, dat)
    } # R loop regions
    colnames(datReg) <- c(yr, 'Crop','Region')
    datReg <- pivot_longer(datReg, cols = as.character(1982:2016), names_to= "Year", values_to="Yield" )
    
    # Plotting #####
    dat <- subset(datReg, Region == regions$Region[1])
    p1 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[1],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[2])
    p2 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[2],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[3])
    p3 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[3],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[4])
    p4 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[4],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[5])
    p5 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[5],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[6])
    p6 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[6],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[7])
    p7 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[7],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[8])
    p8 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[8],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[9])
    p9 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[9],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[10])
    p10 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[10],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    dat <- subset(datReg, Region == regions$Region[11])
    p11 <- ggplot(na.omit(dat), aes(x = Yield, color = Crop)) + 
      geom_density() + 
      scale_fill_manual(values= myColors) +
      labs(title=regions$Region[11],
           x = 'Standardized Yield') +
      theme(legend.position = "NULL")
    
    myLegend <- get_legend(p0) %>% as_ggplot()
    p0 <- p0 + theme(legend.position = "NULL")
    
    x <- gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p0, ncol= 3,
                                 nrow= 4) 
    x <- gridExtra::grid.arrange(x, myLegend, ncol=1, nrow=2, heights = c(12,1),
                                 top = paste0('Density Plots of the standardized yield using the ', t,' estimator'))
    ggsave(x, filename= paste0(fileloc1, loc3, 'DensityPlot_',t,'.tiff'), 
           width = 11, height=12, dpi=350)
    
    rm(r, i, m, dat, yd_mR, yd_rR, yd_sR, yd_wR, datReg, 
       p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, myLegend, x)
  } # t Loop estimator type
} # End hpc 

T1c <- Sys.time()
print(paste0('Finished plotting standardized yield files at: ', T1c,
             '. Time elapsed:', T1c-T1b))

# 1.7 Percentile ##########
#     This section will calculate the quartiles, deciles, 5th and 95th 
# percentiles, and +/- one standard deviation for each type of detrended crop 
# yield. All data is saved with the appropriate name in Yield List and written
# to the appropriate file location.
T1b <- Sys.time()
print(paste0('Started Percentile yield analysis at: ', T1b))

for (t in c('OLS','M','MM','TS')){
  for (i in 1:length(crop)){
    datYd <- YieldList[[paste0(crop[i], '_detrend_', t)]][,c(1:4,7:41)]
    
    # Quartile 25%, 50%, 75%
    dat <- t(apply(datYd[,5:ncol(datYd)], MARGIN = 1, FUN = yd_quartile))
    dat <- cbind(latlon, dat) %>%
      as_tibble()
    colnames(dat) <- c(colnames(latlon), paste0('y', yr))
    name <- paste0(crop[i], '_quartile', t)
    YieldList[[name]] <- dat
    write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop[i],'_',t, '_quartile','.csv'), 
              row.names = FALSE)
    # Decile 10%, 50%, 90%
    dat <- t(apply(datYd[,5:ncol(datYd)], MARGIN = 1, FUN = yd_decile))
    dat <- cbind(latlon, dat) %>%
      as_tibble()
    colnames(dat) <- c(colnames(latlon), paste0('y', yr))
    name <- paste0(crop[i], '_quartile')
    YieldList[[name]] <- dat
    write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop[i],'_',t, '_decile','.csv'), 
              row.names = FALSE)
    # Nickle 5%, 50%, 95%
    dat <- t(apply(datYd[,5:ncol(datYd)], MARGIN = 1, FUN = yd_nickle))
    dat <- cbind(latlon, dat) %>%
      as_tibble()
    colnames(dat) <- c(colnames(latlon), paste0('y', yr))
    name <- paste0(crop[i], '_quartile')
    YieldList[[name]] <- dat
    write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop[i],'_',t, '_nickle','.csv'), 
              row.names = FALSE)
    # SigmaOne mean +/- one standard deviation, mean 
    dat <- t(apply(datYd[,5:ncol(datYd)], MARGIN = 1, FUN = yd_sigmaOne))
    dat <- cbind(latlon, dat) %>%
      as_tibble()
    colnames(dat) <- c(colnames(latlon), paste0('y', yr))
    name <- paste0(crop[i], '_quartile')
    YieldList[[name]] <- dat
    write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop[i],'_',t, '_sigmaOne','.csv'), 
              row.names = FALSE)
  } # i Loop Crop
} # t Loop estimator type

rm(i, dat, dats, name, t)
T1c <- Sys.time()
print(paste0('Finished plotting standardized yield files at: ', T1c,
             '. Time elapsed:', T1c-T1b))

# 1.8 Categorical ##########
#     This section will calculate the categorial of the chosen yield if opted for.
T1b <- Sys.time()
print(paste0('Started Percentile yield analysis at: ', T1b))
print(cat)

if (cat == TRUE){
  for (q in c('quartile', 'decile', 'nickle', 'sigmaOne')){
    for (i in 1:length(crop)){
      dat <- YieldList[[paste0(crop[i], '_', q, '_', type)]][,c(1:4,7:41)]
      
      # Catagorical
      dat <- apply(dat[,5:ncol(dat)], MARGIN = 2, FUN = category)
      dat <- cbind(latlon, dat) %>%
        as_tibble()
      colnames(dat) <- c(colnames(latlon), paste0('y', yr))
      name <- paste0(crop[i], '_failure')
      YieldList[[name]] <- dat
      write.csv(dat, file = paste0(fileloc1, loc1, 'Yield_', crop[i], '_',
                                   type,'_',q, '.csv'),
                row.names = FALSE)
    } # i Loop Crop
  } # q Loop types of percentiles 
}

T1c <- Sys.time()
print(paste0('Finished plotting standardized yield files at: ', T1c,
             '. Time elapsed:', T1c-T1b))
print(paste0('Total elapsed time: ', T1c-T1a))

# END
# P4_MannKendall.R
# This R program will open yearly Agroclimate indices.
#
#
# T. A. Schillerberg
#               Jul. 2020
#      Updated: Mar. 2022

# Local Computer
setwd("/Research/AgroclimaticConditions/Code")
fileloc1 <- '/Research/AgroclimaticConditions/Data/'
loc1 <- 'Global_AgroInd/Agro092021_mu/'
loc2 <- 'Global_AgroInd/Agro102021_MK/'

# fileloc1 <- '~/AgroClmInd/data/'

options(show.error.locations = TRUE)

# Variables to change ##########################################################
yr <- 1982:2017
core <- 1
part <- 1

# Libraries ####################################################################
library(Kendall)
library(rworldmap)
# library(sf)
library(tidyverse)

# Functions ####################################################################

TMann_Kendall <- function(X, varis){
  # https://vsp.pnnl.gov/help/Vsample/Design_Trend_Mann_Kendall.htm
  # https://www.statisticshowto.com/mann-kendall-trend-test/
  #
  # The MK test tests whether to reject the null hypothesis () and accept
  #   the alternative hypothesis (), where
  # Ho: No monotonic trend
  # Ha: Monotonic trend is present
  #   The initial assumption of the MK test is that the  is true and that the
  #   data must be convincing beyond a reasonable doubt before  is rejected and  
  #   is accepted.
  #
  # Assumptions:
  # - When no trend is present, the measurements (observations or data) obtained
  #   over time are independent and identically distributed. The assumption of
  #   independence means that the observations are not serially correlated over
  #   time.
  # - The observations obtained over time are representative of the true conditions
  #   at sampling times.
  # - The sample collection, handling, and measurement methods provide unbiased
  #   and representative observations of the underlying populations over time.
  #
  # If more than one observation occur at one time the median value should be used.
  
  # packages #####
  require(Kendall)
  require(tidyverse)
  
  # variables #####
  ID <- X
  datMK <- varis[["dat"]][ID,]
  datMK <- datMK[2:(length(datMK)-1)]
  latlonPt <- varis[["latlon"]][ID,]
  
  # calculation #####
  if (varis[["mask"]][ID] == 1){
    # print(ID)
    K <- array(NA, dim = 5)
    
    # Test the Mann-Kendal
    datA <- as.matrix(datMK) %>% unname
    datB <- matrix(-999, nrow = 1, ncol = length(datMK))
    datC <- matrix(0, nrow = 1, ncol = length(datMK))
    datD <- matrix(NA, nrow = 1, ncol = length(datMK))
    datE <- matrix(datA[1], nrow=1, ncol = length(datMK))
    
    datMK[datMK == -999] <- NA
    
    if (identical(datA, datB) == TRUE){
      # There is no listed data points therefore no trend
      K <- array(c(NA,1,NA,NA,NA), dim = 5)
    } else if (identical(datA, datC) == TRUE) {
      # There is no listed data points therefore no trend
      K <- array(c(NA,1,NA,NA,NA), dim = 5)
    }else if (identical(datA, datD) == TRUE) {
      # There is no listed data points
      K <- array(NA, dim = 5)
    }else if (identical(datA, datE) == TRUE) {
      # There is no trend homogonus
      K <- array(c(NA,1,NA,NA,NA), dim = 5)
    } else if (sum(is.na(datMK)) > (length(datMK)/2)) {
      # Missing data is greater than 50% Can not compute trend
      K <- array(c(NA,1,NA,NA,NA), dim = 5)
    } else {
      # print(ID)
      K <- unlist(Kendall::MannKendall(datMK)) %>% unname
      # Order: tau, p-value, S, D, varS
    }
    
  } else if (varis[["mask"]][ID] == 0){
    # The data point is non-cropland or ocean
    K <- array(NA, dim = 5)
  } else {
    print(paste('Error at point ', ID, ' when calculating the mask'))
  }
  K
  # rm(ID, varis, latlonPt, datA, datB, datC, datD, K, datMK)
}

craftbrewer_pal <- function (type = "seq", 
                             palette = 1, direction = 1) {
  #referance: https://stackoverflow.com/questions/62543112/how-to-make-discrete-gradient-color-bar-with-geom-contour-filled
  pal <- scales:::pal_name(palette, type)
  force(direction)
  function(n) {
    n_max_palette <- RColorBrewer:::maxcolors[names(RColorBrewer:::maxcolors) == palette]
    
    if (n < 3) {
      pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    } else if (n > n_max_palette){
      rlang::warn(paste(n, "colours used, but", palette, "has only",
                        n_max_palette, "- New palette created based on all colors of", 
                        palette))
      n_palette <- RColorBrewer::brewer.pal(n_max_palette, palette)
      colfunc <- grDevices::colorRampPalette(n_palette)
      pal <- colfunc(n)
    }
    else {
      pal <- RColorBrewer::brewer.pal(n, pal)
    }
    pal <- pal[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

scale_fill_craftfermenter <- function(..., type = "seq", palette = 1, 
                                      direction = -1, na.value = "grey50", 
                                      guide = "coloursteps", aesthetics = "fill") {
  #referance: https://stackoverflow.com/questions/62543112/how-to-make-discrete-gradient-color-bar-with-geom-contour-filled
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "fermenter", ggplot2:::binned_pal(craftbrewer_pal(type, palette, direction)), 
               na.value = na.value, guide = guide, ...)
}
Ta <- Sys.time()
print(paste('Variables and functions loaded at: ', Ta, sep=''))

# PART I  -- Mann-Kendal #######################################################
#     This section will load in the required variables 
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
RowID <- rowid_to_column(latlon)

yr <- 1982:2017
file1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
          'StartFieldOp')
file2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')

crop <- c('maize', 'rice', 'soya', 'wheat')

AgroMK <- list()

rm(latID, lonID, latlonID)

# 1.2 Mask read in ##########
loc <- 'EarthStat/'
file <- 'EarthStat_Sacks_Mask.csv'
mask <- read_csv(paste(fileloc1, loc, file, sep = ''), col_names = TRUE,
                cols(.default = col_double()))

# 1.3 Non-crop Dependent Mann-Kendall #####

for (i in 1:length(file1)){
  print(file1[i])
  dat <- read_csv(paste0(fileloc1, loc1, file1[i], '.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  dat <- dat[, -which(names(dat) %in% c('lat','lon','latID','lonID'))]
  # dat[dat == -999] <- NA
  variables <- list('mask' = mask$CroplandDate, 'latlon'=latlon, 'dat'= dat)
  dat <- parallel::mclapply(X = RowID$rowid, FUN = TMann_Kendall, varis = variables,
                            mc.cores = core) %>%
    unlist() %>%
    matrix(ncol = 5, byrow = TRUE)
  colnames(dat) <- c('tau', 'pvalue','S','D', 'varS')
  dat <- cbind(latlon, dat)
  
  write.csv(dat, file = paste0(fileloc1, loc2, 'MK_', file1[i], '.csv'),
            row.names = FALSE)
  name <- file1[i]
  AgroMK[[name]] <- dat
  
  T1 <- Sys.time()
  print(paste('Finished MK trend analysis of ', file1[i], ' at: ',T1,sep=''))
}

Tb <- Sys.time()
C <- Tb-Ta
print(paste('Finished MK trend analysis of non-crop dependent analysis at: ', Tb,
            ' Time lapsed: ', C, sep=''))
# rm(dat, i, variables, name)

# 1.4 Crop Dependent Mann-Kendall #####

for (i in 1:length(file2)){
  for(j in 1:length(crop)){
    dat <- read_csv(paste(fileloc1, loc1, file2[i],'_',crop[j], '.csv', sep=''),
                    col_names = TRUE, cols(.default = col_double()))
    dat <- dat[, -which(names(dat) %in% c('lat','lon','latID','lonID'))]
    # dat[dat == -999] <- NA
    maskCrop <- unlist(mask[,(16 + j)]) %>% as.numeric
    variables <- list('mask' = maskCrop, 'latlon'=latlon, 'dat'= dat)
    dat <- parallel::mclapply(X = RowID$rowid, FUN = TMann_Kendall, varis = variables,
                              mc.cores = core) %>%
      unlist() %>%
      matrix(ncol = 5, byrow = TRUE)
    colnames(dat) <- c('tau', 'pvalue','S','D', 'varS')
    dat <- cbind(latlon, dat)
    
    write.csv(dat, file = paste(fileloc1, loc2, 'MK_' ,file2[i], '_', crop[j], '.csv', sep = ''), 
              row.names = FALSE)
    name <- paste(file2[i],"_",crop[j], sep='')
    AgroMK[[name]] <- dat
    
    T1 <- Sys.time()
    print(paste('Finished MK trend analysis of ', file2[i],'_',crop[j], ' at: ',T1,sep=''))
  }
}

Tc <- Sys.time()
C <- Tc-Tb
print(paste('Finished MK trend analysis of non-crop dependent analysis at: ', Tc,
            ' Time lapsed: ', C, sep=''))

rm (dat, Tb, i, j, maskCrop, variables, name)

# PART II  -- Plotting Mann-Kendall ############################################
#     This section will plot the result from the previous section. This section 
# will make a plot that is similar to Chen et al. 2019 (Nature Sustainability) 
# where the cropland that is not significant is grey and the significant regions 
# reflect the trend of MK. Non-cropland regions are white
#
# References:

# 2.1 Variable needed (just plotting) ##########
# Make sure to run functions! 

file1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp')
file2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')

crop <- c('maize', 'rice', 'soya', 'wheat')
AgroMK <- list()

for (i in 1:length(file1)){
  dat <- read_csv(paste0(fileloc1, loc2, 'MK_', file1[i], '.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  name <- file1[i]
  AgroMK[[name]] <- dat
}

for (i in 1:length(file2)){
  for (j in 1:length(crop)){
    dat <- read_csv(paste0(fileloc1, loc2, 'MK_', file2[i],'_',crop[j], '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
    name <- paste(file2[i],"_",crop[j], sep='')
    AgroMK[[name]] <- dat
  }
}


dat <- read_csv(paste(fileloc1, loc1, 'AccFrostDays', '.csv', sep=''),
                col_names = TRUE, cols(.default = col_double()))
frost <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]

rm(i, j, dat)
# 2.2 Variables needed cont. ##########
# world map
baseData <- map_data('world')

# Get the limits for all MK results
limits <- matrix(data=NA, nrow = length(names(AgroMK)), ncol = 2)
for (i in 1:length(names(AgroMK))){
  dat <- AgroMK[[i]]
  obs <- dat[,5:6]
  df_obs <- data.frame(dat$lon, dat$lat, obs, round(obs$tau,2))
  colnames(df_obs) <- c('lon', 'lat', 'tau', 'pvalue', 'new_tau')
  limits[i,] <- t(as.matrix(round(range(df_obs$tau, na.rm = TRUE))))
}
mylimits <- c(min(limits[,1]), max(limits[,2]))

# 2.3 Plotting and saving ##########
for (i in names(AgroMK)){
  dat <- AgroMK[[i]]
  obs <- dat[,5:6]
  df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
  colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
  # mylimits <- c(-1,1)
  mybreaks <- seq(-1, 1, 0.25)
  
  # Change to 1 sig =< 0.01, 2 sig =< 0.05, 3 sig =< 0.1  0 = non significant
  #df_obs$new_tau[df_obs$pvalue <= 0.01] <-4
  #df_obs$new_tau[df_obs$pvalue <= 0.05] <-3
  #df_obs$new_tau[df_obs$pvalue <= 0.1] <-df_obs$tau
  df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
  
  ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
    theme_bw() +
    labs(title=paste('MK trend ', names(AgroMK[i]), sep=""), x="", y="") +  
    theme(plot.title = element_text(hjust = 0.5, size=15)) + 
    # Significant Values
    metR::geom_contour_fill(aes(z = tau)) +
    scale_fill_craftfermenter(
      breaks = mybreaks, 
      palette = "Spectral", 
      limits = c(-1,1),
      guide = guide_colorsteps(
        even.steps = TRUE,
        frame.colour = "black", 
        ticks.colour = "black", # you can also remove the ticks with NA
        barwidth=20)) +
    
    # Non-Significant Values
    ggnewscale::new_scale("fill") +
    geom_tile(data = subset(df_obs, new_tau == -999), aes(fill=new_tau)) +
    scale_fill_gradientn(name = 'Non-significant', colours = "grey80", na.value="white", limits=c(-999,-999), guide = 'legend') +
    
    # World Map
    geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
                 colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
    # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
    coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
    theme(legend.position= "bottom", 
          legend.key.height  = unit(0.5, "cm"), legend.key.width = unit(2, "cm"), legend.text = element_text(size = 10),
          panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
          plot.margin = margin(r=0.5,unit="cm"))
  ggsave(filename = paste(fileloc1, loc2,'MK_', i, ".tiff", sep=''), width = 8, height = 5.5, dpi = 350)
}

rm(dat, i, limits, obs, df_obs, p1_obs)

Td <- Sys.time()
C <- Td-Tc
print(paste('Finished MK trend ploting at: ', Td,
            ' Time lapsed: ', C, sep=''))
# PART III  -- Regional Analysis ##############################################
#     This section will conduct a regional analysis for each Agroclimate indice 
# and determine which percentage of land has significant trends as determined 
# by the p-value =  0.1, 0.5, 0.01. 
#
# References:

# 3.1 Variables Needed #####
regionMask <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6RegionsMask.csv'),
                       col_names = TRUE)
regionKey <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6Regions.csv'),
                      col_names = TRUE)

myLevels <- c('Temperate','Frost Free', "Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
              "Oceania","South & Southeast Asia", "Sub-Saharah Africa", "Temprate South America", "Tropical South America",
              "West-Central Asia")
myLevelsAcr <- c('Temp','FrFree','CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA')

regionsNon <- bind_cols(myLevels, as.data.frame(matrix(0, nrow=length(myLevelsAcr), 
                                                        ncol=length(file1)*3)))
regionsC <- bind_cols(myLevels, as.data.frame(matrix(0, nrow=length(myLevelsAcr), 
                                                        ncol=length(file2)*3)))

# 3.2 Regional Analysis #####
# 3.2.a NonCrop Dependent #####
name <- vector()
for (k in 1:length(file1)){
  for (i in 1:length(myLevelsAcr)){
    # Calculation of regional #####
    if (i == 1){
      mu <- apply(frost[,5:ncol(frost)], MARGIN = 1, FUN= mean, na.rm=TRUE)
      m <- which(mu >= 1)
    } else if(i == 2) {
      mu <- apply(frost[,5:ncol(frost)], MARGIN = 1, FUN= mean, na.rm=TRUE)
      m <- which(mu < 1)
    } else {
      n <- which(colnames(regionMask) == myLevelsAcr[i])
      m <- which(regionMask[,n] == 1)
    }
    
    dat <- AgroMK[[file1[k]]][m,]
    
    regionsNon[i,(2+(k-1)*3)] <- # % of cropland that is p-value < .1 
      as_tibble(round(length(which(dat$pvalue <= 0.1)) / 
                        length(which(is.na(dat$pvalue)==FALSE)),4)*100)
    regionsNon[i,(2+(k-1)*3+1)] <- # % of cropland that is p-value < .05
      as_tibble(round(length(which(dat$pvalue <= 0.05)) / 
                        length(which(is.na(dat$pvalue)==FALSE)),4)*100)
    regionsNon[i,(2+(k-1)*3)+2] <- # % of cropland that is p-value < .01
      as_tibble(round(length(which(dat$pvalue <= 0.01)) / 
                        length(which(is.na(dat$pvalue)==FALSE)),4)*100)
  }
  name <- cbind(name, paste(names(AgroMK)[k], '_0.1',sep=''), 
                paste(names(AgroMK)[k], '_0.05',sep=''),
                paste(names(AgroMK)[k], '_0.01',sep=''))
}
colnames(regionsNon) <- c('Region', name)
write.csv(regionsNon, file = paste(fileloc1, loc2, 'Regions','Non', '.csv', sep = ''), 
          row.names = FALSE)

Te <- Sys.time()
C <- Te-Td
print(paste('Finished MK trend analysis of non-crop dependent Agroclimate regional trends at: ', Te,
            ' Time lapsed: ', C, sep=''))
# 3.2.b Crop Dependent #####

for (l in 1:length(crop)){
  regionsC <- bind_cols(myLevels, as.data.frame(matrix(0, nrow=length(myLevelsAcr), 
                                                        ncol=length(file2)*3)))
  name <- vector()
  for (k in 1:length(file2)){
    for (i in 1:length(myLevelsAcr)){
      # Calculation of regional #####
      if (i == 1){
        mu <- apply(frost[,5:ncol(frost)], MARGIN = 1, FUN= mean, na.rm=TRUE)
        m <- which(mu >= 1)
      } else if(i == 2) {
        mu <- apply(frost[,5:ncol(frost)], MARGIN = 1, FUN= mean, na.rm=TRUE)
        m <- which(mu < 1)
      } else {
        n <- which(colnames(regionMask) == myLevelsAcr[i])
        m <- which(regionMask[,n] == 1)
      }

      
      dat <- AgroMK[[paste0(file2[k],'_',crop[l])]][m,]
      
      regionsC[i,(2+(k-1)*3)] <- # % of cropland that is p-value < .1 
        as_tibble(round(length(which(dat$pvalue <= 0.1)) / 
                          length(which(is.na(dat$pvalue)==FALSE)),4)*100)
      regionsC[i,(2+(k-1)*3+1)] <- # % of cropland that is p-value < .05
        as_tibble(round(length(which(dat$pvalue <= 0.05)) / 
                          length(which(is.na(dat$pvalue)==FALSE)),4)*100)
      regionsC[i,(2+(k-1)*3)+2] <- # % of cropland that is p-value < .01
        as_tibble(round(length(which(dat$pvalue <= 0.01)) / 
                          length(which(is.na(dat$pvalue)==FALSE)),4)*100)
    }
    name <- cbind(name, paste(file2[k], '_0.1',sep=''), 
                  paste(file2[k], '_0.05',sep=''),
                  paste(file2[k], '_0.01',sep=''))
  }
  
  colnames(regionsC) <- c('Region', name)
  write.csv(regionsC, file = paste(fileloc1, loc2, 'Regions','_',crop[l], '.csv', sep = ''), 
            row.names = FALSE)
}

Tf <- Sys.time()
C <- Tf-Te
print(paste('Finished MK trend analysis of crop dependent agroclimate regional trends at: ', Tf,
            ' Time lapsed: ', C, sep=''))

# END

# PART IV -- Correlation & Significance ######################################

# 4.1 Variables Needed ##########
file1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp')
file2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')

myLevelsAcr <- c('Temp','FrFree','CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA')

crop <- c('maize', 'rice', 'soya', 'wheat')
AgroInd <- list()
AgroMK <- list()

# 4.2 Opening Files ##########
regionMask <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6RegionsMask.csv'),
                       col_names = TRUE)

for (i in 1:length(file1)){
  dat <- read_csv(paste(fileloc1, loc1, file1[i], '.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double()))
  dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
  name <- file1[i]
  AgroInd[[name]] <- dat
  
  dat <- read_csv(paste0(fileloc1, loc2, 'MK_', file1[i], '.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  name <- file1[i]
  AgroMK[[name]] <- dat
}

for (i in 1:length(file2)){
  for (j in 1:length(crop)){
    dat <- read_csv(paste(fileloc1, loc1, file2[i],'_',crop[j], '.csv', sep=''),
                    col_names = TRUE, cols(.default = col_double()))
    dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
    name <-  paste(file2[i],"_",crop[j], sep='')
    AgroInd[[name]] <- dat
    
    dat <- read_csv(paste0(fileloc1, loc2, 'MK_', file2[i],'_',crop[j], '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
    name <- paste(file2[i],"_",crop[j], sep='')
    AgroMK[[name]] <- dat
  }
}

rm(i, j, dat)

# 4.3 Trend Analysis ##########
for (j in myLevelsAcr[3:length(myLevelsAcr)]){
  for (i in names(AgroMK)){
    dat <- AgroMK[[i]]
    obs <- dat[,5:6]
    df_obs <- data.frame(obs, round(obs$tau,2)); rm(obs)
    colnames(df_obs) <- c('tau','pvalue','new_tau')
    
    df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
    
    dat <- AgroInd[[i]]
    dat <- cbind(dat, df_obs)
    #Regional
    n <- which(colnames(regionMask) == j)
    o <- which(regionMask[,n] == 1)
    dat <- dat[o,]
    dat$new_tau[dat$new_tau == -999] <- NA
    
    # Make new dataframe with the means and trends
    df_inde <- dat[, which(names(dat) %in% c('lat','lon', 'tau','new_tau'))] %>%
      cbind(Avg = apply(dat[,3:37], FUN = mean, MARGIN = 1, na.rm = TRUE)) %>%
      cbind(Median = apply(dat[,3:37], FUN = median, MARGIN = 1, na.rm = TRUE))
    # Correl <- cor(df_inde$Median, df_inde$tau, use="complete.obs", method="spearman")
    
    ggplot(data=na.omit(df_inde), aes(x = tau, y=Avg)) +
      geom_point(aes(color = factor(new_tau)))
  }
}




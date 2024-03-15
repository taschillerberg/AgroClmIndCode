# P1_mask.R
# This R program create a mask of the cropland for each crop. To do this the 
# program will read in a tiff image change the resolution then create and fill a 
# data frame containing a binary column (0-land, 1- cropland, NA ocean/water) 
# and another containing the amount of harvested area. This program will also 
# read in several files containing planting and harvesting dates of several crops.
# A data frame for each crop will be created where the start, mean, and end of
# planting and harvest will be written for each crop.
#
# T. A. Schillerberg
#               Aug. 2020
#      Updated: Aug. 2021

setwd("")
fileloc1 <- '/Research/AgroclimaticConditions/Data/EarthStat/'
fileloc2 <- '/Research/AgroclimaticConditions/Data/'

options(show.error.locations = TRUE)

# Libraries ####################################################################
library(raster)
library(sp)
library(ggplot2)
library(ncdf4)
library(tidyverse)
library(rgdal)
library(maptools)       # For unionSpatialPolygons()

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


# PART I --  Creation of the mask ##############################################
#     This section will create the global crop mask for each crop. 
#
# References: Monfreda et al. (2008)
# https://www.neonscience.org/resources/learning-hub/tutorials/raster-data-r

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
rm(latID, lonID, latlonID)

crop <- c('maize','rice','soybean','wheat')
loc <- '_HarvAreaYield_Geotiff/'

# 1.2 Opening of files ##########

# Create a data file mask
mask <- tibble(latlon) %>%
  add_column(CroplandArea = -999) %>%
  add_column(MaizeArea = -999) %>%
  add_column(RiceArea = -999) %>%
  add_column(SoyaArea = -999) %>%
  add_column(WheatArea = -999) %>%
  add_column(Land = -999) %>%
  add_column(Cropland = 0) %>%
  add_column(Maize = 0) %>%
  add_column(Rice = 0) %>%
  add_column(Soya = 0) %>%
  add_column(Wheat = 0) %>%
  add_column(CroplandDate = 0) %>%
  add_column(MaizeDate = 0) %>%
  add_column(RiceDate = 0) %>%
  add_column(SoyaDate = 0) %>%
  add_column(WheatDate = 0) %>%
  add_column(WWheatDate = 0)
# Total area, Maize area, Rice area, Soya area, Wheat area,
# Land, Cropland (maize, rice, soya, or wheat), Maize cropland, 
# Rice cropland, Soya cropland, CroplandDate (dates avaliable for planting/harvest),
# MaizeDate (dates avaliable), RiceDate (dates avaliable), 
# SoyaDate (dates avaliable), WheatDate (dates avaliable)

for (i in 1:length(crop)){
  #  Open the tiff files
  dat <- raster(paste(fileloc1, crop[i], loc, crop[i], loc, crop[i],
                      '_HarvestedAreaHectares.tif', sep=''))
  # #  Look at the attributes
  # dat
  #  Calculate and save the min and max values of the raster to the raster obj
  dat <- setMinMax(dat)
  # #  Distribution of the raster pixel values
  # hist(dat, main = 'Distribution of Harvested Area Hectares', col = 'purple')
  # #  Plot the raster data
  plot(dat, main = paste(crop[i], ' Harvested Area Hectares'))
  
  #  Re-Sampling raster to be the same resolution via calculating the sum of the
  #  finer resolution cells
  dat.resize <- raster::aggregate(dat, 6, fun=sum) %>%
    raster::flip(direction = 2) # Flips over the y axis
  dat.resize
  # plot(dat.resize, main = paste(crop[i], ' Harvested Area Hectares Resized'))
  
  # Put into the dataframe 
  dat.resize.matrix <- as.matrix(dat.resize)
  dimnames(dat.resize.matrix) <- list(lat, lon)
  # Fill the area column
  mask[,(i+5)] <- apply(X = latlon, MARGIN = 1, FUN = pt_latlon, datX = dat.resize.matrix)
}

rm(i, dat, dat.resize, dat.resize.matrix)

# 1.3 Fill and format the mask ##########
# Add the column areas together to get the total
mask$CroplandArea <- mask$MaizeArea + mask$RiceArea + mask$SoyaArea + mask$WheatArea

# Create the land mask
mask$Land[is.na(mask$MaizeArea) == TRUE] <- 0
mask$Land[mask$Land == -999] <- 1 # Does not capture Antartica

# Create the Cropland Masks
mask$Cropland[mask$CroplandArea > 0] <- 1
mask$Maize[mask$MaizeArea > 0] <- 1
mask$MaizeArea[is.na(mask$MaizeArea) == TRUE] <- 0
mask$Rice[mask$RiceArea > 0] <- 1
mask$Soya[mask$SoyaArea > 0] <- 1
mask$Wheat[mask$WheatArea > 0] <- 1

# PART II -- Planting and Harvesting ###########################################
#     This section will open the planning and harvesting nc files and create 
# a dataframe.
#
# References: Sacks et al. (2010)

# 2.1. Variables needed ##########
loc <- 'Sacks2010/'
crop2 <- c('Maize.crop', 'Rice.crop','Soybeans.crop', 'Wheat.crop', 
          'Wheat.Winter.crop')
season <- c('plant', 'plant.start', 'plant.end','plant.range', 
            'harvest', 'harvest.start','harvest.end','harvest.range')

# 2.2. Opening of files ##########

Crop_season <- list()

for (i in 1:length(crop2)){
  crop_date <- tibble(latlon) %>%
    cbind(matrix(data = -999, nrow = dim(latlon)[1], ncol = length(season)))
  
  dat_nc <- ncdf4::nc_open(paste(fileloc2, loc, crop2[i], 
                                 '.calendar.fill.nc', sep=''))
  lat_nc <- ncdf4::ncvar_get(dat_nc, 'latitude')
  lon_nc <- ncdf4::ncvar_get(dat_nc, 'longitude')
  
  for (j in 1:length(season)){
    dat <- ncdf4::ncvar_get(dat_nc, season[j])
    fillvalue <- ncdf4::ncatt_get(dat_nc, season[j], '_FillValue')
    dat[dat == fillvalue$value] <- NA
    
    dat <- raster::raster(t(dat), xmn=min(lon_nc), xmx=max(lon_nc), 
                          ymn=min(lat_nc), ymx=max(lat_nc))
    plot(dat, main = paste(crop[i],' ', season[j]))
    dat.matrix <- as.matrix(dat)
    dimnames(dat.matrix) <- list(lat_nc, lon_nc)
    dat.matrix <- dat.matrix[order(as.numeric(row.names(dat.matrix))),]
    
    crop_date[,(4+j)] <- apply(X=latlon, MARGIN=1, FUN=pt_latlon, datX=dat.matrix)
    crop_date[,(4+j)] <- round(as.numeric(crop_date[,(4+j)]))
  }
  ncdf4::nc_close(dat_nc)
  colnames(crop_date) <- c(colnames(latlon),season)
  name <- crop2[i]
  Crop_season[[name]] <- crop_date
}
rm(crop_date, dat_nc, lat_nc, lon_nc, dat, fillvalue, dat.matrix, name)

# Make sure that cropland and planting days match
mask$MaizeDate <- mask$Maize
mask$RiceDate <- mask$Rice
mask$SoyaDate <- mask$Soya
mask$WheatDate <- mask$Wheat
mask$WWheatDate <- mask$WheatDate
mask$MaizeDate[is.na(Crop_season[["Maize.crop"]]$plant) | is.na(Crop_season[["Maize.crop"]]$plant.start) | is.na(Crop_season[["Maize.crop"]]$plant.end)] <- 0
mask$MaizeDate[is.na(Crop_season[["Maize.crop"]]$harvest) | is.na(Crop_season[["Maize.crop"]]$harvest.start) | is.na(Crop_season[["Maize.crop"]]$harvest.end)] <- 0
mask$RiceDate[is.na(Crop_season[["Rice.crop"]]$plant) | is.na(Crop_season[["Rice.crop"]]$plant.start) | is.na(Crop_season[["Rice.crop"]]$plant.end)] <- 0
mask$RiceDate[is.na(Crop_season[["Rice.crop"]]$harvest) | is.na(Crop_season[["Rice.crop"]]$harvest.start) | is.na(Crop_season[["Rice.crop"]]$harvest.end)] <- 0
mask$SoyaDate[is.na(Crop_season[["Soybeans.crop"]]$plant) | is.na(Crop_season[["Soybeans.crop"]]$plant.start) | is.na(Crop_season[["Soybeans.crop"]]$plant.end)] <- 0
mask$SoyaDate[is.na(Crop_season[["Soybeans.crop"]]$harvest) | is.na(Crop_season[["Soybeans.crop"]]$harvest.start) | is.na(Crop_season[["Soybeans.crop"]]$harvest.end)] <- 0
mask$WheatDate[is.na(Crop_season[["Wheat.crop"]]$plant) | is.na(Crop_season[["Wheat.crop"]]$plant.start) | is.na(Crop_season[["Wheat.crop"]]$plant.end)] <- 0
mask$WheatDate[is.na(Crop_season[["Wheat.crop"]]$harvest) | is.na(Crop_season[["Wheat.crop"]]$harvest.start) | is.na(Crop_season[["Wheat.crop"]]$harvest.end)] <- 0
mask$WWheatDate[is.na(Crop_season[["Wheat.Winter.crop"]]$plant) | is.na(Crop_season[["Wheat.Winter.crop"]]$plant.start) | is.na(Crop_season[["Wheat.crop"]]$plant.end)] <- 0
mask$WWheatDate[is.na(Crop_season[["Wheat.Winter.crop"]]$harvest) | is.na(Crop_season[["Wheat.Winter.crop"]]$harvest.start) | is.na(Crop_season[["Wheat.crop"]]$harvest.end)] <- 0

mask$CroplandDate <- mask$MaizeDate + mask$RiceDate + mask$SoyaDate + mask$WheatDate + mask$WWheatDate
mask$CroplandDate[mask$CroplandDate > 1] <- 1
outfile <- "EarthStat_Sacks_Mask"
write.csv(mask, file = paste(fileloc1, outfile, '.csv', sep = ''), row.names = FALSE)

rm(crop, season, dat_nc, lat_nc, lon_nc, dat, dat.matrix, crop_date, 
   fillvalue, i, j, name)

# 2.3. Writing the files ##########

for (i in 1:length(names(Crop_season))){
  dat <- Crop_season[[i]]
  write.csv(dat, file = paste(fileloc2, loc, 'Growth_Season_', names(Crop_season)[i], 
                              '.csv',sep=''), row.names = FALSE)
}
rm(dat, i, loc)

print('End Part II Planting and Harvesting Dates')

# PART III -- Regions Mask IPCC ################################################
#     This section will convert the spatial polygon into a mask to be used later
#
# References: https://gis.stackexchange.com/questions/61633/convert-a-spatial-polygon-object-to-data-frame-using-r
# https://gis.stackexchange.com/questions/88830/overlaying-spatial-polygon-with-grid-and-checking-in-which-grid-element-specific

# 3.1 Variables Needed ##########
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
baseData <- map_data('world')
load(paste0(fileloc2,"IPCC6Regions/IPCC-WGI-reference-regions-v4_R.rda"), verbose = TRUE)

# 3.2 Formating ##########
refregions <- as(IPCC_WGI_reference_regions_v4, "SpatialPolygons")
refregions_QGIS <- readOGR(paste0(fileloc2,"IPCC6Regions/IPCC-WGI-reference-regions-v4_shapefile_2/RegionV1_3.shp"))
refregions <- refregions_QGIS

grid <- raster(extent(refregions), resolution = c(0.5, 0.5), crs=proj4string(refregions))
pts <- rasterToPoints(grid, spatial = T)

summary(IPCC_WGI_reference_regions_v4)
summary(refregions)
# ID2 <- over(pts, IPCC_WGI_reference_regions_v4) %>%
#   cbind(as.data.frame(pts)) %>%
#   arrange(x,y, by_group=FALSE)
ID <- over(pts, refregions) %>%
  cbind(as.data.frame(pts)) %>%
  arrange(x,y, by_group=FALSE)
ggplot(data=ID, aes(x=x, y=y)) +
  theme_bw() +
  geom_tile((aes(fill = Acronym))) +
  scale_fill_discrete() +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

ID$Acronym <- ID$Acronym %>% 
  as.character()
ID$Type <- ID$Type %>%
  as.character()
ID$Name <- ID$Name %>%
  as.character()
# 1
m <- which(ID$Acronym == 'CNA')
ID$Acronym[m] <- 'NAM'; ID$Name[m] <- 'North America'
# 2
m <- which(ID$Acronym == 'CAR')
ID$Acronym[m] <- 'CAC'; ID$Name[m] <- 'Central America & Caribbean'
# 3
m <- which(ID$Acronym == 'SAM')
ID$Acronym[m] <- 'SAM'; ID$Name[m] <- 'Tropical South America'
# 4
m <- which(ID$Acronym == 'SSA')
ID$Acronym[m] <- 'TSA'; ID$Name[m] <- 'Temprate South America'
# 5
m <- which(ID$Acronym == 'WAF')
ID$Acronym[m] <- 'SAF'; ID$Name[m] <- 'Sub-Sahara Africa'
# 6
m <- which(ID$Acronym == 'WCE')
ID$Acronym[m] <- 'EUM'; ID$Name[m] <- 'Europe & Mediterranean'
# 7
m <- which(ID$Acronym == 'ARP')
ID$Acronym[m] <- 'WCA'; ID$Name[m] <- 'West-Central Asia'
# 8
m <- which(ID$Acronym == 'SEA')
ID$Acronym[m] <- 'SEA'; ID$Name[m] <- 'South & Southeast Asia'
# 9
m <- which(ID$Acronym == 'EAS')
ID$Acronym[m] <- 'EAS'; ID$Name[m] <- 'East Asia'
# 10
m <- which(ID$Acronym == 'NAU')
ID$Acronym[m] <- 'OCE'; ID$Name[m] <- 'Oceania'

Acr <- ID$Acronym %>% 
  unique() %>% 
  na.exclude()

Region <- as.character(ID$Name) %>%
  unique() %>% 
  na.exclude()

mask <- matrix(0, nrow = dim(latlon)[1], ncol=length(Acr)) %>%
  as.data.frame()
for (i in 1:length(Acr)){
  mask[,i] <- replace(mask[,i],ID$Acronym == Acr[i], 1 )
}
colnames(mask) <- Acr
dat <- cbind(latlon, ID[,1:4], mask)

# Testing
ggplot(data=dat, aes(x=lon, y=lat)) +
  theme_bw() +
  geom_tile(aes(fill = CAC)) +
  scale_fill_continuous() +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

# 3.3 Creating a DataFrame ##########
regions <- tibble(Region) %>%
  add_column(Acr) %>%
  add_column(Type = Acr)

for (i in 1:dim(regions)[1]){
  m <- which(ID$Acronym == regions$Acr[i])
  regions$Type[i] <- ID$Type[m[1]]
}

# Modify to be only regions needed
crop <- matrix(0, nrow = dim(regions)[1], ncol = 4)
colnames(crop) <- c('Maize','Rice','Soy','Wheat')
regions <- cbind(regions, crop)
# 1
m <- which(regions$Acr == 'NAM')
regions[m,4:7] <- c('1','1','1','1')
# 2
m <- which(regions$Acr == 'CAC')
regions[m,4:7] <- c('1','1','1','1')
# 3
m <- which(regions$Acr == 'SAM')
regions[m,4:7] <- c('1','1','1','1')
# 4
m <- which(regions$Acr == 'TSA')
regions[m,4:7] <- c('1','1','1','1')
# 5
m <- which(regions$Acr == 'SAF')
regions[m,4:7] <- c('1','1','1','1')
# 6
m <- which(regions$Acr == 'EUM')
regions[m,4:7] <- c('1','1','1','1')
# 7
m <- which(regions$Acr == 'WCA')
regions[m,4:7] <- c('1','1','0','1')
# 8
m <- which(regions$Acr == 'SEA')
regions[m,4:7] <- c('1','1','1','1')
# 9
m <- which(regions$Acr == 'EAS')
regions[m,4:7] <- c('1','1','1','1')
# 10
m <- which(regions$Acr == 'OCE')
regions[m,4:7] <- c('1','1','0','1')

# dat <- dat[,which(names(dat) %in% c('lat','lon','latID','lonID','Name','CAC','EAS',
#                                     'EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA'))]

# 3.4 Saving ##########
write.csv(dat, file = paste0(fileloc2, 'IPCC6Regions/IPCC6RegionsMask.csv'), row.names = FALSE)
write.csv(regions, file = paste0(fileloc2, 'IPCC6Regions/IPCC6Regions.csv'), row.names = FALSE)
rm(refregions, grid, pts, Acr, Region, mask, m, regions)


# PART IV -- Regions Mask Orginal ##############################################
#     This section will convert region to a mask format
# 
# References: 

# 4.1 Variables Needed ###########
regions <- read_csv(paste0(fileloc1, 'BreadbasketRegions.csv'), 
                    col_names = TRUE) 

dat <- latlon %>%
  cbind(matrix('NOT', nrow=nrow(latlon), ncol = 1)) %>%
  cbind(matrix('Ocean', nrow=nrow(latlon), ncol = 1)) %>%
  cbind(matrix('NOT', nrow=nrow(latlon), ncol = 1)) %>%
  cbind(matrix('NOT', nrow=nrow(latlon), ncol = 1)) %>%
  cbind(matrix(0, nrow = nrow(latlon), ncol = (dim(regions)[1])))

colnames(dat) <- c(colnames(latlon), "Continent","Type","Name","Acronym", regions$Name)
dat$Continent <- as.character(dat$Continent)
dat$Type <- as.character(dat$Type)
dat$Name <- as.character(dat$Name)
dat$Acronym <- as.character(dat$Acronym)

# 4.2 Creating a data frame ##########
for (i in 1:dim(regions)[1]){
  lonR <- seq(regions$BLlon[i], regions$TRlon[i], by = 0.5)
  latR <- seq(regions$BLlat[i], regions$TRlat[i], by = 0.5)
  
  #Filling of the regional dataframe 
  for (j in 1:length(lonR)){
    m <- which(dat$lon == lonR[j] & dat$lat == latR[1])
    dat[m:(m+length(latR)-1), (5)] <- regions$Continent[i]
    dat[m:(m+length(latR)-1), (6)] <- regions$Type[i]
    dat[m:(m+length(latR)-1), (7)] <- regions$Name[i]
    dat[m:(m+length(latR)-1), (8)] <- regions$Acronym[i]
    dat[m:(m+length(latR)-1), (8+i)] <- 1
  }
}

regions <- cbind(regions$Name, regions$Acronym, regions$Type)
colnames(regions) <- c('Name','Acronym','Type')

# 4.3 Saving ###########
write.csv(dat, file = paste0(fileloc1, 'RegionsMask.csv'), row.names = FALSE)
write.csv(regions, file = paste0(fileloc1, 'Regions.csv'), row.names = FALSE)

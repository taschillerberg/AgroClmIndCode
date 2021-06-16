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
#      Updated: Dec. 2020

# setwd("~/OneDrive - Auburn University/Research/AgroclimaticConditions/Code")
# fileloc1 <- '~/OneDrive - Auburn University/Research/AgroclimaticConditions/Data/EarthStat/'
# fileloc2 <- '~/OneDrive - Auburn University/Research/AgroclimaticConditions/Data/'

setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/AgroclimaticConditions/Code")
fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/AgroclimaticConditions/Data/EarthStat/'
fileloc2 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/AgroclimaticConditions/Data/'

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
  add_column(cropland = -999) %>%
  add_column(maize = -999) %>%
  add_column(rice = -999) %>%
  add_column(soya = -999) %>%
  add_column(wheat = -999) %>%
  add_column(total_a = -999) %>%
  add_column(maize_a = -999) %>%
  add_column(rice_a = -999) %>%
  add_column(soya_a = -999) %>%
  add_column(wheat_a = -999) %>%
  add_column(cropland2 = -999) %>%
  add_column(maize2 = -999) %>%
  add_column(rice2 = -999) %>%
  add_column(soya2 = -999) %>%
  add_column(wheat2 = -999)

for (i in 1:length(crop)){
  #  Open the tiff files
  dat <- raster(paste(fileloc1, crop[i], loc, crop[i], loc, crop[i],
                      '_HarvestedAreaHectares.tif', sep=''))
  #  Look at the attributes
  dat
  #  Calculate and save the min and max values of the raster to the raster obj
  dat <- setMinMax(dat)
  #  Distribution of the raster pixel values
  # hist(dat, main = 'Distribution of Harvested Area Hectares', col = 'purple')
  #  Plot the raster data
  plot(dat, main = paste(crop[i], ' Harvested Area Hectares'))
  
  #  Re-Sampling raster to be the same resolution via calculating the sum of the
  #  finer resolution cells
  dat.resize <- raster::aggregate(dat, 6, fun=sum) %>%
    raster::flip(direction = 2) # Flips over the y axis
  dat.resize
  plot(dat.resize, main = paste(crop[i], ' Harvested Area Hectares Resized'))
  
  # Put into the dataframe 
  dat.resize.matrix <- as.matrix(dat.resize)
  dimnames(dat.resize.matrix) <- list(lat, lon)
  mask[,(i+10)] <- apply(X = latlon, MARGIN = 1, FUN = pt_latlon, datX = dat.resize.matrix)
}

rm(i, dat, dat.resize, dat.resize.matrix)

# 1.3 Fill and format the mask ##########
# Add the column areas together to get the total
mask$maize_a[is.na(mask$maize_a) == TRUE] <- NA
mask$rice_a[is.na(mask$maize_a) == TRUE] <- NA
mask$soya_a[is.na(mask$maize_a) == TRUE] <- NA
mask$wheat_a[is.na(mask$maize_a) == TRUE] <- NA

mask$maize[is.na(mask$maize_a) == TRUE] <- NA
mask$rice[is.na(mask$maize_a) == TRUE] <- NA
mask$soya[is.na(mask$maize_a) == TRUE] <- NA
mask$wheat[is.na(mask$maize_a) == TRUE] <- NA

# Add the column areas together to get the total
mask$total_a <- mask$maize_a+ mask$rice_a + mask$soya_a + mask$wheat_a

# Change land with no cropland to 0 and NA
mask$maize[mask$maize_a == 0] <- 0
mask$rice[mask$rice_a == 0] <- 0
mask$soya[mask$soya_a == 0] <- 0
mask$wheat[mask$wheat_a == 0] <- 0

mask$maize_a[mask$maize_a == 0] <- NA
mask$rice_a[mask$rice_a == 0] <- NA
mask$soya_a[mask$soya_a == 0] <- NA
mask$wheat_a[mask$wheat_a == 0] <- NA

# Converting to ones
mask$cropland[is.na(mask$total_a) == TRUE] <- NA
mask$cropland[mask$total_a == 0] <- 0
mask$cropland[mask$total_a > 0] <- 1
mask$maize[mask$maize_a > 0] <- 1
mask$rice[mask$rice_a > 0] <- 1
mask$soya[mask$soya_a > 0] <- 1
mask$wheat[mask$wheat_a > 0] <- 1

mask$total_a[mask$total_a == 0] <- NA

# 1.4 Save the mask ##########

outfile <- "EarthStat_Mask_HarvestedArea_sum_Dec2020"
write.csv(mask, file = paste(fileloc1, outfile, '.csv', sep = ''), row.names = FALSE)
print('End of Part I ctreation of the mask')
rm(outfile)
#      For the cropland, maize, rice, soya, wheat columns there is a 
# NA (ocean/water), 0 (land/non-cropland/crop), or 1 (cropland/crop). For the 
# remaining columns total_a, maize_a, rice_a, soya_a, wheat_a the fill values 
# are NA (ocean, water, non-cropland) or >0.00 (cropland in ha).

# PART II -- Planting and Harvesting ###########################################
#     This section will open the planning and harvesting nc files and create 
# a dataframe.
#
# References: Sacks et al. (2010)

# 2.1. Variables needed ##########
loc <- 'Sacks2010/'
crop <- c('Maize.crop', 'Rice.crop','Soybeans.crop', 'Wheat.crop', 
          'Wheat.Winter.crop')
season <- c('plant', 'plant.start', 'plant.end','plant.range', 
            'harvest', 'harvest.start','harvest.end','harvest.range')

# 2.2. Opening of files ##########

Crop_season <- list()

for (i in 1:length(crop)){
  crop_date <- tibble(latlon) %>%
    cbind(matrix(data = -999, nrow = dim(latlon)[1], ncol = length(season)))
  
  dat_nc <- ncdf4::nc_open(paste(fileloc2, loc, crop[i], 
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
  name <- crop[i]
  Crop_season[[name]] <- crop_date
}

###### Make sure that cropland and planting days match
mask$maize2 <- mask$maize
mask$rice2 <- mask$rice
mask$soya2 <- mask$soya
mask$wheat2 <- mask$wheat
mask$maize2[is.na(Crop_season[["Maize.crop"]]$plant) | is.na(Crop_season[["Maize.crop"]]$plant.start) | is.na(Crop_season[["Maize.crop"]]$plant.end)] <- NA
mask$maize2[is.na(Crop_season[["Maize.crop"]]$harvest) | is.na(Crop_season[["Maize.crop"]]$harvest.start) | is.na(Crop_season[["Maize.crop"]]$harvest.end)] <- NA
mask$rice2[is.na(Crop_season[["Rice.crop"]]$plant) | is.na(Crop_season[["Rice.crop"]]$plant.start) | is.na(Crop_season[["Rice.crop"]]$plant.end)] <- NA
mask$rice2[is.na(Crop_season[["Rice.crop"]]$harvest) | is.na(Crop_season[["Rice.crop"]]$harvest.start) | is.na(Crop_season[["Rice.crop"]]$harvest.end)] <- NA
mask$soya2[is.na(Crop_season[["Soybeans.crop"]]$plant) | is.na(Crop_season[["Soybeans.crop"]]$plant.start) | is.na(Crop_season[["Soybeans.crop"]]$plant.end)] <- NA
mask$soya2[is.na(Crop_season[["Soybeans.crop"]]$harvest) | is.na(Crop_season[["Soybeans.crop"]]$harvest.start) | is.na(Crop_season[["Soybeans.crop"]]$harvest.end)] <- NA
mask$wheat2[is.na(Crop_season[["Wheat.crop"]]$plant) | is.na(Crop_season[["Wheat.crop"]]$plant.start) | is.na(Crop_season[["Wheat.crop"]]$plant.end)] <- NA
mask$wheat2[is.na(Crop_season[["Wheat.crop"]]$harvest) | is.na(Crop_season[["Wheat.crop"]]$harvest.start) | is.na(Crop_season[["Wheat.crop"]]$harvest.end)] <- NA

mask$cropland2 <- mask$maize2 + mask$rice2 + mask$soya2 + mask$wheat2
mask$cropland2[mask$cropland2 > 1] <- 1
outfile <- "EarthStat_Mask_HarvestedArea_sum_Dec2020_2"
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
stop() 
# PART III -- Testing ##########################################################
#     This section is for testing the ploting and is not currently necessary to 
# include and run
#
# References: 

# 3.1 Calculating the differences ##########
# Visually the maps look similar which is why the filled version of the planting
# and harvesting dates were used, because otherwise there were regions that were
# left out. For example maize in N. America stretched into Canada but was not 
# represented in Sacks 2010 unfilled data

differences <- tibble(latlon) %>%
  matrix(data = -999, nrow = dim(latlon)[1], ncol = 8)




# 3.2 Ploting ##########
obs <- mask$cropland
df_obs <- data.frame(latlon$lat, latlon$lon, obs)

# If the y-axis needs flipped
# df_obs$lat <- df_obs$lat * -1
colnames(df_obs) <- c('lat', 'lon', 'obs')
limits <- round(range(df_obs$obs, na.rm = TRUE))

# world map 
baseData <- map_data('world')

# ploting and saving
p1_obs <- ggplot(data=df_obs, aes(x=lon,y=lat, fill=obs)) +   theme_bw() +
  labs(title=paste("Testing",sep=""), x="", y="")+ 
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile() + 
  # scale_fill_discrete(na.value="white", limits=limits)+
  scale_fill_gradient(low="blue", high="yellow", na.value="white", limits=limits) +
  #scale_colour_gradientn(name = "",colours = terrain.colors(10), 
  #                       limits=limits ,na.value="white") + 
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group), 
               colour="black", fill="white", alpha=0) +
  coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE)+ 
  theme(legend.position="bottom",legend.title = element_blank())+ 
  theme(legend.key.height  = unit(0.5, "cm"), 
        legend.direction="horizontal",legend.text = element_text(size = 15))+
  theme(plot.margin=margin(t=0,unit="cm"))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=10,face="bold"))+
  theme(legend.key.size = unit(1.5, "cm"))
plot(p1_obs)



# 3.3 Plotting for PowerPoint ##########
# includes regions

df_obs$obs[df_obs$obs == 0] <- NA
df_obs$obs[df_obs$obs == 1] <- 'crop'
mycolors <- c('#e4710d') # Same color as text on Powerpoint GSA Presentation
regions <- read_csv(paste(fileloc1, 'BreadbasketRegions.csv', sep=''), 
                    col_names = TRUE) 


p1_obs <- ggplot(data=na.omit(df_obs), aes(x=lon, y=lat, fill=obs)) +
  theme_bw() +
  geom_tile() +
  scale_fill_manual(values = mycolors, labels = c('Cropland')) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[1], xmax=regions$TRlon[1], 
  #                                      ymin=regions$BLlat[1], ymax=regions$TRlat[1], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[2], xmax=regions$TRlon[2], 
  #                                      ymin=regions$BLlat[2], ymax=regions$TRlat[2], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[3], xmax=regions$TRlon[3], 
  #                                      ymin=regions$BLlat[3], ymax=regions$TRlat[3], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[4], xmax=regions$TRlon[4], 
  #                                      ymin=regions$BLlat[4], ymax=regions$TRlat[4], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[5], xmax=regions$TRlon[5], 
  #                                      ymin=regions$BLlat[5], ymax=regions$TRlat[5], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[6], xmax=regions$TRlon[6], 
  #                                      ymin=regions$BLlat[6], ymax=regions$TRlat[6], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[7], xmax=regions$TRlon[7], 
  #                                      ymin=regions$BLlat[7], ymax=regions$TRlat[7], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[8], xmax=regions$TRlon[8], 
  #                                      ymin=regions$BLlat[8], ymax=regions$TRlat[8], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[9], xmax=regions$TRlon[9], 
  #                                      ymin=regions$BLlat[9], ymax=regions$TRlat[9], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[10], xmax=regions$TRlon[10], 
  #                                      ymin=regions$BLlat[10], ymax=regions$TRlat[10], fill=NA),
  #           color='#115fbf', size = 1) +
  # geom_rect(data = NULL, mapping = aes(xmin=regions$BLlon[11], xmax=regions$TRlon[11], 
  #                                      ymin=regions$BLlat[11], ymax=regions$TRlat[11], fill=NA),
  #           color='#115fbf', size = 1) +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  labs(title = 'Cropland', x = 'Longitude', y = 'Latitude')
ggsave(p1_obs, filename = paste0(fileloc1, "Cropland",'.tiff'), 
       width = 8, height = 5, dpi=350)



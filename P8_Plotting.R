# P8_Plotting.R
# This R script will make final figures to be used in presentations.
#
# T. A. Schillerberg
#               Jun. 2021
#      Updated: Jul. 2022

# Local Computer
setwd("")
fileloc1 <- '/Research/AgroclimaticConditions/Data/'
fileloc2 <- '/Research/AgroclimaticConditions/Results/'

loc1 <- 'Iizumi2020/gdhy_v1.2_v1.3_20190128/'
loc2 <- 'Global_AgroInd/Agro092021_mu/'
loc3 <- 'Global_AgroInd/Agro102021_RF/'
loc4 <- 'Global_AgroInd/Agro202303_Dist/'
loc5 <- 'Global_AgroInd/Agro102021_MK/'

# Variables to change ##########################################################
type <- 'M' #'OLS' 'M' "MM' 'TS' for when calculating categorical yield
per <- 'quartile'   # 'quartile'  'decile'  'nickle'   'sigmaOne'

# Libraries ####################################################################
library(tidyverse)
# library(BRRR)
library(viridis)
library(cowplot)
# library(gridExtra)
library(grid)
library(ggtext)
library(viridis)
library(RColorBrewer)
library(ggradar)

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
# SM Figure 1 #################################################################
# . SM F1.1 Variables Needed -------------------------------------------------
myFrostColors <- c('#998eC3','#F1a340')

lat <- seq(-89.75, 89.75, by = 0.5) # y-axis
lon <- seq(-179.75, 179.75, by = 0.5) # x-axis
latlon <- expand.grid(lat, lon) %>%
  rename('lat' = 'Var1') %>%
  rename('lon' = 'Var2') %>%
  cbind(NA)

baseData <- map_data('world')
agro1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp') [4]

myRegAlfa <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
               "Oceania","South & Southeast Asia", "Sub-Sahara Africa", "Temperate South America", "Tropical South America",
               "West-Central Asia")
myRegAlfaShort <- c("C America & Caribb.", "E Asia", "Europe & Mediterr.", "N America",
                    "Oceania","S & SE Asia", "Sub-Sahara Africa", "Temp S America", "Trop S America",
                    "W-C Asia")

# . SM F1.2 Opening Files --------------------------------------------------
dat <- read_csv(paste(fileloc1, loc2, agro1, '.csv', sep=''),
                col_names = TRUE, cols(.default = col_double()))
dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]

refregions_QGIS <- rgdal::readOGR(paste0(fileloc1,"IPCC6Regions/IPCC-WGI-reference-regions-v4_shapefile_2/RegionV1_2_a.shp"))
refregions_fortified <- broom::tidy(refregions_QGIS, region = "Name")

regionKey <- read_csv(paste0(fileloc1,"IPCC6Regions/IPCC6Regions.csv"), 
                      col_names = TRUE)

maizeYd <- read_csv(paste0(fileloc1, loc1, 'Yield_', 'maize', '_', type,'_',per,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
riceYd <- read_csv(paste0(fileloc1, loc1, 'Yield_', 'rice', '_', type,'_',per,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
soyaYd <- read_csv(paste0(fileloc1, loc1, 'Yield_', 'soya', '_', type,'_',per,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
wheatYd <- read_csv(paste0(fileloc1, loc1, 'Yield_', 'wheat', '_', type,'_',per,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . SM F1.3 Calculation -------------------------------------------------------
mu <- apply(dat[,5:ncol(dat)], MARGIN = 1, FUN= mean, na.rm=TRUE)
m <- which(mu >= 1)
latlon[m,3] <- 'temperate'
m <- which(mu < 1)
latlon[m,3] <- 'tropical'

df_obs <- cbind(latlon, maizeYd$y2002, riceYd$y2004, soyaYd$y2007, wheatYd$y2002)
colnames(df_obs) <- c('lat', 'lon', 'Frost', 'Maize','Rice','Soya','Wheat')
df_obs$Maize[df_obs$Maize == 2 | df_obs$Maize == 3 | df_obs$Maize == 4] <- 1
df_obs$Rice[df_obs$Rice == 2 | df_obs$Rice == 3 | df_obs$Rice == 4] <- 1
df_obs$Soya[df_obs$Soya == 2 | df_obs$Soya == 3 | df_obs$Soya == 4] <- 1
df_obs$Wheat[df_obs$Wheat == 2 | df_obs$Wheat == 3 | df_obs$Wheat == 4] <- 1

df_obs$Maize[df_obs$Maize == 1 & df_obs$Frost == 'temperate']<- 'temperate'
df_obs$Maize[df_obs$Maize == 1 & df_obs$Frost == 'tropical']<- 'tropical'
df_obs$Maize[df_obs$Maize == 1 ]<- NA
df_obs$Rice[df_obs$Rice == 1 & df_obs$Frost == 'temperate']<- 'temperate'
df_obs$Rice[df_obs$Rice == 1 & df_obs$Frost == 'tropical']<- 'tropical'
df_obs$Rice[df_obs$Rice == 1 ]<- NA
df_obs$Soya[df_obs$Soya == 1  & df_obs$Frost == 'temperate']<- 'temperate'
df_obs$Soya[df_obs$Soya == 1 & df_obs$Frost == 'tropical']<- 'tropical'
df_obs$Soya[df_obs$Soya == 1 ]<- NA
df_obs$Wheat[df_obs$Wheat == 1 & df_obs$Frost == 'temperate']<- 'temperate'
df_obs$Wheat[df_obs$Wheat == 1 & df_obs$Frost == 'tropical']<- 'tropical'
df_obs$Wheat[df_obs$Wheat == 1 ]<- NA

# . SM F1.4 Plotting ----------------------------------------------------------
p1 <- ggplot(data=df_obs, aes(x=lon, y=lat, fill=Maize)) +
  theme_bw() +
  geom_tile() +
  scale_fill_manual(values = myFrostColors, labels = c('Temperate','Tropics'),
                    na.value = NA) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", size=0.5) +
  geom_polygon(data = refregions_fortified, aes( x = long, y = lat, group = group),
               fill="NA", color="red", size = 1) +
  annotate(geom = 'text', x=-110, y=10,  label = '1', color='blue', size=10) +
  annotate(geom = 'text', x=110,  y=35,  label = '2', color='blue', size=10) +
  annotate(geom = 'text', x=-20,  y=50,  label = '3', color='blue', size=10) +
  annotate(geom = 'text', x=-100, y=50,  label = '4', color='blue', size=10) +
  annotate(geom = 'text', x=130,  y=-25, label = '5', color='blue', size=10) +
  annotate(geom = 'text', x=86,   y=0,   label = '6', color='blue', size=10) +
  annotate(geom = 'text', x=25,   y=0,   label = '7',color='blue', size=10) +
  annotate(geom = 'text', x=-63,  y=-33, label = '8', color='blue', size=10) +
  annotate(geom = 'text', x=-60,  y=-7,  label = '9', color='blue', size=10) +
  annotate(geom = 'text', x=50,   y=27,  label = '10', color='blue', size=10) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")

p2 <-ggplot(data=df_obs, aes(x=lon, y=lat, fill=Rice)) +
  theme_bw() +
  geom_tile() +
  scale_fill_manual(values = myFrostColors, labels = c('Temperate','Tropics'),
                    na.value = NA) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")

p3 <-ggplot(data=df_obs, aes(x=lon, y=lat, fill=Soya)) +
  theme_bw() +
  geom_tile() +
  scale_fill_manual(values = myFrostColors, labels = c('Temperate','Tropics'),
                    na.value = NA) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")

p4 <-ggplot(data=df_obs, aes(x=lon, y=lat, fill=Wheat)) +
  theme_bw() +
  geom_tile() +
  scale_fill_manual(values = myFrostColors, labels = c('Temperate','Tropics'),
                    na.value = NA) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")

myLegend <- ggplot(data=na.omit(df_obs), aes(x=lon, y=lat, fill=Frost)) +
  theme_bw() +
  geom_tile() +
  scale_fill_manual(values = myFrostColors, labels = c('Temperate','Tropics')) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
myLegend <- get_legend(myLegend, position = 'right' ) %>% as_ggplot()

# . SM F1.5 Make table --------------------------------------------------------
regions_new <- regionKey[,-3]
m <- which(regions_new$Acr %in% c('NAM','CAC','SAM','TSA','SAF','EUM','WCA','SEA','EAS','OCE'))
regions_new <- regions_new[m,-2]
regions_new <- cbind(regions_new, matrix(ncol=1, nrow = dim(regions_new)[1]))
colnames(regions_new)[6] <- 'Regional Crops'
for (i in 1:dim(regions_new)[1]){
  crop <- ''
  if (regions_new$Maize[i] == 1){ crop <- paste0(crop,'Maize ')}
  if (regions_new$Rice[i] == 1){ crop <- paste0(crop,'Rice ')}
  if (regions_new$Soy[i] == 1){ crop <- paste0(crop,'Soy ')}
  if (regions_new$Wheat[i] == 1){ crop <- paste0(crop,'Wheat ')}
  regions_new$"Regional Crops"[i] <- crop
}
regions_new <- regions_new[,c(1,6)] %>%
  arrange(Region)
m <- which(regions_new$Region == 'Temprate South America')
regions_new$Region[8] <- 'Temperate South America'
regions_new$Region <- paste0(regions_new$Region, ' (', myRegAlfaShort, ')')

# . SM F1.6 Combine ----------------------------------------------------------
SMF1A <- plot_grid(p1, p2,
                   p3, p4,
                   labels = c("A","B",
                              "C","D"),
                   label_size = 14,
                   rel_widths = c(1,1),
                   rel_heights = c(1,1),
                   nrow = 2)
SMF1B <- plot_grid(gridExtra::tableGrob(regions_new), myLegend,
                   rel_widths = c(1,.5),
                   nrow= 1)
SMF1 <- plot_grid(SMF1A,
                  SMF1B,
                  rel_heights = c(1.4,0.90),
                  nrow = 2)

ggsave(SMF1, filename = paste0(fileloc2, "SMFigure1.tiff"),
       width = 10, height = 10, dpi = 350, bg='white')
# ggsave(SMF1, filename = paste0(fileloc2, "SMFigure1.tiff"),
#        width = 7, height = 7, dpi = 350, bg='white')
#
rm(list=ls()[! ls() %in% c('fileloc1','fileloc2','loc1','loc2','loc3','loc4','loc5','type','per',
                           'get_legend','as_ggplot','craftbrewer_pal','scale_fill_craftfermenter')])
# Figure 1 #####################################################################
# . F1.1 Variables Needed ------------------------------------------------------
cropx <- c('maize', 'rice','soya', 'wheat')

myFrostColors <- c("Total"="#000000","Temperate"="#998eC3","Tropical"="#F1a340")
myFrostLevels <- c("Temperate","Tropics")
myFrostColorAcr <- c("Temperate"="#998eC3","Tropical"="#F1a340")

myRegAlfa <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
               "Oceania","South & Southeast Asia", "Sub-Sahara Africa", "Temperate South America", "Tropical South America",
               "West-Central Asia")
myRegAlfaShort <- c("C America & Caribb.", "E Asia", "Europe & Mediterr.", "N America",
                    "Oceania","S & SE Asia", "Sub-Sahara Africa", "Temp S America", "Trop S America",
                    "W-C Asia")
myRegAlfaAcr <- c('CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA')

myRegTT <- c( "East Asia", "Europe & Mediterranean", "North America","West-Central Asia",'Temperate',
              "Central America & Caribbean","Oceania","South & Southeast Asia", "Sub-Sahara Africa", 
              "Temperate South America", "Tropical South America",'Tropics')
myRegTTShort <- c( "E Asia", "Europe & Mediterr.", "N America","W-C Asia",'**Temperate**',
                   "C America & Caribb.","Oceania","S & SE Asia", "Sub-Sahara Africa", 
                   "Temp S America", "Trop S America",'**Tropics**')
myRegTTAcr <- c('EAS','EUM','NAM','WCA', 'Temp','CAC','OCE','SEA','SAF','TSA','SAM','Tropics')

myColors <- c("Central America & Caribbean"="#DAA51B","East Asia"="#52BCA3",
              "Europe & Mediterranean"="#2F8AC4", "North America"="#CC3A8E", "Oceania"="#E58606",
              "South & Southeast Asia"="#99C945", "Sub-Sahara Africa"="#ED645A", "Temperate South America"="#764E9F",
              "Tropical South America"="#5D69B1", "West-Central Asia" = "#24796C", "Temperate"="#4d4e4f", "Tropics"="#adb4b8")
myColorsAcr <- c("CAC"="#DAA51B","EAS"="#52BCA3",
                 "EUM"="#2F8AC4", "NAM"="#CC3A8E", "OCE"="#E58606",
                 "SEA"="#99C945", "SAF"="#ED645A", "TSA"="#764E9F",
                 "SAM"="#5D69B1", "WCA" = "#24796C", "Temp"="#4d4e4f", "Tropics"="#adb4b8")

myBreaks <- seq(from=1982, to=2016, by=2)
myBreaksFail <- seq(0,100,25)

# . F1.2 Opening files ---------------------------------------------------------
GlobEvents <- read_csv(paste0(fileloc1, loc4, 'GlobalFailureHighYieldEvents_',type,'_',per, '.csv'), 
                       col_names = TRUE) 

# . F1.3 Panel A Failure Timeseries --------------------------------------------
# . . F1.3.a Plotting ====
# Miaze
dat <- subset(GlobEvents, Crop == cropx[1]) %>%
  subset(Region == 'Total' | Region == 'Temperate'| Region == 'Tropical')
p1 <- ggplot(data= dat, aes(x=Year, y= FailPer, group = Region, color= Region)) +theme_bw() +
  theme(plot.title = element_text(size = 20)) +
  scale_color_manual(values= myFrostColors, labels = c('Total','Temperate','Tropical')) +
  labs(title=paste(""), x="Year",
       y="Cropland experiencing synchronized failure (%)", fill = "Region") +
  geom_line(size=2) +
  geom_point() +
  scale_x_continuous(limits = c(1981.4,2016.6), breaks=myBreaks, expand=c(0,0),
                     guide=guide_axis(angle=25)) +
  guides(color=guide_legend(title=" "))

# Rice
dat <- subset(GlobEvents, Crop == cropx[2]) %>%
  subset(Region == 'Total' | Region == 'Temperate'| Region == 'Tropical')
p2 <- ggplot(data= dat, aes(x=Year, y= FailPer, group = Region, color= Region)) +theme_bw() +
  theme(plot.title = element_text(size = 20)) +
  scale_color_manual(values= myFrostColors, labels = c('Total','Temperate','Tropical')) +
  labs(title=paste(""), x="Year",
       y="Cropland experiencing synchronized failure (%)", fill = "Region") +
  geom_line(size=2) +
  geom_point() +
  scale_x_continuous(limits = c(1981.4,2016.6), breaks=myBreaks, expand=c(0,0),
                     guide=guide_axis(angle=25)) +
  theme(legend.position = "NULL")

# Soya
dat <- subset(GlobEvents, Crop == cropx[3]) %>%
  subset(Region == 'Total' | Region == 'Temperate'| Region == 'Tropical')
p3 <- ggplot(data= dat, aes(x=Year, y= FailPer, group = Region, color= Region)) +theme_bw() +
  theme(plot.title = element_text(size = 20)) +
  scale_color_manual(values= myFrostColors, labels = c('Total','Temperate','Tropical')) +
  labs(title=paste(""), x="Year",
       y="Cropland experiencing synchronized failure (%)", fill = "Region") +
  geom_line(size=2) +
  geom_point() +
  scale_x_continuous(limits = c(1981.4,2016.6), breaks=myBreaks, expand=c(0,0),
                     guide=guide_axis(angle=25)) +
  theme(legend.position = "NULL")

# Wheat
dat <- subset(GlobEvents, Crop == cropx[4]) %>%
  subset(Region == 'Total' | Region == 'Temperate'| Region == 'Tropical')
p4 <- ggplot(data= dat, aes(x=Year, y= FailPer, group = Region, color= Region)) +theme_bw() +
  theme(plot.title = element_text(size = 20)) +
  scale_color_manual(values= myFrostColors, labels = c('Total','Temperate','Tropical')) +
  labs(title=paste(""), x="Year",
       y="Cropland experiencing synchronized failure (%)", fill = "Region") +
  geom_line(size=2) +
  geom_point() +
  scale_x_continuous(limits = c(1981.4,2016.6), breaks=myBreaks, expand=c(0,0),
                     guide=guide_axis(angle=25)) +
  theme(legend.position = "NULL")

# . . F1.3.b Legend & Combine ====
myLegend <- get_legend(p1, position = 'right' ) %>% as_ggplot()
p1 <- p1 +
  theme(legend.position = "NULL")

pA <- plot_grid(p1,p2,p3,p4,myLegend,
                nrow = 1,
                labels = c('A','B','C','D',''),
                rel_widths = c(1,1,1,1,0.25))

# . F1.4 Panel B Contributions -------------------------------------------------
# . . F1.4.a Calculations ====
datGlob <- GlobEvents %>%
  subset(Region == "Total")
failRegGlob <- as_tibble(matrix(nrow=0, ncol=4), .name_repair = "unique")

# Regional Contributions
for (i in 1:length(myRegAlfaAcr)){
  dat <- GlobEvents %>% subset(Region == myRegAlfaAcr[i])
  x <- cbind(dat$Region, dat$Year) %>%
    cbind(dat$Crop) %>%
    cbind(round(dat$FailCells/datGlob$FailCells,4)) %>%
    cbind(round(dat$RegionalCells/datGlob$RegionalCells,4))
  failRegGlob <- rbind(failRegGlob,x)
}
colnames(failRegGlob) <- c('Region','Year','Crop','FailPer', "CropPer")
failRegGlob$Year <- as.numeric(as.character(failRegGlob$Year))
failRegGlob$FailPer <- as.numeric(as.character(failRegGlob$FailPer))
failRegGlob$CropPer <- as.numeric(as.character(failRegGlob$CropPer))

# Find the limits for the color ramp
upLim <- range(failRegGlob$FailPer, na.rm = TRUE)[2] %>%
  round(digits=4) + 0.04
myBreaksFail <- seq(0,upLim,round(upLim/4, digits = 2))

# . . F1.4.b Plotting ====
# Maize 
dat <- failRegGlob %>%
  subset(Crop == cropx[1])
dat <- dat %>%
  mutate(Region = factor(Region, levels=myRegAlfaAcr)) %>%
  arrange(Region)
p1 <- ggplot(dat, aes(x = Year, y=Region, fill = FailPer)) +
  theme_bw() +
  geom_tile() + 
  metR::geom_contour_fill(aes(z = FailPer)) +
  scale_fill_craftfermenter(
    breaks = myBreaksFail, 
    na.value=NA,
    direction = 1,
    palette = "YlGn", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=1,
      title=NULL)) +
  scale_y_discrete(labels=myRegAlfaShort, guide=guide_axis(angle=10)) +
  labs(title="", y="", x="Year") + 
  scale_x_continuous(limits = c(1981.4,2016.6), breaks=myBreaks, expand=c(0,0),
                     guide=guide_axis(angle=25))

# Rice 
dat <- failRegGlob %>%
  subset(Crop == cropx[2])
dat <- dat %>%
  mutate(Region = factor(Region, levels=myRegAlfaAcr)) %>%
  arrange(Region)
p2 <- ggplot(dat, aes(x = Year, y=Region, fill = FailPer)) +
  theme_bw() +
  geom_tile() + 
  metR::geom_contour_fill(aes(z = FailPer)) +
  scale_fill_craftfermenter(
    breaks = myBreaksFail, 
    na.value=NA,
    direction = 1,
    palette = "YlGn", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=1,
      title=NULL)) +
  scale_y_discrete(labels=NULL, guide=guide_axis(angle=10)) +
  labs(title="", y="", x="Year") +
  scale_x_continuous(limits = c(1981.4,2016.6), breaks=myBreaks, expand=c(0,0),
                     guide=guide_axis(angle=25))+ 
  theme(legend.position = "NULL")

# Soy
dat <- failRegGlob %>%
  subset(Crop == cropx[3])
dat <- dat %>%
  mutate(Region = factor(Region, levels=myRegAlfaAcr)) %>%
  arrange(Region)
p3 <- ggplot(dat, aes(x = Year, y=Region, fill = FailPer)) +
  theme_bw() +
  geom_tile() + 
  metR::geom_contour_fill(aes(z = FailPer)) +
  scale_fill_craftfermenter(
    breaks = myBreaksFail, 
    na.value=NA,
    direction = 1,
    palette = "YlGn", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=1,
      title=NULL)) +
  scale_y_discrete(labels=NULL, guide=guide_axis(angle=10)) +
  labs(title="", y="", x="Year") + 
  scale_x_continuous(limits = c(1981.4,2016.6), breaks=myBreaks, expand=c(0,0),
                     guide=guide_axis(angle=25))+ 
  theme(legend.position = "NULL")

# Wheat
dat <- failRegGlob %>%
  subset(Crop == cropx[1])
dat <- dat %>%
  mutate(Region = factor(Region, levels=myRegAlfaAcr)) %>%
  arrange(Region)
p4 <- ggplot(dat, aes(x = Year, y=Region, fill = FailPer)) +
  theme_bw() +
  geom_tile() + 
  metR::geom_contour_fill(aes(z = FailPer)) +
  scale_fill_craftfermenter(
    breaks = myBreaksFail, 
    na.value=NA,
    direction = 1,
    palette = "YlGn", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=1,
      title=NULL)) +
  scale_y_discrete(labels=NULL, guide=guide_axis(angle=10)) +
  labs(title="", y="", x="Year") + 
  scale_x_continuous(limits = c(1981.4,2016.6), breaks=myBreaks, expand=c(0,0),
                     guide=guide_axis(angle=25))+ 
  theme(legend.position = "NULL")

# . . F1.4.c Legend & Combine ====
myLegend <- get_legend(p1, position = 'right' ) %>% as_ggplot()
p1 <- p1 +
  theme(legend.position = "NULL")
pB <- plot_grid(p1,p2,p3,p4,myLegend,
                nrow = 1,
                labels = c('E','F','G','H',''),
                rel_widths = c(1.3,1,1,1,0.25))

# . F1.4 Panel C Contributions Cropland ----------------------------------------
# . . F1.4.a Calculations ====
regGlob <- as_tibble(matrix(nrow=0, ncol=4), .name_repair = "unique")

for (i in 1:length(myRegAlfaAcr)){
  y <- failRegGlob %>% subset(Region == myRegAlfaAcr[i]) %>%
    subset(Crop == cropx[1])
  y <- mean(y$FailPer, na.rm = TRUE) %>%
    round(4)
  x <- failRegGlob %>% subset(Region == myRegAlfaAcr[i]) %>%
    subset(Crop == cropx[1])
  x <- mean(x$CropPer, na.rm = TRUE) %>%
    round(4)
  m <- cbind(myRegAlfaAcr[i], cropx[1]) %>%
    cbind(y) %>%
    cbind(x)
  
  y <- failRegGlob %>% subset(Region == myRegAlfaAcr[i]) %>%
    subset(Crop == cropx[2])
  y <- mean(y$FailPer, na.rm = TRUE) %>%
    round(4)
  x <- failRegGlob %>% subset(Region == myRegAlfaAcr[i]) %>%
    subset(Crop == cropx[2])
  x <- mean(x$CropPer, na.rm = TRUE) %>%
    round(4)
  r <- cbind(myRegAlfaAcr[i], cropx[2]) %>%
    cbind(y) %>%
    cbind(x)
  
  y <- failRegGlob %>% subset(Region == myRegAlfaAcr[i]) %>%
    subset(Crop == cropx[3])
  y <- mean(y$FailPer, na.rm = TRUE) %>%
    round(4)
  x <- failRegGlob %>% subset(Region == myRegAlfaAcr[i]) %>%
    subset(Crop == cropx[3])
  x <- mean(x$CropPer, na.rm = TRUE) %>%
    round(4)
  s <- cbind(myRegAlfaAcr[i], cropx[3]) %>%
    cbind(y) %>%
    cbind(x)
  
  y <- failRegGlob %>% subset(Region == myRegAlfaAcr[i]) %>%
    subset(Crop == cropx[4])
  y <- mean(y$FailPer, na.rm = TRUE) %>%
    round(4)
  x <- failRegGlob %>% subset(Region == myRegAlfaAcr[i]) %>%
    subset(Crop == cropx[4])
  x <- mean(x$CropPer, na.rm = TRUE) %>%
    round(4)
  w <- cbind(myRegAlfaAcr[i], cropx[4]) %>%
    cbind(y) %>%
    cbind(x)
  
  regGlob <- rbind(regGlob,m) %>%
    rbind(r) %>%
    rbind(s) %>%
    rbind(w)
}
colnames(regGlob) <- c('Region','Crop','yFail','xCropland')
regGlob$yFail <- as.numeric(as.character(regGlob$yFail)) 
regGlob$xCropland <- as.numeric(as.character(regGlob$xCropland))
# . . F1.4.b Plotting ====
dat <- subset(regGlob, Crop == cropx[1])
p1 <- ggplot(data= dat, aes(x = xCropland, y=yFail)) +
  geom_point(size = 2, aes(color=Region)) +
  labs(title = '', x = 'Cropland Ratio', y= 'Failure Ratio') +
  scale_color_manual(values= myColorsAcr, limits = myRegAlfaAcr, labels = myRegAlfaShort)  +
  geom_abline(intercept = 0, slope= 1, color = 'grey50', linetype =2) +
  ylim(0, 0.33) + xlim(0, 0.33) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_light() +
  theme(legend.text = element_markdown())

dat <- subset(regGlob, Crop == cropx[2])
p2 <- ggplot(data= dat, aes(x = xCropland, y=yFail)) +
  geom_point(size = 2, aes(color=Region)) +
  labs(title = '', x = 'Cropland Ratio', y= '') +
  scale_color_manual(values= myColorsAcr, limits = myRegAlfaAcr, labels = myRegAlfaShort)  +
  geom_abline(intercept = 0, slope= 1, color = 'grey50', linetype =2) +
  ylim(0, 0.33) + xlim(0, 0.33) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_light() +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "NULL")

dat <- subset(regGlob, Crop == cropx[3])
p3 <- ggplot(data= dat, aes(x = xCropland, y=yFail)) +
  geom_point(size = 2, aes(color=Region)) +
  labs(title = '', x = 'Cropland Ratio', y= '') +
  scale_color_manual(values= myColorsAcr, limits = myRegAlfaAcr, labels = myRegAlfaShort)  +
  geom_abline(intercept = 0, slope= 1, color = 'grey50', linetype =2) +
  ylim(0, 0.33) + xlim(0, 0.33) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_light() +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "NULL")

dat <- subset(regGlob, Crop == cropx[4])
p4 <- ggplot(data= dat, aes(x = xCropland, y=yFail)) +
  geom_point(size = 2, aes(color=Region)) +
  labs(title = '', x = 'Cropland Ratio', y= '') +
  scale_color_manual(values= myColorsAcr, limits = myRegAlfaAcr, labels = myRegAlfaShort)  +
  geom_abline(intercept = 0, slope= 1, color = 'grey50', linetype =2) +
  ylim(0, 0.33) + xlim(0, 0.33) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_light() +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "NULL")

# . . F1.4.c Legend & Combine ====
myLegend2 <- get_legend(p1, position = 'right' ) %>% as_ggplot()
p1 <- p1 +
  theme(legend.position = "NULL")
pC <- plot_grid(p1,p2,p3,p4,myLegend2,
                nrow = 1,
                labels = c('I','J','K','L',''),
                rel_widths = c(1,1,1,1,0.4))

# . F1.5 Final Figure ----------------------------------------------------------
F1 <- plot_grid(pA,
                pB,
                pC,
                nrow = 3,
                rel_widths = c(1,1,1))
ggsave(F1, filename = paste(fileloc2,'Figure1', ".tiff", sep=''),
       width = 20, height = 13, dpi = 350, bg='white')

rm(list=ls()[! ls() %in% c('fileloc1','fileloc2','loc1','loc2','loc3','loc4','loc5','type','per',
                           'get_legend','as_ggplot','craftbrewer_pal','scale_fill_craftfermenter')])


# Figure 2 #####################################################################
#     This section will display RF preformance: ROC & AUC values. 
#
# References: https://www.py4u.net/discuss/881533
# . F2.1 Variables Needed -----------------------------------------------------
cropx <- c('maize', 'rice','soya', 'wheat')

myColorsAcr <- c("CAC"="#DAA51B","EAS"="#52BCA3",
                 "EUM"="#2F8AC4", "NAM"="#CC3A8E", "OCE"="#E58606",
                 "SEA"="#99C945", "SAF"="#ED645A", "TSA"="#764E9F",
                 "SAM"="#5D69B1", "WCA" = "#24796C", "Temp"="#4d4e4f", "Tropics"="#adb4b8")

myRegAlfa <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
               "Oceania","South & Southeast Asia", "Sub-Sahara Africa", "Temperate South America", "Tropical South America",
               "West-Central Asia",'**Temperate**','**Tropics**')
myRegAlfaShort <- c("C America & Caribb.", "E Asia", "Europe & Mediterr.", "N America",
                    "Oceania","S & SE Asia", "Sub-Sahara Africa", "Temp S America", "Trop S America",
                    "W-C Asia",'**Temperate**','**Tropics**')

myRegAlfaAcr <- c('CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA', 'Temp','Tropics')

# . F2.2 Opening Files --------------------------------------------------------
ROCReg <- read_csv(paste0(fileloc1, loc3, 'ROCRegional_RF_',type,'_',per, '.csv'), 
                   col_names = TRUE) 
ROCRegCV <- read_csv(paste0(fileloc1, loc3, 'ROCRegional_RF_CV_',type,'_',per, '.csv'), 
                     col_names = TRUE) 
AUCReg <- read_csv(paste0(fileloc1, loc3, 'AUCRegional_RF_',type,'_',per, '.csv'),
                   col_names = TRUE)
AUCRegCV <- read_csv(paste0(fileloc1, loc3, 'AUCRegional_RF_CV_',type,'_',per, '.csv'),
                     col_names = TRUE)

# . F2.3 Formating data frame -------------------------------------------------
ROCReg <- ROCReg %>%
  add_column(LineWidth =1)
m <- which(ROCReg$Region == 'FrFree');  ROCReg$Region[m] = 'Tropics'; ROCReg$LineWidth[m] = 2.3
m <- which(ROCReg$Region == 'Temp');    ROCReg$LineWidth[m] = 2.3
m <- which(ROCRegCV$Region == 'FrFree');  ROCRegCV$Region[m] = 'Tropics'

m <- which(AUCReg$Region == 'FrFree');  AUCReg$Region[m] = 'Tropics'
m <- which(AUCRegCV$Region == 'FrFree');  AUCRegCV$Region[m] = 'Tropics'

# . F2.4 Plotting ROC Curves --------------------------------------------------
# . . F2.4.a Maize ====
ROC <- subset(ROCReg, Crop == cropx[1])
ROC2 <- na.omit(ROC)
m <- which(myRegAlfaAcr %in% unique(ROC2$Region))
myRegAlfa2 <- myRegAlfa[m]
myColorsAcr2 <- myColorsAcr[m]
myRegAlfaAcr2 <- myRegAlfaAcr[m]

p1 <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`)) +
  geom_path(data= ROC, aes(color = Region), size = ROC$LineWidth) +
  labs(title = '') +
  scale_color_manual(values= myColorsAcr2, limits = myRegAlfaAcr2, labels = myRegAlfaShort) +
  geom_abline(intercept = 0, slope= 1, color = 'grey50', linetype =2) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_light() +
  theme(legend.text = element_markdown())

# . . F2.4.b Rice ====
ROC <- subset(ROCReg, Crop == cropx[2])
ROC2 <- na.omit(ROC)
m <- which(myRegAlfaAcr %in% unique(ROC2$Region))
myRegAlfa2 <- myRegAlfa[m]
myColorsAcr2 <- myColorsAcr[m]
myRegAlfaAcr2 <- myRegAlfaAcr[m]

p2 <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`)) +
  geom_path(data= ROC, aes(color = Region), size = ROC$LineWidth) +
  labs(title = '') +
  scale_color_manual(values= myColorsAcr2, limits = myRegAlfaAcr2, labels = myRegAlfa2) +
  geom_abline(intercept = 0, slope= 1, color = 'grey50', linetype =2) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_light() +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "NULL")

# . . F2.4.c Soy ====
ROC <- subset(ROCReg, Crop == cropx[3])
ROC2 <- na.omit(ROC)
m <- which(myRegAlfaAcr %in% unique(ROC2$Region))
myRegAlfa2 <- myRegAlfa[m]
myColorsAcr2 <- myColorsAcr[m]
myRegAlfaAcr2 <- myRegAlfaAcr[m]

p3 <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`)) +
  geom_path(data= ROC, aes(color = Region), size = ROC$LineWidth) +
  labs(title = '') +
  scale_color_manual(values= myColorsAcr2, limits = myRegAlfaAcr2, labels = myRegAlfa2) +
  geom_abline(intercept = 0, slope= 1, color = 'grey50', linetype =2) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_light() +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "NULL")

# . . F2.4.d Wheat ====
ROC <- subset(ROCReg, Crop == cropx[4])
ROC2 <- na.omit(ROC)
m <- which(myRegAlfaAcr %in% unique(ROC2$Region))
myRegAlfa2 <- myRegAlfa[m]
myColorsAcr2 <- myColorsAcr[m]
myRegAlfaAcr2 <- myRegAlfaAcr[m]

p4 <- ggplot(data= NULL, aes(x = `False positive rate`, y=`True positive rate`)) +
  geom_path(data= ROC, aes(color = Region), size = ROC$LineWidth) +
  labs(title = '') +
  scale_color_manual(values= myColorsAcr2, limits = myRegAlfaAcr2, labels = myRegAlfa2) +
  geom_abline(intercept = 0, slope= 1, color = 'grey50', linetype =2) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_light() +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "NULL")

# . F2.5 Legend ---------------------------------------------------------------
myLegend <- get_legend(p1, position = 'right' ) %>% as_ggplot()
p1 <- p1 +
  theme(legend.position = "NULL")

# . F2.6 AUC Table ------------------------------------------------------------
# Ref: https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
AUC <- AUCReg %>% spread(Crop, AUC) 
AUC <- arrange(AUC, Region)

AUC2 <- AUC 
AUC2$maize <- as.character(AUC2$maize); AUC2$rice <- as.character(AUC2$rice)
AUC2$soya <- as.character(AUC2$soya);   AUC2$wheat <- as.character(AUC2$wheat)
#Means and SD
for (i in 1:4){
  for (j in 1:dim(AUC)[1]){
    m <- which(AUCRegCV$Crop == cropx[i] & AUCRegCV$Region == AUC$Region[j])
    AUCMu <- mean(c(AUCRegCV$AUC[m],as.numeric(AUC[j,(1+i)])), na.rm = TRUE) %>%
      round(digits=4)
    AUCSD <- sd(c(AUCRegCV$AUC[m],as.numeric(AUC[j,(1+i)])), na.rm = TRUE) %>%
      round(digits = 4)
    value <- paste0(AUCMu)
    # value <- paste0(AUCMu,' (',AUCSD,')')
    AUC2[j,(1+i)] <- value
  }
}

# Regional Mean
for (j in 1:dim(AUC)[1]){
  m <- which(AUCRegCV$Region == AUC$Region[j])
  AUCMu <- mean(c(AUCRegCV$AUC[m],as.numeric(AUC[j,(1+i)])), na.rm = TRUE) %>%
    round(digits=4)
  AUCSD <- sd(c(AUCRegCV$AUC[m],as.numeric(AUC[j,(1+i)])), na.rm = TRUE) %>%
    round(digits = 4)
  value <- paste0(AUCMu)
  # value <- paste0(AUCMu,' (',AUCSD,')')
  AUC2[j,6] <- value
}

AUC2 <- AUC2 %>% arrange(factor(Region, levels = myRegAlfaAcr))
AUC2$Region <- c(myRegAlfaShort[1:10], 'Temperate','Tropics')
AUC2$soya[which(AUC2$soya == "NaN")] <- '-'
# AUC2$soya[which(AUC2$soya == "NaN (NA)")] <- '-'
colnames(AUC2) <- c("Region","Maize","Rice","Soy","Wheat","Mean")

t4 <- gridExtra::tableGrob(AUC2, rows=NULL)


# . F2.5 Combine & Save -------------------------------------------------------
pA <- plot_grid(p1, p2,
                p3, p4,
                nrow = 2,
                labels = c('A','B','C','D'),
                rel_widths = c(1,1))
pB <- plot_grid(myLegend, t4,
                ncol = 2,
                labels = c("","E"),
                rel_widths=c(1,1))
F2 <- plot_grid(pA,
                pB,
                nrow =2,
                rel_heights = c(1,.705))
ggsave(F2, filename = paste(fileloc2,'Figure2', ".tiff", sep=''),
       width = 12, height = 11, dpi = 350, bg='white')

rm(list=ls()[! ls() %in% c('fileloc1','fileloc2','loc1','loc2','loc3','loc4','loc5','type','per',
                           'get_legend','as_ggplot','craftbrewer_pal','scale_fill_craftfermenter')])


# Figure 3 #####################################################################
# . F3.1 Variables Needed ------------------------------------------------------
myRegAlfaLong <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
                   "Oceania","South & Southeast Asia", "Sub-Sahara Africa", "Temperate South America", "Tropical South America",
                   "West-Central Asia",'Temperate','Tropics')

myRegAlfaAcr <- c('CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA', 'Temp','Tropics')

agro1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp')
agro2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')
myAgro <- c(agro1, agro2)

myAgroShort <- c('SpFrost', "FallFrost", "GrowSeason", "FrostDays", "StFieldOp", "GDD",
                 "HeatDays", "Precip",   "Dry Days", "FieldCondP", "FieldCondM", "FieldCondH")
# . F3.2 Opening files ---------------------------------------------------------
ImpReg <- read_csv(paste0(fileloc1, loc3, 'ImpRegional_RF_',type,'_',per, '.csv'), 
                   col_names = TRUE)
ImpRegCV <- read_csv(paste0(fileloc1, loc3, 'ImpRegional_RF_CV_',type,'_',per, '.csv'), 
                     col_names = TRUE)
# . F3.3 Formating -------------------------------------------------------------
m <- which(ImpReg$Region == 'FrFree'); ImpReg$Region[m] <- 'Tropics'
m <- which(ImpRegCV$Region == 'FrFree'); ImpRegCV$Region[m] <- 'Tropics'

# . F3.4 Plotting Spider Plots -------------------------------------------------
datImp <- array()

for (i in 11:length(myRegAlfaAcr)){
  # . . F3.4.a Maize ####
  dat <- subset(ImpReg, Region == myRegAlfaAcr[i]) %>%
    subset(Crop == 'maize') %>%
    #arrange(desc(MeanDecreaseAccuracy))
    arrange(MeanDecreaseAccuracy)
  dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
    arrange(factor(AgroInd, levels = myAgro))
  datImp <- tibble(dat$Imp) %>% t()
  first <- datImp
  colnames(datImp) <- dat$AgroInd
  if (length(dat$AgroInd) == 10){
    start <-3
  }else {start <-1}
  colnames(datImp) <- myAgroShort[start:length(myAgroShort)]
  # datImp <- rbind(rep(dim(datImp)[2],dim(datImp)[2]) , rep(1,dim(datImp)[2]) , datImp)
  
  datCV <- subset(ImpRegCV, Region == myRegAlfaAcr[i]) %>%
    subset(Crop == 'maize')
  # Order by importance reformat table
  for (j in 1:500){
    dat <- subset(datCV, CValid == j) %>%
      #arrange(desc(MeanDecreaseAccuracy))
      arrange(MeanDecreaseAccuracy)
    dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
      arrange(factor(AgroInd, levels = myAgro))
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
  
  # . . F3.4.b Rice ####
  dat <- subset(ImpReg, Region == myRegAlfaAcr[i]) %>%
    subset(Crop == 'rice') %>%
    #arrange(desc(MeanDecreaseAccuracy))
    arrange(MeanDecreaseAccuracy)
  dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
    arrange(factor(AgroInd, levels = myAgro))
  datImp <- tibble(dat$Imp) %>% t()
  first <- datImp
  colnames(datImp) <- dat$AgroInd
  if (length(dat$AgroInd) == 10){
    start <-3
  }else {start <-1}
  colnames(datImp) <- myAgroShort[start:length(myAgroShort)]
  # datImp <- rbind(rep(dim(datImp)[2],dim(datImp)[2]) , rep(1,dim(datImp)[2]) , datImp)
  
  datCV <- subset(ImpRegCV, Region == myRegAlfaAcr[i]) %>%
    subset(Crop == 'rice')
  # Order by importance reformat table
  for (j in 1:500){
    dat <- subset(datCV, CValid == j) %>%
      #arrange(desc(MeanDecreaseAccuracy))
      arrange(MeanDecreaseAccuracy)
    dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
      arrange(factor(AgroInd, levels = myAgro))
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
  
  # . . F3.4.c Soya ####
  dat <- subset(ImpReg, Region == myRegAlfaAcr[i]) %>%
    subset(Crop == 'soya') %>%
    #arrange(desc(MeanDecreaseAccuracy))
    arrange(MeanDecreaseAccuracy)
  dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
    arrange(factor(AgroInd, levels = myAgro))
  datImp <- tibble(dat$Imp) %>% t()
  first <- datImp
  colnames(datImp) <- dat$AgroInd
  if (length(dat$AgroInd) == 10){
    start <-3
    colnames(datImp) <- myAgroShort[start:length(myAgroShort)]
  }else if (length(dat$AgroInd) == 1){
    colnames(datImp) <- dat$AgroInd
  }else {
    start <-1
    colnames(datImp) <- myAgroShort[start:length(myAgroShort)]}
  
  # datImp <- rbind(rep(dim(datImp)[2],dim(datImp)[2]) , rep(1,dim(datImp)[2]) , datImp)
  if (dim(dat)[1] > 1){
    datCV <- subset(ImpRegCV, Region == myRegAlfaAcr[i]) %>%
      subset(Crop == 'soya')
    # Order by importance reformat table
    for (j in 1:500){
      dat <- subset(datCV, CValid == j) %>%
        #arrange(desc(MeanDecreaseAccuracy))
        arrange(MeanDecreaseAccuracy)
      dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
        arrange(factor(AgroInd, levels = myAgro))
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
  
  
  # . . F3.4.d Wheat ####
  dat <- subset(ImpReg, Region == myRegAlfaAcr[i]) %>%
    subset(Crop == 'wheat') %>%
    #arrange(desc(MeanDecreaseAccuracy))
    arrange(MeanDecreaseAccuracy)
  dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
    arrange(factor(AgroInd, levels = myAgro))
  datImp <- tibble(dat$Imp) %>% t()
  first <- datImp
  colnames(datImp) <- dat$AgroInd
  if (length(dat$AgroInd) == 10){
    start <-3
  }else {start <-1}
  colnames(datImp) <- myAgroShort[start:length(myAgroShort)]
  # datImp <- rbind(rep(dim(datImp)[2],dim(datImp)[2]) , rep(1,dim(datImp)[2]) , datImp)
  
  datCV <- subset(ImpRegCV, Region == myRegAlfaAcr[i]) %>%
    subset(Crop == 'wheat')
  # Order by importance reformat table
  for (j in 1:500){
    dat <- subset(datCV, CValid == j) %>%
      #arrange(desc(MeanDecreaseAccuracy))
      arrange(MeanDecreaseAccuracy)
    dat <- add_column(dat, Imp = 1:nrow(dat)) %>%
      arrange(factor(AgroInd, levels = myAgro))
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
  
  # . . F3.4.e Final Plot ####
  if (i == 11){
    myTitleA <- ggdraw() +
      draw_label(paste0(myRegAlfaLong[i]),
                 fontface = "bold",
                 angle = 0,
                 size=14)
    F4A <- plot_grid(p1, p2, p3, p4,
                     ncol = 4,
                     labels = c('A','B','C','D')
    )
  } else if (i == 12){
    myTitleB <- ggdraw() +
      draw_label(paste0(myRegAlfaLong[i]),
                 fontface = "bold",
                 angle = 0,
                 size=14)
    F4B <- plot_grid(p1, p2, p3, p4,
                     ncol = 4,
                     labels = c('E','F','G','H')
    )
    
    F4 <- plot_grid(F4A,
                    F4B,
                    ncol = 1,
                    rel_heights = c(1,1))
    ggsave(F4, filename = paste(fileloc2,'Figure3', ".tiff", sep=''),
           width = 12, height = 6, dpi = 350, bg='white')
    
  }
}

rm(list=ls()[! ls() %in% c('fileloc1','fileloc2','loc1','loc2','loc3','loc4','loc5','type','per',
                           'get_legend','as_ggplot','craftbrewer_pal','scale_fill_craftfermenter')])


# SM Figure 2 ###################################################################
# . SMF2.1 Variables Needed ------------------------------------------------------
cropx <- c('maize', 'rice','soya', 'wheat')


myRegAlfaShort <- c("C America & Caribb.", "E Asia", "Europe & Mediterr.", "N America",
                    "Oceania","S & SE Asia", "Sub-Sahara Africa", "Temp S America", "Trop S America",
                    "W-C Asia",'**Temperate**','**Tropics**')
myRegAlfaAcr <- c('CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA', 'Temp','Tropics')


myRegTT <- c( "East Asia", "Europe & Mediterranean", "North America","West-Central Asia",'Temperate',
              "Central America & Caribbean","Oceania","South & Southeast Asia", "Sub-Sahara Africa", 
              "Temperate South America", "Tropical South America",'Tropics')
myRegTTShort <- c( "E Asia", "Europe & Mediterr.", "N America","W-C Asia",'**Temperate**',
                   "C America & Caribb.","Oceania","S & SE Asia", "Sub-Sahara Africa", 
                   "Temp S America", "Trop S America",'**Tropics**')
myRegTTAcr <- c('EAS','EUM','NAM','WCA', 'Temp','CAC','OCE','SEA','SAF','TSA','SAM','Tropics')

agro1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp')
agro2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')
myAgro <- c(agro1, agro2)
myAgroShort <- c('SpFrost', "FallFrost", "GrowSeason", "FrostDays", "StFieldOp", "GDD",
                 "HeatDays", "Precip",   "Dry Days", "FieldCondP", "FieldCondM", "FieldCondH")

# . SMF2.2 Opening files ---------------------------------------------------------
ImpReg <- read_csv(paste0(fileloc1, loc3, 'ImpRegional_RF_',type,'_',per, '.csv'), 
                   col_names = TRUE)
ImpRegCV <- read_csv(paste0(fileloc1, loc3, 'ImpRegional_RF_CV_',type,'_',per, '.csv'), 
                     col_names = TRUE)

# . SMF2.3 Formating -------------------------------------------------------------
m <- which(ImpReg$Region == 'FrFree'); ImpReg$Region[m] <- 'Tropics'
m <- which(ImpRegCV$Region == 'FrFree'); ImpRegCV$Region[m] <- 'Tropics'

ImpReg$CValid <- 501
ImpRegNewCV <- rbind(ImpRegCV, ImpReg)

# . SMF2.4 Calculations & Plotting Importance -----------------------------------
# . . SMF2.4.a Plotting Maize ====
ImpRegNewCV <- array()
for (j in 1:length(myRegAlfaAcr)){
  dat <- subset(ImpRegCV, Crop ==cropx[1]) %>%
    subset(Region == myRegAlfaAcr[j]) %>%
    subset(select = -c(failure, nonfailure,MeanDecreaseGini))
  dat <- spread(dat, key= CValid, value = MeanDecreaseAccuracy)
  dat <- cbind(AgroInd = dat[,1], 
               Region = myRegAlfaAcr[j],
               Crop = cropx[1],
               MeanDecreaseAccuracy = apply(dat[,4:dim(dat)[2]], MARGIN=1, mean, na.rm=TRUE)) 
  dat <- dat %>%
    arrange(desc(MeanDecreaseAccuracy))
  dat <- add_column(dat, Imp = 1:nrow(dat))
  ImpRegNewCV <- rbind(ImpRegNewCV, dat)
}
ImpRegNewCV <- ImpRegNewCV %>%
  mutate(AgroInd = factor(AgroInd, levels = rev(myAgro))) %>% 
  mutate(Region = factor(Region, levels = myRegTTAcr)) %>%
  # mutate(Region = factor(Region, levels = myRegAlfaAcr)) %>%
  arrange(AgroInd, Region)
dat <- subset(ImpRegNewCV, Crop == cropx[1]) 
dat$FirstImp <- FALSE
dat$FirstImp[dat$Imp ==1] <- TRUE
dat$Imp <- as.factor(dat$Imp)
dat$FirstImp <- as.factor(dat$FirstImp)

p1 <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
  geom_tile(aes(fill=Imp, color = FirstImp), size = 1.5) + 
  scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
  scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  geom_text(aes(label=Imp)) +
  geom_text(data = subset(dat, Imp == 8 | Imp == 9 |Imp == 10 | Imp == 11 | Imp ==12), 
            aes(label = Imp), color = "white") +
  labs(title= "", y="", x="") +
  # guides(fill=guide_legend(title="Importance")) +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegTTAcr, labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfaAcr, labels = myRegAlfaShort) +
  scale_y_discrete(labels = rev(myAgroShort)) + 
  guides(fill=guide_legend(title="Importance")) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())

# . . SMF2.4.b Plotting Rice ====
ImpRegNewCV <- array()
for (j in 1:length(myRegAlfaAcr)){
  dat <- subset(ImpRegCV, Crop ==cropx[2]) %>%
    subset(Region == myRegAlfaAcr[j]) %>%
    subset(select = -c(failure, nonfailure,MeanDecreaseGini))
  dat <- spread(dat, key= CValid, value = MeanDecreaseAccuracy)
  dat <- cbind(AgroInd = dat[,1], 
               Region = myRegAlfaAcr[j],
               Crop = cropx[2],
               MeanDecreaseAccuracy = apply(dat[,4:dim(dat)[2]], MARGIN=1, mean, na.rm=TRUE)) 
  dat <- dat %>%
    arrange(desc(MeanDecreaseAccuracy))
  dat <- add_column(dat, Imp = 1:nrow(dat))
  ImpRegNewCV <- rbind(ImpRegNewCV, dat)
}
ImpRegNewCV <- ImpRegNewCV %>%
  mutate(AgroInd = factor(AgroInd, levels = rev(myAgro))) %>% 
  mutate(Region = factor(Region, levels = myRegTTAcr)) %>%
  # mutate(Region = factor(Region, levels = myRegAlfaAcr)) %>%
  arrange(AgroInd, Region)
dat <- subset(ImpRegNewCV, Crop == cropx[2]) 
dat$FirstImp <- FALSE
dat$FirstImp[dat$Imp ==1] <- TRUE
dat$Imp <- as.factor(dat$Imp)
dat$FirstImp <- as.factor(dat$FirstImp)

p2 <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
  #geom_tile(aes(fill=Imp)) + 
  geom_tile(aes(fill=Imp, color = FirstImp), size = 1.5) + 
  scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
  scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  geom_text(aes(label=Imp)) +
  geom_text(data = subset(dat, Imp == 8 | Imp == 9 |Imp == 10 | Imp == 11 | Imp ==12), 
            aes(label = Imp), color = "white") +
  labs(title= "", y="", x="") +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegTTAcr, labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfaAcr, labels = myRegAlfaShort) +
  scale_y_discrete(labels = rev(myAgroShort)) + 
  guides(fill=guide_legend(title="Importance")) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())

# . . SMF2.4.c Plotting Soya ====
ImpRegNewCV <- array()
for (j in 1:length(myRegAlfaAcr)){
  dat <- subset(ImpRegCV, Crop ==cropx[3]) %>%
    subset(Region == myRegAlfaAcr[j]) %>%
    subset(select = -c(failure, nonfailure,MeanDecreaseGini))
  dat <- spread(dat, key= CValid, value = MeanDecreaseAccuracy)
  dat <- cbind(AgroInd = dat[,1], 
               Region = myRegAlfaAcr[j],
               Crop = cropx[3],
               MeanDecreaseAccuracy = apply(dat[,4:dim(dat)[2]], MARGIN=1, mean, na.rm=TRUE)) 
  dat <- dat %>%
    arrange(desc(MeanDecreaseAccuracy))
  dat <- add_column(dat, Imp = 1:nrow(dat))
  ImpRegNewCV <- rbind(ImpRegNewCV, dat)
}
ImpRegNewCV <- ImpRegNewCV %>%
  mutate(AgroInd = factor(AgroInd, levels = rev(myAgro))) %>% 
  mutate(Region = factor(Region, levels = myRegTTAcr)) %>%
  # mutate(Region = factor(Region, levels = myRegAlfaAcr)) %>%
  arrange(AgroInd, Region)
dat <- subset(ImpRegNewCV, Crop == cropx[3]) 
dat$FirstImp <- FALSE
dat$FirstImp[dat$Imp ==1] <- TRUE
dat$Imp <- as.factor(dat$Imp)
dat$FirstImp <- as.factor(dat$FirstImp)

p3 <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
  #geom_tile(aes(fill=Imp)) + 
  geom_tile(aes(fill=Imp, color = FirstImp), size = 1.5) + 
  scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
  scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  geom_text(aes(label=Imp)) +
  geom_text(data = subset(dat, Imp == 8 | Imp == 9 |Imp == 10 | Imp == 11 | Imp ==12), 
            aes(label = Imp), color = "white") +
  labs(title= "", y="", x="") +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegTTAcr, labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfaAcr, labels = myRegAlfaShort) +
  scale_y_discrete(labels = rev(myAgroShort)) + 
  guides(fill=guide_legend(title="Importance")) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())

# . . SMF2.4.d Plotting Wheat ====
ImpRegNewCV <- array()
for (j in 1:length(myRegAlfaAcr)){
  dat <- subset(ImpRegCV, Crop ==cropx[4]) %>%
    subset(Region == myRegAlfaAcr[j]) %>%
    subset(select = -c(failure, nonfailure,MeanDecreaseGini))
  dat <- spread(dat, key= CValid, value = MeanDecreaseAccuracy)
  dat <- cbind(AgroInd = dat[,1], 
               Region = myRegAlfaAcr[j],
               Crop = cropx[4],
               MeanDecreaseAccuracy = apply(dat[,4:dim(dat)[2]], MARGIN=1, mean, na.rm=TRUE)) 
  dat <- dat %>%
    arrange(desc(MeanDecreaseAccuracy))
  dat <- add_column(dat, Imp = 1:nrow(dat))
  ImpRegNewCV <- rbind(ImpRegNewCV, dat)
}
ImpRegNewCV <- ImpRegNewCV %>%
  mutate(AgroInd = factor(AgroInd, levels = rev(myAgro))) %>% 
  mutate(Region = factor(Region, levels = myRegTTAcr)) %>%
  # mutate(Region = factor(Region, levels = myRegAlfaAcr)) %>%
  arrange(AgroInd, Region)
dat <- subset(ImpRegNewCV, Crop == cropx[4]) 
dat$FirstImp <- FALSE
dat$FirstImp[dat$Imp ==1] <- TRUE
dat$Imp <- as.factor(dat$Imp)
dat$FirstImp <- as.factor(dat$FirstImp)

p4 <- ggplot(na.omit(dat), aes(x=Region, y=AgroInd, fill=factor(Imp))) +
  #geom_tile(aes(fill=Imp)) + 
  geom_tile(aes(fill=Imp, color = FirstImp), size = 1.5) + 
  scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
  scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  geom_text(aes(label=Imp)) +
  geom_text(data = subset(dat, Imp == 8 | Imp == 9 |Imp == 10 | Imp == 11 | Imp ==12), 
            aes(label = Imp), color = "white") +
  labs(title= "", y="", x="") +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegTTAcr, labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfaAcr, labels = myRegAlfaShort) +
  scale_y_discrete(labels = rev(myAgroShort)) + 
  guides(fill=guide_legend(title="Importance")) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . SMF2.5 Combine & Save --------------------------------------------------------
SMF2 <- plot_grid(p1, p2,
                  p3, p4,
                  nrow = 2,
                  labels = c('A','B','C','D'),
                  rel_widths = c(1,1))

ggsave(SMF2, filename = paste(fileloc2,'SMFigure2', ".tiff", sep=''),
       width = 14, height = 10, dpi = 350, bg='white')

rm(list=ls()[! ls() %in% c('fileloc1','fileloc2','loc1','loc2','loc3','loc4','loc5','type','per',
                           'get_legend','as_ggplot','craftbrewer_pal','scale_fill_craftfermenter')])
# Figure 4, 5, SM3  #############################################################
# . F4.1 Variables Needed ------------------------------------------------------
statVar <- c('Mu', 'Min')[2]
alpha = 0.05
df <- 500-1
t.score = qt(p=alpha/2, df=df,lower.tail=FALSE)

crop <- c('maize', 'rice','soya', 'wheat')
myColors <- c('maize' = '#1B9E77', 'rice' = '#D95F02', 
              'soya'= '#7570B3','wheat'='#E7298A')
MyColors <- c('Maize' = '#1B9E77', 'Rice' = '#D95F02', 
              'Soy'= '#7570B3','Wheat'='#E7298A')

myRegAlfa <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
               "Oceania","Southeast Asia", "Sub-Sahara Africa", "Temperate South America", "Tropical South America",
               "West-Central Asia", 'Temperate','Tropics')
myRegAlfaAcr <- c('CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA', 'Temp','FrFree')
myRegTTShort <- c( "E Asia", "Europe & Mediterr.", "N America","W-C Asia",'**Temperate**',
                   "C America & Caribb.","Oceania","S & SE Asia", "Sub-Sahara Africa", 
                   "Temp S America", "Trop S America",'**Tropics**')
myRegTTAcr <- c('EAS','EUM','NAM','WCA', 'Temp','CAC','OCE','SEA','SAF','TSA','SAM','Tropics')

agro1 <- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
           'StartFieldOp')
agro2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')
myAgro <- c(agro1, agro2)
myAgroShort <- c('SpFrost', "FallFrost", "GrowSeason", "FrostDays", "StFieldOp", "GDD",
                 "HeatDays", "Precip",   "Dry Days", "FieldCondP", "FieldCondM", "FieldCondH")
myUnits <- c('DOY','DOY','Days','Days','DOY','?C','Days','mm','Days','Days','Days','Days','Days')

# . F4.2 Opening files ----------------------------------------------------------
PDPReg <- read_csv(paste0(fileloc1, loc3, 'PDPRegional_RF_',type,'_',per, '.csv'), col_names = TRUE) 
PDPRegCV <- read_csv(paste0(fileloc1, loc3, 'PDPRegional_RF_CV_', type,'_',per,'.csv'), 
                     col_names = TRUE)
regionMask <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6RegionsMask.csv'),
                       col_names = TRUE)

if (statVar == 'Mu'){
  AgroInd <- list()
  # F3.2a Opening of non-crop dependent #
  for (i in 1:length(agro1)){
    dat <- read_csv(paste(fileloc1, loc2, agro1[i], '.csv', sep=''),
                    col_names = TRUE, cols(.default = col_double()))
    dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
    name <- agro1[i]
    AgroInd[[name]] <- dat
  }
  # F3.2b Opening of crop dependent #
  for (i in 1:length(agro2)){
    for(j in 1:length(crop)){
      dat <- read_csv(paste(fileloc1, loc2, agro2[i],'_',crop[j], '.csv', sep=''),
                      col_names = TRUE, cols(.default = col_double()))
      dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
      name <-  paste(agro2[i],"_",crop[j], sep='')
      AgroInd[[name]] <- dat
    }
  }
  rm(i, j, dat, name) 
}

# . F4.3 Calculating Mu or Minimum ---------------------------------------------
datMu <- matrix(NA, ncol= 5)

if (statVar == 'Mu'){
  # . . . F4.3.a Mu Non-crop dependent ####
  for (a in 1:length(agro1)){
    dat <- AgroInd[[agro1[a]]]
    for (i in 1:length(myRegAlfaAcr)){
      if (i == 11){
        # Temperate Regions
        x <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
        m <- which(x >= 1)
      } else if (i == 12){
        # Tropical regions
        x <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
        m <- which(x < 1)
      } else {
        n <- which(colnames(regionMask) == myRegAlfaAcr[i])
        m <- which(regionMask[,n] == 1)
      } 
      datR <- dat[m,]
      datR[datR == -999] <- NA
      datR <- matrix(datR[,5:ncol(datR)], ncol = 1) %>%
        unlist()
      mu <- matrix(myRegAlfaAcr[i]) %>%
        cbind(agro1[a]) %>%
        cbind('Mu') %>%
        cbind(mean(datR, na.rm = TRUE) %>%
                round(digits = 2)) %>%
        cbind('maize')
      datMu <- rbind(datMu, mu)
      mu <- matrix(myRegAlfaAcr[i]) %>%
        cbind(agro1[a]) %>%
        cbind('Mu') %>%
        cbind(mean(datR, na.rm = TRUE) %>%
                round(digits = 2)) %>%
        cbind('rice')
      datMu <- rbind(datMu, mu)
      mu <- matrix(myRegAlfaAcr[i]) %>%
        cbind(agro1[a]) %>%
        cbind('Mu') %>%
        cbind(mean(datR, na.rm = TRUE) %>%
                round(digits = 2)) %>%
        cbind('soya')
      datMu <- rbind(datMu, mu)
      mu <- matrix(myRegAlfaAcr[i]) %>%
        cbind(agro1[a]) %>%
        cbind('Mu') %>%
        cbind(mean(datR, na.rm = TRUE) %>%
                round(digits = 2)) %>%
        cbind('wheat')
      datMu <- rbind(datMu, mu)
    }
  }
  # . . . F4.3.b Mu Crop dependent ####
  for (a in 1:length(agro2)){
    for (c in 1:length(crop)){
      agro <- paste0(agro2[a],'_',crop[c])
      dat <- AgroInd[[agro]]
      for (i in 1:length(myRegAlfaAcr)){
        if (i == 11){
          x <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
          m <- which(x >= 1)
        } else if (i == 12){
          x <- apply(AgroInd[['AccFrostDays']][,5:ncol(AgroInd[['AccFrostDays']])], MARGIN = 1, FUN= mean, na.rm=TRUE)
          m <- which(x < 1)
        } else {
          n <- which(colnames(regionMask) == myRegAlfaAcr[i])
          m <- which(regionMask[,n] == 1)
        } 
        datR <- dat[m,]
        datR[datR == -999] <- NA
        datR <- matrix(datR[,5:ncol(datR)], ncol = 1) %>%
          unlist()
        mu <- matrix(myRegAlfaAcr[i]) %>%
          cbind(agro2[a]) %>%
          cbind('Mu') %>%
          cbind(mean(datR, na.rm = TRUE) %>%
                  round(digits = 2)) %>%
          cbind(crop[c])
        datMu <- rbind(datMu, mu)
      }
    }
  }
  rm(AgroInd,a,i,dat,x,n,m, datR,mu,c)
} else if(statVar == 'Min'){
  # . . . F4.3.c Min  ####
  for (i in myRegAlfaAcr){
    for (c in crop){
      for (a in c(agro1, agro2)){
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
  rm(dat, minx, mu, a, c, i, m)
} else {print('statVar not reconized.')}

colnames(datMu) <- c("Region","Agro", 'Method','X','Crop') 
datMu <- as_tibble(datMu[2:nrow(datMu),])
datMu$X<- as.numeric(datMu$X)

# . F4.4 Main Text plots ---------------------------------------------------------
# yLimit <- range(PDPReg$yhat, na.rm = TRUE) %>% round(2) 
# # yLimit[1] <- yLimit[1] - 0.01
# yLimit[2] <- yLimit[2] + 0.05
yhat <- 0.23

# . . F4.4.a Temperate ====
PDP <- subset(PDPReg, Region == myRegAlfaAcr[11])
PDPCV <- subset(PDPRegCV, Region == myRegAlfaAcr[11])
methodValue <- subset(datMu, Region == myRegAlfaAcr[11])

# . . . F4.4.a.1 Temperate Maize ====
var <- c(myAgro[8], myAgro[2])
varShort <-c(myAgroShort[8],myAgroShort[2])
cropx <- crop[1]
myColorsx <- myColors[1]
# . . . Var 1 
datM <- subset(PDP, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[1]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p1A <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[8]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[1], y="Probability of Crop Failure", color = "Legend")

# . . . Var 2 
datM <- subset(PDP, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[2]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.88
p1B <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[2]), color = 'grey25') +
  xlim(200,370) + ylim(0.23,0.5) +
  labs(x=varShort[2], y="", color = "Legend")

# . . . Var 3 
p1C <- NULL


# . . . F4.4.a.2 Temperate Rice ====
var <- c(myAgro[6], myAgro[8], myAgro[1])
varShort <-c(myAgroShort[6], myAgroShort[8], myAgroShort[1])
cropx <- crop[2]
myColorsx <- myColors[2]
# . . . Var 1
datM <- subset(PDP, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[1]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p2A <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[6]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[1], y="Probability of Crop Failure", color = "Legend")

# . . . Var 2 
datM <- subset(PDP, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[2]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p2B <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[8]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[2], y="", color = "Legend")

# . . . Var 3 
datM <- subset(PDP, AgroClmInd == var[3]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[3]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[3]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p2C <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[1]), color = 'grey25') +
  xlim(0,200) + ylim(0.23,0.5) +
  labs(x=varShort[3], y="", color = "Legend")

# . . . F4.4.a.3 Temperate Soy ====
var <- c(myAgro[6], myAgro[8], myAgro[9])
varShort <-c(myAgroShort[6], myAgroShort[8], myAgroShort[9])
cropx <- crop[3]
myColorsx <- myColors[3]
# . . . Var 1 
datM <- subset(PDP, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[1]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p3A <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[6]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[1], y="Probability of Crop Failure", color = "Legend")

# . . . Var 2 
datM <- subset(PDP, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[2]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p3B <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + ylab("") +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[8]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[2], y="", color = "Legend")
# . . . Var 3
datM <- subset(PDP, AgroClmInd == var[3]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[3]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[3]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p3C <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + ylab("") +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[9]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[3], y="", color = "Legend")

# . . . F4.4.a.4 Temperate Wheat ====
var <- c(myAgro[8], myAgro[12])
varShort <-c(myAgroShort[8], myAgroShort[12])
cropx <- crop[4]
myColorsx <- myColors[4]
# . . . Var 1
datM <- subset(PDP, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[1]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p4A <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[8]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[1], y="Probability of Crop Failure", color = "Legend")

# . . . Var 2
datM <- subset(PDP, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[2]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p4B <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[12]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[2], y="", color = "Legend")
# . . . Var 3 
p4C <- NULL


# . . . F4.4.a.5 Legend ####
myDat <- data.frame(col1= 1:4,
                    col2=1:4,
                    col3= c("Maize", "Rice", "Soy", "Wheat"))
colnames(myDat) <- c('X','Y', 'Crop')

myLegend <- ggplot(myDat)+
  geom_line(aes(x=X, y=Y, color=Crop))+
  theme_light() +
  theme(legend.title = element_blank())+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  scale_color_manual(values= MyColors)
myLegend <- get_legend(myLegend, position = 'bottom') %>% as_ggplot()

# . . . F4.4.a.6 Combine & Save ====
F4 <- plot_grid(p1A,p1B,p1C,
                p2A,p2B,p2C,
                p3A,p3B,p3C,
                p4A,p4B,p4C,
                ncol = 3
)
F4 <- plot_grid(F4,
                myLegend,
                ncol=1,
                rel_heights = c(1,0.05))
ggsave(F4, filename = paste0(fileloc2, 'Figure4', '.tiff'), 
       width = 11, height = 12, dpi=350, bg='white')

# . . F5.4.b Tropic ====
PDP <- subset(PDPReg, Region == myRegAlfaAcr[12])
PDPCV <- subset(PDPRegCV, Region == myRegAlfaAcr[12])
methodValue <- subset(datMu, Region == myRegAlfaAcr[12])

# . . . F5.4.b.1 Tropic Maize ====
var <- c(myAgro[6], myAgro[12], myAgro[10])
varShort <-c(myAgroShort[6],myAgroShort[12], myAgroShort[10])
cropx <- crop[1]
myColorsx <- myColors[1]
# . . . Var 1 
datM <- subset(PDP, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[1]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble()
xpos <- range(datM$X)[2] * 0.75
p1A <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[6]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[1], y="Probability of Crop Failure", color = "Legend")

# . . . Var 2 
datM <- subset(PDP, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[2]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p1B <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[12]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[2], y="", color = "Legend")

# . . . Var 3 
datM <- subset(PDP, AgroClmInd == var[3]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[3]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[3]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble()
xpos <- range(datM$X)[2] * 0.75
p1C <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[10]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[3], y="", color = "Legend")


# . . . F5.4.b.2 Tropic Rice ====
var <- c(myAgro[10], myAgro[6], myAgro[11])
varShort <-c(myAgroShort[10], myAgroShort[6], myAgroShort[11])
cropx <- crop[2]
myColorsx <- myColors[2]
# . . . Var 1
datM <- subset(PDP, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[1]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble()
xpos <- range(datM$X)[2] * 0.75
p2A <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[10]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[1], y="Probability of Crop Failure", color = "Legend")

# . . . Var 2 
datM <- subset(PDP, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[2]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p2B <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + ylab("") +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[6]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[2], y="", color = "Legend")

# . . . Var 3 
datM <- subset(PDP, AgroClmInd == var[3]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[3]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[3]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p2C <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + ylab("") +
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[11]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[3], y="", color = "Legend")

# . . . F5.4.b.3 Tropic Soy ====
var <- c(myAgro[6], myAgro[12])
varShort <-c(myAgroShort[6], myAgroShort[12])
cropx <- crop[3]
myColorsx <- myColors[3]
# . . . Var 1 
datM <- subset(PDP, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[1]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble()
xpos <- range(datM$X)[2] * 0.77
p3A <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[6]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[1], y="Probability of Crop Failure", color = "Legend")

# . . . Var 2 
datM <- subset(PDP, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[2]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[2]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p3B <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[12]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[2], y="", color = "Legend")
# . . . Var 3
p3C <- NULL

# . . . F5.4.b.4 Tropic Wheat ====
var <- c(myAgro[6])
varShort <-c(myAgroShort[6])
cropx <- crop[4]
myColorsx <- myColors[4]
# . . . Var 1
datM <- subset(PDP, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)
datCV <- subset(PDPCV, AgroClmInd == var[1]) %>%
  subset(Crop == cropx)

for (j in 1:dim(datM)[1]){
  datB <- subset(datCV, X == datM$X[j])$yhat
  datM$SE[j] <- sd(datB)/sqrt(length(datB))
  datM$yhatMin[j] <- datM$yhat[j] - t.score * datM$SE[j]
  datM$yhatMax[j] <- datM$yhat[j] + t.score * datM$SE[j]
}
dat <- subset(methodValue, Agro == var[1]) %>%
  subset(Crop == cropx)
dat <- cbind(dat, yhat) %>% as_tibble() 
xpos <- range(datM$X)[2] * 0.75
p4A <- ggplot(NULL, aes(x= X, y= yhat))+ 
  theme_light() + 
  geom_point(data=dat, color= myColorsx) +
  geom_ribbon(data = na.omit(datM), aes(x= X, ymin=yhatMin, ymax=yhatMax),
              fill= myColorsx, alpha = 0.5) +
  geom_line(data= na.omit(datM), color= myColorsx, size= 1) +
  geom_text(data=dat, x=xpos, y=0.25, size=5, 
            label=paste0(round(dat$X,0),'',myUnits[6]), color = 'grey25') +
  ylim(0.23,0.5) +
  labs(x=varShort[1], y="Probability of Crop Failure", color = "Legend")

# . . . Var 2
p4B <- NULL
# . . . Var 3 
p4C <- NULL


# . . . F5.4.b.5 Legend ####
myDat <- data.frame(col1= 1:4,
                    col2=1:4,
                    col3= c("Maize", "Rice", "Soy", "Wheat"))
colnames(myDat) <- c('X','Y', 'Crop')

myLegend <- ggplot(myDat)+
  geom_line(aes(x=X, y=Y, color=Crop))+
  theme_light() +
  theme(legend.title = element_blank())+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  scale_color_manual(values= MyColors)
myLegend <- get_legend(myLegend, position = 'bottom') %>% as_ggplot()

# . . . F5.4.b.6 Combine & Save ====
F5 <- plot_grid(p1A,p1B,p1C,
                p2A,p2B,p2C,
                p3A,p3B,p3C,
                p4A,p4B,p4C,
                ncol = 3
)
F5 <- plot_grid(F5,
                myLegend,
                ncol=1,
                rel_heights = c(1,0.05))
ggsave(F5, filename = paste0(fileloc2, 'Figure5', '.tiff'), 
       width = 11, height = 12, dpi=350, bg='white')
# . SMF3.5 Optimal Values --------------------------------------------------------
m <- which(datMu$Region == 'FrFree'); datMu$Region[m] <- 'Tropics'

# . . SMF3.5.a Maize ####
methodValue <- subset(datMu, Crop == crop[1])
methodValue$Xx <- methodValue$X
methodValue$X <- round(methodValue$X)
p1 <- ggplot(na.omit(methodValue), aes(x=Region, y=Agro, fill=factor(X))) +
  geom_tile(aes(fill=factor(X)), size = 1.5) + 
  scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  geom_text(aes(label=factor(X))) +
  geom_text(data = subset(methodValue, Xx >= 100), 
            aes(label = factor(X)), color = "white") +
  labs(title= "", y="", x="") +
  # guides(fill=guide_legend(title="Importance")) +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegTTAcr, labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfaAcr, labels = myRegAlfaShort) +
  scale_y_discrete(limits= rev(myAgro), labels = rev(myAgroShort)) + 
  guides(fill=guide_legend(title="Importance")) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())

# . . SMF3.5.a Rice ####
methodValue <- subset(datMu, Crop == crop[2])
methodValue$Xx <- methodValue$X
methodValue$X <- round(methodValue$X)
p2 <- ggplot(na.omit(methodValue), aes(x=Region, y=Agro, fill=factor(X))) +
  geom_tile(aes(fill=factor(X)), size = 1.5) + 
  scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  geom_text(aes(label=factor(X))) +
  geom_text(data = subset(methodValue, Xx >= 100), 
            aes(label = factor(X)), color = "white") +
  labs(title= "", y="", x="") +
  # guides(fill=guide_legend(title="Importance")) +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegTTAcr, labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfaAcr, labels = myRegAlfaShort) +
  scale_y_discrete(limits= rev(myAgro), labels = rev(myAgroShort)) + 
  guides(fill=guide_legend(title="Importance")) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())

# . . SMF3.5.c Soy ####
methodValue <- subset(datMu, Crop == crop[3])
methodValue$Xx <- methodValue$X
methodValue$X <- round(methodValue$X)
p3 <- ggplot(na.omit(methodValue), aes(x=Region, y=Agro, fill=factor(X))) +
  geom_tile(aes(fill=factor(X)), size = 1.5) + 
  scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  geom_text(aes(label=factor(X))) +
  geom_text(data = subset(methodValue, Xx >= 100), 
            aes(label = factor(X)), color = "white") +
  labs(title= "", y="", x="") +
  # guides(fill=guide_legend(title="Importance")) +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegTTAcr, labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfaAcr, labels = myRegAlfaShort) +
  scale_y_discrete(limits= rev(myAgro), labels = rev(myAgroShort)) + 
  guides(fill=guide_legend(title="Importance")) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())

# . . SMF3.5.d Wheat ####
methodValue <- subset(datMu, Crop == crop[4])
methodValue$Xx <- methodValue$X
methodValue$X <- round(methodValue$X)
p4 <- ggplot(na.omit(methodValue), aes(x=Region, y=Agro, fill=factor(X))) +
  geom_tile(aes(fill=factor(X)), size = 1.5) + 
  scale_fill_viridis(viridis(12), discrete = TRUE, direction = -1) +
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  geom_text(aes(label=factor(X))) +
  geom_text(data = subset(methodValue, Xx >= 100), 
            aes(label = factor(X)), color = "white") +
  labs(title= "", y="", x="") +
  # guides(fill=guide_legend(title="Importance")) +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegTTAcr, labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfaAcr, labels = myRegAlfaShort) +
  scale_y_discrete(limits= rev(myAgro), labels = rev(myAgroShort)) + 
  guides(fill=guide_legend(title="Importance")) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . SMF3.5.e Combine & Save ####
SMF3 <- plot_grid(p1, p2,
                  p3, p4,
                  nrow = 2,
                  labels = c('A','B','C','D'),
                  rel_widths = c(1,1))

ggsave(SMF3, filename = paste(fileloc2,'SMFigure3', ".tiff", sep=''),
       width = 14, height = 10, dpi = 350, bg='white')

rm(list=ls()[! ls() %in% c('fileloc1','fileloc2','loc1','loc2','loc3','loc4','loc5','type','per',
                           'get_legend','as_ggplot','craftbrewer_pal','scale_fill_craftfermenter')])
# Figure 6, SM5, SMF5  ##########################################################
# . F6.1 Variables Needed --------------------------------------------------------
myRegAlfa <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
               "Oceania","South & Southeast Asia", "Sub-Sahara Africa", "Temperate South America", "Tropical South America",
               "West-Central Asia",'Temperate','Tropics')
myRegAlfaBold <- c("Central America & Caribbean", "East Asia", "Europe & Mediterranean", "North America",
                   "Oceania","South & Southeast Asia", "Sub-Sahara Africa", "Temperate South America", "Tropical South America",
                   "West-Central Asia",'**Temperate**','**Tropics**')
myRegAlfaShort <- c("C America & Caribb.", "E Asia", "Europe & Mediterr.", "N America",
                    "Oceania","S & SE Asia", "Sub-Sahara Africa", "Temp S America", "Trop S America",
                    "W-C Asia",'**Temperate**','**Tropics**')
myRegAlfaAcr <- c('CAC','EAS','EUM','NAM','OCE','SEA','SAF','TSA','SAM','WCA', 'Temp','Tropics')

myRegTT <- c( "East Asia", "Europe & Mediterranean", "North America","West-Central Asia",'Temperate',
              "Central America & Caribbean","Oceania","South & Southeast Asia", "Sub-Sahara Africa", 
              "Temperate South America", "Tropical South America",'Tropics')
myRegTTShort <- c( "E Asia", "Europe & Mediterr.", "N America","W-C Asia",'**Temperate**',
                   "C America & Caribb.","Oceania","S & SE Asia", "Sub-Sahara Africa", 
                   "Temp S America", "Trop S America",'**Tropics**')
myRegTTAcr <- c('EAS','EUM','NAM','WCA', 'Temp','CAC','OCE','SEA','SAF','TSA','SAM','Tropics')


agro1<- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 
          'AccFrostDays', 'StartFieldOp')
agro1Red <- c('LastSpringFrost', 'FirstFallFrost', 'AccFrostDays')
agro2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')
myAgroShortF1 <- c('SpFrost', "FallFrost", "GrowSeason", "FrostDays", "StFieldOp")
myAgroShortF1Red <- c('SpFrost',"FallFrost","FrostDays")
myAgroShortF2 <- c("GDD","HeatDays","Precip","Dry Days",
                   "FieldCondP","FieldCondM","FieldCondH")

myAgro <- c('SpFrost', "FallFrost", "FrostDays", "GDD", "HeatDays",
            "Precip",   "Dry Days", "FieldCondP", "FieldCondM", "FieldCondH")
myAgroAcr <- c('LSF',"FFF","AFD","GDD","HSD","TP","DD","FCP","FCM","FCH")

cropx <- c('maize', 'rice', 'soya', 'wheat')
AgroMK <- list()
myBreaks <- seq(0,100,25)

impVar <- list(maizeImp = c((8-2),(1),(2),(2),(8-2),(5-2),(6-2),(11-2),(6-2),(9-2),(12-2),(6-2)),
               riceImp = c((8-2),2,(8-2),(6-2),(8-2),(8-2),(8-2),(6-2),(8-2),(6-2),(12-2),(6-2)),
               soyaImp = c((6-2),(6-2),(8-2),0,(6-2),(10-2),0,(7-2),(6-2),(9-2),(12-2),(12-2)),
               wheatImp = c((8-2),(4-2),1,(8-2),(8-2),(9-2),(8-2),(6-2),(6-2),9,(6-2),(6-2)))
myFrostColors <- c("+"="#998eC3","-"="#F1a340")

baseData <- map_data('world')

# . F6.2 Opening Files -----------------------------------------------------------
regionMask <- read_csv(paste0(fileloc1,'IPCC6Regions/IPCC6RegionsMask.csv'),
                       col_names = TRUE)

for (i in 1:length(agro1)){
  dat <- read_csv(paste0(fileloc1, loc5, 'MK_', agro1[i], '.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  name <- agro1[i]
  AgroMK[[name]] <- dat
}
for (i in 1:length(agro2)){
  for (j in 1:length(cropx)){
    dat <- read_csv(paste0(fileloc1, loc5, 'MK_', agro2[i],'_',cropx[j], '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
    name <- paste(agro2[i],"_",cropx[j], sep='')
    AgroMK[[name]] <- dat
  }
}

nonMK <- read_csv(paste0(fileloc1, loc5, 'RegionsNon', '.csv'),
                  col_names = TRUE)
maizeMK <- read_csv(paste0(fileloc1, loc5, 'Regions_maize', '.csv'),
                    col_names = TRUE)
riceMK <- read_csv(paste0(fileloc1, loc5, 'Regions_rice', '.csv'),
                   col_names = TRUE)
soyaMK <- read_csv(paste0(fileloc1, loc5, 'Regions_soya', '.csv'),
                   col_names = TRUE)
wheatMK <- read_csv(paste0(fileloc1, loc5, 'Regions_wheat', '.csv'),
                    col_names = TRUE)

frost <- read_csv(paste0(fileloc1, loc2, agro1[4], '.csv'),
                  col_names = TRUE, cols(.default = col_double()))
frost <- frost[, -which(names(frost) %in% c('1981','2017','2018'))]
frost <- apply(frost[,5:ncol(frost)], MARGIN = 1, FUN= mean, na.rm=TRUE)

maizeYd <- read_csv(paste0(fileloc1, loc1, 'Yield_', 'maize', '_', type,'_',per,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
riceYd <- read_csv(paste0(fileloc1, loc1, 'Yield_', 'rice', '_', type,'_',per,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
soyaYd <- read_csv(paste0(fileloc1, loc1, 'Yield_', 'soya', '_', type,'_',per,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
wheatYd <- read_csv(paste0(fileloc1, loc1, 'Yield_', 'wheat', '_', type,'_',per,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . F6.3 Formating ---------------------------------------------------------------
# . . F6.3.a Formating MK Regional ====
nonMK$Region[nonMK$Region == 'Frost Free'] <- 'Tropics'
nonMK$Region[nonMK$Region == 'Temprate South America'] <- "Temperate South America"
nonMK$Region[nonMK$Region == 'Sub-Saharah Africa'] <- 'Sub-Sahara Africa'
maizeMK$Region[maizeMK$Region == 'Frost Free'] <- 'Tropics'
maizeMK$Region[maizeMK$Region == 'Temprate South America'] <- "Temperate South America"
maizeMK$Region[maizeMK$Region == 'Sub-Saharah Africa'] <- 'Sub-Sahara Africa'
riceMK$Region[riceMK$Region == 'Frost Free'] <- 'Tropics'
riceMK$Region[riceMK$Region == 'Temprate South America'] <- "Temperate South America"
riceMK$Region[riceMK$Region == 'Sub-Saharah Africa'] <- 'Sub-Sahara Africa'
soyaMK$Region[soyaMK$Region == 'Frost Free'] <- 'Tropics'
soyaMK$Region[soyaMK$Region == 'Temprate South America'] <- "Temperate South America"
soyaMK$Region[soyaMK$Region == 'Sub-Saharah Africa'] <- 'Sub-Sahara Africa'
wheatMK$Region[wheatMK$Region == 'Frost Free'] <- 'Tropics'
wheatMK$Region[wheatMK$Region == 'Temprate South America'] <- "Temperate South America"
wheatMK$Region[wheatMK$Region == 'Sub-Saharah Africa'] <- 'Sub-Sahara Africa'

nonMK <- nonMK %>%
  subset(select=-c(LastSpringFrost_0.05,LastSpringFrost_0.01,
                   FirstFallFrost_0.05,FirstFallFrost_0.01 ,
                   ClimGrowingSeason_0.05,ClimGrowingSeason_0.01,
                   AccFrostDays_0.05,AccFrostDays_0.01,
                   StartFieldOp_0.05,StartFieldOp_0.01))
colnames(nonMK) <- c('Region', myAgroShortF1)
nonMK <- nonMK[,-c(4,6)] %>%
  mutate(Region = factor(Region, levels = myRegTT)) %>%
  arrange(Region) %>%
  gather( key='AgroInd',value="MKsig",-Region)  %>%
  cbind(Trend = NA) %>%
  cbind(TrendPerSig = NA) %>%
  cbind(TrendPerSigPos = NA) %>%
  cbind(TrendPerSigNeg = NA) %>%
  cbind(FirstImp = FALSE)

maizeMK <- maizeMK %>%
  subset(select=-c(GrowDegDay_0.05,GrowDegDay_0.01,
                   HeatStress_0.05,HeatStress_0.01,
                   TotPrecip_0.05,TotPrecip_0.01,
                   DryDay_0.05,DryDay_0.01,
                   SMPlanting_0.05,SMPlanting_0.01,
                   SMMidSeason_0.05,SMMidSeason_0.01,
                   SMHarvestSeason_0.05,SMHarvestSeason_0.01))
colnames(maizeMK) <- c('Region', myAgroShortF2)
maizeMK <- mutate(maizeMK, Region = factor(Region, levels = myRegTT)) %>%
  arrange(Region) %>%
  gather( key='AgroInd',value="MKsig",-Region)%>%
  cbind(Trend = NA) %>%
  cbind(TrendPerSig = NA) %>%
  cbind(TrendPerSigPos = NA) %>%
  cbind(TrendPerSigNeg = NA) %>%
  cbind(FirstImp = FALSE)

riceMK <- riceMK %>%
  subset(select=-c(GrowDegDay_0.05,GrowDegDay_0.01,
                   HeatStress_0.05,HeatStress_0.01,
                   TotPrecip_0.05,TotPrecip_0.01,
                   DryDay_0.05,DryDay_0.01,
                   SMPlanting_0.05,SMPlanting_0.01,
                   SMMidSeason_0.05,SMMidSeason_0.01,
                   SMHarvestSeason_0.05,SMHarvestSeason_0.01))
colnames(riceMK) <- c('Region', myAgroShortF2)
riceMK <- mutate(riceMK, Region = factor(Region, levels = myRegTT)) %>%
  arrange(Region) %>%
  gather( key='AgroInd',value="MKsig",-Region)%>%
  cbind(Trend = NA) %>%
  cbind(TrendPerSig = NA) %>%
  cbind(TrendPerSigPos = NA) %>%
  cbind(TrendPerSigNeg = NA) %>%
  cbind(FirstImp = FALSE)


soyaMK <- soyaMK %>%
  subset(select=-c(GrowDegDay_0.05,GrowDegDay_0.01,
                   HeatStress_0.05,HeatStress_0.01,
                   TotPrecip_0.05,TotPrecip_0.01,
                   DryDay_0.05,DryDay_0.01,
                   SMPlanting_0.05,SMPlanting_0.01,
                   SMMidSeason_0.05,SMMidSeason_0.01,
                   SMHarvestSeason_0.05,SMHarvestSeason_0.01))
colnames(soyaMK) <- c('Region', myAgroShortF2)
soyaMK <- mutate(soyaMK, Region = factor(Region, levels = myRegTT)) %>%
  arrange(Region) %>%
  gather( key='AgroInd',value="MKsig",-Region)%>%
  cbind(Trend = NA) %>%
  cbind(TrendPerSig = NA) %>%
  cbind(TrendPerSigPos = NA) %>%
  cbind(TrendPerSigNeg = NA) %>%
  cbind(FirstImp = FALSE)

wheatMK <- wheatMK %>%
  subset(select=-c(GrowDegDay_0.05,GrowDegDay_0.01,
                   HeatStress_0.05,HeatStress_0.01,
                   TotPrecip_0.05,TotPrecip_0.01,
                   DryDay_0.05,DryDay_0.01,
                   SMPlanting_0.05,SMPlanting_0.01,
                   SMMidSeason_0.05,SMMidSeason_0.01,
                   SMHarvestSeason_0.05,SMHarvestSeason_0.01))
colnames(wheatMK) <- c('Region', myAgroShortF2)
wheatMK <- mutate(wheatMK, Region = factor(Region, levels = myRegTT)) %>%
  arrange(Region) %>%
  gather( key='AgroInd',value="MKsig",-Region) %>%
  cbind(Trend = NA) %>%
  cbind(TrendPerSig = NA) %>%
  cbind(TrendPerSigPos = NA) %>%
  cbind(TrendPerSigNeg = NA) %>%
  cbind(FirstImp = FALSE)

maizeMK <- rbind(nonMK, maizeMK)
riceMK <- rbind(nonMK, riceMK)
soyaMK <- rbind(nonMK, soyaMK)
wheatMK <- rbind(nonMK, wheatMK)

# . . F6.3.b Formating MK Gridcell ====
for (i in 1:length(cropx)){
  if(i == 1){
    cropMK <- maizeMK
  } else if (i == 2){
    cropMK <- riceMK
  } else if (i == 3){
    cropMK <- soyaMK
  } else if (i == 4){
    cropMK <- wheatMK
  }
  
  for (j in 1:length(myRegTTAcr)){
    if (j == 5){
      n <- which(frost >= 1)
    } else if (j == 12){
      n <- which(frost < 1)
    } else {
      m <- which(colnames(regionMask) == myRegTTAcr[j])
      n <- which(regionMask[,m]==1)
    }
    o <- which(cropMK[,1] == myRegTT[j])
    
    # . . . F7.3.b.1 Last Spring Frost ----
    dat <- AgroMK[['LastSpringFrost']][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[1],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[1],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[1],]$Trend <- '+'
      cropMK[o[1],]$TrendPerSig <- round(cropMK[o[1],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[1],]$Trend <- '-'
      cropMK[o[1],]$TrendPerSig <- round(cropMK[o[1],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[1],]$Trend <- '+/-'
    } else {
      cropMK[o[1],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 1){
      cropMK[o[1],]$FirstImp <- TRUE
    }
    
    # . . . F7.3.b.2 First Fall Frost ----
    dat <- AgroMK[['FirstFallFrost']][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[2],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[2],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[2],]$Trend <- '+'
      cropMK[o[2],]$TrendPerSig <- round(cropMK[o[2],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[2],]$Trend <- '-'
      cropMK[o[2],]$TrendPerSig <- round(cropMK[o[2],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[2],]$Trend <- '+/-'
    } else {
      cropMK[o[2],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 2){
      cropMK[o[2],]$FirstImp <- TRUE
    }
    
    # . . . F7.3.b.3 Accumulated Frost Days ----
    dat <- AgroMK[['AccFrostDays']][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[3],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[3],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[3],]$Trend <- '+'
      cropMK[o[3],]$TrendPerSig <- round(cropMK[o[3],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[3],]$Trend <- '-'
      cropMK[o[3],]$TrendPerSig <- round(cropMK[o[3],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[3],]$Trend <- '+/-'
    } else {
      cropMK[o[3],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 3){
      cropMK[o[3],]$FirstImp <- TRUE
    } 
    
    # . . . F7.3.b.4 Growing Degree Days ----
    dat <- AgroMK[[paste0('GrowDegDay_',cropx[i])]][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[4],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[4],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[4],]$Trend <- '+'
      cropMK[o[4],]$TrendPerSig <- round(cropMK[o[4],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[4],]$Trend <- '-'
      cropMK[o[4],]$TrendPerSig <- round(cropMK[o[4],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[4],]$Trend <- '+/-'
    } else {
      cropMK[o[4],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 4){
      cropMK[o[4],]$FirstImp <- TRUE
    }
    
    # . . . F7.3.b.5 Heat Stress Days ----
    dat <- AgroMK[[paste0('HeatStress_',cropx[i])]][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[5],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[5],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[5],]$Trend <- '+'
      cropMK[o[5],]$TrendPerSig <- round(cropMK[o[5],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[5],]$Trend <- '-'
      cropMK[o[5],]$TrendPerSig <- round(cropMK[o[5],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[5],]$Trend <- '+/-'
    } else {
      cropMK[o[5],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 5){
      cropMK[o[5],]$FirstImp <- TRUE
    }
    
    # . . . F7.3.b.6 Total Precipitation ----
    dat <- AgroMK[[paste0('TotPrecip_',cropx[i])]][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[6],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[6],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[6],]$Trend <- '+'
      cropMK[o[6],]$TrendPerSig <- round(cropMK[o[6],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[6],]$Trend <- '-'
      cropMK[o[6],]$TrendPerSig <- round(cropMK[o[6],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[6],]$Trend <- '+/-'
    } else {
      cropMK[o[6],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 6){
      cropMK[o[6],]$FirstImp <- TRUE
    }
    
    # . . . F7.3.b.7 Dry Days ----
    dat <- AgroMK[[paste0('DryDay_',cropx[i])]][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[7],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[7],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[7],]$Trend <- '+'
      cropMK[o[7],]$TrendPerSig <- round(cropMK[o[7],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[7],]$Trend <- '-'
      cropMK[o[7],]$TrendPerSig <- round(cropMK[o[7],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[7],]$Trend <- '+/-'
    } else {
      cropMK[o[7],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 7){
      cropMK[o[7],]$FirstImp <- TRUE
    }
    
    # . . . F7.3.b.8 Field Conditions Planting ----
    dat <- AgroMK[[paste0('SMPlanting_',cropx[i])]][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[8],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[8],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[8],]$Trend <- '+'
      cropMK[o[8],]$TrendPerSig <- round(cropMK[o[8],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[8],]$Trend <- '-'
      cropMK[o[8],]$TrendPerSig <- round(cropMK[o[8],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[8],]$Trend <- '+/-'
    } else {
      cropMK[o[8],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 8){
      cropMK[o[8],]$FirstImp <- TRUE
    }
    
    # . . . F7.3.b.9 Field Conditions Mid Season ----
    dat <- AgroMK[[paste0('SMMidSeason_',cropx[i])]][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[9],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[9],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[9],]$Trend <- '+'
      cropMK[o[9],]$TrendPerSig <- round(cropMK[o[9],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[9],]$Trend <- '-'
      cropMK[o[9],]$TrendPerSig <- round(cropMK[o[9],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[9],]$Trend <- '+/-'
    } else {
      cropMK[o[9],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 9){
      cropMK[o[9],]$FirstImp <- TRUE
    }
    
    # . . . F7.3.b.10 Field Conditions Harvest ----
    dat <- AgroMK[[paste0('SMHarvestSeason_',cropx[i])]][n,]
    dat$tau[dat$pvalue > 0.1] <- NA
    posTrend <- which(dat$tau > 0) %>%
      length()
    negTrend <- which(dat$tau < 0) %>%
      length()
    cropMK[o[10],]$TrendPerSigPos <- posTrend / (posTrend + negTrend)
    cropMK[o[10],]$TrendPerSigNeg <- negTrend / (posTrend + negTrend)
    if (posTrend > negTrend){
      cropMK[o[10],]$Trend <- '+'
      cropMK[o[10],]$TrendPerSig <- round(cropMK[o[10],]$TrendPerSigPos * 100,0)
    } else if (posTrend < negTrend){
      cropMK[o[10],]$Trend <- '-'
      cropMK[o[10],]$TrendPerSig <- round(cropMK[o[10],]$TrendPerSigNeg * 100,0)
    } else if (posTrend == negTrend) {
      cropMK[o[10],]$Trend <- '+/-'
    } else {
      cropMK[o[10],]$Trend <- NA
    }
    if (impVar[[paste0(cropx[i],'Imp')]][j] == 10){
      cropMK[o[10],]$FirstImp <- TRUE
    }
    
  } # End regional j loop
  
  if(i == 1){
    maizeMK <- cropMK
  } else if (i == 2){
    riceMK <- cropMK
  } else if (i == 3){
    soyaMK <- cropMK
  } else if (i == 4){
    wheatMK <- cropMK
  }
} # End i crop loop
# . . F6.3.c Formating Dataframes ====
m <- which(maizeMK$Region == myRegTT[6] & maizeMK$AgroInd == myAgro[1] | 
             maizeMK$Region == myRegTT[6] & maizeMK$AgroInd == myAgro[2])
n <- which(maizeMK$Region == myRegTT[7] & maizeMK$AgroInd == myAgro[1]| 
             maizeMK$Region == myRegTT[7] & maizeMK$AgroInd == myAgro[2])
o <- which(maizeMK$Region == myRegTT[8] & maizeMK$AgroInd == myAgro[1]| 
             maizeMK$Region == myRegTT[8] & maizeMK$AgroInd == myAgro[2])
p <- which(maizeMK$Region == myRegTT[9] & maizeMK$AgroInd == myAgro[1]| 
             maizeMK$Region == myRegTT[9] & maizeMK$AgroInd == myAgro[2])
q <- which(maizeMK$Region == myRegTT[10] & maizeMK$AgroInd == myAgro[1]| 
             maizeMK$Region == myRegTT[10] & maizeMK$AgroInd == myAgro[2])
r <- which(maizeMK$Region == myRegTT[11] & maizeMK$AgroInd == myAgro[1]| 
             maizeMK$Region == myRegTT[11] & maizeMK$AgroInd == myAgro[2])
s <- which(maizeMK$Region == myRegTT[12] & maizeMK$AgroInd == myAgro[1]| 
             maizeMK$Region == myRegTT[12] & maizeMK$AgroInd == myAgro[2])

maizeMK[c(m,n,o,p,q,r,s),3:5]  <- NA; riceMK[c(m,n,o,p,q,r,s),3:5]  <- NA
soyaMK[c(m,n,o,p,q,r,s),3:5]  <- NA; wheatMK[c(m,n,o,p,q,r,s),3:5]  <- NA

# Soya 
m <- which(maizeMK$Region == myRegTT[4] | maizeMK$Region == myRegTT[7])
soyaMK[m, 3:5] <- NA

# . F6.4 Plotting ----------------------------------------------------------------
# . . F6.4.a Maize ====
p1 <- ggplot(maizeMK, aes(x=Region, y=AgroInd, fill=MKsig)) +
  # % of Significant Trends
  geom_tile(aes(fill=MKsig), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) + 
  scale_fill_craftfermenter(
    breaks = myBreaks, 
    na.value= NA,
    palette = "YlOrBr", 
    limits = c(0,100),
    direction = 1,
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=12,
      title = "")) +
  # Importance  
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  # Trend Direction
  # geom_text(aes(label=Trend)) +
  geom_vline(aes(xintercept=5.5)) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfa, labels = myRegAlfaShort) +
  scale_y_discrete(labels=rev(c(myAgroShortF1Red,myAgroShortF2)), 
                   limits=rev(c(myAgroShortF1Red,myAgroShortF2))) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . F6.4.b Rice ====
p2 <- ggplot(riceMK, aes(x=Region, y=AgroInd, fill=MKsig)) +
  # % of Significant Trends
  geom_tile(aes(fill=MKsig), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) +  
  scale_fill_craftfermenter(
    breaks = myBreaks, 
    na.value= NA,
    palette = "YlOrBr", 
    limits = c(0,100),
    direction = 1,
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=12,
      title = "")) +
  # Importance  
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  # Trend Direction
  # geom_text(aes(label=Trend)) +
  geom_vline(aes(xintercept=5.5)) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfa, labels = myRegAlfaShort) +
  scale_y_discrete(labels=rev(c(myAgroShortF1Red,myAgroShortF2)), 
                   limits=rev(c(myAgroShortF1Red,myAgroShortF2))) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . F6.4.c Soy ====
p3 <- ggplot(soyaMK, aes(x=Region, y=AgroInd, fill=MKsig)) +
  # % of Significant Trends
  geom_tile(aes(fill=MKsig), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) + 
  scale_fill_craftfermenter(
    breaks = myBreaks, 
    na.value= NA,
    palette = "YlOrBr", 
    limits = c(0,100),
    direction = 1,
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=12,
      title = "")) +
  # Importance  
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  # Trend Direction
  # geom_text(aes(label=Trend)) +
  geom_vline(aes(xintercept=5.5)) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfa, labels = myRegAlfaShort) +
  scale_y_discrete(labels=rev(c(myAgroShortF1Red,myAgroShortF2)), 
                   limits=rev(c(myAgroShortF1Red,myAgroShortF2))) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . F6.4.d Wheat ====
p4 <- ggplot(wheatMK, aes(x=Region, y=AgroInd, fill=MKsig)) +
  # % of Significant Trends
  geom_tile(aes(fill=MKsig), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) +  
  scale_fill_craftfermenter(
    breaks = myBreaks, 
    na.value= NA,
    palette = "YlOrBr", 
    limits = c(0,100),
    direction = 1,
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=12,
      title = "")) +
  # Importance  
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  # Trend Direction
  # geom_text(aes(label=Trend)) +
  geom_vline(aes(xintercept=5.5)) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfa, labels = myRegAlfaShort) +
  scale_y_discrete(labels=rev(c(myAgroShortF1Red,myAgroShortF2)), 
                   limits=rev(c(myAgroShortF1Red,myAgroShortF2))) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . F6.4.e Legend ====
myLegend <- ggplot(maizeMK, aes(x=Region, y=AgroInd, fill=MKsig)) +
  # % of Significant Trends
  geom_tile(aes(fill=MKsig), size = 1.5) + 
  scale_fill_craftfermenter(
    breaks = myBreaks, 
    na.value= NA,
    palette = "YlOrBr", 
    limits = c(0,100),
    direction = 1,
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=12,
      title = "")) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort)
myLegend <- get_legend(myLegend, position = 'bottom') %>% as_ggplot()

# . . F6.4.f Combining & Save ====
F6 <- plot_grid(p1, p2,
                p3,p4,
                nrow = 2,
                labels = c('A','B','C','D'))
F6 <- plot_grid(F6,
                myLegend,
                nrow = 2,
                rel_heights = c(1,0.05))
ggsave(F6, filename = paste(fileloc2,'Figure6',".tiff", sep=''),
       width = 14, height = 10, dpi = 350, bg='white')
# . SMF5.5 Plotting ----------------------------------------------------------------
# . . SM5.5.a Maize ====
p1 <- ggplot(maizeMK, aes(x=Region, y=AgroInd, fill=Trend)) +
  # % of Significant Trends
  geom_tile(aes(fill=Trend), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) + 
  scale_fill_manual(values = myFrostColors, labels = c('Increasing','Decreasing'),
                    na.value = NA) +
  # Importance  
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  # Trend Direction
  geom_text(aes(label=TrendPerSig)) +
  geom_vline(aes(xintercept=5.5)) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfa, labels = myRegAlfaShort) +
  scale_y_discrete(labels=rev(c(myAgroShortF1Red,myAgroShortF2)), 
                   limits=rev(c(myAgroShortF1Red,myAgroShortF2))) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . SM5.5.b Rice ====
p2 <- ggplot(riceMK, aes(x=Region, y=AgroInd, fill=Trend)) +
  # % of Significant Trends
  geom_tile(aes(fill=Trend), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) + 
  scale_fill_manual(values = myFrostColors, labels = c('Positive','Negative'),
                    na.value = NA) +
  # Importance  
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  # Trend Direction
  geom_text(aes(label=TrendPerSig)) +
  geom_vline(aes(xintercept=5.5)) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfa, labels = myRegAlfaShort) +
  scale_y_discrete(labels=rev(c(myAgroShortF1Red,myAgroShortF2)), 
                   limits=rev(c(myAgroShortF1Red,myAgroShortF2))) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . SM5.5.c Soy ====
p3 <- ggplot(soyaMK, aes(x=Region, y=AgroInd, fill=Trend)) +
  # % of Significant Trends
  geom_tile(aes(fill=Trend), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) + 
  scale_fill_manual(values = myFrostColors, labels = c('Positive','Negative'),
                    na.value = NA) +
  # Importance  
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  # Trend Direction
  geom_text(aes(label=TrendPerSig)) +
  geom_vline(aes(xintercept=5.5)) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfa, labels = myRegAlfaShort) +
  scale_y_discrete(labels=rev(c(myAgroShortF1Red,myAgroShortF2)), 
                   limits=rev(c(myAgroShortF1Red,myAgroShortF2))) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . SM5.5.d Wheat ====
p4 <- ggplot(wheatMK, aes(x=Region, y=AgroInd, fill=Trend)) +
  # % of Significant Trends
  geom_tile(aes(fill=Trend), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) + 
  scale_fill_manual(values = myFrostColors, labels = c('Positive','Negative'),
                    na.value = NA) +
  # Importance  
  # scale_color_manual('FirstImp', values = c('#00000000', 'red')) +
  # Trend Direction
  geom_text(aes(label=TrendPerSig)) +
  geom_vline(aes(xintercept=5.5)) +
  labs(title= "", y="", x="") +
  theme_light() +
  theme(legend.position= "bottom") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = myRegTTShort) +
  # scale_x_discrete(guide = guide_axis(angle = 45), limits = myRegAlfa, labels = myRegAlfaShort) +
  scale_y_discrete(labels=rev(c(myAgroShortF1Red,myAgroShortF2)), 
                   limits=rev(c(myAgroShortF1Red,myAgroShortF2))) + 
  theme(legend.position = "NULL") +
  theme(axis.text.x = element_markdown())
# . . SM5.5.e Legend ====
myLegend <- ggplot(maizeMK, aes(x=Region, y=AgroInd, fill=Trend)) +
  # % of Significant Trends
  geom_tile(aes(fill=Trend), size = 1.5) + 
  # geom_tile(aes(fill=MKsig, color = FirstImp), size = 1.5) + 
  scale_fill_manual(values = myFrostColors, labels = c('Increasing','Decreasing'),
                    na.value = NA) + 
  guides(fill=guide_legend(title=""))

myLegend <- get_legend(myLegend, position = 'bottom') %>% as_ggplot()

# . . SM5.5.e Combine & Save ====
SMF5 <- plot_grid(p1, p2,
                  p3,p4,
                  nrow = 2,
                  labels = c('A','B','C','D'))
SMF5 <- plot_grid(SMF5,
                  myLegend,
                  nrow = 2,
                  rel_heights = c(1,0.05))
ggsave(SMF5, filename = paste(fileloc2,'SMFigure5',".tiff", sep=''),
       width = 14, height = 10, dpi = 350, bg='white')

# . SMF4.5 Calculations ----------------------------------------------------------------
limits <- matrix(data=NA, nrow = length(names(AgroMK)), ncol = 2)
for (i in 1:length(names(AgroMK))){
  dat <- AgroMK[[i]]
  obs <- dat[,5:6]
  df_obs <- data.frame(dat$lon, dat$lat, obs, round(obs$tau,2))
  colnames(df_obs) <- c('lon', 'lat', 'tau', 'pvalue', 'new_tau')
  limits[i,] <- t(as.matrix(round(range(df_obs$tau, na.rm = TRUE))))
}
mylimits <- c(min(limits[,1]), max(limits[,2]))

yd <- cbind(maizeYd$lat, maizeYd$lon, maizeYd$y2002, riceYd$y2004, soyaYd$y2007, wheatYd$y2002)
colnames(yd) <- c('lat', 'lon', 'Maize','Rice','Soya','Wheat')
yd <- as_tibble(yd)

# . . SM4.5.a GDD ====
# Maize ____
dat <- AgroMK[['GrowDegDay_maize']]
obs <- dat[,5:6]
df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
mybreaks <- seq(-1, 1, 0.25)
df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
df_obs$tau[is.na(yd$Maize)] <- NA
df_obs$new_tau[is.na(yd$Maize)] <- NA

p1A <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = '', y = 'Latitude') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")
# Rice ____
dat <- AgroMK[['GrowDegDay_rice']]
obs <- dat[,5:6]
df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
mybreaks <- seq(-1, 1, 0.25)
df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
df_obs$tau[is.na(yd$Rice)] <- NA
df_obs$new_tau[is.na(yd$Rice)] <- NA

p1B <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = '', y = 'Latitude') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")
# Soya ____
dat <- AgroMK[['GrowDegDay_soya']]
obs <- dat[,5:6]
df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
mybreaks <- seq(-1, 1, 0.25)
df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
df_obs$tau[is.na(yd$Soya)] <- NA
df_obs$new_tau[is.na(yd$Soya)] <- NA

p1C <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = '', y = 'Latitude') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")
# Wheat ____
dat <- AgroMK[['GrowDegDay_wheat']]
obs <- dat[,5:6]
df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
mybreaks <- seq(-1, 1, 0.25)
df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
df_obs$tau[is.na(yd$Wheat)] <- NA
df_obs$new_tau[is.na(yd$Wheat)] <- NA

p1D <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")

# . . SM4.5.b Precip ====
# Maize ____
dat <- AgroMK[['TotPrecip_maize']]
obs <- dat[,5:6]
df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
mybreaks <- seq(-1, 1, 0.25)
df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
df_obs$tau[is.na(yd$Maize)] <- NA
df_obs$new_tau[is.na(yd$Maize)] <- NA

p2A <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = '', y = '') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")
# Rice ____
dat <- AgroMK[['TotPrecip_rice']]
obs <- dat[,5:6]
df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
mybreaks <- seq(-1, 1, 0.25)
df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
df_obs$tau[is.na(yd$Rice)] <- NA
df_obs$new_tau[is.na(yd$Rice)] <- NA

p2B <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = '', y = '') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")
# Soya ____
dat <- AgroMK[['TotPrecip_soya']]
obs <- dat[,5:6]
df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
mybreaks <- seq(-1, 1, 0.25)
df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
df_obs$tau[is.na(yd$Soya)] <- NA
df_obs$new_tau[is.na(yd$Soya)] <- NA

p2C <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = '', y = '') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")
# Wheat ____
dat <- AgroMK[['TotPrecip_wheat']]
obs <- dat[,5:6]
df_obs <- data.frame(dat$lat, dat$lon, obs, round(obs$tau,2))
colnames(df_obs) <- c('lat','lon','tau','pvalue','new_tau')
mybreaks <- seq(-1, 1, 0.25)
df_obs$new_tau[df_obs$pvalue > 0.1] <- -999
df_obs$tau[is.na(yd$Wheat)] <- NA
df_obs$new_tau[is.na(yd$Wheat)] <- NA

p2D <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
               colour="black", fill="NA", size=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "", x = 'Longitude', y = '') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm")) +
  theme(legend.position = "NULL")

# . . SM4.5.c Legend ====
myLegend <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
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
      barwidth=20,
      title = "")) +
  # World Map
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "bottom", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm")) 
myLegend <- get_legend(myLegend, position = 'bottom') %>% 
  as_ggplot()

# . . SM4.5.d Combine & Save ====
SMF4 <- plot_grid(p1A, p2A,
                  p1B, p2B,
                  p1C, p2C,
                  p1D, p2D,
                  nrow = 4,
                  labels = c('A','B','C','D','E','F','G','H'))
SMF4 <- plot_grid(SMF4,
                  myLegend,
                  nrow = 2,
                  rel_heights = c(1.,0.1))
ggsave(SMF4, filename = paste(fileloc2,'SMFigure4',".tiff", sep=''),
       width = 8, height = 10, dpi = 350, bg='white')
rm(list=ls()[! ls() %in% c('fileloc1','fileloc2','loc1','loc2','loc3','loc4','loc5','type','per',
                           'get_legend','as_ggplot','craftbrewer_pal','scale_fill_craftfermenter')])

# Figure 6 & 7 #################################################################
# . SM6.1 Variables Needed ------------------------------------------------------
cropx <- c('maize', 'rice','soya', 'wheat')
yr <- c(2002, 2004, 2007, 2002)

agro1<- c('LastSpringFrost', 'FirstFallFrost', 'ClimGrowingSeason', 'AccFrostDays',
          'StartFieldOp')
agro2 <- c('GrowDegDay','HeatStress', 'TotPrecip', 'DryDay', 'SMPlanting',
           'SMMidSeason','SMHarvestSeason')
myAgro <- c(agro1, agro2)
myAgroShort <- c('SpFrost', "FallFrost", "GrowSeason", "FrostDays", "StFieldOp", "GDD",
                 "HeatDays", "Precip",   "Dry Days", "FieldCondP", "FieldCondM", "FieldCondH")
myUnits <- c('DOY','DOY','Days','Days','DOY','?C','Days','mm','Days','Days','Days','Days','Days')
# world map
baseData <- map_data('world')
mycolors <- c('1' = '#D8B365', '0' = '#5AB4AC') #Failure, Nonfailure
colorSpectral <- brewer.pal(11,"Spectral")

# . SM6.2 Opening Files ---------------------------------------------------------
yieldList <- list()
for (i in 1:length(cropx)){
  # Quartile (percentiles 25th and 75th)
  dat <- read_csv(paste(fileloc1, loc1, 'Yield_', cropx[i],'_',type, '_quartile','.csv', sep = ''),
                  col_names = TRUE, cols(.default = col_double()))
  # name <- paste(cropx[i], '_quartile', sep='')
  # yieldList[[name]] <- dat
  if (i == 1){
    yield <- cbind(dat[,1:2], dat[,(5+(yr[i]-1982))])
  } else {
    yield <- cbind(yield, dat[,(5+(yr[i]-1982))])
  }
}
colnames(yield) <- c(colnames(yield[,1:2]), cropx)

AgroInd <- list()
# . . SMEF6.2a Opening of non crop dependent ======
for (i in 1:length(agro1)){
  dat <- read_csv(paste(fileloc1, loc2, agro1[i], '.csv', sep=''),
                  col_names = TRUE, cols(.default = col_double()))
  dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
  
  if (i == 4){
    yield <- cbind(yield, Frost=NA)
    mu <- apply(dat[,5:ncol(dat)], MARGIN = 1, FUN= mean, na.rm=TRUE)
    yield$Frost[mu >= 1] <- 1 #Temperate
    yield$Frost[mu < 1] <- 0 # Tropics
  }
  
  percRank <- apply(dat, MARGIN = 1, quantile, c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
    t() %>%
    as.array()
  dat <- cbind(dat[,1:2], dat[,(5+(yr[1]-1982))], dat[,(5+(yr[2]-1982))],
               dat[,(5+(yr[3]-1982))],dat[,(5+(yr[4]-1982))], NA,NA,NA,NA)
  colnames(dat) <-  c(colnames(yield[,1:2]), cropx, paste0(cropx,'PRank'))
  
  dat$maizePRank[dat$maize <= percRank[,1]] <- 0.09  #10%
  dat$maizePRank[dat$maize > percRank[,1] & dat$maize <= percRank[,2]] <- 0.19 #20%
  dat$maizePRank[dat$maize > percRank[,2] & dat$maize <= percRank[,3]] <- 0.29 #30%
  dat$maizePRank[dat$maize > percRank[,3] & dat$maize <= percRank[,4]] <- 0.39 #40%
  dat$maizePRank[dat$maize > percRank[,4] & dat$maize <= percRank[,5]] <- 0.49 #50%
  dat$maizePRank[dat$maize > percRank[,5] & dat$maize <= percRank[,6]] <- 0.59 #60%
  dat$maizePRank[dat$maize > percRank[,6] & dat$maize <= percRank[,7]] <- 0.69 #70%
  dat$maizePRank[dat$maize > percRank[,7] & dat$maize <= percRank[,8]] <- 0.79 #80%
  dat$maizePRank[dat$maize > percRank[,8] & dat$maize <= percRank[,9]] <- 0.89 #90%
  dat$maizePRank[dat$maize > percRank[,9]] <- 0.99 #>90%
  dat$maizePRank[is.na(yield[,2+1])] <- NA # removes where the crop is not
  
  dat$ricePRank[dat$rice <= percRank[,1]] <- 0.09 #10%
  dat$ricePRank[dat$rice > percRank[,1] & dat$rice <= percRank[,2]] <- 0.19 #20%
  dat$ricePRank[dat$rice > percRank[,2] & dat$rice <= percRank[,3]] <- 0.29 #30%
  dat$ricePRank[dat$rice > percRank[,3] & dat$rice <= percRank[,4]] <- 0.39 #40%
  dat$ricePRank[dat$rice > percRank[,4] & dat$rice <= percRank[,5]] <- 0.49 #50%
  dat$ricePRank[dat$rice > percRank[,5] & dat$rice <= percRank[,6]] <- 0.59 #60%
  dat$ricePRank[dat$rice > percRank[,6] & dat$rice <= percRank[,7]] <- 0.69 #70%
  dat$ricePRank[dat$rice > percRank[,7] & dat$rice <= percRank[,8]] <- 0.79 #80%
  dat$ricePRank[dat$rice > percRank[,8] & dat$rice <= percRank[,9]] <- 0.89 #90%
  dat$ricePRank[dat$rice > percRank[,9]] <- 0.99 #>90%
  dat$ricePRank[is.na(yield[,2+2])] <- NA # removes where the crop is not
  
  dat$soyaPRank[dat$soya <= percRank[,1]] <- 0.09 #10%
  dat$soyaPRank[dat$soya > percRank[,1] & dat$soya <= percRank[,2]] <- 0.19 #20%
  dat$soyaPRank[dat$soya > percRank[,2] & dat$soya <= percRank[,3]] <- 0.29 #30%
  dat$soyaPRank[dat$soya > percRank[,3] & dat$soya <= percRank[,4]] <- 0.39 #40%
  dat$soyaPRank[dat$soya > percRank[,4] & dat$soya <= percRank[,5]] <- 0.49 #50%
  dat$soyaPRank[dat$soya > percRank[,5] & dat$soya <= percRank[,6]] <- 0.59 #60%
  dat$soyaPRank[dat$soya > percRank[,6] & dat$soya <= percRank[,7]] <- 0.69 #70%
  dat$soyaPRank[dat$soya > percRank[,7] & dat$soya <= percRank[,8]] <- 0.79 #80%
  dat$soyaPRank[dat$soya > percRank[,8] & dat$soya <= percRank[,9]] <- 0.89 #90%
  dat$soyaPRank[dat$soya > percRank[,9]] <- 0.99 # >90%
  dat$soyaPRank[is.na(yield[,2+3])] <- NA # removes where the crop is not
  
  dat$wheatPRank[dat$wheat <= percRank[,1]] <- 0.09 #10%
  dat$wheatPRank[dat$wheat > percRank[,1] & dat$wheat <= percRank[,2]] <- 0.19 #20%
  dat$wheatPRank[dat$wheat > percRank[,2] & dat$wheat <= percRank[,3]] <- 0.29 #30%
  dat$wheatPRank[dat$wheat > percRank[,3] & dat$wheat <= percRank[,4]] <- 0.39 #40%
  dat$wheatPRank[dat$wheat > percRank[,4] & dat$wheat <= percRank[,5]] <- 0.49 #50%
  dat$wheatPRank[dat$wheat > percRank[,5] & dat$wheat <= percRank[,6]] <- 0.59 #60%
  dat$wheatPRank[dat$wheat > percRank[,6] & dat$wheat <= percRank[,7]] <- 0.69 #70%
  dat$wheatPRank[dat$wheat > percRank[,7] & dat$wheat <= percRank[,8]] <- 0.79 #80%
  dat$wheatPRank[dat$wheat > percRank[,8] & dat$wheat <= percRank[,9]] <- 0.89 #90%
  dat$wheatPRank[dat$wheat > percRank[,9]] <- 0.99 # >90%
  dat$wheatPRank[is.na(yield[,2+4])] <- NA # removes where the crop is not
  
  name <- agro1[i]
  AgroInd[[name]] <- dat
}
# . . SMF6.2b Opening of crop dependent ====
for (i in 1:length(agro2)){
  for(j in 1:length(cropx)){
    dat <- read_csv(paste(fileloc1, loc2, agro2[i],'_',cropx[j], '.csv', sep=''),
                    col_names = TRUE, cols(.default = col_double()))
    dat <- dat[, -which(names(dat) %in% c('1981','2017','2018'))]
    
    percRank <- apply(dat[,5:ncol(dat)], MARGIN = 1, quantile, c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
      t() %>%
      as.array()
    dat <- cbind(dat[,1:2], dat[,(5+(yr[j]-1982))], NA)
    colnames(dat) <-  c(colnames(yield[,1:2]), 'Crop', 'PRank')
    
    
    dat$PRank[dat$Crop <= percRank[,1]] <- 0.09 #10%
    dat$PRank[dat$crop > percRank[,1] & dat$Crop <= percRank[,2]] <- 0.19 #20%
    dat$PRank[dat$Crop > percRank[,2] & dat$Crop <= percRank[,3]] <- 0.29 #30%
    dat$PRank[dat$Crop > percRank[,3] & dat$Crop <= percRank[,4]] <- 0.39 #40%
    dat$PRank[dat$Crop > percRank[,4] & dat$Crop <= percRank[,5]] <- 0.49 #50%
    dat$PRank[dat$Crop > percRank[,5] & dat$Crop <= percRank[,6]] <- 0.59 #60%
    dat$PRank[dat$Crop > percRank[,6] & dat$Crop <= percRank[,7]] <- 0.69 #70%
    dat$PRank[dat$Crop > percRank[,7] & dat$Crop <= percRank[,8]] <- 0.79 #80%
    dat$PRank[dat$Crop > percRank[,8] & dat$Crop <= percRank[,9]] <- 0.89 #90%
    dat$PRank[dat$Crop > percRank[,9]] <- 0.99 # > 90%
    dat$PRank[is.na(yield[,2+j])] <- NA # removes where the crop is not
    
    name <-  paste(agro2[i],"_",cropx[j], sep='')
    AgroInd[[name]] <- dat
  }
}
# . SMF6.3 Formatting Data -----------------------------------------------------
# Yield Formatting
yield$maize[yield$maize > 1] <- 0.5
yield$rice[yield$rice > 1] <- 0.5
yield$soya[yield$soya > 1] <- 0.5
yield$wheat[yield$wheat > 1] <- 0.5

# . SMF6.4 Temperate ====
mybreaks <- seq(0, 1, 0.1)
# . . SMF6.4.1.a Maize ====
i <- 1
# . . . SMF6.4.1.a.1 Variable 1 #P1 ====
var <- paste0(agro2[3],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()
# colorText = brewer.pal(11,"Spectral")[perMu/10] # Too hard to see

p1 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[8], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[8]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))

# . . . SMF6.4.1.a.2 Variable 2 #P2 ====
df_obs <- cbind(AgroInd[['FirstFallFrost']]$lat,AgroInd[['FirstFallFrost']]$lon,
                AgroInd[['FirstFallFrost']]$maize, AgroInd[['FirstFallFrost']]$maizePRank,
                yield[,(2+i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p2 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[2], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[2]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF6.4.1.a.3 Variable 3 #P3 ====
p3 <- NULL 

# . . SMF6.4.1.b Rice ====
i <- 2
# . . . SMF6.4.1.b.1 Variable 1 #P4 ====
var <- paste0(agro2[1],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p4 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[6], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[6]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF6.4.1.b.2 Variable 2 #P5 ====
var <- paste0(agro2[3],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p5 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[8], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[8]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF6.4.1.b.2 Variable 3 #P6 ====
df_obs <- cbind(AgroInd[['LastSpringFrost']]$lat, AgroInd[['LastSpringFrost']]$lon,
                AgroInd[['LastSpringFrost']]$rice, AgroInd[['LastSpringFrost']]$ricePRank, 
                yield[,(2+i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p6 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[1], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[1]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . SMF6.4.1.c Soya ====
i <- 3
# . . . SMF6.4.1.c.1 Variable 1 #P7 ====
var <- paste0(agro2[1],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p7 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[6], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[6]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF6.4.1.c.1 Variable 2 #P8 ====
var <- paste0(agro2[3],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p8 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[8], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[8]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF6.4.1.c.1 Variable 3 #P9 ====
var <- paste0(agro2[4],"_",cropx[3])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p9 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[9], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[9]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . SMF6.4.1.d Wheat ====
i <- 4
# . . . SMF6.4.1.d.1 Variable 1 #P10 ====
var <- paste0(agro2[3],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p10 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[8], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[8]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF6.4.1.d.1 Variable 2 #P11 ====
var <- paste0(agro2[7],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 1 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p11 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[12], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[12]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 0), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF6.4.1.b.1 Variable 3 #P12 =====
p12 <- NULL 
# . . SMF6.4.1.e Legend ====
df_obs <- cbind(AgroInd[['LastSpringFrost']]$lat,AgroInd[['LastSpringFrost']]$lon,
                AgroInd[['LastSpringFrost']][,(2+i)], yield[,(2+i)]) %>%
  as_tibble()
colnames(df_obs) <- c('lat','lon','AgroPRank','FailTransp')
df_obs$AgroPRank[is.na(df_obs$FailTransp)] <- NA

myLegend <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=NULL, x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) + 
  # Significant Values
  metR::geom_contour_fill(aes(z = AgroPRank)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20,
      title = "")) +
  theme(legend.position= "bottom", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm")) 
myLegend <- get_legend(myLegend, position = 'bottom') %>% 
  as_ggplot()

# . . SMF6.4.1.f Combine and Save ====
# EF1 <- plot_grid(p1,p4,p7,p10,
#                  p2,p5,p8,p11,
#                  p3,p6,p9,p12,
#                  ncol = 4) # rel_heightsc(0.01,.8,.1), width = 10, height=6
EF1 <- plot_grid(p1,p2,p3,
                 p4,p5,p6,
                 p7,p8,p9,
                 p10,p11,p12,
                 ncol = 3) # width = 11, height=9
EF1 <- plot_grid(NULL,
                 EF1,
                 myLegend,
                 ncol=1,
                 rel_heights = c(0.01,0.8,0.1))
ggsave(EF1, filename = paste0(fileloc2, 'SMFigure6','.tiff'), 
       width = 11, height = 9, dpi=350, bg='white')
# . SMF7.5 Tropics ====
# . . SMF7.5.2.a Maize ====
i <- 1
# . . . SMF7.7.2.a.1 Variable 1 #P1 ====
var <- paste0(agro2[1],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p1 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[6], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[6]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF7.7.2.a.2 Variable 2 #P2 ====
var <- paste0(agro2[7],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p2 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[12], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[12]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF7.7.2.a.3 Variable 3 #P3 ====
var <- paste0(agro2[5],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p3 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[10], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[10]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))

# . . SMF7.5.2.b Rice ====
i <- 2
# . . . SMF7.7.2.a.1 Variable 1 #P4 ====
var <- paste0(agro2[5],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p4 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[10], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[10]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF7.7.2.a.2 Variable 2 #P5 ====
var <- paste0(agro2[1],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p5 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[6], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[6]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . . SMF7.7.2.a.3 Variable 3 #P6 ====
var <- paste0(agro2[6],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p6 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[11], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[11]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))
# . . SMF7.5.2.c Soya ====
i <- 3
# . . . SMF7.7.2.a.1 Variable 1 #P7 ====
var <- paste0(agro2[1],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p7 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[6], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[6]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))

# . . . SMF7.7.2.a.2 Variable 2 #P8 ====
var <- paste0(agro2[7],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p8 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[12], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[12]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))

# . . . SMF7.7.2.a.3 Variable 3 #P9 ====
p9 <- NULL 

# . . SMF7.5.2.d Wheat ====
i <- 4
# . . . SMF7.7.2.a.1 Variable 1 #P10 ====
var <- paste0(agro2[1],"_",cropx[i])
df_obs <- cbind(AgroInd[[var]]$lat,AgroInd[[var]]$lon,AgroInd[[var]]$Crop,
                AgroInd[[var]]$PRank, yield[,(2 + i)], yield$Frost) %>%
  as_tibble(.name_repair = 'unique')
colnames(df_obs) <- c('lat','lon','AgroIndV','AgroPRank','FailTransp', 'TempTrop')
m <- which(df_obs$TempTrop == 0 & df_obs$FailTransp == 1)
perMu <- round(df_obs$AgroPRank[m],1) %>%
  mean(na.rm=TRUE) + 0.05
perMu <- round(perMu, 1) * 100
indMu <- df_obs$AgroIndV[m] %>% mean(na.rm = TRUE) %>% 
  round(0)
perInd <- quantile(AgroInd[[var]]$Crop[m], c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm = TRUE) %>%
  t() %>%
  as.array()

p10 <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=myAgroShort[6], x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  geom_tile(aes(fill = AgroPRank,alpha= FailTransp)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20)) +
  # Text
  geom_text(x=-120, y=-50, size=3, label=paste0(indMu,' ',myUnits[6]), color = 'grey25') +
  # Non Region
  ggnewscale::new_scale("fill") +
  geom_tile(data = subset(df_obs, TempTrop == 1), aes(fill=TempTrop)) +
  scale_fill_gradientn(name = 'Non-significant', colours = "grey80", 
                       na.value="white", limits=c(0,1), guide = 'legend') +
  # World Map
  geom_polygon(data=baseData, size=.25, aes(x=long, y=lat, group=group),
               colour="black", fill="white", alpha=0, inherit.aes = FALSE) +
  # coord_fixed(ratio=1.1, xlim=range(df_obs$lon), ylim=range(df_obs$lat), expand = FALSE, clip = "on") +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  theme(legend.position= "NULL", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm"))

# . . . SMF7.7.2.a.2 Variable 2 #P11 ====
p11 <- NULL 

# . . . SMF7.7.2.a.3 Variable 3 #P12 ====
p12 <- NULL 
# . . SMF7.5.2.e Legend ====
df_obs <- cbind(AgroInd[['LastSpringFrost']]$lat,AgroInd[['LastSpringFrost']]$lon,
                AgroInd[['LastSpringFrost']][,(2+i)], yield[,(2+i)]) %>%
  as_tibble()
colnames(df_obs) <- c('lat','lon','AgroPRank','FailTransp')
df_obs$AgroPRank[is.na(df_obs$FailTransp)] <- NA

myLegend <- ggplot(data= na.omit(df_obs), aes(x= lon, y= lat)) +
  theme_bw() +
  labs(title=NULL, x="", y="") +  
  theme(plot.title = element_text(hjust = 0.5, size=15)) + 
  # Significant Values
  metR::geom_contour_fill(aes(z = AgroPRank)) +
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "Spectral", 
    limits = c(0,1),
    guide = guide_colorsteps(
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black", # you can also remove the ticks with NA
      barwidth=20,
      title = "")) +
  theme(legend.position= "bottom", 
        legend.key.height  = unit(0.5, "cm"), 
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        plot.margin = margin(r=0.5,unit="cm")) 
myLegend <- get_legend(myLegend, position = 'bottom') %>% 
  as_ggplot()

# . . SMF7.5.2.f Combine and Save ====
# EF1 <- plot_grid(p1,p4,p7,p10,
#                  p2,p5,p8,p11,
#                  p3,p6,p9,p12,
#                  ncol = 4) # rel_heightsc(0.01,.8,.1), width = 10, height=6
EF1 <- plot_grid(p1,p2,p3,
                 p4,p5,p6,
                 p7,p8,p9,
                 p10,p11,p12,
                 ncol = 3) # width = 11, height=9
EF1 <- plot_grid(NULL,
                 EF1,
                 myLegend,
                 ncol=1,
                 rel_heights = c(0.01,0.8,0.1))
ggsave(EF1, filename = paste0(fileloc2, 'SMFigure7','.tiff'), 
       width = 11, height = 9, dpi=350, bg='white')

rm(list=ls()[! ls() %in% c('fileloc1','fileloc2','loc1','loc2','loc3','loc4','loc5','type','per',
                           'get_legend','as_ggplot','craftbrewer_pal','scale_fill_craftfermenter')])
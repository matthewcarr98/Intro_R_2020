#Mathhew Carr
#South African elevation 
#Day4 homework

#Loading in packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scales)
library(SDMTools)
library(raster)

#load raw data
load("data\\gebco_sa.asc")

#read the raw data (asc file)
geb_asc <- read.asc("data\\gebco_sa.asc")

#converting asc file to raster
geb_ras <- raster(geb_asc)

#converting raster to spatialpixeldataframe
geb_final <- as(geb_ras, "SpatialPixelsDataFrame")

#set as data frame                                                  
bathy_wide_final <- as.data.frame(geb_final)


#plotting the map showing elevastion

gebco_plot <- ggplot(data = bathy_wide_final, x = x, y = y) +
  geom_raster(aes(x = x, y = y, fill = layer), show.legend = TRUE) +
  scale_fill_gradientn("elevation/ \nDepth (m)", values = scales::rescale(c(-6129, 0, 1, 3374)), colours = c("darkblue", "darkcyan", "cyan", "darkgreen", "khaki")) +
  labs(x = "longitude", y = "latitude") + 
  scale_x_continuous(breaks = seq(10, 35, 5),
                     labels = c("10", "15", "20", "25", "30", "35"),
                     position = "bottom", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides( fill = guide_legend(title = "elevation")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

#view plot
gebco_plot










######unadventurous data (extra work)
#loading in data
##load("data/gebco_sa.Rdata")

# see the borders for the tidy data
##tail(bathy_wide)

#tidying the data to include a column for longitude and elevation
##bw_tidy <- bathy_wide %>% 
  ##gather(key = "long", value = "elev", '7.99166666665':'39.97500006395')#smallest to biggest as seen in tails

#make data numeric for plotting
##bw_tidier <- as.data.frame(apply(bw_tidy, 2, as.numeric))

#plotting the map showing elevastion
#gebco_plot2 <- ggplot(data = bw_tidier , x = long, y = lat) +
  #geom_raster(aes(x = long, y = lat, fill = elev), show.legend = TRUE) +
  #scale_fill_gradientn("elevation/ \nDepth (m)", values = scales::rescale(c(-6129, 0, 1, 3374)), colours = c("darkblue", "darkcyan", "cyan", "darkgreen", "khaki")) +
  #labs(x = "longitude", y = "latitude") + 
  #scale_x_continuous(breaks = seq(10, 35, 5),
                     #labels = c("10", "15", "20", "25", "30", "35"),
                     #position = "bottom", expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +
 #guides( fill = guide_legend(title = "elevation")) +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #panel.background = element_blank())

#gebco_plot2
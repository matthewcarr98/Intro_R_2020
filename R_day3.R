# ggplot
#Matthew Carr
#30 January 2020
# mapping on day 3

#loading packages
library(tidyverse) # activate tidyverse
library(boot) # #library function actv the packages

#Plotting with boot
# Load data
urine <- boot::urine

# Create a quick scatterplot
ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond)) + 
  labs(x = "osmoregulation", y = "pH")


## Mappig in R

# Loading in nthe libraries
library(tidyverse)
library(ggpubr)

# Load data
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
#load("data/MUR.RData")
load("data/MUR_low_res.RData")



# The colour pallette we will use for ocean temperature
# customed made colour palette
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

#mapping the shoreline of south africa
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point()
#lined map, (polygon) 
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "blue", fill = "green", size = 5, aes(group = group)) # The land mask

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "orange", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders

# cutting out the lines not in southern africa boarder
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent

# rename the MUR data
sst <- MUR_low_res

#coloufull plot
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) 

#changing the theme name
P <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

P <- P + guides( fill = guide_legend(title = "temp"))

# add this code to normal line
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  guides( fill = guide_legend(title = "temp"))


final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4) # Fine tune position of legend
  )
final_map

### Chapter 10


library(tidyverse)
library(scales)
library(ggsn)
library(maps)

load("data/africa_map.RData")

ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1

sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", 
           x = 15.1, y = -32.0, 
           size = 5.0, 
           angle = 30, 
           colour = "navy") +
  annotate("text", label = "Indian\nOcean", 
           x = 33.2, y = -34.2, 
           size = 5.0, 
           angle = 330, 
           colour = "springgreen")
sa_2

sa_3 <- sa_2 + 
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
          dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
          transform = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3

sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,  # change these values to to move africa pic 
                    ymin = -30, ymax = -24)
sa_4

sa_final <- sa_4 +
  scale_x_continuous(breaks = seq(16, 32, 4),
                     labels = c("16°E", "20°E", "24°E", "28°E", "32°E"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(-36, -24, 4),
                     labels = c("36.0°S", "32.0°S", "28.0°S", "24.0°S"),
                     position = "right") +
  labs(x = "", y = "")
sa_final



########Exersize 8.5#########
#packages - install
library(tidyverse)
library(ggplot2)
# faceting
library(tidyverse)

library(ggpubr)

eck <-read_csv("C:/Users/BCB User/Desktop/Lab 4/Intro_R_2020/data/ecklonia.csv")
View(ecklonia)

# creating line graph
eck_line1 <- ggplot(data = eck, aes(x = ID, y = stipe_mass, colour = site)) + # add points on line
  geom_line(aes(group = site)) +
  labs(x = "ID", y = "stipe_mass") + # nming axis
  theme(legend.position = "bottom", legend.box = "vertical") # moving legends to the bottom
eck_line1 # ctrl enter to see grapgh

#cretaing a scatter plot
eck_scat<- ggplot(data = eck, aes(x = ID, y = stipe_mass, colour = site)) +
  geom_point(aes(group = site)) + # changing size of points
  labs(x = "ID", y = "stipe mass") +
  theme(legend.position = "bottom", legend.box = "vertical")
eck_scat

# creating a smooth scatter plot
eck_scat2<- ggplot(data = eck, aes(x = ID, y = stipe_mass)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~site, ncol = 2) + # This is the line that creates the facets(makes to one graph into)
  labs(x = "ID", y = "stipe_mass")
eck_scat2




eck_box <- ggplot(data = eck, aes(x = site, y = stipe_mass)) +
  geom_boxplot(aes(fill = site)) +
  labs(x = "site", y = "stipe mass")
eck_box

#Creating histogram (not correct)
#hist_eck <- ggplot(data = eck, aes(x = stipe_mass)) +
# geom_histogram(aes(colour = site, fill = site)) + # colour of the variable that you are plotting and fill it up (can start with code geom_histogram())
#labs(x = "ID", y = "stipe_mass")# namig of histogram
#hist_eck

# Facet everthing together
eck_final <- ggarrange(eck_line1, eck_scat, eck_scat2, eck_box, 
                       ncol = 2, nrow = 2, # Set number of rows and columns
                       labels = c("A", "B", "C", "D"), # Label each figure
                       common.legend = TRUE) # Create common legend
eck_final

#creating a palette
#matt_palette <- c("#48B4B6", "#4CB89A", "#6BB97B", "#90B55F", "#B6AE4E", "#D9A44F")



######## Exersize 11.4#########

# open world map to see cordinates to cut
ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

# Obtaining the map by putting the cordinates of the landmass in  
N_Z <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(160, 180), ylim = c(-30, -60), expand = 0) +
  labs(x = "Longitude", y = "Latitude")# (Force lon/lat extent), cropppingg of the island from the world map using codinates
N_Z

# add names of oceans and landmass on map
N_Z_2 <- N_Z +
  annotate("text", label = "Pacific\nOcean", 
           x = 170, y = -50, # cordinates on the map where words will appear
           size = 5.0, 
           angle = 30, # angle of the text
           colour = "navy") +
annotate("text", label = "New Zealand", 
         x = 169, y = -45, 
         size = 5.0, 
         angle = 30, 
         colour = "red") +
  annotate("text", label = "Tasman/nsea", 
           x = 165, y = -35, 
           size = 5.0, 
           angle = 30, 
           colour = "green")
N_Z_2

#add North star
N_Z_3 <- N_Z_2 +
  north(x.min = 175, x.max = 180, y.min = -51, y.max = -55, # Set location of symbol
        scale = 1.2, symbol = 16)# scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
#          dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
#          transform = TRUE, model = "WGS84") + # Set appearance
N_Z_3

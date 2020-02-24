#Mattehw Carr
#24 fEBRUARY 2020
# R CHEATSHEET

# load in packages
library(tidyverse) # more general functions
library(ggplot2) #use fro graphs

library(readr)
library(ggpubr)
library(dplyr)

# Load in data
df <- read.csv(CSV.name)# can also inport the data from files and the copy past the code

#delim to seperate columns
df <- read_delim("data/df.csv")


# Before you start, codes to see and better understand data
head(df) # first 6 rows of data
tail(df) # last 6 rows of the data
tail(df, n = 2) # n = 2 only shows you the last 2 rows, not 6
View(df) # opens the data frame so that you can see it
glimpse(df) # gives the overview of df                      
dim(df) # dimensions of the df like number of columns and rows
str(df) # structure of the df
summary(df) # summary of the stats of the df, by looking at the columns data (like, min, max, mean, median, etc)
colnames(df) # gives column names of df
names(df) # shows the name of each column in the df


#######################################################################

library(tidyverse)
#Functions used through tidyverse

#select
df_sel <- df %>% # pike, this is used after seleced df/ ran a function and want to add other functions 
  select(x, y) # Selecting specific columns of df, like colomns "x" and "y"

#filter
df_filter_x <- df %>% # obvs_x = observation i.e. rows with "obsv_x" in it
  filter(x == "observation_x") %>% # filters to only include col x and use the "==" to select only rows of "observation X"
  nrow() # number of remaining rows
# "obsv_x" can be a word or value

#filter belw 100
df<100 <- df %>%
  filter(x <100) # to see all values below 100

#slice
df_slice <- df %>% # pike, this is used after seleced df/ ran a function and want to add other functions
  select(x, y) %>% # Selecting specific columns 
  slice(10:20) # slicing rows from 10 to 20 in selected cols, to only show these in new df

#mutate
df1 <-df %>% # creating new df from old df, select df then %>% 
  select(x, y) %>% # seelect certian cols
  mutate(x_half = (x)/2) %>% # mutate will add a new col, with the function/calc and add to the existing dataset 
  group_by(x, y) %>% # grouping cols x and y togeteher after crtian functions
  
  
  # Summarise to get more than one function you added yourself (like max, min, median,mean, quarter, etc)
  df_sum <- df %>% # selecting df to summarize
  summarise(mean = round(mean(x, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(x, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(x, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(x), # sum of the values of that col
            min = min(x), # minimum value of the col
            qrt1 = quantile(x, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(x), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = median(x, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(x),# the maximum value of the col x
            n = n()) # number of observations
df_sum # click on this to view table of summarised data just created

#####################################################################################################################################

#ggplot2 
#Plotting of graphs, etc

library(ggplot2)

#Point graph/ scatter plot

point_graph <- ggplot(data = df, aes(x = x, y = y)) + # ggplot to tell it you are making a plot, data = df to show the data being used, aes stand for aestetics x = xcol, y = ycol
  geom_point(shape = 5, colour = "green", fill = "yellow") + # geom_point is he point/scatter grapgh, shape number is thes siz of points colouris the outline colour and fill fills the rest in that colour
  
  #line graph  
  geom_line <- ggplot(data = df, aes(x = x, y = y)) + # ggplot to tell it you are making a plot, data = df to show the data being used, aes stand for aestetics x = xcol, y = ycol
  geom_line()# instead of points its lines

Line graph
geaom_line2 <- ggplot(data = df, aes(x = x, y = y, colour = z)) + # assign name to graph to save n label
  geom_point() +
  geom_line(aes(group = q)) + # colour lines according to var q
  labs(x = "x (units)", y = "y (units)") # run code and R saves it as "image" as numbers i.e. details

geom_line #to view


# Scatter(point and line) graph
ggplot(data = df, aes(x = x, y = y)) + 
  geom_point() + # creating a pt graph (scatter)
  geom_line(aes(group = z)) # adding line to points

# Adding legend
ggplot(data = df, aes(x = x, y = wei, colour = Diet)) + # same graph just add colour = "legend" to show legend
  geom_point() +
  geom_line(aes(group = Chick))

# smooth add line through point with lm
ggplot(data = df, aes(x = x, y = y, colour = z)) +
  geom_point(aes(x = y)) + # can make var x a func of y using aes
  geom_smooth(method = "lm", size = 1) + # lm = linear model meaning line of best fit
  theme_bw() + # changing theme of graph 
  labs(x = "x title", y = "y title", colour = "legend_name") + # Change legend name
  theme(legend.position = "bottom") # move legend postion to bottom or top

# smooth add line through point with "gam
ggplot(data = df, aes(x = x, y = y, colour = z)) +
  geom_point(aes(x = y)) + # can make var x a func of y using aes
  geom_smooth(method = "gam", size = 1) + # # grouped/ multiple obserrvations not just one
  theme_bw() + # changing theme of graph 
  labs(x = "x title", y = "y title", colour = "legend_name") + # Change legend name
  theme(legend.position = "bottom") # move legend postion to bottom or top

#Histogram (histo only used for frquancy data of continues data)
hist<- ggplot(data = df, aes(x = x)) + # continuous data only need independent var because y is frequent
  geom_histogram(aes(fill = y), position = "dodge", binwidth = 100) + # bins = classes in y col, forms legend, size 100
  labs(x = "x", y = "y") 
hist # view 

#Bar grapgh (not just cont data like histo)
bar <- ggplot(data = df, aes(x = x, y = y)) +
  geom_bar(stat = "identity", aes(fill = x)) + 
  labs(x = "x", y = "y")  
bargraph #view

# Box plot
box <- ggplot(data = df, aes(x = x, y = y)) +
  geom_boxplot(aes(fill = x)) + # adds legend
  labs(x = "x", y = "y")
box


# facetting ( adding multiple graphhs together, that has same x and y axis)
library(ggpubr)

ggplot(data = df, aes(x = x, y = y, colour = z)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~z, ncol = 2) + # this line is where it add the graphs togethr
  labs(x = "x (units)", y = "y (units)")

# Arranging diff plot together
ggarrange(line, point, hist, box, # add names of all plots want to add
          ncol = 2, nrow = 2, # Set number of rows and cols
          labels = c("A", "B", "C", "D"), # Label 
          common.legend = TRUE) # Create common legend ((all must have same legend i.o.t. do this))all must have same legend

####################################################################################################################################################

#Mapping
#BECAUSE MAPPPING REQUIRES CERTIAN DF LIKE BORDER AND PROVINCES, I WILL BE KEEPING THE REAL DATASETS IN TO DIFFERINTIATE

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
  scale_fill_manual("Temp. (?C)", values = cols11) +
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


#######NZ map############
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

################################################################################################################




#Matthew Carr
#excersize 2
# 13 March 2020

#Loading packages 
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggthemes)
library(ggpubr)
library(readxl)
library(color )
library(RColorBrewer)
library(ggpubr)

# Question 1 --------------------------------------------------------------

#Loading the data
load("data/SACTNmonthly_v4.0.RData")

# calculate yearly mean
sactn <- SACTNmonthly_v4.0 %>% 
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate:: day(date))

sac_mean <- sactn %>% #Load data %>% #separate the dates into own category 
  group_by(site, year,src) %>% #group_by to get the mean of every differrent site and year, not just overall mean
 mutate(mean_temp = round(temp), 2) %>% #create new column called mean temp
  summarise(mean_temp = mean(temp, na.rm = TRUE)) #get the mean of every site


#plotting the means

na.omit(monthly_2)#omit the na values from the data

#MAke grapghs numeric
sac_mean$year <-  as.numeric(sac_mean$year)

#Creating a plot

mean_plot <- sac_mean %>% 
  filter(src == "KZNSB") %>% # filter to only include the observation from KZNSB
  ggplot(data = , aes(x= year, y = mean_temp))+
  geom_line(aes() , colour = "red", size = 1)+#Changing to make the grapgh red
  scale_x_continuous(breaks = seq(1980, 2000, 20)) + # Show from year 1980 to 2000 (from 1980 to 2000, in incruments of 20)
  scale_y_continuous(breaks = seq(20, 24, 2))+ 
  facet_wrap(.~site, nrow = 9)+#facets all the different sites and order them by 9 coloumns
  labs( x = "Year", y = "Temperature (degrees Celcuis)") + #name the axis
  ggtitle("KZNSB: series of annual means")# adding a title


mean_plot


# question 2 --------------------------------------------------------------



#Loading the data

laminaria <- read_excel("data/laminaria.xlsx")

#laminaria <- read_delim("data/laminaria.csv", 
                       # ";", escape_double = FALSE, trim_ws = TRUE)

#removing na values 
na.omit(laminaria)

#selecting columns of interest

lam_sel <-  laminaria %>% 
  select(region, site, blade_length, blade_weight)


plot_lam <- lam_sel %>% 
  filter(region == "FB") %>% 
  ggplot(aes(x = blade_length, y = blade_weight, colour = site)) + 
  geom_point() + 
  geom_line(aes(x = blade_length, y = blade_weight)) +
  scale_x_continuous(breaks = seq(100, 175, 25)) +
  scale_y_continuous(breaks = seq(0, 3, 1)) +
  facet_wrap(.~site)+#
  labs(x= "blade length", y = "blade weight") +
  scale_color_brewer(palette = "Accent")
plot_lam

#The Roman rock is not shown in this graph because the colour palette only has 8 colours and not 9 to colour
#in all 9 graphs, so it is not shown. To fix this just run the code again, but this time use another palette 
#that has atleast 9 colours in it, like the palette "Paired" shown below.


plot_lam_fix <- lam_sel %>% 
  filter(region == "FB") %>% 
  ggplot(aes(x = blade_length, y = blade_weight, colour = site)) + 
  geom_point() + 
  geom_line(aes(x = blade_length, y = blade_weight)) +
  scale_x_continuous(breaks = seq(100, 175, 25)) +
  scale_y_continuous(breaks = seq(0, 3, 1)) +
  facet_wrap(.~site)+#
  labs(x= "blade length", y = "blade mass (kg)") +
  scale_color_brewer(palette = "Paired (cm)")
plot_lam_fix


arrange <-ggarrange(plot_lam, plot_lam_fix, 
          labels = c("A", "B"),
          ncol = 2)

arrange

# Question 3 --------------------------------------------------------------



#loading in program datasets

tooth <- datasets::ToothGrowth

sd <- ToothGrowth%>% 
  group_by(supp, dose) %>% #group by used the grapghs variables
  summarise(mean_len = mean(len),# mean length and stdv length
            sd_len = sd(len))#sd of len


#teeth_plot <- ToothGrowth#renamed teeth_plot

tooth_plot <- ggplot(sd, aes(x = dose, y = mean_len, fill = supp)) +
  geom_col(aes(fill = supp), position = "dodge", colour = "black") + #create columns
  geom_errorbar(aes(ymin = mean_len - sd_len, #creates error bar for the sd min and max
                    ymax = mean_len + sd_len),
                position = "dodge") +
  labs(x = "Dose (mg/d)", y = "Tooth length (mm)") +#name axis
  ggtitle("Dosage of supplements in relation to tooth growth")#creates title
tooth_plot


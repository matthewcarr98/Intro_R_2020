#summary stats
#29th jan 2020
#matthew Carr

#loadinng package
library(tidyverse)
library(ggplot2)

#loading data

laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

laminaria %>% #chose data frame
  summarise(avg_bld_wdt = mean(blade_length)) #calculate mean blade length

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length)) # Create a summary of the sd of the total lengths


laminaria %>% 
  group_by(site == "kommetjie", add =TRUE) %>% 
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length),
            med_stp_ln = median(total_length),
            var_stp_ln = var(total_length)) 

#ploting - function ggplot

ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) + #remebetr to plus and not pike ( %>% )
  geom_point(shape = 22, colour = "blue", fill = "white") + # anything in the geom() will influences the geom point
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")

#plotting 

Chickweight <- datasets::ChickWeight

ggplot(data = ChickWeight, aes(x = Time, y = weight)) + 
  geom_point(shape = 1, colour = "blue", fill = "white") + #point on the lines
  geom_line(aes(colour = "red", group = Chick)) 
#labs(x = "Time (kg)", y = "weight (g)")


ggplot(data = ChickWeight, aes(x = Time, y = weight, colour= Diet)) + 
  geom_point() + #point on the lines
  geom_line(aes( group = Chick))

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) + # aes in goe point makes weight cicles size of yoour labels
  geom_smooth(method = "lm", size = 1.2)

# faceting
library(tidyverse)
#packages - install
library(ggpubr)


ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~Diet, ncol = 2) + # This is the line that creates the facets
  labs(x = "Days", y = "Mass (g)")


ChickLast <- ChickWeight %>% 
  filter(Time == 21) # takes up to 21 in time polt

line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) + # you can highlight the name (line1) and crl enter to see the plot
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)")
line_1 # you can highlight the name (line1) and crl enter to see the plot

lm_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Days", y = "Mass (g)")
lm_1


# Note that we are using 'ChickLast', not 'ChickWeight'
histogram_1 <- ggplot(data = ChickLast, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")
histogram_1

# Note that we are using 'ChickLast', not 'ChickWeight'
box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
box_1

ggarrange(line_1, lm_1, histogram_1, box_1, # ,ake sure ggplot is install
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend




# Exersize6
#plotting 

#Install packages
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Beaver2 build in dataset
beaver2 <- datasets::beaver2

# plotting line graph
b_line1 <- ggplot(data = beaver2, aes(x = time, y = temp, colour = day)) +
  geom_point() + # add points on line
  geom_line(aes(x = time, y = temp, colour = day, size = 21)) +
  labs(x = "time", y = "temp") + # nming axis
  theme(legend.position = "bottom", legend.box = "vertical") # moving legends to the bottom
b_line1 # ctrl enter to see grapgh

#Plotting scatter grapgh
b_scat<- ggplot(data = beaver2, aes(x = time, y = temp, colour = day)) +
  geom_point(aes(x = time, y = temp, colour = day, size = temp)) + # changing size of points
  labs(x = "time", y = "temp") +
  theme(legend.position = "bottom", legend.box = "vertical")
b_scat

#Plotting deom smooth
be_sm <- ggplot(data = beaver2, aes(x = time, y = temp, colour = day)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "10_min", y = "temp_degree_celsuis")
be_sm

# Making ggarrnge adding graphs together
be_ggrng <- ggarrange(b_line1, b_scat, be_sm, # ,ake sure ggplot is install
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C"), # Label each figure
          common.legend = TRUE)# Create common legend

  be_ggrng






#Beaver_1000 <- beaver2[(beaver2[,2]>1000),]
#be_hist <- ggplot(data = Beaver_1000, aes(x = temp)) +
  #geom_histogram(aes(fill = activ), position = "dodge", binwidth = 100) +
  #labs(x = "time_10min", y = "temp_in_degree_celsuis")
# be_hist
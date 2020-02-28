#Matthew Carr
#19 February 2020
# Biostats, day 1
# chapter 2 and 3
###################################################################################################################

# loads the tidyverse functions; it contains the 'as_tibble()' function
library(tidyverse)
# the 'ChickWeight' data are built into R;
# here we assign it as a tibble to an object named 'chicks'
chicks <- as_tibble(ChickWeight)

# always viw data with tail, head, glimpse before doing the rest of the work
head(chicks) # head shows the top 6 rows 
tail(chicks) # tails shows bottom 6 rows 
glimpse(chicks)

tail(chicks, n = 2)# only the last 2 rows, not 6
colnames(chicks) # col names, showing variable names
summary(chicks) #


# combine a series of numbers into a vector;
# hint: use this function in the exercises that we will require from you
# later on...
dat1 <- c(23, 45, 23, 66, 13)
mean(dat1)

chicks %>% #choosing dataset chicks
  summarise(mean_wt = mean(weight))#summarise(the name of the table = the function (mean) of column weiht)

chicks %>% 
  summarise(mean_wt = round(mean(weight), 1))#same, then round the mean of weights to 1 value

chicks %>% 
  summarise(sd_wt = sd(weight))

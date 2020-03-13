#Matthew Carr 3733741
# biostats 4
#13/03/2020
#aim: Looking at anovas


# loading packages\ -------------------------------------------------------


library(tidyverse)
library(ggplot)

# loading the data --------------------------------------------------------

chicks <- as_tibble(ChickWeight)

# do the anylasis ---------------------------------------------------------

# First grab the data


# Then subset out only the sample sets to be compared

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)# looks at the diet 1 &2 in diet column in the tim 21

# the null hypothesis is (include everything) = there is no difference in the weight of chicks in either diet 1 or 2

ht(chicks_sub)

t.test(weight~ Diet, data = chicks_sub)


chicks_sub2 <- chicks %>% 
  filter(Diet %in% c(1, 4), Time == 21)

t.test(weight~ Diet, data = chicks_sub2)



chicks_all <- chicks %>% 
  select(weight, Diet,Time) %>% 
  filter(Time == 21)#
chicks_all

#mean_c <- chicks_all %>%
 # group_by(Diet) %>% 
  #summarise(mean_weight = mean(weight))
#mean_c

box_mw <- ggplot(data = chicks_all, aes(x = Diet, y = weight))+
  geom_boxplot(aes(fill = Diet))

box_mw


# loading ANOVA -----------------------------------------------------------


#
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

# to seee where tthe significant difference are in 3
TukeyHSD(chicks.aov1)

#Only where both the lwr and upr are above 0 shows thta there is significant difference
plot(TukeyHSD(chicks.aov1))



# null hypothesis is  that there re no differnce between day 1 and day 21 chicks weight


# example of do chicks grow from day 0 to day21 (##########revise###########) -------------------


 chick_growth <- chicks%>% 
   mutate(Time1 = as.factor(Time)) %>% 
   filter(Time %in% c(0,21))

 chick_growth <- aov(weight ~ Time, data = chick_growth)
 
summary(chick_growth)




# example does diet have affect on how they grow ########################(revise)#######################################--------------------------


#chick_growth_diet<- chicks%>% 
  #mutate(Time1 = as.factor(Time)) %>% 
  #filter(Time %in% c(0,12, 21))

summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 12, 21))))

#OR

chick_sample <- chicks%>% 
  mutate(Time1 = as.factor(Time)) %>% 
  filter(Time %in% c(0, 12, 21))

chicks.aov2 <- aov(weight~ Diet + Time, data = chick_sample)

summary(chicks.aov2)

# replace + with *

summary(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 12, 21))))

########################################

#mine
line_diet <- ggplot(data= chicks, aes(x= Time, y= weight, colour = Diet))+
  geom_point()+
  geom_smooth(method = "lm")

line_diet

#long way

chicks_sample2 <- chicks %>% 
  filter(Time %in% c(0, 12, 21)) %>% 
  mutate(Time = as.factor(Time))

chicks.aov3 <- aov(weight ~Diet +Time, data = chicks_sample2)
summary(chicks.aov3)

chicks.aov4 <- aov(weight ~Diet * Time, data = chicks_sample2)
summary(chicks.aov4)


chicks %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_weight = mean(weight)) %>% 
  ggplot(aes(x = Time, y = mean_weight, colour = Diet)) +
  geom_point()+
  geom_line()
 #geom_smooth(method = "lm")

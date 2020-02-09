library(tidyverse)
library(ggplot2)

datasets::BOD

# answer is c


datasets::EuStockMarkets # tidy(?)
datasets::DNase # tidy
datasets::Formaldehyde #tidy
datasets::Orange #TIDY
datasets::UCBAdmissions #tidy(?)

#tidy data are b, c, d, e, f 

#######section 2####################################################

library(dplyr)
library(dslabs)
data("murders")

glimpse(murder)
tail(murders)
head(murders)

#The murders dataset is showing the amount of murders compared to the population of every 
#state in a certain region of the country. The data shows that the higher the population 
#of the state id the higher the amount of murders are.

#mutate the data to show the population as a smaller numerical form
murders <- mutate(murders, population_in_millions = population / 10^6)

st_pop <- murders %>% 
  select(state, population)

# removing Florida state from the data
rm_fl <- murders %>% 
  filter(state != "Florida")

#new data frame that is removing all the southern regoins
no_south <- murders %>% 
  filter(region != "South") # There are 34 states after the south is removed (17 states where removed)

#only selecting the states New York and Texas
ny_tx <-  murders %>% 
  filter(state %in% c("New York", "Texas"))

#Calculate the population size of the South and West regionally

s_w <-murders %>% 
  filter(region %in% c("South", "West" ))
sum_s_w <- sum(s_w$population) #$ = select
#187619987 population in the south and west

#The population size of the Northeast region
NE_r <- murders %>% 
  filter(region %in% c("Northeast"))
ne_pop <- sum(NE_r$population)
#55317240

#plot of murders compared to the poulatoin size
m_c_p <- ggplot(murders) +
  geom_point(aes(x = population, y = total)) +
  geom_smooth(aes(x = population, y = total))
m_c_p

#scatter Plot of murders by region
re_pop <- ggplot(murders, aes(x = region, y = total)) +
  geom_bar(stat = "identity", aes( fill = region))
re_pop

#plot showing the population size comparison of South and west
sw_pop <- ggplot(s_w, aes(x = region, y = population)) +
  geom_bar(stat = "identity", aes( fill = region))
sw_pop
#South has a higher population size

#data frame where the total>20 but <100

btwn_20_100_2 <- filter(murders, total >=20, total <= 100)

#ovr20 <- murders %>% 
#filter(total >20)
#btwn_20_100 <- ovr20 %>% 
# filter(total <100)

#object, containing from 10th to 24th row and 26th row.

slct_row <- slice(murders, 10:24, 26)


murders_tibble <- as_tibble(murders)

murders_tib_group <- murders_tibble %>% 
  group_by(region)

####### section 3###################################################

library(dplyr)
library(dslabs)
data(heights)

#the table shows the height in inches between males and females, 
#on averagethe majority of the heights are males

glimpse(heights)
head(heights)
tail(heights)
names(heights)

#put male and females in own data frame
male <- heights %>% 
  filter(sex == "Male")#== when its not number etc,(string)
female <- heights %>% 
  filter(sex == "Female")
#getting the sd, average, max, min and medion of males and females
avg_male <- male %>% summarise(mean(height))
sd_male <-  male %>% summarise(sd(height))
min_male <-  male %>% summarise(min(height))
max_male <- male %>% summarise(max(height))
med_males <- male %>% summarise(median(height))

avg_female <- female %>% summarise(mean(height))
sd_female <-  female %>% summarise(sd(height))
min_female <-  female %>% summarise(min(height))
max_female <- female %>% summarise(max(height))
med_females <- female %>% summarise(median(height))

#############Section 4######################################################

#loading in vectors

sec4 <- data_frame(x= c( 1, 6, 21, 19 , NA, 73, NA), y = c(NA, NA, 3, NA, 13, 24, NA))

#a) Count the number of elements are missing in both x and y

count_NA <- sec4 %>%
  select(everything()) %>%  
  summarise_all(funs(sum(is.na(.))))

#b) Transform the code, used above a), into a function

new_func <- function(dataset){dataset %>% select(everything()) %>% #crreating a function of a code by replacing the df with function
    summarise_all(funs(sum(is.na(.))))}

#c) Create three new vectors and test the function created in (b)

new_df <- data.frame(x = c( 5, 10, 15, 20 , 25, NA, NA, 40), y = c(16, NA, 23, 8, 11, 12, 56, 21), z = c(4, 5, 1, 15, 89, 65, NA, NA))

#test the fuction of b
test_b <- new_func(new_df)

###############Section 5############################################################
library(ggplot2)
library(dplyr)

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))

Seasonal_tidy <- gather(Seasonal_data, seasons, ssn_values, -year)



#ssn_plot <- ggplot(Seasonal_tidy, aes(x = seasons, y = ssn_values)) +
#geom_bar(stat = "identity", aes( fill = ssn_values))
#ssn_plot


ssn_point <- ggplot(Seasonal_tidy, aes(x = seasons, y = ssn_values)) +
  geom_point(aes(x = seasons, y = ssn_values))
ssn_point

#yr_plot <- ggplot(Seasonal_tidy, aes(x = year, y = ssn_values)) +
#geom_bar(stat = "identity", aes( fill = year))
#yr_plot

yr_point <- ggplot(Seasonal_tidy, aes(x = year, y = ssn_values)) +
  geom_point(aes(x = year, y = ssn_values))
yr_point

#The greater seasonal values will be in the summer as it normally has the highest temperatures and
#longest exposure to sunlight. The hypothesis is correct in by showing in the graphs that the highest value was seen
#in the summer of 2017, with the lowest vallues recorded in winter. The highest values where seen in the year 2017 as
#indicated in  the yr_point graph.


cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   
                   seconds = c(12, 44, 15))
cats_data

cats_sep <- separate(cats_data, position, into = c("first_place", "second_place", "third_place"))

cats_unite <- unite(cats_sep, "total_time", minutes, seconds)

#################section 6###########################################################################

library(readr)

#loading in downloaded csv file with the data
cfseal <- read_csv("cfseal.csv")
View(cfseal)


seal_gather <- gather(cfseal, organs, organ_values, -X1, -age, -weight) 
#put all the columns (excluding age and weight/the organs) in one column by using the gather fuction 

seal_spread <- spread(seal_gather, organs, organ_values )
#use spread to seperate the previously gathered values and spread them out into their own columns

seal_join <- unite(cfseal,"age_weight", age, weight)
#joining 2 columns and their values into one column using unite function

seal_seperate <- separate(seal_join, age_weight,into = c("age", "weight"))
#seperating the combined column of  joining into tqo seperate columns

seal_arrnge <- arrange(cfseal, age)
#arraging the age column by a specific age

seal_select <- select(cfseal, age)
#seleting only a specific coloumn (age)

seal_group <- cfseal %>% 
  group_by(age) %>% 
  summarise(sum_weight = sum(weight))
#summing the total weight of the seals with same age by using group by function

seal_mutate <- mutate(cfseal, maturity = ifelse(age > 60, "old", "young"))
#using mutate to make new column that show if the seals maturity(old or young), by looking if the age is over or below 60

# DAy 1 R
# Data, laminaria, statistical analuses
# 28 January 2020
# Data anylasis and manipulation, intro to R

# loading packages
library(tidyverse)


laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#viewing data
head(laminaria) # first 6 rows
tail(laminaria) # last 6 rows
glimpse(laminaria) # opens the data
names(laminaria) #columns names)

# tidyverse

lam_sub <- laminaria %>% # Tell R which dataframe we are using
  select(site, total_length) # Select only specific columns

lam_slice <- laminaria %>% 
  select(site, total_length) %>% # Select specific columns first
  slice(56:78)

lam_kom <- laminaria %>%
  filter(site == "Kommetjie")

laminaria %>% # Tell R which dataset to use
  filter(site == "Kommetjie") %>% # Filter out only records from Kommetjie
  nrow() # Count the number of remaining rows

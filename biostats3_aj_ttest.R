#Matthew Carr
#biotstats, AJ
#6 March 2020
#T tests


library(tidyverse)
library(ggplot2)

# Random normal data
set.seed(666)

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h


shapiro.test(r_dat$dat)

   
#     
Adat <- r_dat %>% 
  filter(sample == "A")

Bdat <- r_dat %>% 
  filter(sample =="B")

shapiro.test(Adat$dat)

shapiro.test(Bdat$dat)

# the data is normally distrubuted


#Homoscedasticity
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

#two sample two sided t test
# the null hypothesis(H0) that there is no differences in the means of samples belonging to groups A and B

#so reject H0 when p <= 0.05

t.test(dat ~ sample, data = r_dat, var.equal =TRUE)# var.equal=TRUE, variables are same(TRUE)


#chp 6.2

library(ggpubr)

# create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")


mean(r_one$dat)
sd(r_one$dat)

# check normality
shapiro.test(r_one$dat)

# compare random data against a population mean of 20
t.test(r_one$dat, mu = 10)
# two sided cause it could change mu (values) smaller or bigger from given mean 20

#6.2.1

# check against the trailing tail
#compare random data against a population mena of 20
t.test(r_one$dat, mu = 30, alternative = "greater")

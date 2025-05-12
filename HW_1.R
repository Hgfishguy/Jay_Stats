library(tidyverse) # cool shit
library(dplyr) # transformation packages
library(doBy) 
library(moments) # skewdness and kurtosis
library(GGally)
library(openxlsx) # specific to xlsx files
library(readxl) # reads excel files

flounder_raw = read_excel('/Users/suzanneguy/R_Projects/Jay Stats/STATS 2025/Homework Assignments/Homework 1/Flounder_Data.xls')
head(flounder_raw)

flounder = flounder_raw %>%
  filter(!is.na(Spine))

### Question 1

summary(flounder) # mean, median, quartiles
sd(flounder$Length) # standard deviation of variables
sd(flounder$Spine)
sd(flounder$Lapillus)

flounder_st_dev = flounder %>% # saving for later
  filter(!is.na(Spine)) %>%
  summarize(length_sd = sd(Length), spine_sd = sd(Spine), lapillus_sd = sd(Lapillus))
flounder_st_dev

ggplot(data = flounder) + # box plots
  geom_boxplot(aes(y = Length))
ggplot(data = flounder) +
  geom_boxplot(aes(y = Spine)) 
ggplot(data = flounder) +
  geom_boxplot(aes(y = Lapillus))

# skewdness and kurtosis (see moments package)

skewness(flounder$Length)
skewness(flounder$Spine)
skewness(flounder$Lapillus)

kurtosis(flounder$Length)
kurtosis(flounder$Spine)
kurtosis(flounder$Lapillus)

### Question 2

(0.47)^2 # variance
sqrt((0.2209/160)) # SE
(0.47/8.21)*100 # CV
(1.96)*(0.47/sqrt(160)) # 95% CI
(2.57)*(0.47/sqrt(160)) # 99% CI

### Question 3 

flounder$LN_length = log(flounder$Length)
summary(flounder)
sd(flounder$LN_length)

flounder$SL_ratio = (flounder$Spine)/(flounder$Length)
flounder$asinsqrt_SL_ratio = asin(sqrt(flounder$SL_ratio))
summary(flounder)
sd(flounder$asinsqrt_SL_ratio)

hist(flounder$Length)
hist(flounder$Spine)
hist(flounder$Lapillus)
hist(flounder$LN_length)
hist(flounder$SL_ratio)
hist(flounder$asinsqrt_SL_ratio)

### Question 4

(sqrt(2.312)/23.213)*100 # CV
(1.96)*(sqrt(2.312)/sqrt(137)) # 95% CI
(2.57)*(sqrt(2.312)/sqrt(137)) # 99% CI

library(lmtest) # For Durbin-Watson statistic
library(tidyverse) # cool shit (dplyr)
library(doBy) # lowkey forget what this one does
library(moments) # skewdness and kurtosis
library(readxl) # reads excel files
library(nortest) # Lilliefors KS test
library(DescTools) # ’Dunnets test for multiple comparisons of means
library(emmeans) # for estimated marginal means and Bonferroni correction
library(rstatix) # stats
library(GGally) # scatter plot/histograms
library(olsrr) # For Model selection based on R squared
library(car) # for Partial Regression plots

### Question 1

fecundity_data = read_excel('/Users/suzanneguy/R_Projects/Jay_Stats/STATS 2025/Homework Assignments/Homework 8/HW_8_Data.xlsx')

fecundity_clean = na.omit(fecundity_data) # cleaning data





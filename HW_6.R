library(tidyverse) # cool shit (dplyr)
library(doBy) # lowkey forget what this one does
library(moments) # skewdness and kurtosis
library(readxl) # reads excel files
library(nortest) # Lilliefors KS test
library(DescTools) # ’Dunnets test for multiple comparisons of means
library(emmeans) # for estimated marginal means and Bonferroni correction
library(rstatix) # stats
library(GGally) # scatter plot/histograms


### Question 1

# H1 0: The body weight of the crabs is not correlated with the gill weight
# H1: The body weight of the crabs is positively correlated with the gill weight

crab_data = data.frame(Gill_Weight_g = c(0.159, 0.179,0.1,0.045,0.384,0.230,0.1,0.320,0.08,0.220,0.320,0.210),
                       Body_Weight_g = c(14.4,15.2,11.3,2.5,22.7,14.9,1.41,15.81,4.19,15.39,17.25,9.52))

# testing normality of columns using KS test
Transform_normality_KS <- crab_data %>% 
  map(~ tidy(lillie.test(.))) %>%
  bind_rows(.id = "Variable")
Transform_normality_KS
# variables normally distributed! (p > 0.05)

Transform_normality_Shapiro <- crab_data %>%
  map(~ tidy(shapiro.test(.))) %>%
  bind_rows(.id = "Variable")
Transform_normality_Shapiro
# shapiro test also normal

# pearsons correlation test
crab_pearsons <- crab_data %>% # select data
  select(Gill_Weight_g,Body_Weight_g) %>% # select variables you want to check correlation between
  cor_test(method = "pearson") %>% # select method
  mutate(
    cor = round(cor, 3),
    p = ifelse(p < 0.001, "<0.001", round(p, 3)),
    correlation = paste0(cor, " (p=", p, ")")
  )

crab_pearsons_long <- crab_pearsons %>%
  select(var1, var2, correlation) %>%
  pivot_wider(names_from = var2, values_from = correlation)
crab_pearsons_long
# body weight and gill weight positively correlated! (n = 12, r = 0.87, p < 0.001)

vars = c("Gill_Weight_g", "Body_Weight_g")
ggpairs(crab_data[, vars],
        lower = list(continuous = wrap("cor", size = 4, method = "pearson")), 
        upper = list(continuous = wrap("points", alpha = 0.7, size = 1)), # scatter plots
diag = list(continuous = wrap("barDiag", fill = "skyblue")), 
title = "Scatterplot and Histogram Matrix (Pearson variables)")
# visualization of correlation

### Question 2

# Lots of hypotheses possible, all follow these templates
# H0: The paramteter (___) is not correlated with the parameter (___)
# H: The paramteter (___) IS correlated with the parameter (___)

hydro_data = read_excel('/Users/suzanneguy/R_Projects/Jay_Stats/STATS 2025/Homework Assignments/Homework 2/Hydro_Data.xlsx')

vars_2 = c("B_Temp", "B_Oxy", "B_pH", "B_Sal","S_Temp", "S_Oxy", "S_pH", "S_Sal")
ggpairs(hydro_data[, vars_2],
        lower = list(continuous = wrap("cor", size = 4, method = "spearman")), 
        upper = list(continuous = wrap("points", alpha = 0.7, size = 1)), # scatter plots
        diag = list(continuous = wrap("barDiag", fill = "skyblue")), 
        title = "Scatterplot and Histogram Matrix (Spearman variables)")
# this large matrix is an easy way to quickly pick out correlations, though it does not report p-values (must be done individually)
# See plot for significant correlations (denoted with " * ")

### Question 3

# See HW_6_Sample_Size(Q3) for calculations





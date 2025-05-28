library(tidyverse)
library(readxl)
library(nortest)
library(moments)
library(nortest)
library(rstatix) # stats tests
library(DescTools) # non-parametric paired t-tests

### Question 1

hydro = read_excel('/Users/suzanneguy/R_Projects/Jay_Stats/STATS 2025/Homework Assignments/Homework 2/Hydro_Data.xlsx')

lillie.test(hydro$B_Temp) # normal
lillie.test(hydro$S_Temp) # normal
lillie.test(hydro$B_Oxy) # normal
lillie.test(hydro$S_Oxy) # normal
lillie.test(hydro$B_Sal) # normal
lillie.test(hydro$S_Sal) # abnormal
lillie.test(hydro$B_pH) # abnormal
lillie.test(hydro$S_pH) # normal


t.test(hydro$B_Temp, hydro$S_Temp, paired = TRUE, alternative = "two.sided") # not the same!
t.test(hydro$B_Oxy, hydro$S_Oxy, paired = TRUE, alternative = "two.sided") # not the same!
t.test(hydro$B_Sal, hydro$S_Sal, paired = TRUE, alternative = "two.sided") # not the same!
t.test(hydro$B_pH, hydro$S_pH, paired = TRUE, alternative = "two.sided") # not the same!

### Question 2

# are these coral polyps propagated from one specimen? 
coral = data.frame(C_Diet = c(175, 132, 218, 151, 200, 219, 234, 149, 187, 123, 248, 206, 179, 206), 
                   M_Diet = c(142, 311, 337, 262, 302, 195, 253, 199, 236, 216, 211, 176, 249, 214))
summary(coral)

lillie.test(coral$C_Diet) # normal
lillie.test(coral$M_Diet) # normal

<<<<<<< HEAD
# changing data frame to t.test friendly format
long_coral = coral %>%
  pivot_longer(cols = c("C_Diet", "M_Diet"), names_to = "Food", values_to = "Diameter")

long_coral %>% levene_test(Diameter ~ Food) # homogenous data!

t.test(Diameter ~ Food, data = long_coral) # difference in means likely! 
# p-value = 0.01201, t = -2.7242

### Question 3

# factor levels must be numeric to compute wilcox test (non parametric t-test)
coral_manwhit <- long_coral %>% mutate(
  FOOD = factor(Food, levels = c("M_Diet", "C_Diet")),
  FOOD_numeric = as.numeric(FOOD))

wilcox.test(Diameter ~ FOOD_numeric, coral_manwhit) # also finds difference
# p-value larger than parametric alternative (p = 0.02292)

# good for now
=======



>>>>>>> ece835b35d98621d3f9665c8e20e262c707df63b


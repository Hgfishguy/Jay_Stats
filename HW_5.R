library(tidyverse) # cool shit (dplyr)
library(doBy) # lowkey forget what this one does
library(moments) # skewdness and kurtosis
library(readxl) # reads excel files
library(nortest) # Lilliefors KS test
library(DescTools) # ’Dunnets test for multiple comparisons of means
library(emmeans) # for estimated marginal means and Bonferroni correction
library(rstatix) # for statistical tests and summary tools


### Question 1

mesocosm_data = read_excel('/Users/suzanneguy/R_Projects/Jay_Stats/STATS 2025/Homework Assignments/Homework 5/Mesocosm_Data_for_RMAOV.xls')
head(mesocosm_data)

# keep only mixed and non sed-addition tanks

question_1_data = mesocosm_data %>%
  filter(MIX == 1) %>%
  filter(SED == 2)

question_1_data %>% kruskal_test(PROD2 ~ NUT)
# p = 0.0509, just above acceptance threshold :(
# A Kruskal-Wallice test was performed and found no significant difference in mean productivity between nutrient additions
# (n = 9, stat = 5.96, p = 0.0509)

### Question 2

question_2_data = mesocosm_data %>%
  filter(SED == 2)
# removing sed-addition tanks

friedman_data = question_2_data %>%
  group_by(MIX, NUT) %>%
  summarise(mean_prod = mean(PROD2)) %>%
  ungroup() # test doesn't work if data is grouped!

friedman_data %>% friedman_test(mean_prod ~ NUT|MIX)
# No significant affect of nutrient on prod (p = 0.223)

friedman_data %>% friedman_test(mean_prod ~ MIX|NUT)
# no significant affect of mixing on prod (p = 0.0833)

### Question 3

# repeated measures anova

repeat_data = mesocosm_data %>%
  pivot_longer(cols = c("PROD0", "PROD1", "PROD2"), names_to = "Time", values_to = "Chl.a") %>%
  convert_as_factor(TANK, Time) 

repeat_data %>% group_by(MIX, SED, NUT, Time) %>%
  get_summary_stats(Chl.a, type = "mean_sd")

repeat_data %>%
  group_by(Time) %>%
  group_modify(~ tidy(lillie.test(.x$Chl.a)))
# not very normal (other than day 1 ;-; )

repeat_data %>%
  group_by(Time) %>%
  shapiro_test(Chl.a)
# still abnormal!

repeat_data$MIX = as.factor(repeat_data$MIX)
repeat_data$NUT = as.factor(repeat_data$NUT)
repeat_data$SED = as.factor(repeat_data$SED)
# need qualitative explanatory variables for levene test!

repeat_data %>%
  group_by(Time) %>%
  levene_test(Chl.a ~ MIX*SED*NUT, center = "mean")
# levene test shows heteroskedascticity

repeat_aov <- aov(Chl.a ~ (MIX*SED*NUT*factor(Time)) + Error(TANK/Time), data = repeat_data)
summary(repeat_aov)
# nut and sed seem to have significant affect on Chl.a


library(tidyverse)
library(nortest) # needed for Lilliefors Test
library(rstatix) # for statistical tests and summary tools
library(DescTools) # Dunnet’s test for multiple comparisons of means
library(emmeans) # for estimated marginal means and Bonferroni correction

### Question 1 

shrimp_hrt = data.frame(Sex = c("Male", "Male","Male","Male","Male", "Female","Female","Female","Female","Female","Male", "Male","Male","Male","Male", "Female","Female","Female","Female","Female"), 
                        Treatment = c("Hormone","Hormone","Hormone","Hormone","Hormone","Hormone","Hormone","Hormone","Hormone","Hormone", "Control","Control","Control","Control","Control","Control","Control","Control","Control","Control"),
                        Length = c(32.0 , 23.8 , 28.8 , 25.0 , 29.3,39.1 , 26.2 , 21.3 , 35.8 , 40.2,14.5 , 11.0 , 10.8 , 14.3 , 10.0,16.5 , 18.4 , 12.7 , 14.0 , 12.8))

# first, test for normality

# shrimp_hrt$Treatment = as.factor(shrimp_hrt$Treatment)

shrimp_hrt %>%
  group_by(Treatment) %>%
  group_modify(~ tidy(lillie.test(.x$Length)))
# treatment groups normal! (p > 0.05)

shrimp_hrt %>%
  group_by(Sex) %>%
  group_modify(~ tidy(lillie.test(.x$Length)))
# sex groups normal! (p > 0.05)

# levene test
shrimp_hrt %>%
  levene_test(Length ~ Treatment*Sex, center = "mean")
# not homogeneous variance!!! (p << 0.05)

# summary of data
shrimp_hrt %>%
  group_by(Treatment, Sex) %>%
  get_summary_stats(Length, type = "mean_sd")

shrimp_aov = aov(Length ~ Treatment*Sex, data = shrimp_hrt)
summary(shrimp_aov)
# Only treatment seems to have a significant affect on length! 
# Treatment and Sex do not seem to have a harmonic affect! 

plot(shrimp_aov) # residuals!




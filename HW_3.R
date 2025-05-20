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

### Question 2

bahamas_data = data.frame(Lagoon = c("Salt Pond","Salt Pond","Salt Pond","Salt Pond","Salt Pond","Salt Pond","Salt Pond","Salt Pond","Salt Pond","Salt Pond", "Flat Lake","Flat Lake","Flat Lake","Flat Lake","Flat Lake","Flat Lake","Flat Lake","Flat Lake","Flat Lake","Flat Lake"),
                          Season = c("Summer", "Summer","Summer","Summer","Summer", "Winter", "Winter","Winter","Winter","Winter","Summer", "Summer","Summer","Summer","Summer", "Winter", "Winter","Winter","Winter","Winter"),
                          Salinity = c(70.9, 67.9,69.9,68.1,70,59.2,53.8, 47.6,48.3,51.7,65.7,59.4,67.7,63.2,61.8,50.8,50.5,53.9,52.1,51.7))
# normality?
bahamas_data %>%
  group_by(Season) %>%
  group_modify(~tidy(lillie.test(.x$Salinity)))
# normal!

bahamas_data %>%
  group_by(Lagoon) %>%
  group_modify(~tidy(lillie.test(.x$Salinity)))
# Salt Pond abnormal! 

bahamas_aov = aov(Salinity ~ Lagoon*Season, data = bahamas_data)
summary(bahamas_aov)
# Both lagoon and season had a significant affect on Salinity, though no harmonic interaction was detected

plot(bahamas_aov)

library(tidyverse)
library(nortest) # needed for Lilliefors Test
library(rstatix) # for statistical tests and summary tools
library(DescTools) # ’Dunnets test for multiple comparisons of means
library(emmeans) # for estimated marginal means and Bonferroni correction

### Question 1

salinity_data = data.frame(Tide = c("Spring","Spring","Spring","Spring","Spring","Spring","Spring","Spring","Spring","Spring","Spring","Spring","Neap","Neap","Neap","Neap","Neap","Neap","Neap","Neap","Neap","Neap","Neap","Neap"),
                           Site = c(1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,2,2,2,2,3,3,3,3),
                           Salinity = c(21.5,19.6,20.9,22.8,14.5,17.4,15.0,17.8,16.0,20.3,18.5,19.3,14.8,15.6,13.5,16.4,12.1,11.4,12.7,14.5,14.4,14.7,13.8,12.0))
# see hw_3 for part 1a
# normal and homogeneous
salinity_data %>%
  group_by(Tide, Site) %>%
  get_summary_stats(Salinity, type = "mean_sd")

salinity_data$Site = as.factor(salinity_data$Site)

salinity_aov = aov(Salinity ~ Site + Tide, data = salinity_data)
summary(salinity_aov) # both tide and site have an impact (p << 0.0001)

plot(salinity_aov) # looks good!

lillie.test(salinity_aov$residuals) # residuals normally distributed

salinity_data %>% 
  group_by(Site, Tide) %>%
  summarise(Outcome = mean(Salinity, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Site, y = Outcome, color = Tide, group = Tide)) +
  geom_line() +
  geom_point() +
  ylab("Estimated Marginal Means") +
  scale_color_discrete(name = "Tide")

salinity_data %>% 
  group_by(Site, Tide) %>%
  summarise(Outcome = mean(Salinity, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Tide, y = Outcome, color = Site, group = Site)) +
  geom_line() +
  geom_point() +
  ylab("Estimated Marginal Means") +
  scale_color_discrete(name = "Site")

salinity_data %>%
  group_by(Tide) %>%
  get_summary_stats(Salinity, type = "mean_sd")

emmeans(salinity_aov, pairwise ~ Tide)

pairwise.t.test(salinity_data$Salinity, salinity_data$Site,p.adjust.method ="bonferroni")

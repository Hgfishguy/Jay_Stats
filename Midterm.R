library(tidyverse) # cool shit (dplyr)
library(doBy) # lowkey forget what this one does
library(moments) # skewdness and kurtosis
library(readxl) # reads excel files
library(nortest) # Lilliefors KS test
library(DescTools) # ’Dunnets test for multiple comparisons of means
library(emmeans) # for estimated marginal means and Bonferroni correction
library(rstatix) # stats

### Midterm Exam 1
# 5/22/25

### Question 1

# Hypotheses:
# H1: The mean chl a of each pH treatment is different (highest [chl a] for pH of 7)
# H2: The mean chl a of each nutrient addition is different (highest [chl a] for 30_uM)
# H3: There is a significant interaction between pH and nutrient addition 

## Question 1a 

# Using a Model 1 two-factor anova to test hypotheses
# Test assumes data are continuous, normally distributed, have homogeneous variances, and have independent factor levels

# first, we must import the data
nitrate_data = data.frame("pH" = c(4,4,4,5,5,5,6,6,6,7,7,7),
                          "10_uM" = c(5.2,5.9,6.3,7.1,7.4,7.5,7.6,7.2,7.4,7.2,7.5,7.2),
                          "20_uM" = c(7.4,7.0,7.6,7.4,7.3,7.1,7.6,7.5,7.8,7.4,7,6.9),
                          "30_uM" = c(6.3,6.7,6.1,7.3,7.5,7.2,7.2,7.3,7,6.8,6.6,6.4))
summary(nitrate_data)

# converting data into a usable format
nitrate_long = nitrate_data %>%
  pivot_longer(cols = c("X10_uM", "X20_uM", "X30_uM"), names_to = "Nitrate", values_to = "Chl.a")

# checking normality using Lilliefords KS test
# grouping by both pH and nitrate
nitrate_long %>%
  group_by(pH) %>%
  group_modify(~ tidy(lillie.test(.x$Chl.a)))
nitrate_long %>%
  group_by(Nitrate) %>%
  group_modify(~ tidy(lillie.test(.x$Chl.a)))
# All pH groups normally distributed (p >> 0.05)
# # 10_uM nitrate group not normally distributed (p = 0.00102), but ANOVA are robust to small departures from normality

# Checking homogeneity
nitrate_long$pH = as.factor(nitrate_long$pH) # explanatory variables must be factors (qualitative)
nitrate_long$Nitrate = as.factor(nitrate_long$Nitrate)

nitrate_long %>%
  levene_test(Chl.a ~ pH*Nitrate, center = "mean")
# Homogeneous data (p = 0.215)

# getting summary stats for later
nitrate_long %>%
  group_by(Nitrate, pH) %>%
  get_summary_stats(Chl.a, type = "mean_sd")

nitrate_aov = aov(Chl.a ~ pH*Nitrate, data = nitrate_long)
summary(nitrate_aov)
# Interaction factor significant :(

## Question 1b

plot(nitrate_aov) # clustered factors
lillie.test(nitrate_aov$residuals) # normally distributed residuals

# profile plot to parse out interaction
nitrate_long %>%
  group_by(Nitrate, pH) %>%
  summarise(Outcome = mean(Chl.a), .groups = "drop") %>%
  ggplot(aes(x = pH, y = Outcome, color = Nitrate, group = Nitrate)) +
  geom_line() +
  geom_point() +
  ylab("Estimated Marginal Means") +
  theme_bw()
# VERY high interaction at pH = 5, and lines cross several times

# Bonferroni test to determine factor level effect s
pairwise.t.test(nitrate_long$Chl.a, nitrate_long$pH,
                p.adjust.method ="bonferroni")
# Similarities: 4-7,5-6,5-7,6-7
pH_BF = emmeans(nitrate_aov, "pH")
pH_BF

pairwise.t.test(nitrate_long$Chl.a, nitrate_long$Nitrate,
                p.adjust.method ="bonferroni")
# All treatments similar? Interaction of factors skew results
# Similarities: 
NO3_BF = emmeans(nitrate_aov, "Nitrate")
NO3_BF

# Conclusion:

# A model 1, mutlifactor ANOVA with two random factors (pH with four levels and nutrient addition with three)
# was used to determine impacts of acidity and nitrogen supplements on phytoplankton biomass. The data relatively satisfied the assumptions 
# of the model, with only slight departures in normality. Nitrate additions had a significant effect on [Chl a] (F = 10.824, p = 0.000446), 
# though post-hoc bonferroni tests found little difference between levels. pH also had a significant affect on [Chl a] (F = 21.939, p = 4.63e-07),
# with the bonferroni multiple comparisons test yielding 4-7,5-6,5-7,6-7. However, the factor interaction was significant (F = 8.004, p = 8.19e-05), 
# Which may have skewed factor significance. A profile plot returned similar interaction significance. 

## Question 1c

# Using a non-parametric alternative (Friedman test)
# no normality/homogeneous assumptions, data restricted to 1 obs per treatment

# must summarize replicates into one mean for analysis
friedman_data = nitrate_long %>%
  group_by(pH, Nitrate) %>%
  summarise(mean_chla = mean(Chl.a)) %>%
  ungroup()

friedman_pH = friedman_data %>% friedman_test(mean_chla ~ pH|Nitrate)
friedman_pH

friedman_Nitrate = friedman_data %>% friedman_test(mean_chla ~ Nitrate|pH)
friedman_Nitrate
# neither found to be significant (p > 0.05)

# Question 1d 

# The parametric test (model 1, multifactor ANOVA) found a significant effect of pH and nutrients (NO3)
# on phytoplankton biomass. It also found that there was a significant interaction between the factors.
# The non-parametric alternative (Friedman test) found no such significance in factors. 

### Question 2

fishfood_data = data.frame(Site = c(1,2,3,4,5,6),
                           FishMeal = c(58,324,206,94,39,418),
                           Control = c(47,331,163,75,30,397))
fishfood_long = fishfood_data %>%
  pivot_longer(cols = c(FishMeal, Control), names_to = "Feed", values_to = "Worms")
head(fishfood_long)

fishfood_long$Site = as.factor(fishfood_long$Site) # DONT FORGET THIS STEP
fishfood_aov = aov(Worms ~ Site + Feed, data = fishfood_long)
summary(fishfood_aov)
# feed highly significant!!!!

t.test(fishfood_data$FishMeal, fishfood_data$Control, paired = TRUE, alternative = "two.sided")
# found significance, higher error rate?

### Question 3

nematodes_data = data.frame( Muddy = c(2201, 2186, 2296, 2288, 2220, 2241, 2265),
                             Sandy = c(2234, 2215, 2302, 2243, 2238, 2308, NA))
nematodes_long = nematodes_data %>%
  pivot_longer(cols = c( Muddy , Sandy), names_to = "Sediment", values_to = "Count")

nematodes_long %>%
  group_by(Sediment) %>%
  group_modify(~ tidy(lillie.test(.x$Count)))
# normal ish

nematodes_aov = aov(Count ~ Sediment, data = nematodes_long)
summary(nematodes_aov)
# no significance

### Question 4 

nematodes_long %>% kruskal_test(Count ~ Sediment)
# also no sig

### Question 5 

fertilizer_data = data.frame(Plot = c(1,2,3,4,5,6,7,8,9,10), Nutrient = c(142,140,144,144,142,146,149,150,142,148), 
                             Control = c(138,136,147,139,143,141,143,145,136,146))
fertilizer_long = fertilizer_data %>% pivot_longer(cols = c(Nutrient, Control),
                                                   names_to = "Treatment", values_to = "Biomass")
fertilizer_long$Plot = as.factor(fertilizer_long$Plot)
fertilizer_aov = aov(Biomass ~ Treatment + Plot, data = fertilizer_long)
summary(fertilizer_aov)
# treatment significant (slightly)

### Question 6

wilcox = fertilizer_long %>% wilcox_test(Biomass ~ Treatment, p.adjust.method = "bonferroni")
wilcox
# not very significant

### Question 7 


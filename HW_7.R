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

### Question 1

pigment_data = read_excel('/Users/suzanneguy/R_Projects/Jay_Stats/STATS 2025/Homework Assignments/Homework 7/Pigment_Data_usable.xlsx')

# exploring the data
ggplot(data=pigment_data, aes(x = Weight, y = Area)) +
  geom_point(aes(x = Weight, y = Area)) +
  geom_smooth(method = "lm") +
  theme_bw()

# linear regression analysis
pigment_lm = lm(Area ~ Weight, data=pigment_data)
summary(pigment_lm)
# slope of 2389.38, p-value of <<0.001, adj r^2 of 0.9992

dw_stat <- dwtest(pigment_lm)
cat("Durbin-Watson Statistic:", dw_stat$statistic, "\n")
# slight negative autocorrelation (DW = 2.72822)

plot(pigment_lm)
lillie.test(pigment_lm$residuals)
# relativelty normal residuals

ggplot(pigment_data, aes(x = Weight, y = Area)) +
  geom_point(color = "blue") + # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "gray") + # Fit line with 95% CI
  annotate("text",
           x = max(pigment_data$Weight) * 0.8, # where along the x-axis is the text going, I have it set to be at 98% along the total x-axis
           y = min(pigment_data$Area), # where along the y-axis is the text going, I have it set at the bottom
           label = paste0("r\u00B2 = ", round(summary(pigment_lm)$r.squared, 3)), # "\u00B2" is the notation r^2
           size = 4, # size the r-squared value is reported as
           color = "black") +
  labs(x = "pH", y = "Uptake", title = "Linear Regression with 95% Confidence Interval") +
  theme_bw()

# predicting values
values = data.frame(Weight = c(0, 300, 800))
predicted = predict(pigment_lm,values)
head(predicted)

### Question 2

Q2_data = data.frame(X = c(65,100,82,120,133,165,116,150,117,133), Y = c(6.5,6.3,5.9,8.1,14.4,8,6.9,8.7,6.6,6.3))

plot(Q2_data$X,
     Q2_data$Y,
     xlab = "X",
     ylab = "Y",
     main = "Scatterplot of X vs. Y",
     pch = 19, # solid circle point style
     col = "blue") # points colored blue
# Optionally, add a simple trendline
abline(lm(Y ~ X, data = Q2_data),
       col = "red",
       lwd = 2)
# Huge Outlier! 

Q2_adjusted = data.frame(X = c(65,100,82,120,165,116,150,117,133), Y = c(6.5,6.3,5.9,8.1,8,6.9,8.7,6.6,6.3))
# removed outlier

# same code as Q1


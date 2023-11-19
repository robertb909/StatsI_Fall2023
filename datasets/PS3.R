#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# loading packages
lapply(c("stringr", "vioplot", "stargazer","arm"),  pkgTest)

library(ggplot2)
lapply(c(),  pkgTest)

library(dplyr)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

columns <- colnames(data)
columns

data_summary <- summary(data)
data_summary

first_5_rows <- head(data, 5)
first_5_rows

## Question 1

# 1.1 Running a regression where the outcome variable is voteshare and the explanatory variable is difflog.

model1 <- lm(voteshare ~ difflog, data = data)
model1

# Displaying a summary of the regression
summary(model1)

# 1.2 Scatterplot of the two variables incl. regression line.

scatter <-
  ggplot(data, aes(x = difflog, y = voteshare)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Figure 1.1: Voteshare-Difflog Plot ",
       x = "Difference in Campaign Spending (Difflog)",
       y = "Incumbent's Vote Share (Voteshare)") +
  theme_minimal()
scatter

# 1.3 Extracting and saving the residuals in a new object
residuals <- residuals(model1)
rounded_residuals <- round(residuals, 3)

# Displaying
head(rounded_residuals)

# 1.4 Prediction Equation 
ŷ = 0.579 + 0.041*difflog


## Question 2 

# 2.1 Running a regression where the outcome variable is presvote and the explanatory variable is difflog.

model2 <- lm(presvote ~ difflog, data = data)

# Displaying a summary of the regression
summary(model2)

# 2.2 Scatterplot of the two variables incl. regression line.
scatter2 <-ggplot(data, aes(x = difflog, y = presvote)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Figure 2.1: Presvote-Difflog Plot ",
       x = "Difference in Campaign Spending (Difflog)",
       y = "President's Vote Share (Presvote)") +
  theme_minimal()
scatter2

# 2.3
residuals_2 <- residuals(model2)
head(residuals_2)


# 2.4
#Prediction Equation
ŷ = 0.507 + 0.023*difflog


## Question 3

# 3.1 Running a regression where the outcome variable is voteshare and the explanatory variable is presvote.
model3 <- lm(voteshare ~ presvote, data = data)

# Displaying a summary of the regression
summary(model3)

# 3.2 Scatterplot of the two variables incl. regression line.
scatter3 <- ggplot(data, aes(x = presvote, y = voteshare)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Figure 3.1: Voteshare-Presvote Plot",
       x = "President's Vote Share (Presvote)",
       y = "Vote Share (Voteshare)") +
  theme_minimal()

scatter3

# 3.3 Prediction Equation
ŷ = 0.441 + 0.388*presvote

## Question 4 

# 4.1 Regression 
model4 <- lm(residuals_1 ~ residuals_2)
summary(model4)

# 4.2 Creating a scatterplot
scatter4 <- ggplot(data, aes(x = residuals_2, y = residuals_1)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "4.1 Scatterplot of Residuals_1 vs. Residuals_2",
       x = "Residuals_2",
       y = "Residuals_1") +
  theme_minimal()

scatter4

# 4.3 Prediction Equation

ŷ = -5.934e-18 + 2.569e-01*residuals_2

## Question 5

# 5.1 Regression
model5 <- lm(voteshare ~ difflog+presvote, data = data)
summary(model5)

# 5.2 Prediction Equation 
ŷ = 0.448 + 0.0355*difflog + 0.256*presvote



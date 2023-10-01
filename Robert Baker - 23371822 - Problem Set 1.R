#####################
# Robert Baker - 23371822 - Problem Set 1 
# 29 / 09 / 23

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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

library(ggplot2)
lapply(c(),  pkgTest)

library(dplyr)


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# checking data
summary(y)

class(y)


# Finding a 90% confidence interval for the average student IQ 

# Getting the mean

mean_y <- mean(y)
mean_y

# Getting the standard dev.
sd_y <- sd(y)
sd_y

# Getting the Sqr. Root
sqrt_y <- sqrt(length(y))
sqrt_y 

# Now the standard error (standard dev / square root)
standard_error_y <- sd(y)/sqrt_y
standard_error_y

# Calculating the margin of error for a 90% confidence interval using the 
# standard (z) normal distribution (two-tails)

lower_90   <- qnorm(0.05 / 2,
                    mean = mean_y, 
                    sd = sd_y)
upper_90  <- qnorm(0.95 / 2,
                   mean = mean_y,
                   sd = sd_y)

# print
message <- paste("Based on the sample data provided in the vector (y),",
                 "we can estimate with c. 90% confidence",
                 "that the mean is likely to fall within the range of approximately",
                 round(lower_90), "to", round(upper_90))
cat(message)


# Null hypothesis: average student IQ is not higher than nat. av. (μ <= 100)
# Alternative hypothesis: average student IQ is higher than nat av. (μ > 100)

# Calculating the z-statistic for the hypothesis test
z_stat <- (mean_y - 100) / standard_error_y

# Calculating the p-value for the hypothesis test (it's a right-tailed test)
p_value <- 1 - pnorm(z_stat)

# Displaying 
z_stat
p_value

message2 <- paste("The z-statistic of approx.", round(z_stat, 2),
                  "suggests that the sample mean IQ is about", 
                  round(abs(z_stat), 2), "standard errors below", 
                  "the hypothesized population mean of 100.\n",
                  "The p-value of approx.", round(p_value, 2),
                  "is relatively high.", 
                  "This means under the null hypothesis, there is a", 
                  round(p_value * 100, 2), "% chance", 
                  "of observing a sample mean IQ as low as this, or lower,",
                  "just from random sampling variability.")

cat(message2)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)


# Loading packages
# Adopted from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
# with thanks to Hannah Frank for sharing this!

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}


# Exploring the data

head(expenditure)

summary(expenditure)

View(expenditure)

str(expenditure)

levels(expenditure)

# Plotting the relationships among Y, X1, X2, and X3 
# What are the correlations among them?

# Scatterplot 1 - Y v. X1
plot1 <- ggplot(expenditure, aes(x = X1, y = Y, color = factor(Region))) +
  geom_point() +
  labs(title = "Per Capita Income v. Expenditure on Shelters/Housing", x = "Per Capita Personal Income", y = "Per Capita Expenditure on Shelters/Housing") +
  theme_classic()
plot1

# Scatterplot 2 - Y v. X2
plot2 <- ggplot(expenditure, aes(x = X2, y = Y, color = factor(Region))) +
  geom_point() +
  labs(title = "Per Capita Spend on Shelters v. No. of Finacially Insecure Residents", x = "Financially Insecure Residents per 100,000", y = "Per Capita Expenditure on Shelters/Housing Assistance") +
  theme_classic()
plot2

# Scatterplot 3 - Y v. X3
plot3 <- ggplot(expenditure, aes(x = X3, y = Y, color = factor(Region))) +
  geom_point() +
  labs(title = "Urban Resident per 1,000 v. Per Capita Spend on Shelters / Housing", x = "Urban Residents per 1,000", y = "Per Capita Expenditure on Shelters/Housing Assistance") +
  theme_classic()
plot3


# Correlation matrix
cor_matrix <- round(cor(expenditure[, c("Y", "X1", "X2", "X3")]), 2)
cor_matrix


# Creating box plot for Per Capita Expenditure on Shelters/Housing Assistance by Region

# Defining region labels
region_labels <- c("Northeast", "North Central", "South", "West")

ggplot(expenditure, aes(x = as.factor(Region), y = Y, fill = as.factor(Region))) +
  geom_boxplot() +
  labs(title = "Per Capita Expenditure on Shelters/Housing Assistance by Region", 
       x = "Region", 
       y = "Per Capita Expenditure on Shelters/Housing Assistance") +
  scale_x_discrete(labels = region_labels) +
  theme_classic()



# Creating Box plot for Financially Insecure Residents per 100,000 by Region
ggplot(expenditure, aes(x = as.factor(Region), y = X2, fill = as.factor(Region))) +
  geom_boxplot() +
  labs(title = "Financially Insecure Residents per 100,000 by Region", 
       x = "Region", 
       y = "Financially Insecure Residents per 100,000 (X2)") +
  scale_x_discrete(labels = region_labels) +
  theme_classic()

# Box plot for No. of People per Thousand Residing in Urban Areas by Region
ggplot(expenditure, aes(x = as.factor(Region), y = X3, fill = as.factor(Region))) +
  geom_boxplot() +
  labs(title = "No. of People per Thousand Residing in Urban Areas by Region", 
       x = "Region", 
       y = "Number of People per Thousand Residing in Urban Areas (X3)") +
  scale_x_discrete(labels = region_labels) +
  theme_classic()

# Explopring relationship between Y (per capita expenditure on shelters/housing assistance per state) & Region. 
# On average, which region has the highest per capita expenditure on housing assistance?


# Calculating the mean expenditure by region
mean_expenditure_by_region <- expenditure %>%
  mutate(Region_Name = case_when(
    Region == 1 ~ "Northeast",
    Region == 2 ~ "North Central",
    Region == 3 ~ "South",
    Region == 4 ~ "West"
  )) %>%
  group_by(Region_Name) %>%
  summarize(Mean_Expenditure = mean(Y))

# Renaming columns 
colnames(mean_expenditure_by_region) <- c("Region", "Per Capita Mean Expenditure")

# Printing 
print(mean_expenditure_by_region)



# Creating a bar plot for mean expenditure by region
ggplot(mean_expenditure_by_region, aes(x = Region, y = `Per Capita Mean Expenditure`, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(`Per Capita Mean Expenditure`, 1)), vjust = -0.5, size = 3) +  
  labs(title = "Average Per Capita Expenditure on Shelters/Housing Assistance by Region",
       x = "Region",
       y = "Average Per Capita Expenditure") +
  scale_x_discrete(labels = mean_expenditure_by_region$Region) + 
  theme_classic()




# Creating a scatterplot of Y v. X1 
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Relationship between Per Capita Expenditure and Per Capita Personal Income",
       x = "Per Capita Personal Income (X1)",
       y = "Per Capita Expenditure (Y)") +
  theme_classic()



# Creating a scatterplot of Y vs. X1 - with different symbols and colors for regions
ggplot(expenditure, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point(size = 3) +
  labs(title = "Relationship between Per Capita Expenditure, Per Capita Personal Income & Region",
       x = "Per Capita Personal Income",
       y = "Per Capita Expenditure",
       color = "Region",
       shape = "Region") +
  scale_color_manual(values = c("blue", "green", "red", "orange"), 
                     labels = c("Northeast", "North Central", "South", "West")) + 
  scale_shape_manual(values = c(19, 17, 15, 5), 
                     labels = c("Northeast", "North Central", "South", "West")) +  
  theme_classic()





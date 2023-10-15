## Applied Stats / Quant Methods 1
## Robert Baker

## Problem Set 2.1 - Political Science

# Creating a matrix for the observed data 
observed_data <- matrix(c(14, 7, 6, 7, 7, 1), nrow=2, ncol=3)
observed_data

# Calculating row totals
row_totals <- rowSums(observed_data)

# Calculating column totals
col_totals <- colSums(observed_data)

# Calculating total sample size
total_sample_size <- sum(observed_data)

# Checking
total_sample_size

# Initialising empty matrix for 'expected' values
expected_data <- matrix(0, nrow = 2, ncol = 3)

# Calculating expected values using a nested loop
for (i in 1:2) {
  for (j in 1:3) {
    expected_data[i, j] <- (row_totals[i] * col_totals[j]) / total_sample_size
  }
}

expected_data

# checking this is the same as total observed values
sum(expected_data)

# Calculating chi-squared contributions for each cell
chi_squared_contributions <- ((observed_data - expected_data) ^ 2) / expected_data
chi_squared_contributions

# Calculating total chi-squared test statistic
chi_squared_statistic <- sum(chi_squared_contributions)
chi_squared_statistic

# Using R function to check chi-squared 
chi <- chisq.test(observed_data)
chi # these match 

# Calculating Degrees of Freedom
df <- (nrow(observed_data) - 1) * (ncol(observed_data) - 1)
df

# P Value 

# Calculating P-value using the chi-squared distribution
p_value <- 1 - pchisq(chi_squared_statistic, df)
p_value  


# Calculating residuals 
total <- sum(observed_data)

# Check
total

# Calculating standardised residuals for each cell
standardised_residuals <- (observed_data - expected_data) / sqrt(expected_data * (1 - rowSums(expected_data) / total) * (colSums(expected_data) / total))

round(standardised_residuals,2)

sum(standardised_residuals)


####################################


## Problem Set 2.2 - Economics

if (!require("readr")) {
  install.packages("readr")
  library("readr")
}

# loading & reading the data set 
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# Quick Preview
head(data)
str(data)
summary(data)

# Correlations
correlation_matrix <- cor(data)
correlation_matrix


# Histogram
par(mfrow=c(2,3))  
hist(data$GP, main="Area")
hist(data$village, main="Village")
hist(data$reserved, main="Reserved")
hist(data$female, main="Female")
hist(data$irrigation, main="Irrigation")
hist(data$water, main="Water")


# Performing bivariate regression
regression <- lm(water ~ reserved, data=data)
regression

summary(regression_model)
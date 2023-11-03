#set wd

setwd("C:/Users/dring/Documents/GitHub/StatsI_Fall2023/problemSets/PS02/my_answers")
getwd()

# Load packages
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)}

isntall
##################
### Question 1 ###
##################

PolSciTab <- matrix(c(14, 6, 7, 7, 7, 1), ncol = 3, byrow = TRUE)
colnames(PolSciTab) <- c('Not stopped', 'Bribe requested', 'Stopped/given warning')
rownames(PolSciTab) <- c('Upper class', 'Lower class')
PolSciTab <- as.table(PolSciTab)
PolSciTab


## (a)
# Calculating row and column totals
row_tot <- rowSums(PolSciTab)
col_tot <- colSums(PolSciTab)

# calculating grand total
grand_tot <- sum(PolSciTab)

# Calculating the expected values
row_tot
col_tot

exp_val <- matrix(0, nrow = nrow(PolSciTab), ncol = ncol(PolSciTab))
  
for ( i in 1:nrow(PolSciTab)){
  for (j in 1:ncol(PolSciTab)) {
    exp_val[i, j] <- (rowSums(PolSciTab)[i] * colSums(PolSciTab)[j]) / grand_tot
  }
}


# Calculating the Chi squared test statistic
chi_sq <- sum((PolSciTab - ex_val)^2 / ex_val)
chi_sq


## (b)
# Degrees of freedom
deg_f <- (nrow(PolSciTab) -1) * (ncol(PolSciTab) -1)
deg_f
# P-Value
p_val <- pchisq(chi_sq, df = 2, lower.tail = FALSE)
p_val

#The easy way
chi_square <- chisq.test(PolSciTab)
chi_square

# As the P-value is higher than alpha = 0.1, we fail to reject the null hypothesis.
# That is, the result suggests that the variables are independent.



## (c)
# Calculating standardised residuals

# Calculate standardized residuals
std_res <- matrix(0, nrow = nrow(PolSciTab), ncol = ncol(PolSciTab))

# Calculate row and column proportions
row_proportions <- rowSums(PolSciTab) / sum(PolSciTab)
column_proportions <- colSums(PolSciTab) / sum(PolSciTab)

for (i in 1:nrow(PolSciTab)) {
  for (j in 1:ncol(PolSciTab)) {
    observed_value <- PolSciTab[i, j]
    expected_value <- exp_val[i, j]
    row_proportion_i <- row_proportions[i]
    column_proportion_j <- column_proportions[j]
    
    std_res[i, j] <- (observed_value - expected_value) / sqrt(expected_value * (1 - row_proportion_i) * (1 - column_proportion_j))
  }
}

std_res


# Checking my results: 
# Calculate the chi-square test
chisq_result <- chisq.test(PolSciTab)

standardized_residuals <- chisq_result$stdres
standardized_residuals

## (d)

# Agresti and Finlay claim that "values below -3 and above +3 ... are very 
# convincing evidence of a true effect in that cell". However, none of these
# values do, so we have further evidence that these variables are independent.
# So, we have further cause to think that being rich or poor does not affect 
# the likelihood of being asked for a bribe.


##################
### Question 2 ###
##################

Econ <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

## (a)
# Null hypothesis: The reservation policy does not affect the number of new or 
# repaired drinking water facilities in the villages. 
# Alternative hypothesis: The reservation policy affects the number of new or
# repaired drinking water facilities in the villages.

## (b)

str(Econ)

X <- Econ$reserved
Y <- Econ$water

# Subsetting the data I will use, using "water" as my Y and "reserved" as my X
EconRW <- Econ[c("reserved", "water")]
str(EconRW)

# Calculating the slope (beta) and intercept (alpha) by hand
beta <- sum((EconRW$water - mean(EconRW$water)) * (EconRW$reserved - mean(
  EconRW$reserved)))/
  sum((EconRW$reserved - mean(EconRW$reserved))^2)
beta
alpha <- mean(EconRW$water) - beta*mean(EconRW$reserved)
alpha

# Checking my results
reg1 <- lm(EconRW$water~EconRW$reserved, data = EconRW)
reg1
# They seem to be correct

# Calculating the standard deviation
sd_est <- sqrt( sum(resid(reg1)^2) / (dim(EconRW)[1]-2))
sd_est

# or, simply

sigma(reg1)


# SE for beta
beta_se <- sd_est / sqrt(sum((EconRW$reserved - mean(EconRW$reserved))^2))
beta_se

# SE for alpha
alpha_se <- sd_est * sqrt((1/dim(EconRW)[1]) + (mean(EconRW$reserved)^2/sum(
  (EconRW$reserved-mean(EconRW$reserved))^2)))
alpha_se

# TS and P-values
2 * pt((beta-0)/beta_se, dim(EconRW) [1]-2, lower.tail =F) 
2 * pt((alpha-0)/alpha_se, dim(EconRW) [1]-2, lower.tail =F) 

# Checking my answers
model <- summary(lm(EconRW$water~EconRW$reserved, data = EconRW))
model
# The results seem to match!


## (c)

# My interpretation if these results is that we can reject the null hypothesis,
# as the P-value is lower than 0.05. This suggests that villages which have 
# reserved seats for female politicians are more likely to have new or repaired 
# water facilities.

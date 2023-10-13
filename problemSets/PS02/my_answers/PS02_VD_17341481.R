#set wd

setwd("C:/Users/dring/Documents/GitHub/StatsI_Fall2023/problemSets/PS02/my_answers")
getwd()

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()
#load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
library(tidyverse)
library(magrittr)
lapply(c("stringr"),  pkgTest)
install.packages("magrittr")
install.packages("ggplot2")
install.packages("tidyverse")


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

# P-Value
p_val <- pchisq(chi_sq, df = 2, lower.tail = FALSE)


#The easy way
chi_square <- chisq.test(PolSciTab)
chi_square

# As the P-value is higher than alpha = 0.1, we fail to reject the null hypothesis.
# That is, the result suggests that the variables are independent.



## (c)
# Calculating standardised residuals

# easy way

standardized_residuals <- chi_square$stdres
standardized_residuals


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



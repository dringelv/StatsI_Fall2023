# Applied Statistical Analysis I      
# Tutorial 12: Multiple regression, Regression diagnostics  

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
lapply(c("stargazer","ggplot2"),  pkgTest)



# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


### Question 1
# (a)

Prestige$professional <- ifelse(Prestige$type == 'prof', 1, 0)
Prestige$professional

# (b)

lm1 <- lm(Prestige$prestige ~ Prestige$income +
                              Prestige$professional +
                              Prestige$income*Prestige$professional)
summary(lm1)

stargazer(lm1)

# (c)
# pred eq

# (d)
# interpret

# (e)
30.618+(0.001*1000)+22.757

### Question 2

# (a)
# Hypothesis test
0.042/0.016
# Given values
t_statistic <- 2.625
degrees_of_freedom <- 27

# Calculate p-value for a two-sided test
p_value <- 2 * (1 - pt(abs(t_statistic), df = degrees_of_freedom))

# Print the result
cat("p-value:", p_value, "\n")

# (b)
0.042/0.013
# Hypothesis test
# Given values
t_statistic2 <- 3.230769
degrees_of_freedom2 <- 73

# Calculate p-value for a two-sided test
p_value2 <- 2 * (1 - pt(abs(t_statistic2), df = degrees_of_freedom2))

# Print the result
cat("p-value2:", p_value2, "\n")

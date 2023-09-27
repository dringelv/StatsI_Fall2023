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

lapply(c("stringr"),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
print(y)

### Question 1 ###

# calculating the confidence interval
# when the confidence coefficient = .90

# find sum of y
sum(y)

# find mean of y
sum(y)/length(y)
mean(y)
# mean of y = 98.44




lower_90_n <- qnorm(0.05,
                    mean = mean(y),
                    sd = (sd(y)/sqrt(length(y))))
upper_90_n <- qnorm(0.95,
                    mean = mean(y),
                    sd = (sd(y)/sqrt(length(y))))

# print lower bounds of confidence interval
lower_90_n

# print upper bounds of confidence interval
upper_90_n

### Question 2 ###

### Is this school's average IQ higher than the national one?



## Step 1: Assumptions.
# The national average is 100.
meana <- 100
# The national sample has a normal distribution and random sampling.

##Step 2: State hypothesis.
# The average IQ of the students in this school is higher than 
# the average IQ score among all the schools in the country.

##Step 3: Calculate a test statistic.
n <- length(y)
s1 <- sd(y)

y_t <- mean(y) - meana/


# or, a faster way
t.test(y, mu = 100, alternative = "greater")



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

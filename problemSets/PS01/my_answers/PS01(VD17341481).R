#####################
# load libraries
# set wd
# clear global .envir
#####################

# set wd
getwd()
setwd("C:/Users/dring/Documents/GitHub/StatsI_Fall2023/problemSets/PS01/my_answers")

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
library(tidyverse)
library(magrittr)
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stringr"),  pkgTest)
install.packages("magrittr")
install.packages("ggplot2")
install.packages("tidyverse")
#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
print(y)

# Finding the length of y
n <- length(y)
n


### Question 1 ###

# Calculating the confidence interval
# when the confidence coefficient = .90

# Find sum of y
sum(y)

# Find mean of y
sum(y)/length(y)
mean(y)
# mean of y = 98.44

# Finding the standard deviation
sd_y <- sd(y, na.rm = FALSE)
sd_y

# Finding standard error
se_y <- sd_y/sqrt(n)
se_y

# Finding the t-score
t_score <- qt(0.95, df=n-1)
t_score

# Upper bounds of confidence interval
upper_90 <- mean(y)+(t_score)*(sd(y)/sqrt(length(y)))
upper_90

# Lower bounds of confidence interval
lower_90 <- mean(y)-(t_score)*(sd(y)/sqrt(length(y)))
lower_90

# So, the 90% confidence interval for the the average student IQ is 
# 93.96-102.92

### Question 2 ###

### Is this school's average IQ higher than the national one?


## Step 1: Assumptions.
# 1. The national average is 100.
meana <- 100
# 2. The national sample has a normal distribution and random sampling.

## Step 2: State hypothesis.
# The average IQ of the students in this school is higher than the average 
# IQ score among all the schools in the country.
# The null hypothesis then, is that the average IQ in the school is lower or 
# equal to the average of schools nationally.

## Step 3: Calculate a test statistic.
n <- length(y)
sd_y <- sd(y)
se_y <- sd_y/sqrt(n)
y_ts <- (mean(y) - meana)/(se_y)
y_ts

# or, a faster way
t.test(y, mu = 100, alternative = "greater")

## Step 4: Calculate a P-value
p_value <- pt(y_ts,n-1,lower.tail = FALSE)
p_value

# P-value = 0.722

## Step 5: Draw a conclusion
# As the P-value is larger than 0.05, we fail to reject the null hypothesis. 
# So, the average IQ in the school is less than or equal to the average of 
# schools in the country.



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
expenditure

# Familiarising myself with the data
summary(expenditure)
str(expenditure)
head(expenditure)
ls.str(expenditure)


# State - 50 states in the US
# Y - per capita expenditure on shelters/housing assistance in state
# X1 - per capita personal income in state
# X2 - Number of residents per 100,000 that are "financially insecure" in state
# X3 - Number of people per thousand residing in urban areas in state
# Region - 1 = Northeast, 2 = North Central, 3 = South, 4 = West

ggplot(expenditure, aes(X1, Y))+
  geom_point() +
  theme_classic()
# There is a slight positive relationship between Y and X1, on the lower end,
# the relationship is stronger, and gets weaker as values increase.
ggplot(expenditure, aes(X2, Y))+
  geom_point() +
  theme_classic()
# The scatter plot forms a U shape. It seems that the relationship between Y
# and X2 is negative at first, but changes to a positive one near the middle
# of the graph
ggplot(expenditure, aes(X3, Y))+
  geom_point() +
  theme_classic()
# There seems to be a slight positive relationship between Y and X3, as one 
# increases, so does the other
ggplot(expenditure, aes(X2, X1))+
  geom_point() +
  theme_classic()
# The scatter plot forms a U shape. X2 values show little correlation to Y
# values
ggplot(expenditure, aes(X3, X1))+
  geom_point()+
  theme_classic()
# There seems to be a positive relationship between X1 and X3 values. With the 
# exception of some outliers, a lower X1 value correlates to a lower X3 value
# and vice versa.
ggplot(expenditure, aes(X2, X3))+
  geom_point() +
  theme_classic()
# The scatter plot shows a slight U shape. There seems to be very little
# correlation between X2 and X3


## Plotting the relationship between the Region and Y

# A simple plot show the spread of Y according to Region, but this could be 
#clearer
plot(expenditure$Region, expenditure$Y)
expenditure$Y

# Finding the means of Y values grouped by region.
# First, extracting values according to region.
ne_1 <- expenditure[expenditure$Region == 1,] 
nc_2 <- expenditure[expenditure$Region == 2,]
so_3 <- expenditure[expenditure$Region == 3,]
we_4 <- expenditure[expenditure$Region == 4,]

# Then, finding their respective means.
mean1 <- mean(ne_1$Y)
mean2 <- mean(nc_2$Y)
mean3 <- mean(so_3$Y)
mean4 <- mean(we_4$Y)

mean1
mean2
mean3
mean4

# Finally, plotting again, this time showing the means of each region's 
# per capita expenditure on shelters/housing assistance in state
ggplot(expenditure, aes(x=factor(Region), y=Y)) +
  geom_boxplot() +
  stat_summary(fun.y="mean", color="red") +
  labs(title="Box plot", 
       subtitle="Per capita expenditure on shelters/housing
assistance in state grouped by Region",
       x="Region",
       y="Expenditure")
# From this graph we can see that region 4, the western region, has the highest 
# average per capita expenditure on housing assistance.


## Plotting the relationship between Y and X1.
# A simple plot
ggplot(expenditure, aes(X1, Y)) +
  geom_point()

# As discussed above, the graph shows a positive correlation between X1 and Y.

# Plotting the same graph while grouping the results according to region. 
expenditure %>%
  filter(Region %in% c(1,2, 3, 4)) %>%
  group_by(Region) %>%
ggplot(aes(x = X1, y = Y, color = factor(Region), shape = factor(Region)))+
  geom_point() +
  theme_classic()


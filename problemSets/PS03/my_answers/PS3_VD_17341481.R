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

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("stargazer","vioplot","arm","broom","ggplot2","fastDummies"),  pkgTest)


# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

#### Question 1

head(inc.sub)
str(inc.sub)

lm1 <- lm(inc.sub$voteshare ~ inc.sub$difflog)
summary(lm1)

intercept <- coef(lm1)[1]
slope <- coef(lm1)[2]
intercept
slope

scatter1 <-
  ggplot(data =inc.sub, 
         mapping = aes(x = inc.sub$difflog, 
                       y = inc.sub$voteshare)) + 
  geom_point() +
  labs(title = "Scatterplot 1",
       x= 'Difference in campaign spending between incumbent and challenger',
       y = "Incumbent's vote share") +
  geom_smooth(method='lm',col="blue") # Add regression line

scatter1

residuals1 <- lm1$residuals
residuals1

#### Question 2

lm2 <- lm(inc.sub$presvote ~ inc.sub$difflog)
summary(lm2)

scatter2 <-
  ggplot(data =inc.sub, 
         mapping = aes(x = inc.sub$difflog, 
                       y = inc.sub$presvote)) + 
  geom_point() +
  labs(title = "Scatterplot 2",
       x= 'Difference in campaign spending
between incumbent and challenger',
       y = "Vote share of the incumbent party's
presidential candidate") +
  geom_smooth(method='lm',col="blue") # Add regression line

scatter2

residuals2 <- lm2$residuals
residuals2

#### Question 3

lm3 <- lm(inc.sub$voteshare ~ inc.sub$presvote)
summary(lm3)

scatter3 <-
  ggplot(data =inc.sub, 
         mapping = aes(x = inc.sub$presvote,
                       y = inc.sub$voteshare)) + 
  geom_point() +
  labs(title = "Scatterplot 3",
       x= "Incumbent's electoral success",
       y = "Vote share of the incumbent party's
presidential candidate") +
  geom_smooth(method='lm',col="blue") # Add regression line

scatter3

residuals3 <- lm3$residuals
residuals3

#### Question 4

lm4 <- lm(residuals1 ~ residuals2)
summary(lm4)

scatter4 <-
  ggplot(data =inc.sub, 
         mapping = aes(x = residuals2, 
                       y = residuals1)) + 
  geom_point() +
  labs(title = "Scatterplot 4",
       x= "Question 2 residuals",
       y = "Question 1 residuals") +
  geom_smooth(method='lm',col="blue") # Add regression line

scatter4

#### Question 5

lm5 <- lm(inc.sub$voteshare ~ inc.sub$difflog + inc.sub$presvote, data = inc.sub)
summary(lm5)


#### Using stargazer to get LaTex tables
stargazer(lm1)
stargazer(lm2)
stargazer(lm3)
stargazer(lm4)
stargazer(lm5)


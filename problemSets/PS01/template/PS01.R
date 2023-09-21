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

# find sum of y
sum(y)

# find mean of y
sum(y)/length(y)
mean(y)
# mean of y = 98.44

#find sum of demeaned values
demeanedSum <- NULL
for( i in 1:length(y)){
  demeanedSum[i] <- y[i] - mean(y)
}
demeanedSumSImple <- y - mean(y)
sum(demeanedSumSImple)

# sum of squared error
squaredError <- demeanedSum^2
sum(squaredError)

#calculating the confidence interval
#when the confidence coefficient = .90

z90 <- qnorm((1 - .90)/2, lower.tail = FALSE)## (1-confidence coefficient)/2
n <- length(na.omit())
#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

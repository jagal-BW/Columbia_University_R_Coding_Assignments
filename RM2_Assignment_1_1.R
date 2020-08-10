##ensure correct version is used##
RNGversion(vstr = 3.6)

##load packages##

install.packages("tidyverse")
library(tidyverse)
library(lattice)
library(mice)

##create and rename data frame##
library(readr)
fastfood_survey_ <- read_csv("fastfood_survey.csv")
View(fastfood_survey_)

food = fastfood_survey_

############
#QUESTION 1#
############

##find the number of columns/variables##

ncol(food)

#Answer:  [1] 21

############
#QUESTION 2#
############

##subset the data frame to only observe the first 11 variables##  Cutting out columns!!!

data_cluster <- food[,1:11]  #[rows, columns]


##make sure you have only 11 by counting the columns##

ncol(data_cluster)

#Answer:  [1] 11

############
#QUESTION 3#
############

##count the number of NAs in the 'cleanliness' variable##  Counting NAs down a column

sum(is.na(data_cluster$cleanliness))

#ANSWER:  23


############
#QUESTION 4#
############

##omit NAs and count number Of remaining rows without overwriting the original dataset

##first, check the number of NAs that currently exist in the data frames##
sum(is.na(food))
sum(is.na(data_cluster)) #for only 11 variables/altered dataset 

##create a dummy data frame while you remove NAs so you don't overwrite the original##
dummy1 <- na.omit(data_cluster)

##double check to see if NAs have been removed. nrow should be lower number
##than when you checked before and sum(is.na) should be 0 because ALL NAs removed##

nrow(dummy1)
sum(is.na(dummy1))

#ANSWER:  556


############
#QUESTION 5#
############

##impute the missing values##
set.seed(1706)
data_cluster <- mice::complete(mice(data_cluster))
data_cluster = complete(mice(data_cluster)) #what professor had written down

##What is the imputed value of observation 10 for the variable cleanliness?##
##review new data_cluster table in the Global Environment to see##

data_cluster[10, "cleanliness"]     #dataset[row, column]
#OR
data_cluster[10,4]

#Answer:  6

############
#QUESTION 6#
############

##standardize the variables##

data_cluster = scale(data_cluster)
 
##What is the value of observation 10 for the variable cleanliness?##
##review new data_cluster table in the Global Environment to see##

data_cluster[10, "cleanliness"]
data_cluster[10,4]

#Answer:   0.3479976  (totally different than 6)

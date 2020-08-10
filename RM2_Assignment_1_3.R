##ensure correct version is used##
RNGversion(vstr = 3.6)

#load libraries
library(dplyr)

#Pull up original .csv file
getwd()

fastfood_survey <- read.csv("fastfood_survey.csv")

############
#Question 1#
############

#Compared to other clusters, Cluster 1 has the lowest value 
#for which variables?

k_segments = km$cluster
table(k_segments)


#k_segments
#1   2   3 
#254 327  41 

data2 = cbind(fastfood_survey_,k_segments)

data2 %>%
  select(c(1:11),k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

#k_segments speed_of_service variety popularity_with_children cleanliness convenience
#1          1             4.93    3.60                     2.46        5.71        5.08
#2          2             5.59    4.87                     4.15        5.97        5.81
#3          3             2.69    2.79                     2.97        1.85        2.11
#taste price drive_through friendliness quality_of_fries taste_burgers
#1  5.72  4.68          1.75         4.22             4.29          5.43
#2  5.96  5.69          3.00         5.54             5.48          5.88
#3  1.24  1.97          4.27         2.22             2.31          1.74


lapply(12:21,function(x) round(prop.table(table(data2$k_segments,data2[,x]),1),2)*100)

#[[1]]

#1.Less than $10 2.$10-$15 3.$15.01-$20 4.Over $20
#1              36        25           15         23
#2              29        25           20         25
#3              26        16           21         37

#[[2]]

#1.single 2.married 3.other
#1       22        70       8
#2       15        74      11
#3       23        67      10

#[[3]]

#1.male 2.female
#1     48       52
#2     30       70
#3     47       53

#[[4]]

#0  1  2  3  4  5  6  7
#1 55 19 14  8  1  2  0  0
#2 38 22 20 12  5  2  0  0
#3 51 15 20  7  2  2  2  0

#[[5]]

#1.rent 2.own
#1     37    63
#2     28    72
#3     26    74

#[[6]]

#1.house 2.apt 3.duplex
#1      66    29        5
#2      78    20        2
#3      82    18        0

#[[7]]

#1.managerial 2.skilled trade 3.laborer 4.office worker 5.technical 6.professional
#1           14               5         3              11           6             22
#2            9               5         6              12           3             18
#3           11               5         5              16           0             29

#7.stay at home mom 8.student 9.retired
#1                 22        12         5
#2                 33         7         7
#3                 24         5         5

#[[8]]

#1.High School or less 2.Some college 3.College graduate 4.Graduate degree
#1                    28             30                 28                14
#2                    46             28                 18                 8
#3                    35             22                 30                12

#[[9]]

#30 and under 31-39 40-49 50-59 60 and over
#1           42    21    14    14           9
#2           30    24    18    15          13
#3           34    17    20    15          15

#[[10]]

#$40,000 or less $40,001 - $59,999 $60,000 - $79,999 $80,000 and over
#1              10                24                31               35
#2               9                25                34               33
#3              11                16                38               35

# Always add RNG for Proper R version
RNGversion(vstr = 3.6)

# install.packages(‘arules’)
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("readx1")
library(readxl)
library(ggplot2)
library(dplyr)

data(Groceries)
#Description of the dataset can be viewed by typing the following in your R Console.
?Groceries
#(1) Load the dataset called Groceries that accompanies the arules package by executing data(Groceries). 
#How many transactions does the transactions dataset Groceries contain?
#Read Data in R platform---Answer ='s 9835 (rows) and 169 (columns)

#(2)Which of the following are among the top 5 (five) frequently occurring items in the dataset?
#Summary Function provides output of the most requent items in the "basket"
#yogurt and whole milk

summary(Groceries)

#(3)Run a MARKET BASKET analysis to generate a set of RULES with the following 
#parameters: support=0.01 and confidence=0.01. How many RULES were generated?

rules_all = apriori(Groceries,parameter=list(support=0.01,confidence=0.01))

rules_all  #gives direct number of rules --Answer = 610 Rules 

summary(rules_all)


#4. Now, repeat the above market basket analysis but with support=0.001
#and confidence=0.001. How many rules were generated?    41100

rules_all2 = apriori(Groceries,parameter=list(support=0.001,confidence=0.001))

summary(rules_all2)

#5.Let us go back to the first market basket analysis with support of 0.01
#and confidence of 0.01. Among the rules generated, what is the value of
#the largest lift? Type in the value of the largest lift.

summary(rules_all)

#In the lift column is the Max--at the bottom = Max.   :3.3723

#6.In a grocery store setting, rules with just two items are more actionable
#than ones that have many items. For instance, cross-promotions
#(e.g., when you buy a carton of Brand A milk, take 50 cents of Brand X bread)
#or retail merchandising decisions (e.g., place Brand A milk close
#to Brand X bread) are easier to implement for two-item sets. 
#Therefore, now you will generate a list of rules for only two-items. 
#Keep support and confidence at 0.01 (the same as BEFORE). How many two-item rules are created?
#Note, a rule with only one-item should not be included. 
## 426 (the # of rules for 2 item rules)

summary(rules_all)

#From Summary see number of Rules for 1, 2, and 3 item distributions.  2 = 426 Rules

x = inspect(rules_all)

x[order(x$lift,x$confidence, decreasing = T),]

##rule length distribution (lhs + rhs):sizes
#1   2   3 
#88 426  96 

#7.Which of the following rules created from the analysis in 
#the previous question have the highest confidence?   butter => whole milk 

#can use the code below to identify confidence level for single item/rule; however, code 
#above produces list where can also choose confidence levels from given combinations 
#(however, have to look through entire list ==>
#x = inspect(rules_all)
#x[order(x$lift,x$support, decreasing = T),]

#Root vegatbles ==> beef  confidence = 0.15951493


rules_rv = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='root vegetables'))
rules_rv = subset(rules_rv, subset= lift>1)
inspect(rules_rv)

#whipped/sour cream ==> curd =  0.14609929
rules_ws = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='whipped/sour cream'))
rules_ws = subset(rules_ws, subset= lift>1)
inspect(rules_ws)

#butter ==> milk =  0.49724771 (confidence)
rules_bttr = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='butter'))
rules_bttr = subset(rules_bttr, subset= lift>1)
inspect(rules_bttr)

#whole milk 
rules_wm = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='whole milk'))
rules_wm = subset(rules_wm, subset= lift>1)
inspect(rules_wm)


#8.What is support for the rule, soda => whole milk? Support = 0.04006101

rules_soda = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='soda'))
rules_soda = subset(rules_soda, subset= lift>.5)
inspect(rules_soda)

#Couldn't find combination of soda because have lift at >1.  but if less have to use 
#code below or reduce lift to about .5  ==> whole milk; so have to do below

sodarules = subset(rules_all,subset=rhs %pin% 'soda')
inspect(sodarules)

#[26] {whole milk}                  => {soda} 0.04006101 0.1567847  0.8991124  394 


#9.The store manager contends that the support between soda and
#whole milk is not of much value, because most people buy whole milk. 
#She goes on to say the association between soda and whole milk is weak. 
#Is she correct? 
#Yes, she is correct; it is weak. It has the smallest lift of all the Rules and 
#its less than 1.  

#A lift value GREATER than 1 indicates that the rule body and the rule head appear more often together than expected, this means that the occurrence of the rule body has a positive effect on the occurrence of the rule head.
#A lift LESS than 1 indicates that the rule body and the rule head appear less often together than expected, this means that the occurrence of the rule body has a negative effect on the occurrence of the rule head.
#A lift value near 1 indicates that the rule body and the rule head appear almost as often together as expected, this means that the occurrence of the rule body has almost no effect on the occurrence of the rule head.

#10.A shopper just picked up yogurt while shopping for groceries. 
#What is the most likely item the shopper will also buy from the grocery
#store? Assume the shopper only buys two items on this shopping trip.

#run the code below and look at the support for the number of items---here if she only 
#buys 2 items we see the highest SUPPORT for yogurt + whole milk ==>this is the answer
#LOOK at what 2 items have the HIGHEST LIFT (support to show togetherness)
yogurt = subset(rules_all,subset=lhs %pin% 'yogurt')
inspect(yogurt)

################Disregard BELOW
#plotting to show rules but is confusing.  Need to do further research.  Using above is so 
#much clear because shows the SUPPORT numbers and is clear that whole milk is most likely 
#item to be chosen after yogurt. Below seems to show frankfurter is
#most likely item due to higher support and number of rules 

top_yogurt = head(sort(yogurt,decreasing=T,by='support'),26)
plot(top_yogurt, method="grouped", measure = 'lift',shading='lift')

top_yogurt2 = head(sort(yogurt,decreasing=T,by='support'),50)
plot(top_yogurt2, method="grouped", measure = 'lift',shading='lift')




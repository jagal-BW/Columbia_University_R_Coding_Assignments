#Assignment 2. Text Mining.

RNGversion(vstr = 3.6)

#     Part 1

#Set working directory to the source file's location
# package 

library(ggplot2)
library(ggthemes)
library(qdap)


#[1] "C:/Users/yt2364/Documents"


#Rename the data to baby for shorter name and easier work

baby=read.csv('baby_reviews.csv',stringsAsFactors = F)
baby_reviews_ <- read_csv("baby_reviews.csv")
baby=baby_reviews_

#1.How many REVIEWS does this dataset contain?

#ANSWER [1] 4978  NUMBER OF OBSERVATIONS

#2. What is the average review rating?

mean(baby$review_rating) 

#[1] 4.227601 

#3.nchar() is a handy function for counting the number
#of characters in text. What is the average number of
#characters in a review?

mean(nchar(baby$review))

#ANSWER [1] 441.8248 average number of characters

#4. Examine the relationship between review length 
#(measured by number of characters) and rating. 
#Greater the length of the review, better the rating.
#Group of answer choices: true, false

cor(nchar(baby$review),baby$review_rating)
#[1] -0.03606577  negative SO NO correlation b/w Review Length and Rating..

#Pearson Product Moment coefficient of correlation. Correlation is often denoted by r and 
#always ranges from -1 to 1. A coefficient close to 1 indicates positive correlation, ie, y 
#increases as x increases. ... Values near 0 indicate weak or no correlation

cor.test(nchar(baby$review),baby$review_rating)

#The stringr library has a number of handy text search
#functions capable of both literal search and pattern
#matching. The sample code that follows specifies a pattern to identify a word and 
#str_count to count the number of such words in a set of text.
# Example: 
#str_count(string = 'Hmm, how many words are in this sentence?',pattern = '\\S+')

median_words = median(str_count(string = baby$review,pattern = '\\S+')); median_words
[1] 57

#6. How many words are in the longest review?  Answer = 1041
#7. How many words are in the shortest review?  Answer = 2 

summary(str_count(string = baby$review,pattern = '\\S+'))

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00   34.00   57.00   83.89   99.00 1041.00 

#8.Next, let us examine which words are used most 
#frequently in the review. You may use either of two
#approaches for the next two questions:
#Approach 1:  The qdap package comes with a handy 
#function freq_terms that counts frequency of each word
#in the reviews. The following code will list the top 10
#words (if your data is in 'baby').

#Which of the following words are in the Top 10 list?
  #Group of answer choices

baby

easy

love

the

and


source(file = "alternative_freq_terms.R")

alternative_freq_terms(text.var=baby$review,top=10)

#WORD  FREQ
#1   the 20569
#2   and 12204
#3    to 11798
#4     I 10937
#5    it 10882
#6     a  9919
#7    is  7251
#8  this  6033
#9   for  5836
#10   of  5063


#Q.9 Now, let us construct a Top 10 list after excluding stop words. 
#To do this include the following argument in the above freq_words or 
#alternative_freq_words function: stopwords=Top200Words
#Which of the following words are in this Top 10 list? (Are you surprised?)
#Group of answer choices

#baby

#easy

#love

#the

#and

alternative_freq_terms(text.var=baby$review,top=10,stopwords = Top200Words)

#WORD FREQ
#1      baby 2673
#2      it's 1694
#3      easy 1254
#4      love 1165
#5    really 1046
#6      seat 1023
#7     don't  864
#8      used  789
#9  stroller  750
#10      son  695



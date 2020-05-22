#Assignment 2_3


install.packages("tm")
library(tm)
install.packages("dplyr")
library(dplyr)
install.packages("tidytext")
library(tidytext)
install.packages("rpart.plot")
library(rpart.plot)


#      Preprocess the corpus of reviews using functions library(tm). Specifically,

#1. Create a CORPUS from the Variable "review"

baby <- baby_reviews_

corpus = Corpus(VectorSource(baby$review))

# 2.    Use tm_map to

#a) transform TEXT to LOWER CASE (1st code). Check the content of 617 doc
#(random doc) to see if the text is in lower case.

corpus = tm_map(corpus,FUN = content_transformer(tolower))
corpus[[617]][1]

#content is all lower case


#b)REMOVE PUNCTUATION (and check)

corpus = tm_map(corpus,FUN = removePunctuation)
corpus[[617]][1]

#removed all grammar punctuation 

#c) remove ENGLISH STOPWORDS using the following dictionary tm::stopwords('english')

tm::stopwords('english')
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
corpus[[617]][1]

#$content
#[1] " received  dressing table  good condition    easy  fast
#assemble  one person job   enclosed allen wrench   liked  natural
#look   table meets  expectations   problem  encountered   one
#safety belt straps   prestamped   hole  rather  risk  integrity
#strap  creating   hole  contacted  manufacturer  promptly sent us 
#replacement strap    satisfied  purchase although  fix  take
#week   received  overall pleased  recommended"


#d) REMOVE WHITESPACE

corpus = tm_map(corpus,FUN = stripWhitespace)
corpus[[617]][1]

#$content
#[1] " received dressing table good condition easy fast assemble one
#person job enclosed allen wrench liked natural look table meets expectations
#problem encountered one safety belt straps prestamped hole rather risk integrity
#strap creating hole contacted manufacturer promptly sent us replacement strap
#satisfied purchase although fix take week received overall pleased recommended"

#3. Create a DICTIONARY

dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(baby$review))),
lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

#4. Use tm_map to STEM WORDS

corpus = tm_map(corpus,FUN = stemDocument)

#5. Create a DocumentTermMatrix

dtm <- DocumentTermMatrix(corpus)
dtm

#DocumentTermMatrix (documents: 4978, terms: 11730)>>
#Non-/sparse entries: 166863/58225077
#Sparsity           : 100%
#Maximal term length: 149
#Weighting          : term frequency (tf)

#1.How many TERMS does the DOCUMENT TERM MATRIX contain?  SEE THE 5 STEP PROCESS ABOVE/CORPUS

# see the g.# 5. terms: 11730

#2.Inspect document 100 of the document term matrix. How many times does 
#"amazon' appear in this document?

inspect(dtm[100,'amazon'])

#<<DocumentTermMatrix (documents: 1, terms: 1)>>
#  Non-/sparse entries: 1/0
#Sparsity           : 0%
#Maximal term length: 6
#Weighting          : term frequency (tf)
#Sample             :
#  Terms
#Docs  amazon
#100      1

#ANSWER = 1

#3.Now, let us REDUCE the NUMBER of TERMS to a more reasonable number by only keeping
#terms that appear in at least 10% of documents. Save the result as 'xdtm'.
#How many terms remain after REMOVING SPARSE TERMS? The answer is 47

xdtm = removeSparseTerms(dtm,sparse = 0.90)
xdtm

#<<DocumentTermMatrix (documents: 4978, terms: 47)>>
#Non-/sparse entries: 38322/195644
#Sparsity           : 84%
#Maximal term length: 9
#Weighting          : term frequency (tf)

#4 Transform the document term matrix, xdtm created in the previous question
#into a data frame. Use stemCompletion() to complete stemmed words by selecting
#the most prevalent match. In the resulting data frame, which term appears most
#frequently?  

##############DTM ==> DATA FRAME#########################

#   See the last code. The answer is "use'.
#It has the biggest number.
#So, it appears most frequently.

#Group of choices: use, babies, one, like, love.

xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
dictionary = dict_corpus,
type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))

sort(colSums(xdtm),decreasing = T) 

#Browse tokens
#Next, we evaluate the FREQUENCY of the TOKENS as these
#frequencies will be used to weight the tokens. A word that OCCURS MORE
#frequently gets WEIGHTED MORE than one that appears less frequently.

#use    babies       one      like      love       can       get     great 
#3435      3217      2367      1963      1892      1682      1624      1572 
#just    easier    little      work      well     month      time         X 
#1548      1257      1229      1134      1124      1109      1052      1049 
#also      will      much       put      good       fit      make      dont 
#961       944       941       922       909       892       887       873 
#old      need       son      nice     still       now      look      keep 
#857       850       794       736       719       717       715       709 
#trial     thing    bought      take      back      even  daughter     clean 
#701       701       681       669       656       644       641       629 
#doesnt   product      wash      hold     first   perfect recommend 
#620       619       617       611       588       565       558 

# 5. Attach the column containing the review rating to the dataframe created in
# the previous question.

#Which is the third (3rd) most frequintly occuring word
#among reviews with a rating of 5? -- the answer is love.

baby_data=cbind(review_rating = baby$review_rating,xdtm)

d01=filter(baby_data,review_rating==5)

sort(colSums(d01), decreasing = T)

#Review_rating should not count toward the top 3. 

#review_rating   use        babies          love           one         great 
#14670          1955          1891          1429          1373          1067 
#can          like        easier           get          just        little 
#1015           929           868           822           760           704 
#well         month          time          work          also             X 
#666           663           589           578           575           554 
#old           fit          much           son           put          will 
#531           518           507           495           488           468 
#make          need          keep          good           now       perfect 
#467           460           449           448           444           439 
#dont          nice        bought         clean          wash         thing 
#437           417           407           395           392           384 
#take     recommend         still          look      daughter          even 
#383           379           372           370           370           355 
#first          hold         trial       product          back        doesnt 
#342           341           335           335           322           276


##                           Part 4

#1.Now let us use data on WORD FREQUENCIES to PREDICT REVIEW RATING. Split the 
#dataset contaiting review rating and term frequencies into train and test sample.
#Use sample() to create a train sample with 70% of the data and a test sample with
#the remaining 30%. Use a seed of 1031. For a dataset called, baby_data, the
#following code will create the train and test samples.


set.seed(1031)
split = sample(1:nrow(baby_data),size = 0.7*nrow(baby_data))
train = baby_data[split,]
test = baby_data[-split,]

#How many rows are in the test sample?

nrow(test)
#[1] 1494

# 2.Use a CART model to predict review_rating using all other variables, i.e.,
#term frequencies. For the CART model, use rpart().

#Based on the result of the CART model, reviews that contain the term "love
#are rated higher than those that don't contain the term "love'.
# yes , no ?    True

###KNOW HOW TO READ THE CART MODEL---"<1 = NOT IN TO THE LEFT/YES TO THE RIGHT

library(rpart)
library(rpart.plot)

tree = rpart(review_rating~.,train)
rpart.plot(tree)


# 3. Based on results of the CART model, reviews that contain the term "easier"
# are rated lower than those that don't contain the term "easier"
#  False

# 4. Based on results of the CART model, reviews that contain the term 'perfect' 
#are rated lower than those that don't contain the term 'perfect'.

# False

#.5 Use a LINEAR regression to PREDICT REVIEW_RATING using all variables, i.e.
#TERM FREQUENCIES. Examine the results. Locate the most frequently occuring term
# in review in the regression results. Is this term PREDICTIVE of review_rating?
# Yes or No?             No.  Most common word is Perfect at .352 < 1 so goes to other cluster.
                         #for every .352 y goes up by 1--this less than .50.  

reg = lm(review_rating~.,train)
summary(reg)

#6.What is the rmse of the CART model on the test train?

pred_tree = predict(tree,newdata=test)
rmse_tree = sqrt(mean((pred_tree - test$review_rating)^2)); rmse_tree
#[1] 1.114328

#7. What is the rmse of the linear regression model on the test set?
pred_reg = predict(reg, newdata=test)
rmse_reg = sqrt(mean((pred_reg-test$review_rating)^2)); rmse_reg
#[1] 1.120296
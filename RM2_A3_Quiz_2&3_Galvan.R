
RNGversion(vstr = 3.6)

library(recommenderlab)
library(ggplot2)

#use recommerlab for matrix calculations 

#Question 1: Read in the data, product_ratings_data.csv. Preview the document 
#using read.csv(). What format is the data in?
#Answer:  In Tall format

prod_ratings <- read.csv("product_ratings_data.csv")


#########realRatingsMatrix

#Question 2: From the imported data, create a realRatingMatrix object called, 
#ratings_matrix. How many ratings does the realRatingMatrix contain?

#nratings(ratings_matrix) provides the number of ratings/observations for the matrix.
#memorizae the nratings function 

#Answer:  362105 observations/ratings (ratings equals number of observations)

ratings_matrix = as(prod_ratings,Class = 'realRatingMatrix')
as(ratings_matrix,'matrix')
nratings(ratings_matrix)

#Question 3: What rating did u10023 give to prod_14?

getRatings(ratings_matrix['u10023', 'prod_14'])

#Answer:  4


#Question 4: Now, let us split the data into a train sample and a test sample.
#We will use the sample() function with a seed of 1031 to create a train 
#sample with 90% of the data. Run the following code (including the first line) 
#to create the train and test samples.  

set.seed(1031)
split = sample(nrow(ratings_matrix),size = 0.9*nrow(ratings_matrix))
train = ratings_matrix[split,]
test = ratings_matrix[-split,]

#How many rows are in the train sample? 

nrow(train)

#Answer:  4500

#Question 5: How many products (prod) did user 20150 (u20150) rate?
#Pay close attention to the (common) and code used.   This is the sum value for a categorical 
#value 

nratings(ratings_matrix['u20150', ])

#Answer:  44

#Question 6: How many user ratings did product 25 (prod_25) receive?
#Pay close attention to how (comma) the code is written to determine "how many of something
#occurred in the matrix

nratings(train[,'prod_25'])

#Answer:  3745

#Question 7: What is the most common rating in the train sample?
#ANSWER
# 3 (look at plot)---from the plot it appears that the value =2.8/2.9, but answer is 3---because
#ratings are not partial.  They are 1,2,3,4,5.  So if graph near 3 then it is 3.  

ggplot(data=data.frame(ratings_matrix = rowMeans(train)),aes(x=ratings_matrix))+
  geom_histogram(fill='seagreen3')

#Question 8: What is the average rating for product 100 (prod_100) in the train sample?


mean(getRatings(train[,'prod_100']))

#Answer:  2.824492

#Question 9: Now, normalize user ratings using the normalize() function from 
#recommenderLab. Use the defaults of method='center' and row=TRUE. What is the
#average rating for product 100 (prod_100) in the normalized train sample?


summary(getRatings(normalize(train, method='center', row = TRUE)[,'prod_100']))

#Answer:  Mean/Average = 0.08439

#Question 10: Using the normalized user ratings generated above, assess the 
#cosine similarity between the first five users in the train dataset 
#(u395, u21174, u9881, u18449, u8926). Which of the following pairs is most similar?

similarity(normalize(train)[1:5,],method = 'cosine')

#Answer:  u395 and u8926
#The highest value euqals the most similarity---0.3112

#u395     u21174      u9881     u18449
#u21174 0.14566514                                 
#u9881  0.17659251 0.12436945                      
#u18449 0.22152422 0.12175581 0.03717568           
#u8926  0.31126669 0.16451031 0.16810550 0.05123267

#####################
#### Quiz 3  #######


#Question1: Construct a USER-based collaborative filtering recommender using 
#the train data. Use defaults for the parameters in the Recommender function. 
#Based on this recommender, which of the following are in the list of top 5 
#recommended products for u10139? (Note: u10139 is in the test data).

#ANSWER
# "prod_11" "prod_51"


recommenderRegistry$get_entries(data='realRatingMatrix')$UBCF_realRatingMatrix
recom_ubcf = Recommender(train, method='UBCF', parameter=list(method='cosine',nn=25, normalize='center'))
pred_ubcf_topN = predict(recom_ubcf,newdata=test,method='topNList',n=5)
getList(pred_ubcf_topN)['u10139']

#$u10139  TOP 5 picks for u10139
#[1] "prod_11" "prod_45" "prod_26" "prod_51" "prod_12"


#Question 2: Based on the recommender created above, what is the predicted 
#rating of product 1 (prod_1) by user 10139 (u10139)?

#ANSWER
# [1] 3.054352

pred_ubcf = predict(recom_ubcf,newdata=test,type='ratings')
as(pred_ubcf,'matrix')['u10139',]

#prod_1  prod_10 prod_100  prod_11  prod_12  prod_13  prod_14  prod_15  prod_16  prod_17  prod_18 
#3.054352 2.744481 3.192382 3


#Question 3: Construct an ITEM-based collaborative filtering recommender 
#using train data. Use defaults for the parameters in the Recommender 
#function. Based on this recommender, which of the following are in the 
#list of top 5 recommended products for u10139?


recommenderRegistry$get_entries(data='realRatingMatrix')$IBCF_realRatingMatrix # see parameters for IBCF
recom_ibcf = Recommender(train, method='IBCF', parameter=list(k=30, method='cosine',normalize='center'))
recom_ibcf
pred_ibcf_topN = predict(recom_ibcf,newdata=test,method='topNList',n=5)
getList(pred_ibcf_topN)['u10139']


#[1] "prod_22" "prod_3"  "prod_94" "prod_84" "prod_51"

#ANSWER
# "prod_3" "prod_51"


#Question 4:Based on the recommender created in the previous question, what 
#is the predicted rating of product 1 (prod_1) by user 10139 (u10139)?
#as(test,'matrix')['u10139',]   ----code was here but produced NA.  

pred_ibcf = predict(recom_ibcf,newdata=test,type='ratings')
as(pred_ibcf,'matrix')['u10139',] 

#ANSWER
# 3.311504


#Question 5: The recommenderlab library offers a useful framework to cross-validate and 
#evaluate recommenders. Here, we are going to create an evaluation scheme 
#using ratings_matrix, the realRatingMatrix that we had before splitting into
#train and test dataset. The evaluationScheme() function below will do a 
#80:20 split on the data, placing 80% of the data in the train sample. And, 
#we will give the recommender algorithm 30 items from the test set and hold 
#out the other items for computing the error. As with any random sampling 
#operation, it is important to set the seed right before creating the 
#evaluationScheme as done below.

set.seed(1031)
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)

#Now, evaluate accuracy of an ITEM-based collaborative filtering recommender 
#using defaults. To do so, run the following code

recom = Recommender(getData(es,'train'),method='IBCF')
pred_ibcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ibcf = calcPredictionAccuracy(x = pred_ibcf,data = getData(es,'unknown'))

#What is the RMSE for the item-based collaborative filtering recommender?

calcPredictionAccuracy(pred_ibcf,data = getData(es,'unknown'))

#    RMSE      MSE      MAE 
#1.307363 1.709198 1.036664 

#ANSWER
# 1.307363


#Question 6: Now, evaluate the accuracy of the USER-based collaborative 
#filtering recommender using defaults. To do so, modify the code in the 
#previous question. Note, there is no need to recreate the evaluation scheme. 

recom = Recommender(getData(es,'train'),method='UBCF')
pred_ubcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf = calcPredictionAccuracy(x = pred_ubcf,data = getData(es,'unknown'))

#What is the RMSE for the USER-based collaborative filtering recommender?
calcPredictionAccuracy(pred_ubcf,data = getData(es,'unknown'))

#     RMSE       MSE       MAE 
#1.1821212 1.3974104 0.9336698

#ANSWER
# 1.1821212

#Question 7: Next, evaluate the accuracy of another user-based collaborative 
#filtering recommender, with just one change from the previous question. Set 
#the parameter nn to 100. To learn more about the default nn, run:

recommenderRegistry$get_entries()$UBCF_realRatingMatrix

#What is the RMSE for this modified user-based collaborative filtering 
#recommender?

recom_ubcf = Recommender(data = getData(es,'train'),
                         method='UBCF',
                         parameter = list(method='cosine',normalize='center',nn=100))
pred_ubcf = predict(recom_ubcf,newdata=getData(es,'known'), type='ratings')
calcPredictionAccuracy(pred_ubcf,data = getData(es,'unknown'))

#     RMSE       MSE       MAE 
#1.1676130 1.3633201 0.9211638

#ANSWER
#1.1676130


##Question 8: Finally, as a baseline for evaluating personalized recommenders, 
#let us use a non-personalized recommender that relies only on popularity not
#on similarity. To do so, modify the code for the item-based recommender, 
#replacing 'IBCF' by 'POPULAR'. Note, there is no need to recreate the 
#evaluation scheme.

recom = Recommender(getData(es,'train'),method='POPULAR')
pred_pop = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_pop = calcPredictionAccuracy(x = pred_pop,data = getData(es,'unknown'))

#What is the RMSE for this non-personalized recommender?

calcPredictionAccuracy(pred_pop,data = getData(es,'unknown'))

#     RMSE       MSE       MAE 
#1.1692988 1.3672598 0.9241173 

#ANSWER
# 1.1692988





























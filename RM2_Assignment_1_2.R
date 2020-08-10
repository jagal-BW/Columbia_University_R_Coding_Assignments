##ensure correct version is used##
RNGversion(vstr = 3.6)

#load libraries
library(tidyverse)  
library(lattice)
library(mice)
library(caret)
library(dendextend)
library(ggplot2)
library(cluster)
library(mclust)

############
#Question 1#
############

#Compute the Euclidean distance between all observations in 
#data_cluster. How many ELEMENTS are in the distance matrix?

# 193131 (find answer from the Environment tab)

distance = dist(data_cluster,method = 'euclidean') 

#dist is function that calculates distance
#but need to specify what type of distance.  ALSO, need to first calculate DISTANCE first 
#become CAN cluster 

distance

#ANSWER:  Look at Envrinment Tab for Elements in Distance Matrix (193131 elements)

############
#Question 2#
############

#Conduct a Hierarchical cluster analysis using the 
#method "ward.D2". Plot the dendrogram from this process. 
#Let us see how well the dendrogram matches true distances. 
#What is the Cophenetic correlation coefficient?

clusters = hclust(d = distance, method="ward.D2")  

#FIRST HAD TO PREPARE DATA, SCALE, then CALCULATE DISTANCE

#   Hierarchical Cluster = hclust; data is Euclidian distance matrix

#Cophenetic is a measure of how faithfully the tree represents the dissimilarities 
#among observations.  

#The cophenetic distance between two observations is represented in a dendrogram by the 
#height of the link at which those two observations are first joined. That height is the 
#distance between the two subclusters that are merged by that link.

#Cophenetic Correlation Coefficient. The magnitude of this value should be very close to 1 
#for a high-quality solution. This measure can be used to compare alternative cluster solutions 
#obtained using different algorithms.

plot(clusters)                                                                                                                                                                                                                                    
cor(cophenetic(clusters),distance)

#ANSWER :  [1] 0.7903926

############
#Question 3#
############

#Based on the distances shown in the dendrogram alone, which 
#is the best cluster solution? 2,4,5,6
#The clusters furthest apart are best, so 20 or 2 clusters

#https://wheatoncollege.edu/wp-content/uploads/2012/08/How-to-Read-a-Dendrogram-Web-Ready.pdf

plot(clusters)
rect.hclust(tree=clusters,k = 2)

############
#Question 4#
############

#If you decided to go with a two-cluster solution, how many 
#observations would be in the smaller of the two clusters? 
#(save the cluster memberships in an object as you will need 
#it in a later question)

clust.segment2<-cutree(clusters,k=2)
table(clust.segment2)

############
#Question 5#
############

#If you decided to go with a three-cluster solution, how many
#observations would be in the smallest of the three clusters?

clust.segment3<-cutree(clusters,k=3)
table(clust.segment3)

#clust.segment3
#1   2   3 
#338 243  41 

############
#Question 6#
############

#Next, we will cluster the data using K MEANS CLUSTERING. Conduct k-means 
#cluster to generate a two-cluster solution. Use a seed of 1706. 
#Set max iterations to 100.
#(Save the cluster memberships in an object as you will need it 
#in a later question).
#How many observations are in the smaller cluster?

set.seed(1706)
km = kmeans(data_cluster, centers = 2,iter.max = 100)
km

#ANSWER:  K-means clustering with 2 clusters of sizes 43, 579

#> km
#K-means clustering with 2 clusters of sizes 43, 579


############
#Question 7#
############

#Run another k-means clustering, but this time to generate a 
#three-cluster solution. Set seed of 1706, set max to 100. 
#(Save the cluster memberships in an object as you will need it 
#in a later question). 
#How many observations are in the smaller cluster?

km3 = kmeans(data_cluster, centers = 3,iter.max = 100)
km3

#K-means clustering with 3 clusters of sizes 41, 333, 248

#> km3
#K-means clustering with 3 clusters of sizes 41, 333, 248


############
#Question 8#
############

#In the above k-means analysis, we set the number of clusters 
#expected. Now, let us examine a DATA DRIVEN APPROACH to 
#determining the number of clusters. Compute the total within 
#cluster sum of squares solutions from 2 to 10. Use a
#seed of 1706. 

######What is the total cluster sum of squares for a 3-cluster solution?

set.seed(1706)
within_ss = sapply(2:10,FUN = function(x)  kmeans(x = data_cluster, centers = x,iter.max = 100)$tot.withinss)
within_ss

#[1] 4490.513 3800.811 3480.064 3288.869 3153.458 2976.443 2838.331 2761.327 2667.083
#above you see K means CLUSTERS for 2:10 ---so 3 = 3800.811


############
#Question 9#
############

#For the three-cluster solution, what is the ratio of between 
#sum of squares and total sum of squares? (Express as a decimal)
# see output for question 7 that reads:

km3 = kmeans(data_cluster, centers = 3,iter.max = 100)
km3

#Within cluster sum of squares by cluster:
#  [1] 1477.5160 1827.7811  495.8259
#(between_SS / total_SS =  44.4 %)

###(between_SS / total_SS =  44.4 %)
# 44.4/100=0.444  the answer is 0.444

#############
#Question 10#
#############

#Conduct a line graph of clusters (K MEANS GRAPH) (on x-axis) against total 
#within cluster sum of squares (on y-axis). Based on this chart
#which of the following are good cluster solutions? 2,3,5,6,7
#Look for the sharp angles, ignore where it starts to smooth out

within_ss = sapply(X = 1:9, 
FUN = function(x) kmeans(data_cluster,centers = x,iter.max = 100)$tot.withinss)

ratio_ss = sapply(X = 1:9, 
FUN = function(x) {
km3 = kmeans(data_cluster,centers = x,iter.max = 100)
ratio = km3$betweenss/km$totss
return(ratio)
})

dat = data.frame(clusters=1:9,within_ss, ratio_ss)

ggplot(dat,aes(x=clusters,y=within_ss))+
geom_line(color='steelblue',size=1.4)+
scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
geom_vline(xintercept=4)

#Look at curve sharp points (disregard the LINE WE PUT THERE AT 4) (points before line 4)
#ANSWER = 2 AND 3

#############
#Question 11#
#############

#Next, let us examine the Sihouette method, another data driven 
#approach to choosing number of clusters. What is the average 
#silhouette width for a 2 cluster solution? 
#Use pam() from library(cluster) to compute silhouette width.

pam(data_cluster,k = 2)$silinfo$avg.width

#ANSWER:  [1] 0.5869224


#############
#Question 12#
#############

#What is the average silhouette width for a 3 cluster solution?
#The higher the average silhouette width, the better.

pam(data_cluster,k = 3)$silinfo$avg.width

#ANSWER:  [1] 0.1722077

#############
#Question 13#
#############

#Examine average silhouette width for other cluster solutions.
#Based on this criterion, which is the best cluster solution?
#the highest result is the best

pam(data_cluster,k = 2)$silinfo$avg.width

pam(data_cluster,k = 5)$silinfo$avg.width

pam(data_cluster,k = 6)$silinfo$avg.width

pam(data_cluster,k = 7)$silinfo$avg.width

#ANSWER---PAM = 2 CLUSTERS IS BEST BECAUSE HAS HIGHEST AVG SILHOUETTER WIDTH

#> pam(data_cluster,k = 2)$silinfo$avg.width
#[1] 0.5869224
#> 
#  > pam(data_cluster,k = 5)$silinfo$avg.width
#[1] 0.1274857
#> 
#  > pam(data_cluster,k = 6)$silinfo$avg.width
#[1] 0.1143355
#> 
#  > pam(data_cluster,k = 7)$silinfo$avg.width
#[1] 0.09954482


#############
#Question 14#   MODEL BASED CLUSTERING 
#############

#Now, we will make use of a Model-based clustering technique.
#Use Mclust() from library(mclust) to cluster the data. How many 
#clusters has the model decided to group the data into?
#The answer is the number of clusters in the table in the output

clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)

#Clustering table:  (3 clusters)
#  1   2   3 
#342 121 159 

#############
#Question 15#
#############

#Now,use model-based clustering to force a two-cluster solution.
#(Save the cluster memberships in an obejct as you will need it in 
#a later question).
#How many observaiotns are in the smallest cluster?

clusters_mclust2 <- Mclust(data_cluster,G=2)
summary(clusters_mclust2)

#Clustering table:
#1   2 
#451 171 

#############
#Question 16#
#############

#Now, let us compare the 2-cluster solutions obtained from 
#hierarchical cluster to k-means. Specifically, compare the cluster 
#assignments for hierarchical cluster analysis to k-means for 
#the 2-cluster solution.
#For how many observations do the cluster assignments differ?
#The answer is the number in the output table under TRUE

h_segments = cutree(tree = clusters,k=2)
table(h_segments)

#Here is info that we need now and that was needed for Q#6
#(k-mean with 2 clusters)

set.seed(1706)
km = kmeans(x = data_cluster,centers = 2,iter.max=100,nstart=25)
k_segments = km$cluster
table(k_segments)

mean(km$cluster==h_segments)

table(km$cluster==h_segments) #This is what you need to determine from H cluster to K means

#FALSE  TRUE 
#618     4 

#Answer = 4

#############
#Question 17#
#############

#Now compare the 2-cluster solutions for k-means to Model-based 
#clustering. Specifically, compare the cluster assignments for 
#k-means to Model-based clustering.
#For how many observations do the cluster assignments differ?

m_clusters = Mclust(data = data_cluster,G = 2)
m_segments = m_clusters$classification
table(m_segments)

table(m_segments==k_segments) #fron K means to M clustering 


# FALSE  TRUE 
#  494   128 

#ANSWER = 128


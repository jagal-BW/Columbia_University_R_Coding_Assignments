# Always add RNG for Proper R version
RNGversion(vstr = 3.6)

library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
install.packages("gridExtra")
library(gridExtra)
install.packages("xts")
library(xts)
install.packages("zoo")
library(zoo)
install.packages("quantmod")
library(quantmod)
install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp) 
install.packages("fpp2")
library(fpp2) 
install.packages("tseries")
library(tseries)
install.packages("dplyr")
library(dplyr)


#Read the file goog.RDS using the function readRDS(). If goog.RDS is in your working directory, 
#the following code will read the data in and assign it to goog:
  
#  goog = readRDS('goog.RDS')
#If goog.RDS is not in your working directory, set your working directory to the location of 
#the file using setwd() and then run the above code.

#1. What type of data structure is goog?

goog <- readRDS("~/Downloads/goog.RDS")
setwd("~/Downloads")
goog = readRDS('goog.RDS')

summary(goog)
class(goog)  #---gives answer 
                               #attributes(goog) #not necessary 
                               #typeof(goog) #not necessary 
head(goog)

#ANSWER:  [1] "xts" "zoo"

#2. What was google stock price for June, 2010?

plot(goog)
goog #simply type the name of the xts series and it will display the time and quant info

x_Jun = goog["2010"]    #this is a way to limit time series to a YEAR in case too much data!!!
x_Jun

#ANSWER:  Jun 2010   221.0374


#3. Using the monthly stock price for google, what is the average stock price for the year 2010?

x_2010 <- goog["2010"]
mean(x_2010)

#ANSWER:  [1] 260.9768


#4. How many months of data are included in this dataset?

length(goog) #This tells you the amount of rows/months

start(goog)  #When time series data begins 
end(goog)   #when time series data ends

#Answer = 142 months of data in time series


#5.  With time-series data, the PAST is often a GOOD predictor of the future. 
#Let us see if this is true for our data. What is the CORRELATION between google stock price 
#and one-month LAGGED stock price? You can use lag() to obtain a one-month lag for google 
#stock price. When computing correlation with cor(), be sure to set use='complete.obs'.

ts=lag(goog)  #NEW OBJECT NAMED ts

cor(ts, goog, use='complete.obs')     #when doing correlation put both sets

#Answer:      goog.Close
#goog.Close  0.9923893

#6.  In order to have access to a wider array of forecasting models, we will convert the 
#data to a "ts" data type. Also, we will split the data into a train and test sample, 
#using the train sample to estimate a model and the test sample to evaluate it. 
#We will used data from Jan, 2007 to Dec, 2015 for the train sample and the rest for the 
#test sample. 

#The code below will convert goog to a “ts” object and split the data.

google = ts(goog,start=c(2007,01),frequency=12)
train = window(google,start=c(2007,01),end=c(2015,12))
test = window(google,start=c(2016,01),end=c(2018,10))

#How many months of data does train contain?

nrow(train)

#ANSWER:  [1] 108

#7. Autocorrelation examines correlation of a variable and its lagged values. 
#Construct a plot of autocorrelations for train using ggAcf() from the 
#forecast package. Which lag has the strongest autocorrelation?
#Group of answer choices 1, 2, 3, 4 ?

ggAcf(x=train)

#answer: 1  BECAUSE GOES DOWN AS LAG GETS GREATER

#------------- Part 2----------------------------------------

#1. A very simple prediction, often the baseline in linear regression, 
#is to use the average. Use the average to make a prediction for the stock 
#price over the 34 months of the test sample. Let's call this average_model.
#What is the point forecast of the stock price for October 2018?

length(test)

# [1] 34 (length ='s months for time series data)

average_model = meanf(train,h = 34)
average_model

average_model$mean

#Answer: 355.776

#2. Let us examine the accuracy of the above prediction from 
#average_model on the train sample.
#Specifically, what is the RMSE of the prediction in the train sample?
#Hint: Use accuracy() from library(forecast)

accuracy(average_model)

                         #ME     RMSE      MAE       MPE     MAPE     MASE      ACF
#Training set -2.607482e-15   144.5429 121.5551 -15.95834 37.32932 1.691174 0.9424575

#Answer = 144.5429

#3. What is the RMSE of the average_model on the test sample?

accuracy(average_model, x = google)

#                       ME     RMSE      MAE       MPE     MAPE     MASE      ACF1
#Training set -2.607482e-15 144.5429 121.5551 -15.95834 37.32932 1.691174 0.9424575
#Test set      5.650078e+02 588.3496 565.0078  60.12771 60.12771 7.860849 0.9194724
#Theil's U
#Training set        NA
#Test set      11.04178

#Answer: 588.3496

#4. Next, let us examine another simple prediction, one that assumes the 
#future will be the same as the last observation. Let’s call this naive_model. 
#Use naive_model to construct a forecast for stock price over the 
#next 34 months of the test sample. What is the point forecast of 
#the stock price for October 2018?

naive_model = naive(train,h=34)

naive_model

naive_model$mean

#Answer: 758.88

#5. What is the RMSE of the naive_model on the test sample?

accuracy(naive_model, x=google)

#                  ME      RMSE      MAE        MPE      MAPE      MASE       ACF1
#Training set   4.764022  28.31933  21.0405  0.6810607  6.474298 0.2927325 0.09114254
#Test set     161.903814 230.50860 176.4697 14.9513151 17.020994 2.4551900 0.91947241
# Theil's U
#Training set        NA
#Test set      3.904238

#Answer: 230.50860

#------------------Part 3 ------------------------------------------------


#1. There are a number of exponential smoothing models that differ
#in how they handle errors, trend, and seasonality. 
#Let us fit an exponential smoothing model using the following function:
#Call this ets_model.

ets(train,model = 'AAA')

#The errors of ets_model are

#Group of answer choices:

#Additive
#Multiplicative
#None.

ets_model= ets(train,model = 'AAA')
ets_model

#Answer: Additive  ETS(A,A,A) =  Anytime an A = Additive.  See page 20 on time series notes


#2.The trend for ets_model is

#Group of answer choices:

#Additive

#Multiplicative

#None

#Answer: Additive

#3.What is AICc for ets_model?

ets_model

#ETS(A,A,A) 

#Call:
#  ets(y = train, model = "AAA") 

#Smoothing parameters:
#  alpha = 0.999 
#beta  = 0.0279 
#gamma = 1e-04 

#Initial states:
#  l = 287.3821 
#b = -1.0136 
#s = 19.2347 14.1838 16.0311 -2.1065 -10.1807 -4.7354
#-17.7148 -7.7327 -4.9514 -4.2277 6.6149 -4.4154

#sigma:  28.8841

#AIC     AICc      BIC 
#1248.824 1255.624 1294.420 

#Answer: 1255.624


#4.Do the residuals look like white noise? To answer this question,
#examine an Acf() or ggAcf() plot and the result of the Ljung-Box test.

checkresiduals(ets_model)

#Ljung-Box test

#data:  Residuals from ETS(A,A,A)
#Q* = 15.877, df = 6, p-value = 0.01443 (less than .05 not equal to white noise)

#if the autocorrelations of the residuals are very small, 
#we say that the model doesn’t show ‘significant lack of fit’=it fits well.

#Answer: Residuals are not white noise

#5. Use ets_model to construct a forecast for stock price over the next 34 months
#of the test sample. What is the point forecast of the stock price 
#for October 2018?


ets_model_forecast = forecast(ets_model,h=34)

ets_model_forecast

#         Point Forecast    Lo 80     Hi 80    Lo 95     Hi 95
#Oct 2018      1028.4942 708.0594 1348.9290 538.4314 1518.5570

#Answer: 1028.4942


#6.What is the RMSE of the ets_model on the test sample?

accuracy(ets_model_forecast,x = google)

#                  ME      RMSE      MAE       MPE     MAPE     MASE      ACF1
#Training set  3.000699  26.65879 20.71824 0.4825518 6.586215 0.288249 0.1226825
#Test set     41.706412 102.20154 81.22790 2.9705386 8.257877 1.130109 0.8334327
#Theil's U
#Training set        NA
#Test set      1.794293

#Answer: 102.20154


##----------------------------Part 4 ------------------------------------------

# 1. Now, let’s use an ARIMA model. Since, there are a large number of parameters 
#with which to define the ARIMA model, let use the auto.arima() function 
#to automatically determine the best parameters. Use the defaults for auto.arima().
#For instance, if your dataset is called train, run auto.arima(train).
#Call this auto_arima_model. 

#How many Ordinary Autoregressive LAG variables
#have been used in auto_arima_model?

#Group of answer choices:

#0 - answer

#1

#2

#3

auto_arima_model=auto.arima(train)

summary(auto_arima_model)

#Series: train 
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#     4.7640
#s.e.  2.6987

#sigma^2 estimated as 786.6:  log likelihood=-508.05
#AIC=1020.1   AICc=1020.22   BIC=1025.45


## Arima coefficients 0, 1, 0 refer to: p (AR), d (difference), q (MA), 

#answer: 0 

# 2. What is the number of Ordinary Differences used in auto_arima_model?
#Group of answer choices

## Arima coefficients 0, 1, 0 refer to: p (AR), d (difference), q (MA), 

#0

#1 - answer

#2

#3


# 3. How many ordinary Moving average lags have been used in auto_arima_model?
#Group of answer choices

## Arima coefficients 0, 1, 0 refer to: p (AR), d (difference), q (MA), 


#0 - answer

#1

#2

#3

#ANSWER:0

# 4. How many Seasonal autoregressive lag variables have been used in 
#auto_arima_model?
#Group of answer choices

#0 - answer ( no seasonality)

#1

#2

#3

##Answer - 0. From the interpretation of the coefficient "Arima(0,1,0)" 
#the seasoning (_, _, _) is missing that suppose to be next to (0,1,0).
#Series: train 
#ARIMA(0,1,0) with drift 

##Q. 9 has season "(3,1,0)[12]"
#Series: train 
#ARIMA(1,1,1)(3,1,0)[12]

# 5. Do the residuals look like white noise? To answer this question, 
#examine an Acf() or ggAcf() plot and the result of the Ljung-Box test.

#Group of answer choices:

#Residuals resemble white noise - Answer

#Residuals are not white noise 

checkresiduals(auto_arima_model)

#Ljung-Box test

#data:  Residuals from ARIMA(0,1,0) with drift
#Q* = 9.1755, df = 21, p-value = 0.9878  (greater than .05=white noise)

#Model df: 1.   Total lags used: 22

# Answer: Residuals resemble white noise

# 6. Use auto_arima_model to construct a forecast for stock price over the 
#next 34 months of the test sample. What is the point forecast of the stock 
#price for October 2018?

arima_forecast=forecast(auto_arima_model,h=34)
arima_forecast

#Point Forecast
#Oct 2018       920.8568

#answer: 920.8568

# 7. What is the RMSE of auto_arima_model on the test sample?

accuracy(arima_forecast,x = google)


#                    ME      RMSE       MAE        MPE      MAPE      MASE
#Training set  0.002262644  27.78621  20.45591 -0.8593724  6.345831 0.2845992
#Test set     78.533429314 143.69172 112.13889  6.4915040 11.089666 1.5601675
#                    ACF1 Theil's U
#Training set 0.09105305        NA
#Test set     0.89349920  2.456068

#Answer: 143.69172


# 8. Let us see if we can improve our ARIMA model by a variance stabilizing
#transformation. 
#BoxCox.lambda() is a handy function for identifying the optimal value of 
#lambda to stabilize variance. What is the optimal value of lambda?


train2 = auto.arima(train,lambda = BoxCox.lambda(train))

train2

#Series: train 
#ARIMA(0,1,0) with drift 
#Box Cox transformation: lambda= 0.4748787 

#Coefficients:
#  drift
#0.1885
#s.e.  0.1240

#sigma^2 estimated as 1.661:  log likelihood=-178.47
#AIC=360.94   AICc=361.06   BIC=366.29

##Answer: 0.4748787 

# 9. Rather than using auto.arima(), let us specify an ARIMA model as follows:

#Arima(train,order = c(1,1,1),seasonal = c(3,1,0),lambda=BoxCox.lambda(train))

#Call this arima_model. What is the "AICc" for arima_model?

arima_model=Arima(train,order = c(1,1,1),seasonal = c(3,1,0),lambda=BoxCox.lambda(train))
arima_model

#Series: train 
#ARIMA(1,1,1)(3,1,0)[12] 
#Box Cox transformation: lambda= 0.4748787 

#Coefficients:
#  ar1     ma1     sar1     sar2     sar3
#-0.3460  0.5480  -0.8242  -0.4403  -0.2821
#s.e.   0.3249  0.2847   0.1063   0.1393   0.1268

#sigma^2 estimated as 1.796:  log likelihood=-165.1
#AIC=342.21   AICc=343.16   BIC=357.53

##Answer: 343.16  notice its not just AIC; it's AIC and little c


# 10. Examine the results of Ljung-Box test (using the default of 24 lags) 
#to see if the residuals resemble white noise.
#Group of answer choices:

#Residuals resemble white noise

#Residuals are not white noise

checkresiduals(arima_model)

#Ljung-Box test

#data:  Residuals from ARIMA(1,1,1)(3,1,0)[12]
#Q* = 8.3835, df = 17, p-value = 0.9576  (greater than .05=white noise)

#Model df: 5.   Total lags used: 22

##Answer: Residuals resemble white noise

# 11. Use arima_model to construct a forecast for stock price over the next
#34 months of the test sample. What is the point forecast of the stock price 
#for October 2018?

arima_model_forecast=forecast(arima_model,h=34)
arima_model_forecast

#Point Forecast
# Oct 2018      1165.1741

##ANswer: 1165.1741


# 12. What is the RMSE of arima_model on the test sample?

accuracy(arima_model_forecast,x = google)

#                  ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
#Training set   1.82702 25.96986 18.59719  0.2924232 5.765147 0.2587392 -0.04963127
#Test set     -14.21973 56.19252 47.38778 -2.3674559 5.361542 0.6592973  0.55742944
#Theil's U
#Training set        NA
#Test set      1.158525  

#Answer: 56.19252
























































































  
  

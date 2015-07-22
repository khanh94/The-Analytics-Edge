#!/usr/bin/Rscript

# PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT
################################################################

#Read in the data and find proportion of positive returns in December
stocks = read.csv('StocksCluster.csv')
summary(stocks$PositiveDec)

#Largest correlation
cor(stocks)

#Largest and smallest mean return
summary(stocks)

#Splitting data into training and testing set
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

#Logistic Regression Model
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
predictTrain = predict(StocksModel, type="response", newdata=stocksTrain)
table(stocksTrain$PositiveDec, predictTrain > 0.5)
predictTest = predict(StocksModel, type="response", newdata=stocksTest)
table(stocksTest$PositiveDec, predictTest > 0.5)

#Clustering of training and testing datasets
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#Data preprocessing
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
summary(normTrain)
summary(normTest)

#k-means clustering
set.seed(144)
km = kmeans(normTrain, centers = 3)

#Obtaining cluster assignment for datasets
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

#Obtaining training and testing data by subsetting
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

#Logistic regression models
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)

#Predictions using logistic regression
predictStocks1 = predict(StocksModel1, newdata=stocksTest1, type="response")
predictStocks2 = predict(StocksModel2, newdata=stocksTest2, type="response")
predictStocks3 = predict(StocksModel3, newdata=stocksTest3, type="response")
table(stocksTest1$PositiveDec, predictStocks1 > 0.5)
table(stocksTest2$PositiveDec, predictStocks2 > 0.5)
table(stocksTest3$PositiveDec, predictStocks3 > 0.5)

#Compute overall test set accuracy
AllPredictions = c(predictStocks1, predictStocks2, predictStocks3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)


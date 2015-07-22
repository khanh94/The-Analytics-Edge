#!/usr/bin/Rscript

#Election Forecasting Revisited

library(ggplot2)
library(maps)
library(ggmap)

#Loading the US map
statesMap = map_data("state")

#Drawing the US map
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

#Coloring the states by predictions
#Loading the data
polling = read.csv('PollingImputed.csv')
Train = subset(polling, polling$Year<= 2008)
Test = subset(polling, polling$Year == 2012)

#Logistic Regression Model
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

#Number of states where prediction is Republican
sum(predictionDataFrame$TestPredictionBinary)

#Convert the state name to lower case
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

#Merge two data frames
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

#Reordering observations
predictionMap = predictionMap[order(predictionMap$order),]

#Color the US map with the predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black")

#Redraw the map with discrete outcomes
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


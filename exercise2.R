library(dplyr)
framingham <- read.csv("framingham.csv")
str(framingham)
glimpse(framingham)
summary(framingham)

#library that has sample.split
library(caTools)
#creating a seed to make the results easy to replicate
set.seed(1000)
#creating the split
split <- sample.split(framingham$TenYearCHD, SplitRatio = .65)

train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == FALSE)
#Using the . in place of the independent variables means use all of the other variables
#This is great where all of the dependent variables make sense but 
#the approach can cause problems if some of the columns have no possible signifigance
#to the model, such as employee ID numbers when looking at staff
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

predictTest <- predict(framinghamLog, type = "response", newdata = test)
#finding the results of the model and making the confusion matrix
table(test$TenYearCHD, predictTest > 0.5)

#Find accuracy of model 
(1094 +11)/(1094+9+189+11)
#Result: 84.8043% accuracy 
#Creating a baseline model. FOr this we imagine a model that always says false 
#for the dependent variable

(1094+9)/(1094+9+189+11)
#result: 84.6581% accuracy

library(ROCR)
#take the prediction model as the first argument and the test table with dependent
#variable as the second argument
ROCRpred = prediction(predictTest, test$TenYearCHD)

#At this point I am lost.  What the hell is @y.values? And what does "auc" mean
#in this context?
as.numeric(performance(ROCRpred, "auc")@y.values)


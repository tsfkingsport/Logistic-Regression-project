install.packages("mice")
library(mice)
#mice is Multiple Imputation by CHained Equations. I need to look this up 
#on wikipedia at some point. the presentation just says that the method is very 
#sophisticated mathematically. 
library(dplyr)
library(caTools)
library(ROCR)

polling = read.csv("PollingData.csv")

str(polling)

table(polling$Year)

summary(polling)
simple <- polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)

#I need to look up more about what imputed is and how exactly it works. 
#

set.seed(144)
imputed <- complete(mice(simple))

summary(imputed)

polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA

summary(polling)

Train = subset(polling, Year == 2004| Year == 2008)
Test = subset(polling Year ==2012)

table(Train$Republican)
#Baseline is prediction the Republican will win

table(Train$Republican, sign(Train$Rasmussen))

str(Train)

cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

mod1 <- glm(Republican ~ PropR, data = Train, family = "binomial")
summary(mod1)

pred1 <- predict(mod1, type = "response")
table(Train$Republican, pred1 > .5)

mod2 = glm(Republican ~SurveyUSA + DiffCount, data = Train, family = "binomial")
pred2 = predict(mod2, type = "response")
table(Train$Republican, pred2 >= .5)
summary(mod2)
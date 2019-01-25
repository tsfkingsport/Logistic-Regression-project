install.packages("caTools")
install.packages("ROCR")
library(ROCR)
library(caTools)
library(dplyr)
library(ggplot2)

quality <- read.csv("quality.csv")

str(quality)

glimpse(quality)

table(quality$PoorCare)

set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = .75)

qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)

nrow(qualityTrain)
nrow(qualityTest)

QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, 
                  data = qualityTrain, family = binomial)
summary(QualityLog)

predictTrain = predict(QualityLog, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

table(qualityTrain$PoorCare, predictTrain > .5)


table(qualityTrain$PoorCare, predictTrain > .7)


table(qualityTrain$PoorCare, predictTrain > .2)

ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
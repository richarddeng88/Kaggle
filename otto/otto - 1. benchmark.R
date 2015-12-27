library(ggplot2);library(caret);library(randomForest)
train <- read.csv("data/otto/train.csv")
test <- read.csv("data/otto/test.csv")
sample <- read.csv("data/otto/sampleSubmission.csv")

# 1st glance
prop.table(table(train$target))

submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)

rf <- randomForest(train$target, train[,c(-1,-95)], ntree=500, importance=TRUE)
varImpPlot(rf)

imp <- importance(rf, type=1)
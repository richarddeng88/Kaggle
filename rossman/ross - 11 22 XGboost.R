library(readr)
library(xgboost)

train <- train[ which(train$open=='1'),]
train <- train[ which(train$sales!='0'),]

train$date <- as.Date(train$date)
test$date <- as.Date(test$date)

train$month<- as.POSIXlt(train$date)$mon+1
train$day<- as.POSIXlt(train$date)$mday
train$year<- as.POSIXlt(train$date)$year

test$month<- as.POSIXlt(test$date)$mon+1
test$day<- as.POSIXlt(test$date)$mday
test$year<- as.POSIXlt(test$date)$year

train <- train[,-c(3,8)]
test <- test[,-c(4,7)]

feature.names <- names(train)[c(1,2,5:9)]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
    if (class(train[[f]])=="character") {
        levels <- unique(c(train[[f]], test[[f]]))
        train[[f]] <- as.integer(factor(train[[f]], levels=levels))
        test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    }
}

tra <- train[,feature.names]
RMPSE<- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    elab<-exp(as.numeric(labels))-1
    epreds<-exp(as.numeric(preds))-1
    err <- sqrt(mean((epreds/elab-1)^2))
    return(list(metric = "RMPSE", value = err))
}









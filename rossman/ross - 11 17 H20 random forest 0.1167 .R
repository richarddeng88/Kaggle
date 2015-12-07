mer_train <- merge(train,store,by="store")
mer_test <- merge(test,store,by="store")


library(dplyr)
mer_train <- filter(mer_train, sales>0)

mer_train$date <- as.Date(mer_train$date)
mer_test$date <- as.Date(mer_test$date)

mer_train$logsale <- log(mer_train$sales)

mer_train$month<- as.POSIXlt(mer_train$date)$mon+1
mer_train$day<- as.POSIXlt(mer_train$date)$mday
mer_train$year<- as.POSIXlt(mer_train$date)$year+1900

mer_test$month<- as.POSIXlt(mer_test$date)$mon+1
mer_test$day<- as.POSIXlt(mer_test$date)$mday
mer_test$year<- as.POSIXlt(mer_test$date)$year+1900

library(h2o)
## Start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='10G')
## Load data into cluster from R
trainHex<-as.h2o(mer_train)

## Set up variable to use all features other than those specified here
features<-colnames(train)[!(colnames(train) %in% c("id","date","sales","logsale","customers"))]

## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="logsale", 
                          ntrees = 100,
                          max_depth = 45,
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=trainHex)


summary(rfHex)
## Load test data into cluster from R
testHex<-as.h2o(mer_test)
## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(rfHex,testHex))

pred <- expm1(predictions[,1])
summary(pred)
submission <- data.frame(Id=test$id, sales=pred)
write.csv(submission, "kaggle competition/rossman/final_submission.csv",row.names=F)
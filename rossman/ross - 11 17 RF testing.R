train <- read.csv("data/rossman/train.csv")
test <- read.csv("data/rossman/test.csv")
store <- read.csv("data/rossman/store.csv")

names(train) <- c("store","wday","date","sales","customers","open","promo","state_holiday","school_holiday")
names(test)  <- c("id","store","wday","date","open","promo","state_holiday","school_holiday")
names(store) <- c("store", "type", "assortment","distance","since_m","since_y","promo2","p_since_m","p_since_y","interval")
# no missing values. 

train$date <- as.Date(train$date)
test$date <- as.Date(test$date)

train$month<- as.POSIXlt(train$date)$mon+1
train$day<- as.POSIXlt(train$date)$mday
train$year<- as.POSIXlt(train$date)$year+1900

test$month<- as.POSIXlt(test$date)$mon+1
test$day<- as.POSIXlt(test$date)$mday
test$year<- as.POSIXlt(test$date)$year+1900

train$state_holiday <- ifelse(train$state_holiday != "0", 1,0)
test$state_holiday <- ifelse(test$state_holiday !="0", 1, 0)

train <- filter(train, sales>0)


test$open[is.na(test$open)] <- 1
sum(is.na(test))

# merge data with store info
rm_store <- store[, c(1,2,3,4,7)]
rm_store$distance[is.na(rm_store$distance)] <- mean(rm_store$distance, na.rm=T)


mer_train <- merge(train,rm_store,by="store")
mer_test <- merge(test,rm_store,by="store")



## randomforest devided by store. score is 0.12249================= ==============
library(dplyr)
ran_train  <- select(train, -customers,-date)
ran_test <- select(test,-date)

x <-unique(ran_test$store)
sub <-  data.frame(id=0,sales=0);
for (i in x) {
  
  train_i <- filter(ran_train, store==i)
  test_i <- filter(ran_test, store==i)
  rfModel <- randomForest(sales~., data=train_i, importance= T, ntrees=6)
  pred_slaes <- predict(rfModel,test_i)
  sub <- rbind(sub, data.frame(id=test_i$id, sales=pred_slaes))
  
}

sub <- sub[-1,]

submission <- arrange(sub, id)
write.csv(submission, "final_submission.csv",row.names=F)


    ## PERFORMANCE INHANCEMENT - log sales
    library(dplyr);library(randomForest)
    ran_train  <- train %>% mutate(logsales=log(sales)) %>% select(-customers,-date,-sales)
    ran_test <- select(test,-date,-state_holiday)   
    
    x <-unique(ran_test$store)
    sub <-  data.frame(id=0,sales=0);
    for (i in x) {
      
      train_i <- filter(ran_train, store==i)
      test_i <- filter(ran_test, store==i)
      rfModel <- randomForest(logsales~., data=train_i, importance= T, ntrees=9)
      pred_slaes <- predict(rfModel,test_i)
      sub <- rbind(sub, data.frame(id=test_i$id, sales=pred_slaes))
      
    }
    
    sub <- sub[-1,]
    sub$sales <- exp(sub$sales)
    submission <- arrange(sub, id)
    write.csv(submission, "final_submission.csv",row.names=F)


## DEAL WITH MERGED DATA - RANDOM FOREST. not good idea. becaue we merge data by store, this way = add more noice, so there will be more MSE. 
library(dplyr);library(randomForest)
ran_train  <- mer_train %>% mutate(logsales=log(sales)) %>% select(-customers,-date,-sales)
ran_test <- select(mer_test,-date)   

x <-unique(ran_test$store)
sub <-  data.frame(id=0,sales=0);
for (i in x) {
    
    train_i <- filter(ran_train, store==i)
    test_i <- filter(ran_test, store==i)
    rfModel <- randomForest(logsales~., data=train_i, importance= T, ntrees=9)
    pred_slaes <- predict(rfModel,test_i)
    sub <- rbind(sub, data.frame(id=test_i$id, sales=pred_slaes))
    
}

sub <- sub[-1,]
sub$sales <- exp(sub$sales)
submission <- arrange(sub, id)
write.csv(submission, "final_submission.csv",row.names=F)
## random forest put together to analyze.  NOT ENOUGH MEMORY TO WROK ======================================================
    library(randomForest)
    rfModel <- randomForest(sales~., data=ran_train, importance= T, ntrees=10)
    pred_slaes <- predict(rfModel,ran_test)
    
    sub <- data.frame(id=test_i$id, sales=pred_slaes)






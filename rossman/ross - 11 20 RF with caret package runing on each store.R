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

test$open[is.na(test$open)] <- 1
sum(is.na(test))

## randomforest with caret package, ================= ==============
library(dplyr)
ran_train  <- select(train, -customers,-date)
ran_test <- select(test,-date)

x <-unique(ran_test$store)
sub <-  data.frame(id=0,sales=0);
for (i in x) {
    
    train_i <- filter(ran_train, store==i)
    test_i <- filter(ran_test, store==i)
    rfModel <- train(sales~., data=train_i, method="rf",prox=T)
    pred_slaes <- predict(rfModel,test_i)
    sub <- rbind(sub, data.frame(id=test_i$id, sales=pred_slaes))
    
}

sub <- sub[-1,]
submission <- arrange(sub, id)
write.csv(submission, "final_submission.csv",row.names=F)








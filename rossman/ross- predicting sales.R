test$open[is.na(test$open)] <- 1
sum(is.na(test))

mer_train <- merge(train,store,by="store")
mer_test <- merge(test,store,by="store")
#================================================================================
# random forest without divide by store
ran_train <- select(train, -date)
ran_test <- select(test, -date)






#================================================================================
# 0.1680 with negative predictions
train1 <- select(train, -customers,-date)
test1 <- select(test,-date)

x <-unique(test$store)
sub <-  data.frame(id=0,sales=0);
#sub <- matrix(0, dim(test1),2)
for (i in x) {
    
    train_i <- filter(train1, store==i)
    test_i <- filter(test1, store==i)
    linear_fit <- lm(sales~. , train_i)
    predicted_slaes <- predict(linear_fit, test_i)
    sub <- rbind(sub, data.frame(id=test_i$id, sales=predicted_slaes))
}

sub <- filter(sub, id!=0)

submission <- arrange(sub, id)
write.csv(submission, "kaggle competition/rossman/final_submission.csv",row.names=F)
#================================================================================
# RANDOM FOREST - 1.79
library(dplyr)
ran_train  <- select(train, -customers,-date,-state_holiday)
ran_test <- select(test,-date,-state_holiday)

library(randomForest)
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
write.csv(submission, "kaggle competition/rossman/final_submission.csv",row.names=F)


#================================================================================
# MODEL TREE -
library(dplyr)
mod_train  <- select(train, -customers,-date)

judge <- (as.character(mod_train$state_holiday) != "0" )
mod_train$state_holiday <- as.character(mod_train$state_holiday)
mod_train$state_holiday[judge] <- "a"
mod_train$state_holiday <- as.numeric(mod_train$state_holiday)
mod_test <- select(test,-date,-state_holiday)

library(randomForest)
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
write.csv(submission, "kaggle competition/rossman/final_submission.csv",row.names=F)

#=============================================================================================
# 0.1816 so many negative prediction. 
train1 <- select(train, -customers,-date)
train1$day[train1$day!=7] <- "Weekday"
train1$day[train1$day==7] <- "Sunday"
train1$day <- factor(train1$day, c("Weekday", "Sunday"))

test1 <- select(test,-date)
test1$day[test1$day!=7] <- "Weekday"
test1$day[test1$day==7] <- "Sunday"
test1$day <- factor(test1$day, c("Weekday", "Sunday"))

x <-unique(test$store)
sub <-  data.frame(id=0,sales=0);
#sub <- matrix(0, dim(test1),2)
for (i in x) {
    
    train_i <- filter(train1, store==i)
    test_i <- filter(test1, store==i)
    linear_fit <- lm(sales~. , train_i)
    predicted_slaes <- predict(linear_fit, test_i)
    sub <- rbind(sub, data.frame(id=test_i$id, sales=predicted_slaes))
}

sub <- filter(sub, id!=0)

submission <- arrange(sub, id)
write.csv(submission, "kaggle competition/rossman/final_submission.csv",row.names=F)

#=============================================================================================
# 2nd, take out date, stat_holiday and school_holiday, factorize day, open, and promo. 
# we run regression by per store.  
# here due to factor variable cannot be 1, i drop open in to the model. i will fix in next round. 

train1 <- select(train, -customers,-date, -state_holiday, -school_holiday)
train1$day[train1$day!=7] <- "Weekday"
train1$day[train1$day==7] <- "Sunday"
train1$day <- factor(train1$day, c("Weekday", "Sunday"))
train1$open[train1$open==1] <- "open"
train1$open[train1$open==0] <- "close"
train1$open <- factor(train1$open, c("open", "close"))
train1$promo[train1$promo==1] <- "yes"
train1$promo[train1$promo==0] <- "no"
train1$promo <- factor(train1$promo, c("yes", "no"))

test$open[is.na(test$open)] <- 1
sum(is.na(test))

test1 <-select(test, -date, -state_holiday, -school_holiday)
test1$day[test1$day!=7] <- "Weekday"
test1$day[test1$day==7] <- "Sunday"
test1$day <- factor(test1$day, c("Weekday", "Sunday"))
test1$open[test1$open==1] <- "open"
test1$open[test1$open==0] <- "close"
test1$open <- factor(test1$open, c("open", "close"))
test1$promo[test1$promo==1] <- "yes"
test1$promo[test1$promo==0] <- "no"
test1$promo <- factor(test1$promo, c("yes", "no"))

x <-unique(test$store)
sub <-  data.frame(id=0,sales=0);
#sub <- matrix(0, dim(test1),2)
for (i in x) {

        train_i <- filter(train1, store==i)
        test_i <- filter(test1, store==i)
        #linear_fit <- lm(sales ~ day+open+promo, train_i)
        linear_fit <- lm(sales ~ day+promo, train_i)
        predicted_slaes <- predict(linear_fit, test_i)
        sub <- rbind(sub, data.frame(id=test_i$id, sales=predicted_slaes))
}

sub <- filter(sub, id!=0)

submission <- arrange(sub, id)
write.csv(submission, "kaggle competition/rossman/final_submission.csv",row.names=F)


#=============================================================================================
# 4th, first deal with open variable.
# then run regression by store. 

train1 <- select(train, -customers,-date, -state_holiday, -school_holiday)
train1$day[train1$day!=7] <- "Weekday"
train1$day[train1$day==7] <- "Sunday"
train1$day <- factor(train1$day, c("Weekday", "Sunday"))
train1$open[train1$open==1] <- "open"
train1$open[train1$open==0] <- "close"
train1$open <- factor(train1$open, c("open", "close"))
train1$promo[train1$promo==1] <- "yes"
train1$promo[train1$promo==0] <- "no"
train1$promo <- factor(train1$promo, c("yes", "no"))

test$open[is.na(test$open)] <- 1
sum(is.na(test))

test1 <-select(test, -date, -state_holiday, -school_holiday)
test1$day[test1$day!=7] <- "Weekday"
test1$day[test1$day==7] <- "Sunday"
test1$day <- factor(test1$day, c("Weekday", "Sunday"))
test1$open[test1$open==1] <- "open"
test1$open[test1$open==0] <- "close"
test1$open <- factor(test1$open, c("open", "close"))
test1$promo[test1$promo==1] <- "yes"
test1$promo[test1$promo==0] <- "no"
test1$promo <- factor(test1$promo, c("yes", "no"))

x <-unique(test$store)
sub <-  data.frame(id=0,sales=0);
#sub <- matrix(0, dim(test1),2)
for (i in x) {
    
    train_i <- filter(train1, store==i)
    test_i <- filter(test1, store==i)
    #linear_fit <- lm(sales ~ day+open+promo, train_i)
    linear_fit <- lm(sales ~ day+promo, train_i)
    predicted_slaes <- predict(linear_fit, test_i)
    sub <- rbind(sub, data.frame(id=test_i$id, sales=predicted_slaes))
}

sub <- filter(sub, id!=0)

submission <- arrange(sub, id)
write.csv(submission, "kaggle competition/rossman/final_submission.csv",row.names=F)


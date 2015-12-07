library(dplyr)
first_train <- train %>% filter(store==1) %>% select(-customers,-date) 
rm_store <- store[, c(1,2,3,7)]
# rm_store$distance[is.na(rm_store$distance)] <- mean(rm_store$distance, na.rm=T)


mer_train <- merge(train,rm_store,by="store")
mer_test <- merge(test,rm_store,by="store")

library(caret)
set.seed(1001)
intrain <- createDataPartition(y=first_train$sales, p=0.8, list = F)
first_training <- first_train[intrain,]
first_validation <- first_train[-intrain,]
first_validation <- cbind(id=1:dim(first_validation)[1],first_validation)

#  RANDOM FOREST
library(randomForest)
a=50
last <- rep(0,a)
for (i in 1:a){
  
  rfModel <- randomForest(sales~., data=first_training, importance= T, ntrees=i)
  pred_slaes <- predict(rfModel,first_validation)
  sub <- data.frame(id=first_validation$id, sales=pred_slaes)  
  
  # evaluate the prediction using RMSPE
  no_0 <- first_validation %>% filter(sales!=0)
  last[i] <- sqrt(sum(((no_0$sales - sub[no_0$id,]$sales)/no_0$sales)^2)/dim(no_0)[1])
  
}

plot(1:a, last, type = "l")
range(last) #[1] 0.1427507 0.1496177
            # if i keep all sales=0, there is no difference in the result. 

## INCREASING THE PORFORMANCE 
  # logsales
    first_train_log <- train %>% mutate(logsales=log(sales)) %>% filter(store==1, sales>0) %>% select(-customers,-date) 
    hist(first_train$logsales)
    
    library(caret)
    set.seed(1001)
    intrain <- createDataPartition(y=first_train$sales, p=0.8, list = F)
    first_training <- first_train_log[intrain,]
    first_training <- select(first_training, -sales)
    first_validation <- first_train_log[-intrain,]
    first_validation <- cbind(id=1:dim(first_validation)[1],first_validation)
    
    library(randomForest)
    a=50
    last <- rep(0,a)
    for (i in 1:a){
      
      rfModel <- randomForest(logsales~., data=first_training, importance= T, ntrees=i)
      pred_slaes <- predict(rfModel,first_validation)
      sub <- data.frame(id=first_validation$id, sales=pred_slaes)  
      sub$sales <- exp(sub$sales)
      
      # evaluate the prediction using RMSPE
      no_0 <- first_validation %>% filter(sales!=0)
      last[i] <- sqrt(sum(((no_0$sales - sub[no_0$id,]$sales)/no_0$sales)^2)/dim(no_0)[1])
      
    }
    
    plot(1:a, last, type = "l")
    range(last) #[ 1] 0.1289448 0.1352873, we see that the whole range has increased a little bit





## REGRESSION TREE
library(rpart)
m.rpart <- rpart(sales~., data=first_training)
library(rpart.plot)
rpart.plot(m.rpart, digits=3)
rpart.plot(m.rpart, digits=3, fallen.leaves = T, type=1)

pre_rpart <- predict(m.rpart, first_validation)
cor(pre_rpart, first_validation$sales)

sub <- data.frame(id=1:lenght(pre_rpart),sales=pre_rpart )
no_0 <- first_validation %>% filter(sales!=0)
sqrt(sum(((no_0$sales - sub[no_0$id,]$sales)/no_0$sales)^2)/dim(no_0)[1])  # 0.1704

a <- data.frame(prediction=pre_rpart, real=first_validation$sales)

## MODEL TREE
library(RWeka)
m.m5p <- M5P(sales~., data=first_training)

pre_m5p <- predict(m.m5p, first_validation)

cor(pre_m5p, first_validation$sales)

sub <- data.frame(id=1:188,sales=pre_m5p )
no_0 <- first_validation %>% filter(sales!=0)
sqrt(sum(((no_0$sales - sub[no_0$id,]$sales)/no_0$sales)^2)/dim(no_0)[1])

a <- data.frame(prediction=pre_m5p, real=first_validation$sales)
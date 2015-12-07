library(dplyr)
first_train <- train %>% filter(store==1) %>% select(-customers,-date) 

library(caret)
set.seed(1001)
intrain <- createDataPartition(y=first_train$sales, p=0.8, list = F)
first_training <- first_train[intrain,]
first_validation <- first_train[-intrain,]
first_validation <- cbind(id=1:dim(first_validation)[1],first_validation)

# fit the model - boosting
modFit <- train(sales ~ ., method="gbm",data=first_training,verbose=FALSE)
print(modFit)

pred <- predict(modFit,first_validation)
sub <- data.frame(id=first_validation$id, sales= pred)

qplot(pred,sales,data=first_validation)

no_0 <- first_validation %>% filter(sales!=0)
sqrt(sum(((no_0$sales - sub[no_0$id,]$sales)/no_0$sales)^2)/dim(no_0)[1])  # 0.1275

#######============ RF with caret package=========================================================
modFit <- train(sales ~ ., method="rf",data=first_training,prox=T)
print(modFit)

pred <- predict(modFit,first_validation)
sub <- data.frame(id=first_validation$id, sales= pred)

qplot(pred,sales,data=first_validation)

no_0 <- first_validation %>% filter(sales!=0)
sqrt(sum(((no_0$sales - sub[no_0$id,]$sales)/no_0$sales)^2)/dim(no_0)[1])  #0.12755785





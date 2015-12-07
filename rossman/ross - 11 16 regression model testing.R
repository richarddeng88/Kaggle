library(caret)

## DATA SPLITING ACCORDING TO PER STORE. 
training <- train[1,]; validation <- train[1,]
for (i in 1:1115){
    store_train <- train[train$store==i,]
    intrain <- createDataPartition(y=store_train$sales, p=0.8, list = F)
    training <- rbind(training,store_train[intrain,])
    validation <- rbind(validation, store_train[-intrain,])
}
training <- training[-1,]; validation <- validation[-1,]
validation <- cbind(id=1:dim(validation)[1],validation)

## MODEL 1 - REGRESSION 
    ## TRAINING MODEL - REGRESSION MODEL
    library(dplyr)
    reg_train  <- select(training, -customers,-date,-state_holiday)
    reg_validation <- select(validation,-customers,-date,-state_holiday)
    
    x <-unique(reg_validation$store)
    sub <-  data.frame(id=0,sales=0);
    for (i in x) {
        
        train_i <- filter(reg_train, store==i)
        validation_i <- filter(reg_validation, store==i)
        linear_fit <- lm(sales~. , train_i)
        pred_slaes <- predict(linear_fit, validation_i)
        sub <- rbind(sub, data.frame(id=validation_i$id, sales=pred_slaes))
    }
    
    sub <- sub[-1,]
    
    # evaluate the prediction using RMSPE
    no_0 <- validation %>% filter(sales!=0)
    sqrt(sum(((no_0$sales - sub[no_0$id,]$sales)/no_0$sales)^2)/dim(no_0)[1])

    
    
## MODEL 2 - RANDOM FOREST
    ## TRAINING MODEL - REGRESSION MODEL
    library(dplyr)
    ran_train  <- select(training, -customers,-date,-state_holiday)
    ran_validation <- select(validation,-customers,-date,-state_holiday)
    
    library(randomForest)
    x <-unique(ran_validation$store)
    sub <-  data.frame(id=0,sales=0);
    for (i in x) {
        
        train_i <- filter(ran_train, store==i)
        validation_i <- filter(ran_validation, store==i)
        rfModel <- randomForest(sales~., data=train_i, importance= T, ntrees=6)
        pred_slaes <- predict(rfModel,validation_i)
        sub <- rbind(sub, data.frame(id=validation_i$id, sales=pred_slaes))
    }
    
    sub <- sub[-1,]
    
    # evaluate the prediction using RMSPE
    no_0 <- validation %>% filter(sales!=0)
    sqrt(sum(((no_0$sales - sub[no_0$id,]$sales)/no_0$sales)^2)/dim(no_0)[1])
    



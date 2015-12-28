library(caret)

## now, we have training and validation set. 
        rf_model <- randomForest(target~., 
                           data= training[,-1], 
                           ntree=100, 
                           importance=TRUE)
        
        rf_pred <- predict()
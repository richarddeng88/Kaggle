library(caret); library(class)

## now, we have training and validation set. 
        # RF
        
        rf_model <- randomForest(target~., 
                                 data= training[,-1], 
                                 ntree=100, 
                                 importance=TRUE)
        
        rf_pred <- predict(rf_model, validation[,-1])
        confusionMatrix(rf_pred, validation$target) # 0.8062
        
        # naive bayes
        
        
        # KNN
        # ormalize <- function(x) { return ((x - mean(x)) / sd(x))}
        # sapply(training[,2:94], function(x){mean(x)})
        stand <- preProcess(training[,2:94], method = c("center","scale"))
        knn_training <- predict(stand, training)
        knn_validation <- predict(stand, validation)
        
        
        knn_model <- knn(training[,c(-1,-95)], 
                         validation[, c(-1,-95)],
                         training$target,
                         k=8,
                         prob=T)
        knn_pred <- knn_model
        confusionMatrix(knn_pred, validation$target) # 0.7762
        
        
        # LDA
        lda_model <- train(target~., 
                          data= training[,-1], 
                          method="lda")
        lda_pred <- predict(lda_model, validation[,-1])
        confusionMatrix(lda_pred, validation$target) #0.7041
        
        
        # QDA
        qda_model <- train(target~., 
                           data= training[,-1], 
                           method="qda")
        qda_pred <- predict(qda_model, validation[,-1])
        confusionMatrix(qda_pred, validation$target) #0.6673
        
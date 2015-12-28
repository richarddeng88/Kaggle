library(caret); library(class)

## now, we have training and validation set. 
        # RF
        rf_model <- randomForest(target~., 
                                 data= training[,-1], 
                                 ntree=100, 
                                 importance=TRUE)
        
        rf_pred <- predict(rf_model, validation[,-1])
        confusionMatrix(rf_pred, validation$target) # 0.8062
        

        # KNN
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
        
        #knn_model <- train(target~.,
        #                   data=training[,-1],
        #                   method= "knn")
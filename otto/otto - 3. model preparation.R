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
        knn_model <- knn(knn_training[,c(-1,-95)], 
                         knn_validation[, c(-1,-95)],
                         knn_training$target,
                         k=8,
                         prob=T)
        knn_pred <- knn_model
        confusionMatrix(knn_pred, validation$target) # 0.7683
        
                # purning the parameter.
                ctrl <- trainControl(method = "cv", number = 10) 
                grid <- expand.grid(k=c(3,5,7,9,10,12,14,16,18))
                knn_cv_model <- train(target~., 
                                   data=training[,-1], 
                                   method="knn", 
                                   #preProcess=c("center","scale"),
                                   tuneGrid = grid,
                                   #tuneLength = 40,
                                   trControl=ctrl) # need to set up different k values as i want
        
        
        

        
        
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
        
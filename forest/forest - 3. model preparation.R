library(caret); library(class);library(randomForest)

## now, this is only for practicing ensembling. 
        # RF
        rf_model <- randomForest(Cover_Type~.,
                                 #data= train[,-1], 
                                 data =train,
                                 ntree=800)
        
        rf_pred <- predict(rf_model,test)
        submission <- data.frame(Id=test$Id, Cover_Type=rf_pred)
        write.csv(submission, file="submission.csv",row.names = F)
        
        
        # GRADIANT BOOSTING
        Grid <-  expand.grid(
                n.trees = c(100),
                interaction.depth = c(8) ,
                shrinkage = 0.2)
        fitControl <- trainControl(method = "none", classProbs = TRUE)
        GBMmodel <- train(Cover_Type ~ .,
                          data = train,
                          method = "gbm",
                          #trControl = fitControl,
                          verbose = TRUE
                          #tuneGrid = Grid,
                          ## Specify which metric to optimize
                          ## We will optimize AUC of ROC curve as it best encapsulates the predictive power of a model                 
                          #metric = "ROC"
        )
        
        GBMpredTrain = predict(GBMmodel, newdata = train)
        confusionMatrix(GBMpredTrain, train$Cover_Type)
        
        
        # KNN
        proobj <- preProcess(train[,2:11], method = c("center", "scale")) 
        train_body <- predict(proobj, train[,c(-1,-56)])
        test_body <- predict(proobj, test[,-1])
        #knn_training <- cbind(train_body,training[,53])
        
        knn_pred <- knn(train_body, 
                         test_body,
                         train$Cover_Type,
                         k=8,
                         prob=T)
                # caret package
                ctrl <- trainControl(method = "cv", number = 10)
                grid <- expand.grid(k=c(4,6,8,10,12,14,16,18,20))
                knn_model <- train(Cover_Type~., 
                                   data=train[,-1],
                                   method="knn",
                                   tuneGrid = grid,
                                   trControl = ctrl)
        
        knn_pred <- predict(knn_model, test)        
        submission <- data.frame(Id=test$Id, Cover_Type= knn_pred)
        write.csv(submission, file="submission.csv",row.names = F)
        # the score is 0.61198  no.1396
library(randomForest);library(rattle);library(rpart.plot);library(caret)

        # train RF model using "caret" package
                grid <- expand.grid(.mtry=c(2,3,4,5,6,7))
                ctrl <- trainControl(method= "cv", number = 10)
                rf_model <- train(Survived~Pclass+Sex+Age+SibSp+Embarked+Fare+family,
                                  data=train,
                                  method="rf",
                                  importance=T,
                                  tuneGrid =grid,
                                  trControl = ctrl)
                varImpPlot(rf_model$finalModel)
                
                rf_pred <- predict(rf_model, test)
                solution <- data.frame(PassengerID=test$PassengerId, Survived=rf_pred)
                write.csv(solution,file="kaggle/titanic/submission.csv",row.names = F)
                
        # train RF model using "randomForest" package
                rf1_model <- randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Fare+Cabin+family,
                                          data=train,
                                          importance=T,
                                          ntree=2000)
                
                rf_pred <- predict(rf1_model, test)
                solution <- data.frame(PassengerID=test$PassengerId, Survived=rf_pred)
                write.csv(solution,file="kaggle/titanic/submission.csv",row.names = F)
        # train cforest using "party" package
                cf_model <- cforest(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Fare+Cabin+family+Fare2+group,
                                    data=train,
                                    controls = cforest_unbiased(ntree=4000,mtry=4))
                
                cf_pred <- predict(cf_model, test,OOB=T, type="response")
                solution <- data.frame(PassengerID=test$PassengerId, Survived=cf_pred)
                write.csv(solution,file="kaggle/titanic/submission.csv",row.names = F)
                
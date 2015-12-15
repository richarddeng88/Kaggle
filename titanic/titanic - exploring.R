library(caret);library(randomForest);library(rpart.plot)
train <- all_data

table(train$Pclass)
table(train$Sex)
cor(train$Pclass,train$Fare)
plot(train$Pclass, train$Fare)

exclude <- c("PassengerId", "Name","Cabin","ticket")
training <- train[,!names(train) %in% exclude]

# training[order(training$Fare),]
library(dplyr);
a <- arrange(training, desc(Fare))
training %>% group_by(Pclass) %>% summarize(mean(Survived))
b <- training %>% group_by(Age) %>% summarize(mean(Survived))

rpart.plot(tree_model)
varImpPlot(rf_model$finalModel)

# train RF model using "caret" package
    grid <- expand.grid(.mtry=c(2,3,4,5,6,7))
    rf_model <- train(as.factor(Survived)~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                      data=training,
                      importance=T,
                      tuneGrid =grid,
                      method="rf")
    rf_pred <- predict(rf_model, test)
    solution <- data.frame(PassengerID=test$PassengerId, Survived=rf_pred)
    write.csv(solution,file="kaggle/titanic/submission.csv",row.names = F)

# train QDA MODEL
    qda_model <- train(as.factor(Survived)~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                       data=training,
                       method="qda")
    qda_pred <- predict(qda_model, test)
    solution <- data.frame(PassengerID=test$PassengerId, Survived=qda_pred)
    write.csv(solution,file="kaggle/titanic/submission.csv",row.names = F)
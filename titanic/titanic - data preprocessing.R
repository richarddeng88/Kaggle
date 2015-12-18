library(rpart);library(rpart.plot);library(caret)
train <- read.csv("data/titanic/train.csv")
test <- read.csv("data/titanic/test.csv")
train$Survived <- as.factor(train$Survived)

# ====================== DEAL WITH NAs ====================================================================
    # we found out that there are 177 NAs in age variable and 2 in Embarked var. 
        head(train)
        sum(is.na(train))
        sapply(train, function(x) {sum(is.na(x))})

    # after working on "Embarked" variable, we can see this variable become 3 levels. 
        train$Embarked[c(62, 830)] <- "S"
        train$Embarked <- factor(train$Embarked)
        str(train)

    # FILL NAs in age var
    # We make a prediction of a passengers Age using the other variables and a decision tree model. 
    # This time we use method = "anova" since we are predicting a continuous variable.
        age_model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                               data = train[!is.na(train$Age),], 
                               method = "anova")

        rpart.plot(age_model)
        train$Age[is.na(train$Age)] <- predict(age_model, train[is.na(train$Age),])

    #we see there is no more NAs.
        sapply(train, function(x) {sum(is.na(x))})
        
    # let's work on the test set as there are NAs here
    # there is a missing value in "Fare". 
        library(dplyr)
        sapply(test, function(x) {sum(is.na(x))})
        mean_fare_group <- test %>% group_by(Pclass) %>% summarize(mean(Fare, na.rm=T))
        test[is.na(test$Fare),]$Fare <- mean_fare_group$`mean(Fare, na.rm = T)`[3]
        
    # there are a lot of NAs in "Age"
        test$Age[is.na(test$Age)] <- predict(age_model, test[is.na(test$Age),])

        
        
        
## =============================== FEATURE ENGINEERING =================================================
## 
        train$Cabin <- as.character(train$Cabin)
        for(i in 1:dim(train)[1]) {
            if (train$Cabin[i]== "") { train$Cabin[i] <- "None" } else {train$Cabin[i] <- substr(train$Cabin[i],1,1)}
        }

        test$Cabin <- as.character(test$Cabin)
        for(i in 1:dim(test)[1]) {
            if (test$Cabin[i]== "") { test$Cabin[i] <- "None" } else {test$Cabin[i] <- substr(test$Cabin[i],1,1)}
        }
        
        train[train$Cabin=="T",]$Cabin <- "None"
## CREATE DUMMY VARIABLES
        table(train$Cabin);table(test$Cabin)
        dummies <- dummyVars(Pclass~Cabin+Sex+Embarked, data=train)
        new <- data.frame(predict(dummies, newdata=train))
        new_test <- data.frame(predict(dummies, newdata=test))
        
##
        training <- cbind(train[,c(2,3,6,7,8,10)],new)
        testing <- cbind(test, new_test)

        # TRAIN RF MODEL AND EVALUATE
        grid <- expand.grid(.mtry=c(2,3,4,5,6,7))
        rf_model <- train(Survived~.,
                          data=training,
                          importance=T,
                          tuneGrid =grid,
                          method="rf")
        rf_pred <- predict(rf_model, test)
        solution <- data.frame(PassengerID=test$PassengerId, Survived=rf_pred)
        write.csv(solution,file="kaggle/titanic/submission.csv",row.names = F)
        
        

        # ensemble method - random forest 
        rf_model <- randomForest(Survived~.,
                                 data=training,
                                 importance=T,
                                 ntree=1000, mtry=5)
        summary(rf_model)
        importance(rf_model)
        varImpPlot(rf_model)
        rf_pred <- predict(rf_model, testing)
        solution <- data.frame(PassengerID=test$PassengerId, Survived=rf_pred)
        write.csv(solution,file="kaggle/titanic/submission.csv",row.names = F)
        
        
        
        
        
## devide data into one numeric and one categorical
numeric_train <- cbind(Survived=train$Survived, train[,sapply(train, is.numeric)])
category_train <- train[,!sapply(train, is.numeric)]
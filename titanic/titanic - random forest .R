train <- read.csv("data/titanic/train.csv")
test <- read.csv("data/titanic/test.csv")
all_data <- train

# DEAL WITH NAs
    # random forest model cannot deal with data that has missing values. 
    # we found out that there all 177 NAs in age variable
        head(all_data)
        sum(is.na(all_data))
        sapply(all_data, function(x) {sum(is.na(x))})

        # after working on "Embarked" variable, we can see this variable become 3 levels. 
        all_data$Embarked[c(62, 830)] <- "S"
        all_data$Embarked <- factor(all_data$Embarked)
        str(all_data)
    # How to fill in missing Age values?
    # We make a prediction of a passengers Age using the other variables and a decision tree model. 
    # This time we use method = "anova" since we are predicting a continuous variable.
        library(rpart);library(rpart.plot)
        age_model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                           data = all_data[!is.na(all_data$Age),], 
                           method = "anova")
        
        rpart.plot(age_model)
        all_data$Age[is.na(all_data$Age)] <- predict(age_model, all_data[is.na(all_data$Age),])

        #we see there is no more NAs.
        sapply(all_data, function(x) {sum(is.na(x))})
    
    # let's work on the test set as there are NAs here
        # there is a missing value in "Fare". 
        library(dplyr)
        sapply(test, function(x) {sum(is.na(x))})
        mean_fare_group <- test %>% group_by(Pclass) %>% summarize(mean(Fare, na.rm=T))
        test[is.na(test$Fare),]$Fare <- mean_fare_group$`mean(Fare, na.rm = T)`[3]
        
        # there are a lot of NAs in "Age"
        test$Age[is.na(test$Age)] <- predict(age_model, test[is.na(test$Age),])

# TRAIN RF MODEL
    library(randomForest)
    rf_model <- randomForest(as.factor(Survived)~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                             data=all_data,
                             importance=T,
                             ntree=1000)
    summary(rf_model)
    importance(rf_model)
    varImpPlot(rf_model)
    
    # prediction
    rf_pred <- predict(rf_model,test)
    
    #output for kaggle
    solution <- data.frame(PassengerID=test$PassengerId, Survived=rf_pred)
    write.csv(solution,file="kaggle/titanic/submission.csv",row.names = F)
    
# INCREASE PORFORMANCE
    
    
train <- read.csv("data/titanic/train.csv")
test <- read.csv("data/titanic/test.csv")

table(train$Pclass)
table(train$Sex)
cor(train$Pclass,train$Fare)

# tree model
    library(rpart);library(rattle);library(rpart.plot)
    
    tree_model <- rpart(Survived~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, method="class")
    rpart.plot(tree_model)
    fancyRpartPlot(tree_model)

    # prediction
    tree_pred <- predict(tree_model, test, "class")
    
    #output for kaggle
    solution <- data.frame(PassengerID=test$PassengerId, Survived=tree_pred)
    write.csv(solution,file="kaggle/titanic/submission.csv" ,row.names = F)
    

# tree model adjust cp and minsplit
    # the cp parameter determines when the splitting up of the decision tree stops.
    # the minsplit parameter monitors the amount of observations in a bucket.
    library(rpart);library(rattle);library(rpart.plot)
    tree_model <- rpart(Survived~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, 
                        control= rpart.control(minsplit = 50, cp=0),
                        method="class")
    rpart.plot(tree_model)
    fancyRpartPlot(tree_model)
    
        # prediction
        tree_pred <- predict(tree_model, test, "class")
        
        #output for kaggle
        solution <- data.frame(PassengerID=test$PassengerId, Survived=tree_pred)
        write.csv(solution,file="kaggle/titanic/submission_cp_minsplit.csv" ,row.names = F)
        
# feature engineering - add a new var : family_size
    # A valid assumption is that larger families need more time to get together on a sinking ship, and 
    # hence have less chance of surviving. 
    train_two <- train; test_two <- test
    train_two$family_size <- train_two$SibSp +train_two$Parch +1
    test_two$family_size <- test_two$SibSp + test_two$Parch +1
    
    # train the tree model 
    tree_model <- rpart(Survived~ Pclass+Sex+Age+Fare+Embarked+family_size, data=train_two, 
                        # control= rpart.control(minsplit = 50, cp=0),
                        method="class")
    rpart.plot(tree_model)
    fancyRpartPlot(tree_model)
    
        # prediction
        tree_pred <- predict(tree_model, test_two, "class")
        
        #output for kaggle
        solution <- data.frame(PassengerID=test$PassengerId, Survived=tree_pred)
        write.csv(solution,file="kaggle/titanic/submission_family_size.csv" ,row.names = F)
        
# 
    
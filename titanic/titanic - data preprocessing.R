library(rpart);library(rpart.plot);library(caret)
train <- read.csv("data/titanic/train.csv")
test <- read.csv("data/titanic/test.csv")
train$Survived <- as.factor(train$Survived)

# ============================= DEAL WITH NAs ====================================================================
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
## DEALING WITH AGE, transform it to "child", "adult" and "elder".
        train$group <- "adult"
        train$group[train$Age < 15] <- "child"
        train$group[train$Age > 50] <- "elder"
        #table
        table(train$Survived, train$group)

## CLASSIFYING FARE
        train$Fare2 <- "30+"
        train$Fare2[train$Fare < 10] <- "<10"
        train$Fare2[train$Fare >=10 & train$Fare <20 ] <- "20-30"
        train$Fare2[train$Fare >=20 & train$Fare < 30 ] <- "20-30"
        #table
        table(train$Survived, train$Fare2)

## ADD A NEW VARIABLE - family
        train$family <- train$SibSp + train$Parch
        
        
## DEALING WITH cABIN VARIABLE. trasform it to differ levels and make it dummirable. 
        train$Cabin <- as.character(train$Cabin)
        for(i in 1:dim(train)[1]) {
            if (train$Cabin[i]== "") { train$Cabin[i] <- "None" } else {train$Cabin[i] <- substr(train$Cabin[i],1,1)}
        }

        test$Cabin <- as.character(test$Cabin)
        for(i in 1:dim(test)[1]) {
            if (test$Cabin[i]== "") { test$Cabin[i] <- "None" } else {test$Cabin[i] <- substr(test$Cabin[i],1,1)}
        }
        
        train[train$Cabin=="T",]$Cabin <- "None"
 
        
        
        
        
               
                #################### CREATE DUMMY VARIABLES  - a new idea#######################
                        table(train$Cabin);table(test$Cabin)
                        dummies <- dummyVars(Pclass~Cabin+Sex+Embarked, data=train)
                        new <- data.frame(predict(dummies, newdata=train))
                        new_test <- data.frame(predict(dummies, newdata=test))
                        
                        ##
                        training <- cbind(train[,c(2,3,6,7,8,10)],new)
                        testing <- cbind(test, new_test)
                
                

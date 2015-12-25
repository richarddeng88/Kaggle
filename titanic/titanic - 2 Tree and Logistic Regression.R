#### WE HAVE MADE FEATURE ENGINEERED TRAIN DATA SET, NOW WE WANT TO SELECT, TRAIN AND TEST MODELS #########
library(rpart);library(rattle);library(rpart.plot);library(party)      
        
        ## REGULAR DECISION TREE - rpart
                library(rpart)
                tree_model <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Fare, 
                                    data = train,
                                    method="class",
                                    control = rpart.control(minisplit=0, minbucket = 0, maxdepth = 10))
        
                rpart.plot(tree_model)
                fancyRpartPlot(tree_model)

                # prediction
                tree_pred <- predict(tree_model, test,"class")
                
                #output for kaggle
                solution <- data.frame(PassengerID=test$PassengerId, Survived=tree_pred)
                write.csv(solution,file="kaggle/titanic/submission.csv" ,row.names = F)
        
        # CONDITIONAL INFERENCE TREE - ctree
                ctree_model <- ctree(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Fare+Cabin+family+Fare2+group, 
                                     data = train)
                plot(ctree_model)
                # prediction
                ctree_pred <- predict(ctree_model, test)
                #output for kaggle
                solution <- data.frame(PassengerID=test$PassengerId, Survived=ctree_pred)
                write.csv(solution,file="kaggle/titanic/submission.csv" ,row.names = F)
                
        # LOGISTIC REGRESSION
                logit_model <- glm(Survived~Pclass+Sex+group,
                                 #+SibSp+Parch+Embarked+Fare+Cabin+Fare2+group, 
                                 data = train,
                                 family = "binomial")
                logit_model
                summary(log_model)
                logit_pred <- predict(logit_model, train[1,c(3,5,13)],type="response")
                table()









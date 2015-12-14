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
varImpPlot(rf_model)

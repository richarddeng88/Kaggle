train <- read.csv("data/titanic/train.csv")
test <- read.csv("data/titanic/test.csv")

table(train$Pclass)
table(train$Sex)
cor(train$Pclass,train$Fare)


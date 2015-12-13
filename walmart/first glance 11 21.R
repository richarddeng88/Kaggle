train <- read.csv("data/walmart/train.csv")
test <- read.csv("data/walmart/test.csv")
sample <- read.csv("data/walmart/sample_submission.csv")

sapply(train, function(x){sum(is.na(x))}) ## check NAs
sapply(test, function(x){sum(is.na(x))}) ## check NAs
prop.table(table(train$TripType)) 
barplot(table(train$TripType))              # barplot response var
barplot(prop.table(table(train$TripType)))  # barplot response var
## classification problem : TREE, RF, QDA, 
## two variables have 4129 NAS; Response var looks like emblanced. 
# 














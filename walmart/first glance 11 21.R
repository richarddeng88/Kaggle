train <- read.csv("data/walmart/train.csv")
test <- read.csv("data/walmart/test.csv")
sample <- read.csv("data/walmart/sample_submission.csv")

sapply(train, function(x){sum(is.na(x))})
table(train$TripType)

## classification problem.   
# a lot of NAs, so few predictors and quite a lot of types. 
# catigoritize the departmentDescription use tree, CV, SVM, and 














library(xgboost)
data("agaricus.train")
data("agaricus.test")
## the data is about some features of mashroom, the binary response is that 
# whether the type of mashroom is posionous or not
train <- agaricus.train
test <- agaricus.test

# let's investigate the data first
str(train$data)
bst <- xgboost(data=train$data, label = train$label, nrounds = 2, objective= "binary:logistic")
# the result is the classification error on the training data
#[0]	train-error:0.000614
#[1]	train-error:0.001228

# if want to measure the classification by "area under the curve"
bst <- xgboost(data=train$data, label = train$label, nrounds = 2, 
               objective= "binary:logistic", eval_metric="auc")

pred <- predict(bst, test$data)

# we use cross-validation, add the nfold parameter
cv.res <- xgb.cv(data=train$data, nfold=5, label = train$label, nrounds = 2, 
               objective= "binary:logistic", eval_metric="auc")


## higgs example===========================================================
dtrain <- read.csv("data/higgs/training.csv", header = T)
dtest <- read.csv("data/higgs/test.csv", header = T)

dtrain[,33] = dtrain[,33] == "s"
label <- as.numeric(dtrain[,33])
data <- as.matrix(dtrain[,2:31])
weight <- as.numeric(dtrain[,32])

sumwpos <- sum(weight*(label==1))
sumwneg <- sum(weight*(label==0))

xgmat <- xgb.DMatrix(data = data , label=label, weight= weight, missing = -999)
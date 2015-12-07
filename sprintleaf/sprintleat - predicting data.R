library(class)
library(tree)
library(chemometrics)
library(ISLR)
library(glmnet)
library(MASS)
library(randomForest)
library(e1071)

# work on predicting the response 
#train_orgin <- read.csv("data/Springleaf/train.csv", head= T, nrows=10000)
## try to see which column has the most NAs
test<-train_origin
test <- subset(train_origin, select=-c(ID))
mising <- rep(0,dim(test)[2])
for (i in 1:dim(test)[2]) {
    mising[i] <- sum(is.na(test[,i]))
}
plot(1:dim(test)[2], mising)
name <- names(test)
mi <- data.frame(mising,name)
na <- mi[mi[1]>500,]
print(na)
#the result is :
#mising     name
                        #74    6985 VAR_0074
                        #205   9854 VAR_0205
                        #206   9878 VAR_0206
                        #207  10000 VAR_0207
                        #208   8699 VAR_0208
                        #209   9353 VAR_0209
                        #210   8699 VAR_0210
                        #211   8699 VAR_0211
                        #213  10000 VAR_0213
                        #838  10000 VAR_0840

traind = subset(test, select=-c(VAR_0074,VAR_0205,VAR_0206,VAR_0207,VAR_0208,VAR_0209
                                ,VAR_0210,VAR_0211,VAR_0213,VAR_0840))
# take out all char and factor type, make all NA = 0, the train with "target" response
traind = traind[, sapply(traind, is.numeric)]
traind[is.na(traind)] <- -1
#traind <- na.omit(traind)
sum(is.na(traind))

## figure the unique value of the column
col_ct = sapply(traind, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==2]))
target <- traind$target
traind = traind[, !names(train) %in% names(col_ct[col_ct==2])]
traind$target <- target

## split the train_d data set to creat a train and test dataset, both with the response
set.seed(1)
sam <- sample(1:nrow(traind), round(nrow(traind)*8/10))
test <- traind[-sam,]
train <- traind[sam,]

train_x <- train[,-dim(train)[2]]
test_x <- test[,-dim(train)[2]]
train_target <- train[,dim(train)[2]]
test_target <- test[,dim(train)[2]]

# firt of all, let's try use KNN to get a glance on how is the result, temp with k=5 
library(class)
knn_pred <- knn(train_x, test_x, train_target, k=5)
table(knn_pred, test_target)
mean(knn_pred==test_target)
# the result: 
#                       test_target
#            knn_pred    0    1
#                   0 2003  557              error rate:1- 0.7540453 =0.246
#                   1  127   94
# 

table(train_target)
n=30
cv_error =rep(0,n)

library(class)
for (i in 1:n){
    res <- knn.cv(train_x, train_target, k = n, p = 800)
    table(res)
    cv_test_error[i] <- mean(res!=test_target)
}

## 0. using naivebayes to predict
model <- naiveBayes(target ~ ., data = traind)
pred <- predict(model, traind[-sam,])
pred_prob <- predict(model, traind[-sam,], type = "raw")

save <- pred_prob
pred_prob[pred_prob>0.5]=1
pred_prob[pred_prob<=0.5]=0

mean(pred_prob[2]== test$target)
## the result is : 
##                test error rate : 0.7885




## 1. let's have a try using decision tree method 
# including all the variables.
train_tree <- train
train_tree$target <- factor(train_tree$target, levels = c(1,0))

test_target <- rep(0,nrow(train_tree))
tree_target <- tree(target~., data=train_tree, subset=sam)
summary(tree_target)

tree_pred <- predict(tree_target, test, type="class")
tree_pred[tree_pred >0.5]==1
tree_pred[tree_pred <=0.5]=0
#table(tree_pred,test[,1879])
#mean(tree_pred==test[,1879])
#the result is : 

####  using random forest method 
bag_bos <- randomForest(target~., data=train, subset=sam, mtry=20, importance=T)












##2. =========== get n rows data and run knn
set.seed(1)

train_x <- train[,-dim(train)[2]]
test_x <- test[,-dim(train)[2]]
train_target <- train[,dim(train)[2]]
test_target <- test[,dim(train)[2]]

# firt of all, let's try use KNN to get a glance on how is the result, temp with k=5 
library(class)
knn_pred <- knn(train_x, test_x, train_target, k=36)
table(knn_pred, test_target)
mean(knn_pred==test_target)
# the result: when k=5
#                       test_target
#            knn_pred    0    1
#                   0 1451  358              correct rate:0.758
#                   1  126   65
# 
# when k=15, correct rate:0.79
# when k=30, correct rate: 0.800625
# when k=33, correct rate: 0.79875
# when k=35, correct rate: 0.801875
# when k=37, correct rate: 0.80125
# when k=40, correct rate: 0.8
# when k=45, correct rate: 0.7975
# when k=50, correct rate: 0.79875
# when k=80, correct rate: 0.79862
# when k=120, correct rate: 0.796875

# now, let's work on train_x dataset using cross-validation to get the best k
grp=train_target
x=scale(train_x)
k=length(unique(grp))
dat=data.frame(grp,x)

library(chemometrics)
library(class)
set.seed(1)
sam1 <- sample(1:nrow(train_x), nrow(train_x)*8/10)
resknn=knnEval(train_x,grp,sam1,knnvec=seq(1,30,by=1),legpos="bottomright")

### use knn.cv to get the best K
table(train_target)
n=30
cv_error =rep(0,n)

library(class)
for (i in 1:n){
    res <- knn.cv(train_x, train_target, k = n, p = 800)
    table(res)
    cv_test_error[i] <- mean(res!=test_target)
}
plot(1:n, cv_test_rror, type="b", col="blue")


##===============================================================================================
## 3. lasso method, however it is a linear method.

x <- model.matrix(target~., data=traind)[,-1]
y <- traind$target
grid <- 10^seq(10,-2,length=100)
lasso_mod <- glmnet(x[sam,],y[sam], alpha=1,lambda=grid)
plot(lasso_mod)

### runing cross-validation to find the best lambda
set.seed(1)
cv_out <- cv.glmnet(x[sam,], y[sam], alpha=1)
plot(cv_out)
best <- cv_out$lambda.min
best
lasso_pred <- predict(lasso_mod, s=best, newx=x[-sam,])
mean((lasso_pred-y[-sam])^2)

lasso_pred[lasso_pred >0.5] ==1 
lasso_pred[lasso_pred <= 0.5]==0 
mean(lasso_pred==y[-sam])

##============================================================================
## 4.  using LDA
lda.fit=lda(target ~ ., data=train)
lda.fit
plot(lda.fit)

predict(lda.fit,stock_t)
















##@@@@@ try logistic regression @@@@@

glm.fit<- glm(target~VAR_1902+VAR_1823, data=df,family="binomial")
summary(glm.fit)

glm.probs <- predict(glm.fit, df, type="response")

glm.pred <- rep(0,dim(df)[1])
glm.pred[glm.probs > 0.5] <- c(1)
table(glm.pred, df$target)

mean(glm.pred==df$target)

## if i can load the test data set, here is the code using KNN method
library(class)
train_x <- df_train[,-1]
train_target <- df_train[,1934]

test_x <- df_test[,-1]
test_target <- df_test[,1934]




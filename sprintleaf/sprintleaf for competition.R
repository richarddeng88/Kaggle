t = 500
train <- read.csv("data/Springleaf/train.csv", head= T, nrows = t)
test <- read.csv("data/Springleaf/test.csv", head= T,nrows = t)
sample <- read.csv("data/Springleaf/sample_submission.csv")

#### getting and cleaning data


#df$target <- factor(df$target, levels=c(0,1)) # set target varialbe as factor, using KNN to predict
num <- sapply(train,is.numeric)
train <- train[,num]

num1 <- sapply(test,is.numeric)
test <- test[,num1]

m=30

train <- train[,c(2:m,dim(train)[2])]
train <- na.omit(train)

test <- test[,c(2:m)]
test <- na.omit(test)
##=========== get n rows data and run knn
set.seed(1)
n=nrow(train)
devide <- sample(n,(0.8*n))

train_x <- train[,-1]
train_target <- train[,dim(train)[2]]
test_x <- test
#test_target <- we need to predict the outcome

# cross validation to get the best k
grp=train$target
x=scale(train[,-1])
k=length(unique(grp))
dat=data.frame(grp,x)
n=nrow(train)

library(chemometrics)
library(class)
set.seed(1)
devide <- sample(n,round(0.8*n))
resknn=knnEval(x,grp,devide,knnvec=seq(1,30,by=1),legpos="bottomright")

## run K-nearest neighbour classification
library(class)
knn_pred <- knn(train_x, test_x, train_target, k=5)
test_target <- rep(0,dim(test_x)[1])
table(knn_pred, test_target)
mean(knn_pred==test_target)





##===============================================================================================






## split the df data half/half
set.seed(1)
dim(df)
devide <- sample(dim(df)[1],(dim(df)/2))
train_x <- df[devide,-1]
train_target <- df[devide,dim(df)[2]]
test_y <- df[-devide,-1]
test_target <- df[-devide,dim(df)[2]]

library(class)
knn.pred <- knn(train_x, test_x, train_target, k=30)
table(knn.pred, test_target)

##===================================================================================================








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




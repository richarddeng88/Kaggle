require(xgboost)
require(methods)
require(data.table)
require(magrittr)

train <- read.csv("data/otto/train.csv",header = T, stringsAsFactors = F)
test <- read.csv("data/otto/test.csv",header=TRUE, stringsAsFactors = F)

train <- train[,-1]
test <- test[,-1]

# save the names of last column
nameLastCol <- names(train)[ncol(train)]

#The classes are provided as character string in the 94th column called target. As you may know,
#XGBoost doesn't support anything else than numbers. So we will convert classes to integers. 
#Moreover, according to the documentation, it should start at 0.
train$target <- gsub("Class_","",train$target)
y <- as.integer(train$target)-1
table(train$target)

train <- train[,-dim(train)[2]]

for (i in 1:93){
        train[,i] <- as.numeric(train[,i])
        test[,i] <- as.numeric(test[,i])
}

trainMatrix <- as.matrix(train)
testMatrix <- as.matrix(test)

numberOfClasses <- max(y) + 1

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

# here let it be 5 round and 3 fold CV
        cv.nround <- 5
        cv.nfold <- 3
        
        bst.cv = xgb.cv(param=param, data = trainMatrix, label = y, 
                        nfold = cv.nfold, nrounds = cv.nround)

# train a 50 round model 
        nround = 150
        bst = xgboost(param=param, data = trainMatrix, label = y, nrounds=nround)

#
                model <- xgb.dump(bst, with.stats = T)
                model[1:10]
                # Get the feature real names
                names <- dimnames(trainMatrix)[[2]]        
                # Compute feature importance matrix
                importance_matrix <- xgb.importance(names, model = bst)
                # Nice graph
                xgb.plot.importance(importance_matrix[1:10,])
                # tree graph
                xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)
        
        # prediction
        xg_pred <- predict(bst, testMatrix)
        a <- matrix(xg_pred,dim(test)[1],9, byrow=T)
        submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)
        submission[,2:10] <- data.frame(a)
        write.csv(submission, file="submission.csv",row.names = F)
        apply(submission[,2:10],1,sum)
        # score 0.47328
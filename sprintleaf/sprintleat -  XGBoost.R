library(xgboost)
train <- read.csv("data/Springleaf/train.csv", head= T, nrows=20000)
test <- read.csv("data/Springleaf/test.csv", head= T,nrows=5000)

# there are 1934 variables in train
f_names <- names(train)[-ncol(train)]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in f_names) {
    if (class(train[,f])=="factor") {
        # levels <- unique(c(train[[f]], test[[f]])) #
        train[[f]] <- as.integer(train[,f])
        test[[f]]  <- as.integer(test[,f])
    }
}   

# after transfer all the factor type varialbe to numeric, it seems train_fac=0
train_numr = train[, sapply(train, is.numeric)]
train_char = train[, sapply(train, is.character)]
train_fac <- train[, sapply(train, is.factor)]

# see how many missing values for each variables
mising <- rep(0,dim(train)[2])
for (i in 1:dim(train)[2]) {
    mising[i] <- sum(is.na(train[,i]))
}
plot(1:dim(train)[2], mising)

# repalce NA with -1
cat("replacing missing values with -1\n")
train[is.na(train)] <- -1
test[is.na(test)]   <- -1
cat("the total NA number is",sum(is.na(train)),"end")
plot(1:dim(train)[2], mising)

# cat("sampling train to get around 8GB memory limitations\n")
# train <- train[sample(nrow(train), 40000),]
# gc()

cat("sampling train to get the memory limitations\n")
inTrain <- train[sample(nrow(train), 8000),]
h <- sample(nrow(inTrain), 8000)
train <-inTrain[h,] ## why to do this step to mix the order
gc()

cat("Making train and validation matrices\n")
dtrain <- xgb.DMatrix(data.matrix(train[,f_names]), label=train$target)

val<-inTrain[-h,]
gc()

dval <- xgb.DMatrix(data.matrix(val[,f_names]), label=val$target)

watchlist <- list(eval = dval, train = dtrain)

param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.001,
                max_depth           = 14,  # changed from default of 6
                subsample           = 0.6,
                colsample_bytree    = 0.6,
                eval_metric         = "auc"
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 50, # changed from 300
                    verbose             = 2, 
                    early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)










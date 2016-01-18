library(caret); 
## PREPROSESSING
        # GET RID OF ID
        # train <- train[,-1]

        # NAs
        sum(is.na(train));sum(is.na(test)) # indicate no missing value. 
        

        # zero covariate
        nsv <- nearZeroVar(train, saveMetrics = T) 
        train1 <- train[,-which(nsv[,4]==1)]  # train1 <- train[,-nearZeroVar(train)]
        #if_equal_0 <- sapply(train[, which(nsv[,4]==1)], function(x){sum(x)})
        if_equal_0 <- sapply(train[,1:54], function(x){mean(x)})
        which(if_equal_0<0.05)
        
        # REMOVE DESCREIPTIVE FEATURES. 
        
        # CHECK CORRELLATION. 
        M <- abs(cor(train[,2:93]))
        diag(M) <- 0 # every variable has correlation 1 with itself. So i don't need to care the diag(M)
        which(M>0.8, arr.ind = T)
        
        # STANDERDIZE THE DATA.
        
        # CREATE NEW FEATURES.
        
        # DATA SPLITING
        set.seed(1001)
        intrain <- createDataPartition(y=train$target, p=0.75, list = F)
        #intrain <- sample(dim(train)[1],9000)
        training <- train[intrain,]; validation <- train[-intrain,]
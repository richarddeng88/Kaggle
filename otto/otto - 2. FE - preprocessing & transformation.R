library(caret); 
## PREPROSESSING
        # NAs
        sum(is.na(train)) # indicate no missing value. 
        
        # zero covariate
        nsv <- nearZeroVar(train, saveMetrics = T) 
        train1 <- train[,-which(nsv[,4]==1)]  # train1 <- train[,-nearZeroVar(train)]
      
        # REMOVE DESCREIPTIVE FEATURES. 
        
        # CHECK CORRELLATION
        
        
        # CREATE NEW FEATURES.
        
        # DATA SPLITING
        set.seed(1001)
        intrain <- createDataPartition(y=train$target, p=0.75, list = F)
        #intrain <- sample(dim(train)[1],9000)
        training <- train[intrain,]; validation <- train[-intrain,]
        
        
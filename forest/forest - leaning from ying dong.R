library(ggplot2);library(caret);library(randomForest)
train <- read.csv("data/forest/train.csv")
test <- read.csv("data/forest/test.csv")
sample <- read.csv("data/forest/sampleSubmission.csv")

train$Cover_Type <- as.factor(train$Cover_Type)
# 1. DATA EXPLORATION
        # binary rows equal to 1 respectivly,stating that there is no errors.
        wilderness <- train[,12:15]; which(apply(wilderness, 1, sum)!=1)
        soil <- train[,16:55];which(apply(wilderness, 1, sum)!=1)
        
        # for numeric predictors, we have to check if the zeros contain missing values.
        numeric <- train[,2:11]
        sum(is.na(numeric))
        sapply(numeric, mean)
        sapply(numeric, function(x){sum(x==0)})
        
        ggplot(data=train, mapping = aes(x=Hillshade_Noon, y=Hillshade_3pm,color=Cover_Type))+geom_point()
        ggplot(data=train, mapping = aes(x=Hillshade_9am, y=Hillshade_3pm,color=Cover_Type))+geom_point()

        
        # without soil and wilderness, we see random forest importance of each predictor. elevatio is the 
        # most important factor. and latter barchart indicate that elevation alone can seperate 4,5,7 very well. 
        rf <- randomForest(Cover_Type~., 
                           data= train[,c(2:11,56)], 
                           ntree=100, 
                           # mtry=53,
                           importance=TRUE)
        varImpPlot(rf) 
        ggplot(data=train, mapping= aes(x= Elevation, fill=Cover_Type)) + geom_bar(binwidth=30)
        ggplot(data=train, mapping= aes(x= Hillshade_Noon, fill=Cover_Type)) + geom_bar(binwidth=5)
        
# 2. DATA PREPROCESSING
        # zero covariate soil 7 and 15 there is no value , i guess useless predictor.
        # zero covariate
        nsv <- nearZeroVar(train, saveMetrics = T) 
        train1 <- train[,-which(nsv[,4]==1)]  # train1 <- train[,-nearZeroVar(train)]
        #if_equal_0 <- sapply(train[, which(nsv[,4]==1)], function(x){sum(x)})
        if_equal_0 <- sapply(train[,1:54], function(x){mean(x)})
        which(if_equal_0<0.05)
        
        
        
        
# 3. filling in the missing value. 
        
        
# 4. construc new features.
        train$distance_to_hydrolody <- (train$Horizontal_Distance_To_Hydrology^2+train$Vertical_Distance_To_Hydrology^2)^0.5
        train$ele_minus_vdh <- train$Elevation - train$Vertical_Distance_To_Hydrology
        train$ele_plus_vdh <- train$Elevation + train$Vertical_Distance_To_Hydrology
        train$hydro_plus_fire <- train$Horizontal_Distance_To_Hydrology + train$Horizontal_Distance_To_Fire_Points
        train$hydro_minus_fire <- train$Horizontal_Distance_To_Hydrology - train$Horizontal_Distance_To_Fire_Points
        train$hydro_plus_road <- train$Horizontal_Distance_To_Hydrology + train$Horizontal_Distance_To_Roadways
        train$hydro_minus_road <- train$Horizontal_Distance_To_Hydrology - train$Horizontal_Distance_To_Roadways
        train$fire_plus_road <- train$Horizontal_Distance_To_Fire_Points +train$Horizontal_Distance_To_Roadways
        train$fire_minus_road <- train$Horizontal_Distance_To_Hydrology - train$Horizontal_Distance_To_Roadways
        
        test$distance_to_hydrolody <- (test$Horizontal_Distance_To_Hydrology^2+test$Vertical_Distance_To_Hydrology^2)^0.5
        test$ele_minus_vdh <- test$Elevation - test$Vertical_Distance_To_Hydrology
        test$ele_plus_vdh <- test$Elevation + test$Vertical_Distance_To_Hydrology
        test$hydro_plus_fire <- test$Horizontal_Distance_To_Hydrology + test$Horizontal_Distance_To_Fire_Points
        test$hydro_minus_fire <- test$Horizontal_Distance_To_Hydrology - test$Horizontal_Distance_To_Fire_Points
        test$hydro_plus_road <- test$Horizontal_Distance_To_Hydrology + test$Horizontal_Distance_To_Roadways
        test$hydro_minus_road <- test$Horizontal_Distance_To_Hydrology - test$Horizontal_Distance_To_Roadways
        test$fire_plus_road <- test$Horizontal_Distance_To_Fire_Points +test$Horizontal_Distance_To_Roadways
        test$fire_minus_road <- test$Horizontal_Distance_To_Hydrology - test$Horizontal_Distance_To_Roadways
        
        rf_model <- randomForest(Cover_Type~., 
                                 data= train[,c(-1)], 
                                 ntree=500,
                                 importance=TRUE)
        varImpPlot(rf_model)
        rf_pred <- predict(rf_model,test[,-1])
        submission <- data.frame(Id=test$Id, Cover_Type=rf_pred)
        write.csv(submission, file="submission.csv",row.names = F)
        # score: 0.76931   no. 430
        
        
        # zero covariate
        nsv <- nearZeroVar(train, saveMetrics = T) 
        train1 <- train[,-which(nsv[,4]==1)]  # train1 <- train[,-nearZeroVar(train)]
        rf_model <- randomForest(Cover_Type~., 
                                 data= train1[,c(-1)], 
                                 ntree=500,
                                 importance=TRUE)
        varImpPlot(rf_model)
        rf_pred <- predict(rf_model,test[,-1])
        submission <- data.frame(Id=test$Id, Cover_Type=rf_pred)
        write.csv(submission, file="submission.csv",row.names = F)
        # score: 0.76930 
library(ggplot2)
library(lubridate)
library(randomForest)
library(dplyr)
train <- read.csv("data/bike/train.csv")
test <- read.csv("data/bike/test.csv")
sample <- read.csv("data/bike/sample.csv")

# 1st glance
        # HEATMAP - THIS ONE LOOKS GOOD
        library(lattice)
        myheatmap <- function(myMAT,low,high,uselabel){
                colorFun <- colorRampPalette(c("blue","white","red")) 
                
                b <- boxplot(myMAT, plot = FALSE)
                thr <- c(low,high)
                colbins <- 100
                step <- abs(thr[2] - thr[1])/50
                
                myAT <- seq(thr[1], thr[2], step)
                
                myCOLregions <- colorFun(length(myAT))
                
                levelplot(myMAT, at = myAT, col.regions = myCOLregions, 
                          scales=list(x=list(rot=90)), labels=uselabel)
        }
        
        correlation <- cor(train[-1])
        myheatmap(correlation,-1,1,F)

        # transfor date to new variables. 
        train$datetime <- ymd_hms(train$datetime)
        train$weekday <- as.factor(weekdays(train$datetime))
        train$year <- as.factor(year(train$datetime))
        train$month <- as.factor(month(train$datetime))
        train$hour <- as.factor(hour(train$datetime))
        train <- train[,-1]
        str(train)
        
        test$datetime <- ymd_hms(test$datetime)
        test$weekday <- as.factor(weekdays(test$datetime))
        test$year <- as.factor(year(test$datetime))
        test$month <- as.factor(month(test$datetime))
        test$hour <- as.factor(hour(test$datetime))
        str(test)
        
        # ploting
        a <- train %>% group_by(weekday) %>% summarize(mean=mean(count))
        ggplot(data=a, mapping = aes(x=weekday, y=mean)) + geom_line()
        plot(train$weekday, train$count)
        
        b1 <- train %>% group_by(hour) %>% summarize(mean=mean(count), type="count")
        b2 <- train %>% group_by(hour) %>% summarize(mean=mean(registered), type="registered")
        b3 <- train %>% group_by(hour) %>% summarize(mean=mean(casual), type="casual")
        b <- rbind(b1,b2,b3)
        ggplot(data=b, mapping = aes(x=hour, y=mean,color=type)) + geom_line()+labs(y="number of users", title="mean values")

# benchmark -1
        rf_model <- randomForest(count~.,
                                 data = train[,c(-9,-10)],
                                 ntree=500)
        varImpPlot(rf_model)
        importance(rf_model)
        rf_pred <- predict(rf_model, test)
        submission <- data.frame(datetime=test$datetime, count=rf_pred)
        write.csv(submission, file = "submission.csv", row.names=FALSE)
        # score is 0.64535. it seems that i still need to consider the time factor. 

# benchmark -2 
extractFeatures <- function(data) {
    features <- c("season",
                  "holiday",
                  "workingday",
                  "weather",
                  "temp",
                  "atemp",
                  "humidity",
                  "windspeed",
                  "hour")
    data$hour <- hour(ymd_hms(data$datetime))
    return(data[,features])
}

    trainFea <- extractFeatures(train)
    testFea  <- extractFeatures(test)

    train$hour <- hour(ymd_hms(train$datetime))
    test$hour <- hour(ymd_hms(test$datetime))
    
    submission <- data.frame(datetime=test$datetime, count=NA)
    
    for (i_year in unique(year(ymd_hms(test$datetime)))) {
        for (i_month in unique(month(ymd_hms(test$datetime)))) {
            cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
            testLocs   <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
            testSubset <- test[testLocs,]
            trainLocs  <- ymd_hms(train$datetime) <= min(ymd_hms(testSubset$datetime))
            rf <- randomForest(extractFeatures(train[trainLocs,]), train[trainLocs,"count"], ntree=100)
            submission[testLocs, "count"] <- predict(rf, extractFeatures(testSubset))
        }
    }
    
    write.csv(submission, file = "submission.csv", row.names=FALSE)
    # score 0.59522
    
    rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
    imp <- importance(rf, type=1)
    featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
    
    p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
        geom_bar(stat="identity", fill="#53cfff") +
        coord_flip() + 
        theme_light(base_size=20) +
        xlab("Importance") +
        ylab("") + 
        ggtitle("Random Forest Feature Importance\n") +
        theme(plot.title=element_text(size=18))
    
    ggsave("2_feature_importance.png", p)
    
# benchmark -3
    
    
    
    
    
    
    
    
    
    
    
    
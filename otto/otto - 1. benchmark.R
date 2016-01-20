library(ggplot2);library(caret);library(randomForest)
train <- read.csv("data/otto/train.csv")
test <- read.csv("data/otto/test.csv")
sample <- read.csv("data/otto/sampleSubmission.csv")

# 1st glance
# distribution of train target
table(train$target)
ggplot(train, aes(x=target)) + geom_bar(fill="lightblue",color="black")+labs(title="distribution of product categories")

ggplot(train, aes(x=feat_34,y=feat_48, color=target)) + geom_point()

# NAs
    sapply(train, function(x){sum(is.na(x))})
    sapply(test, function(x){sum(is.na(x))})

submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)

# TRAIN RF MODEL  -  BENCHMARKE
        rf <- randomForest(target~., 
                           data= train[,-1], 
                           ntree=100, 
                           importance=TRUE)
        varImpPlot(rf)
        importance(rf)
        imp <- importance(rf, type=1)
        featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
        
        # PREDICTION AND OUTPUT
        submission[,2:10] <- (predict(rf, test[,-1], type="prob")+0.01)/1.09
        #rf_prob <- predict(rf, test[,-1], type="prob")
        rf_pred <- predict(rf,test[,-1])
        # submission[,2:10] <- predict(rf, test[,-1], type="prob")
        write.csv(submission, file="submission.csv",row.names = F)
        # score 0.62142

# ploting the important features
        p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
                geom_bar(stat="identity", fill="#53cfff") +
                coord_flip() + 
                theme_light(base_size=20) +
                xlab("Importance") +
                ylab("") + 
                ggtitle("Random Forest Feature Importance\n") +
                theme(plot.title=element_text(size=18))

        # ggsave("feature_importance.png", p, height=20, width=8, units="in")
        
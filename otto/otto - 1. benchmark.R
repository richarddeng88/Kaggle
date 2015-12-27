library(ggplot2);library(caret);library(randomForest)
train <- read.csv("data/otto/train.csv")
test <- read.csv("data/otto/test.csv")
sample <- read.csv("data/otto/sampleSubmission.csv")

# 1st glance
prop.table(table(train$target))

submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)

# TRAIN RF MODEL  -  BENCHMARKE
        rf <- randomForest(train$target, train[,c(-1,-95)], ntree=500, importance=TRUE)
        varImpPlot(rf)
        importance(rf)
        imp <- importance(rf, type=1)
        featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
        # PREDICTION AND OUTPUT
        submission[,2:10] <- (predict(rf, test[,-1], type="prob")+0.01)/1.09
        rf_prob <- predict(rf, test[,-1], type="prob")
        # submission[,2:10] <- predict(rf, test[,-1], type="prob")
        write.csv(submission, file="submission.csv",row.names = F)

# ploting the important features
        ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
                geom_bar(stat="identity", fill="#53cfff") +
                coord_flip() + 
                theme_light(base_size=20) +
                xlab("Importance") +
                ylab("") + 
                ggtitle("Random Forest Feature Importance\n") +
                theme(plot.title=element_text(size=18))

        ggsave("feature_importance.png", p, height=20, width=8, units="in")
        
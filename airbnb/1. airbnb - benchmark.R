library(rpart);library(rattle);library(rpart.plot);library(dplyr);library(randomForest)
train <- read.csv("data/airbnb/train_users_2.csv",na.strings = "NA")
test <- read.csv("data/airbnb/test_users.csv")
countries <- read.csv("data/airbnb/countries.csv")
age <- read.csv("data/airbnb/age_gender_bkts.csv")
sessions <- read.csv("data/airbnb/sessions.csv")
sample <- read.csv("data/airbnb/sample_submission_NDF.csv")


# 1st glance, exploratory analysis
plot(table(train$language))
unique(train$country_destination)
prop.table(table(train$country_destination))
plot(table(train$country_destination))

ggplot(data=train, mapping = aes(x=date_account_created))

# NAs
sum(is.na(train)); sapply(train, function(x){sum(is.na(x))})
sum(is.na(test)) ; sapply(test, function(x){sum(is.na(x))})

# calculate the elipse time
search_time <- sessions %>% group_by(user_id) %>% summarize(total=mean(secs_elapsed, na.rm=T))
a <- sessions[sessions$user_id=="",]
x <-dim(train)[1]
x <- 100
for (i in 1:x) {
        if (train$id[i] %in% search_time$user_id) {
                train$search_time <- search_time$total
        } else train$search_time <- 0
}

# merger data
merge(x=train, y=search_time, by.x=id, by.y=user_id)

# TRAIN RF MODEL  -  BENCHMARKE
        bench_train <- train[,c(4,6,7,9,10,11,12,16)]
        bench_test <- test[,c(4,6,7,9,10,11,12,16)]

        rf_model <- randomForest(country_destination~.,
                                 data=bench_train,
                                 ntree=500,
                                 importance = T)
        
        varImpPlot(rf_model)
        rf_pred <- predict(rf_model, test)
        



        sum(is.na(bench_train));sum(is.na(bench_test))
        tree_benchmark <- rpart(country_destination~., data=bench_train)
        
        rpart.plot(tree_benchmark)

        tree_pred <- predict(tree_benchmark)

        library(caret)
        ctrl <- trainControl(method = "none", number = 10) 
        lda_model <- train(country_destination~.,
                           data = bench_train,
                           method="lda",
                           trControl=ctrl)
        lda_pred <- predict(lda_model, bench_test)
        
        
        confusionMatrix(lda_pred, testing$classe)


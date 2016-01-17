library(rpart);library(rattle);library(rpart.plot);library(dplyr)
train <- read.csv("data/airbnb/train_users_2.csv")
test <- read.csv("data/airbnb/test_users.csv")
countries <- read.csv("data/airbnb/countries.csv")
age <- read.csv("data/airbnb/age_gender_bkts.csv")
sessions <- read.csv("data/airbnb/sessions.csv")
sample <- read.csv("data/airbnb/sample_submission_NDF.csv")


# 1st glance
plot(table(train$language))
unique(train$country_destination)
prop.table(table(train$country_destination))
plot(table(train$country_destination))

sum(is.na(train))
sapply(train, function(x){sum(is.na(x))})
sum(is.na(test))
sapply(test, function(x){sum(is.na(x))})

# calculate the elipse time
search_time <- sessions %>% group_by(user_id) %>% summarize(total=mean(secs_elapsed, na.rm=T))


# TRAIN RF MODEL  -  BENCHMARKE
        bench_train <- train[,c(9,10,11,12,16)]
        bench_test <- test[,c(9,10,11,12)]
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


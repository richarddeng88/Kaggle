library(rpart)
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
# TRAIN RF MODEL  -  BENCHMARKE
        bench_train <- train[,c(-1,-2,-4,-6)]
        bench_test <- test[,c(-1,-2,-4,-6)]
        sum(is.na(bench_train));sum(is.na(bench_test))
        tree_benchmark <- rpart(country_destination~., data=bench_train)






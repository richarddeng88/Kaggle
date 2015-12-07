train <- read.csv("data/rossman/train.csv")
test <- read.csv("data/rossman/test.csv")
sample <- read.csv("data/rossman/sample_submission.csv")
store <- read.csv("data/rossman/store.csv")

names(train) <- c("store","wday","date","sales","customers","open","promo","state_holiday","school_holiday")
names(test)  <- c("id","store","wday","date","open","promo","state_holiday","school_holiday")
names(store) <- c("store", "type", "assortment","distance","since_m","since_y","promo2","p_since_m","p_since_y","interval")
# no missing values. 

train$date <- as.Date(train$date)
test$date <- as.Date(test$date)

train$month<- as.POSIXlt(train$date)$mon+1
train$day<- as.POSIXlt(train$date)$mday
train$year<- as.POSIXlt(train$date)$year+1900

test$month<- as.POSIXlt(test$date)$mon+1
test$day<- as.POSIXlt(test$date)$mday
test$year<- as.POSIXlt(test$date)$year

train$state_holiday <- ifelse(train$state_holiday != "0", 1,0)

## take out the "sales" to prepare for k-means
library(dplyr)
k_train <- select(train, -store,-sales,-date,-open,-month,-day,-year)

k_train <- as.data.frame(lapply(k_train, scale))

set.seed(2345)
clusters <- kmeans(k_train, 4)

table(store$type)
clusters$size
clusters$centers

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
[7] "size"         "iter"         "ifault" 




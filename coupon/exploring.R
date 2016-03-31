library(ggplot2);library(caret);library(randomForest)

area_train <- read.csv("data/coupon/coupon_area_train.csv")
detail_train <- read.csv("data/coupon/coupon_detail_train.csv")
list_train <- read.csv("data/coupon/coupon_list_train.csv")
visit_train <- read.csv("data/coupon/coupon_visit_train.csv")
location <- read.csv("data/coupon/prefecture_locations.csv")
user_list <- read.csv("data/coupon/user_list.csv")
        
area_test <- read.csv("data/coupon/coupon_area_test.csv")
sample <- read.csv("data/forest/sampleSubmission.csv")
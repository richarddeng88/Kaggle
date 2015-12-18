train <- read.csv("data/walmart/train.csv")
test <- read.csv("data/walmart/test.csv")
sample <- read.csv("data/walmart/sample_submission.csv")
library(caret);library(dplyr)
# TripType - a categorical id representing the type of shopping trip the customer made. TripType_999 is an "other" category.
# VisitNumber - an id corresponding to a single trip by a single customer
# Weekday - the weekday of the trip
# Upc - the UPC number of the product purchased
# ScanCount - the number of the given item that was purchased. A negative value indicates a product return.
# DepartmentDescription - a high-level description of the item's department
# FinelineNumber - a more refined category for each of the products, created by Walmart

# attributes of predictors
    # check how many missing values.
    sapply(train, function(x){sum(is.na(x))})
    sapply(test, function(x){sum(is.na(x))}) 
    table(train$ScanCount)  # maybe a weekly grocery trep may buy more items. 

    ## check length of unique value of each variable
    sapply(train, function(x) length(unique(x))) 
    sapply(test, function(x) length(unique(x))) 
    
# atrributes of response
    prop.table(table(train$TripType)) 
    barplot(prop.table(table(train$TripType)))  # barplot response var, 39,40 account 30%
    
## classification problem : TREE, RF, QDA, 
## two variables have 4129 NAS; Response var looks like emblanced. 

## feature engineering
    # a new variable indication how many items bought a vist 
    library(dplyr)
    a <- unique(train$VisitNumber)
    for (i in a[63800:95764]){
        train[train$VisitNumber==i,]$items <- dim(train[train$VisitNumber==i,])[1]
    }
    
    b <- unique(test$VisitNumber)
    for (i in b){
        test[test$VisitNumber==i,]$items <- dim(test[test$VisitNumber==i,])[1]
    }

    # it seems ucp and fineNumber are unrelevant, i take them out and solve NAs problem. 
    clean_train <- select(train,-VisitNumber, -Upc, -FinelineNumber) 
    clean_test <- select(test,-VisitNumber, -Upc, -FinelineNumber) 
    sapply(clean_train, function(x) {sum(is.na(x))}) 
    sapply(clean_test, function(x) {sum(is.na(x))})
    
    clean_train$TripType <- as.factor(clean_train$TripType)
    
    # detect relationship between items bought and trip type 
    length(unique(clean_train$items))
    cor(train$TripType,train$items)
    test_train<- train
    test_train$TripType[test_train$TripType==999]<-45
    #[1]   3   4   5   6   7   8   9  12  14  15  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32
    #[26]  33  34  35  36  37  38  39  40  41  42  43  44 999
    plot(test_train$TripType, test_train$items, pch=20)
    e <- test_train %>% group_by(TripType) %>% summarize(mean=mean(items))
    plot(e$TripType, e$mean)
    
# DATA SPLIT
library(caret)
intrain <- createDataPartition(clean_train$TripType, p=0.8, list=F)
training <- clean_train[intrain,]; testing <- clean_train[-intrain,]

# TRAIN MODEL
    library(randomForest)
    rf_model <- randomForest(TripType~., training, mtry=4)
    
    library(rpart)
    tree_model <- rpart(TripType~.items+DepartmentDescription+Weekday, clean_train)
    tree_pred <- predict(tree_model, testing)
    mean(tree_pred== testing$TripType)






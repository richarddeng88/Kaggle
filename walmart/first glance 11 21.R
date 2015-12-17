train <- read.csv("data/walmart/train.csv")
test <- read.csv("data/walmart/test.csv")
sample <- read.csv("data/walmart/sample_submission.csv")

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
    for (i in a){
        train[train$VisitNumber==a,]$items <- dim(train[train$VisitNumber==a,])[1]
    }


    # it seems ucp and fineNumber are unrelevant, i take them out and solve NAs problem. 
    train <- select(train, -Ucp, -FinelineNumber) 
    test <- select(test, -Ucp, -FinelineNumber) 
    
# DATA SPLIT
library(caret)
intrain <- createDataPartition(train$TripType, p=0.8, list=F)
training <- train[intrain,]; testing <- train[-intrain,]

# TRAIN MODEL









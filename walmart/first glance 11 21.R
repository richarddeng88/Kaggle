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
    sapply(train, function(x){sum(is.na(x))}) ## check NAs
    sapply(test, function(x){sum(is.na(x))}) ## check NAs
    table(train$ScanCount)  # maybe a weekly grocery trep may buy more items. 

    #
    sapply(train, function(x) length(unique(x))) # check length of unique value of each variable
    sapply(test, function(x) length(unique(x))) # check length of unique value of each variable
# atrributes of response
    prop.table(table(train$TripType)) 
    barplot(table(train$TripType))              # barplot response var
    barplot(prop.table(table(train$TripType)))  # barplot response var
    
## classification problem : TREE, RF, QDA, 
## two variables have 4129 NAS; Response var looks like emblanced. 
# 














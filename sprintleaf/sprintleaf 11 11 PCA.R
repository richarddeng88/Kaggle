library(caret)
##DATA READING
train_d <- read.csv("data/Springleaf/train.csv", head= T)

## DATA CLEANING
# only keep the numeric type data and take out the ID variable
train_d <- train_d[,sapply(train_d,is.numeric)]
train_d <- train_d[,-1]

# see how many NA each column
na <- sapply(train_d, function(x){sum(is.na(x))})
ta <- na[na>1000]
train_d <- subset(train_d, select = !(names(train_d) %in% names(ta)))

# making all NA = 1 OR = mean
train_d[is.na(train_d)]  <- 1 
sum(is.na(train_d))

library(ggplot2);library(caret);library(randomForest)
train <- read.csv("data/expedia/train.csv",stringsAsFactors = F,nrows = 1000)
test <- read.csv("data/expedia/test.csv", stringsAsFactors = F)
destinations <- read.csv("data/expedia/destinations.csv", stringsAsFactors = F)
sample <- read.csv("data/expedia/sample_submission.csv", stringsAsFactors = F)

# I don't the number of rows of train data. here I try to count it using R.utils package. 
library(R.utils)
## get the rowcount
row_count <- countLines("data/expedia/train.csv")
cat("Row count : ", row_count[1], "; Predictor column count : ", ncol(train))

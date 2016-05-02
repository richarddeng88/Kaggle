train <- read.csv("data/santander/train.csv",stringsAsFactors = F)
test <- read.csv("data/santander/test.csv", stringsAsFactors = F)
sample <- read.csv("data/santander/sample_submission.csv", stringsAsFactors = F)

sum(is.na(train))
sum(is.na(test))

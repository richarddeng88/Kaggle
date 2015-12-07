library(caret)
##DATA READING
train_O <- read.csv("data/Springleaf/train.csv", head= T)

## DATA CLEANING
    # only keep the numeric type data and take out the ID variable
    train_d <- train_O[,sapply(train_d,is.numeric)]
    train_d <- train_d[,-1]


        # see how many NA each column
    na <- sapply(train_d, function(x){sum(is.na(x))})
    ta <- na[na>1000]
    train_d <- subset(train_d, select = !(names(train_d) %in% names(ta)))

        # making all NA = 1 OR = mean
    train_d[is.na(train_d)]  <- 1 
    sum(is.na(train_d))

        # see the length of unique value for each column
    #length <- sapply(train_d, function(x) length(unique(x)))
    #length[length<5]

    #To normalize these features, we need to create a normalize() function in R.
    # train_1 <- as.data.frame(scale(train_d)[-1871])
    
    # normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
    # train_1 <- as.data.frame(lapply(train_d[1:1870], normalize))
    train_d$target<- factor(train_d$target,levels = c(0,1), labels = c("yes","no"))
    # train_1 <- cbind(train_1,target=train_d$target)

## DATA SPLITTING
invalid <- createDataPartition(y=train_d$target, p=0.1, list=F)
validation <- train_d[-invalid,]

intrain <- createDataPartition(train_d[invalid,]$target, p=0.75,list = F)
training <- train_d[invalid,][intrain,-1871]
testing <- train_d[invalid,][-intrain,-1871]
training_target <- train_d[invalid,][intrain,1871]
testing_target <- train_d[invalid,][-intrain,1871]

library(class)
pred <- knn(train=training, test = testing, training_target, k=50) # k = sqrt(80000)





















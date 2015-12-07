train_origin <- read.csv("data/Springleaf/train.csv", head= T, nrows=10000)
test_origin <- read.csv("data/Springleaf/test.csv", head= F,skip = 140001, nrows=5232)
sample <- read.csv("data/Springleaf/sample.csv")

## try to see which column has the most NAs
names(test_origin) <- names(train_origin[,-dim(train_origin)[2]])

process <- subset(train_origin, select=-c(ID))
mising <- rep(0,dim(process)[2])
for (i in 1:dim(process)[2]) {
    mising[i] <- sum(is.na(process[,i]))
}
plot(1:dim(process)[2], mising)
mi <- data.frame(mising,names(process))
na <- mi[mi[1]>14000*0.05,]
print(na)
#the result is :
#mising     name
#74    6985 VAR_0074
#205   9854 VAR_0205
#206   9878 VAR_0206
#207  10000 VAR_0207
#208   8699 VAR_0208
#209   9353 VAR_0209
#210   8699 VAR_0210
#211   8699 VAR_0211
#212   1179 VAR_0212
#213  10000 VAR_0213
#838  14000 VAR_0840

traind = subset(process, select=-c(VAR_0074,VAR_0205,VAR_0206,VAR_0207,VAR_0208,VAR_0209
                                   ,VAR_0210,VAR_0211,VAR_0212,VAR_0213,VAR_0840))


# take out all char and factor type, make all NA = 0, the train with "target" response
traind = traind[, sapply(traind, is.numeric)]
traind[is.na(traind)]  <- -1
cat("the total NA number is",sum(is.na(traind)),"end")

#======================
#this time we don't split the data, just run KNN on train and test dataset
process <- subset(test_origin, select=-c(ID))
mising <- rep(0,dim(process)[2])
for (i in 1:dim(process)[2]) {
    mising[i] <- sum(is.na(process[,i]))
}
plot(1:dim(process)[2], mising)
mi <- data.frame(mising,names(process))
na <- mi[mi[1]>14000*0.05,]
print(na)
#the result is the same as the train data
#mising     name
#74    6985 VAR_0074
#205   9854 VAR_0205
#206   9878 VAR_0206
#207  10000 VAR_0207
#208   8699 VAR_0208
#209   9353 VAR_0209
#210   8699 VAR_0210
#211   8699 VAR_0211
#212   1179 VAR_0212
#213  10000 VAR_0213
#838  14000 VAR_0840

testd = subset(process, select=-c(VAR_0074,VAR_0205,VAR_0206,VAR_0207,VAR_0208,VAR_0209
                                  ,VAR_0210,VAR_0211,VAR_0212,VAR_0213,VAR_0840))


# take out all char and factor type, make all NA = 0, the train with "target" response
testd = testd[, sapply(testd, is.numeric)] ## testd is without "target" column
testd[is.na(testd)] <- -1
cat("the total NA number is",sum(is.na(testd)),"end")

train_x <- traind[,-dim(traind)[2]]
test_x <- testd
train_target <- traind[,dim(traind)[2]]
## test_target 

library(class)
knn_pred <- knn(train_x, test_x, train_target, k=35,prob = T)
# when k=15, correct rate:0.79
# when k=30, correct rate: 0.800625
# when k=33, correct rate: 0.79875
# when k=35, correct rate: 0.801875
# when k=37, correct rate: 0.80125
# when k=40, correct rate: 0.8
# when k=45, correct rate: 0.7975
# when k=50, correct rate: 0.79875
# when k=80, correct rate: 0.79862
# when k=120, correct rate: 0.796875

submission <- data.frame(test_origin$ID,attributes(knn_pred)[3])
names(submission) <- c("ID", "target")
write.csv(submission, "data/knn_submission28.csv",row.names=FALSE)
a28 <- read.csv("data/knn_submission28.csv", head= T)
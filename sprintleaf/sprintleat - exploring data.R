library(caret)
library(readr)
library(xgboost)
library(ggplot2)
library(R.utils)
library(gridExtra)
library(lubridate)
library(data.table)
library(Matrix)
require(plyr)
require(Hmisc)
library(maps)
library(maptools)
library(sp)
library(corrplot)

time.1 <- Sys.time()
format(time.1, "%d-%m-%Y--%H:%M:%S")

train_d <- read.csv("data/Springleaf/train.csv", head= T)
#test_d <- read.csv("data/Springleaf/test.csv", head= T,nrows = 10001:20001)
#sample <- read.csv("data/Springleaf/sample.csv")



#### getting and cleaning data
train <- train_d
#df$target <- factor(df$target, levels=c(0,1)) # set target varialbe as factor, using KNN to predict

# If we only focus on numeric columns and take out ID and target, the code is as below:
colnumr <- colnames(train[,sapply(train,is.numeric)]) 
colnumr <- colnumr[!colnumr %in% c("ID","target")]
train1 <- train[,colnumr]

## remove the id and target
train = subset(train, select=-c(ID, target))

library(R.utils)
## get the rowcount
row_count <- countLines("data/Springleaf/train.csv")
cat("Row count : ", row_count[1], "; Predictor column count : ", ncol(train))

## check the proportion of NA values.
NA_per <- length(train[is.na(train)])/(ncol(train)*nrow(train)) 
cat("The percentage of NA is: ", NA_per)
## check for the duplicated rows.
nrow(train) - nrow(unique(train))

## here we see columns with only one unique value. 
col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==1]))
## mine experiment
num = rep(NA,150)
col_ct = sapply(train, function(x) length(unique(x)))
for (i in 1:150)
{
num[i] = length(col_ct[col_ct==i])
}
num
sum(num)

## we can remove these columns that we don't want
train = train[, !names(train) %in% names(col_ct[col_ct==1])]

##Identify and separate the numeric and non numeric rows.
train_numr = train[, sapply(train, is.numeric)]
train_char = train[, sapply(train, is.character)]
train_fac <- train[, sapply(train, is.factor)]

## Lets digs into the character features, unique() will only show unique atrributes.
str(lapply(train_fac, unique), vec.len = 4)

## It looks like NA is represented in character columns by -1 or [] or blank values, lets 
# convert these to explicit NAs. 
train_fac[train_fac==-1] = "NA"
train_fac[train_fac==""] = "NA"
train_fac[train_fac=="[]"] = "NA"

## We place the date columns in a new dataframe and parse the dates
#train_date = train_fac[,grep("JAN1|FEB1|MAR1", train_fac)]
train_date = train_fac[,c("VAR_0073","VAR_0075","VAR_0156","VAR_0157","VAR_0158",
        "VAR_0159","VAR_0166","VAR_0167","VAR_0168","VAR_0169","VAR_0176","VAR_0177","VAR_0178","VAR_0179","VAR_0204","VAR_0217")]

## Now lets separate out the dates from the character columns and look at them further.
train_fac = train_fac[, !colnames(train_fac) %in% colnames(train_date)]
train_date = sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date = do.call(cbind.data.frame, train_date)

#Take out the times to a different data frame.
train_time = train_date[,colnames(train_date) %in% c("VAR_0204","VAR_0217")]
train_time = data.frame(sapply(train_time, function(x) strftime(x, "%H:%M:%S")))
train_hour = as.data.frame(sapply(train_time, function(x) as.numeric(as.character(substr( x ,1, 2)))))

# plot the date
par(mar=c(2,2,2,2),mfrow=c(4,4))
for(i in 1:16) hist(train_date[,i], "weeks", format = "%d %b %y", main = colnames(train_date)[i], xlab="", ylab="")

#ploting the time
par(mar=c(2,2,2,2),mfrow=c(1,2))
for(i in 1:2) hist(train_hour[,i], main = paste(colnames(train_hour)[i], "hourly"), breaks = c(0:24), xlab="", ylab="")

#Here we take a look at the geographical break down of the state features.
par(mfrow=c(1,1))
mapUSA <- map('state', fill=T, plot=F)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))


mapStates = function(df, feat){
    dat = data.frame(table(df[,feat]))
    names(dat) = c("state.abb", "value")
    dat$states <- tolower(state.name[match(dat$state.abb,  state.abb)])
    
    idx <- match(unique(nms),  dat$states)
    dat2 <- data.frame(value = dat$value[idx], state = unique(nms))
    row.names(dat2) <- unique(nms) 
    USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
    spplot(USAsp['value'], main=paste(feat, "value count"), col.regions=rev(heat.colors(21)))
}
grid.arrange(mapStates(train_fac, "VAR_0274"), mapStates(train_fac, "VAR_0237"),ncol=2)

#Now lets look at the number of unique values per column.
num_ct = sapply(train_numr, function(x) length(unique(x)))
char_ct = sapply(train_fac, function(x) length(unique(x)))
date_ct = sapply(train_date, function(x) length(unique(x)))
all_ct = rbind(data.frame(count=num_ct, type="Numerical"), 
               data.frame(count=char_ct, type="Character"), 
               data.frame(count=date_ct, type="Date"))
# lets plot the unique values per feature
g1 = ggplot(all_ct, aes(x = count, fill=type)) + 
    geom_histogram(binwidth = 1, alpha=0.7, position="identity") + 
    xlab("Unique values per feature (0-100)")+ theme(legend.position = "none") + 
    xlim(c(0,100)) +theme(axis.title.x=element_text(size=14, ,face="bold"))
g2 = ggplot(all_ct, aes(x = count, fill=type)) +  
    geom_histogram(binwidth = 100, alpha=0.7, position="identity") + 
    xlab("Unique values per feature(101+)")  + xlim(c(101,nrow(train))) +
    theme(axis.title.x=element_text(size=14, ,face="bold"))
grid.arrange(g1, g2, ncol=2)























df_test <- df[,c(2:500,dim(df)[2])]
df_test <- na.omit(df_test)

##============ get n rows data and run knn
set.seed(1)
n=5000
devide <- sample(n,(0.8*n))

train_x <- df_test[1:n,][devide,-1]
test_x <- df_test[1:n,][-devide,-1]
#train_x <- df_test[1:n,][devide,1:3]
#test_x <- df_test[1:n,][-devide,1:3]
train_target <- df_test[1:n,][devide,dim(df_test)[2]]
test_target <- df_test[1:n,][-devide,dim(df_test)[2]]

# cross validation to get the best k
grp=df_test[1:n,]$target
x=scale(df_test[1:n,-1])
k=length(unique(grp))
dat=data.frame(grp,x)

library(chemometrics)
library(class)
set.seed(123)
devide <- sample(n,(0.8*n))
resknn=knnEval(x,grp,devide,knnvec=seq(1,30,by=1),legpos="bottomright")


## run K-nearest neighbour classification
library(class)
knn_pred <- knn(train_x, test_x, train_target, k=5)
table(knn_pred, test_target)
mean(knn_pred==test_target)





##===============================================================================================






## split the df data half/half
set.seed(1)
dim(df)
devide <- sample(dim(df)[1],(dim(df)/2))
train_x <- df[devide,-1]
train_target <- df[devide,dim(df)[2]]
test_y <- df[-devide,-1]
test_target <- df[-devide,dim(df)[2]]

library(class)
knn.pred <- knn(train_x, test_x, train_target, k=30)
table(knn.pred, test_target)

##===================================================================================================








##@@@@@ try logistic regression @@@@@

glm.fit<- glm(target~VAR_1902+VAR_1823, data=df,family="binomial")
summary(glm.fit)

glm.probs <- predict(glm.fit, df, type="response")

glm.pred <- rep(0,dim(df)[1])
glm.pred[glm.probs > 0.5] <- c(1)
table(glm.pred, df$target)

mean(glm.pred==df$target)

## if i can load the test data set, here is the code using KNN method
library(class)
train_x <- df_train[,-1]
train_target <- df_train[,1934]

test_x <- df_test[,-1]
test_target <- df_test[,1934]




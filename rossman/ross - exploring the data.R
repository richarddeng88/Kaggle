options(digits=4)
library(dplyr)
par(mfrow=c(1,1))

train <- read.csv("data/rossman/train.csv")
test <- read.csv("data/rossman/test.csv")
sample <- read.csv("data/rossman/sample_submission.csv")
store <- read.csv("data/rossman/store.csv")

names(train) <- c("store","wday","date","sales","customers","open","promo","state_holiday","school_holiday")
names(test)  <- c("id","store","wday","date","open","promo","state_holiday","school_holiday")
names(store) <- c("store", "type", "assortment","distance","since_m","since_y","promo2","p_since_m","p_since_y","interval")
# no missing values. 

train$date <- as.Date(train$date)
test$date <- as.Date(test$date)

train$month<- as.POSIXlt(train$date)$mon+1
train$day<- as.POSIXlt(train$date)$mday
train$year<- as.POSIXlt(train$date)$year

test$month<- as.POSIXlt(test$date)$mon+1
test$day<- as.POSIXlt(test$date)$mday
test$year<- as.POSIXlt(test$date)$year


#na_per <- length(train[is.na(train)])
#cat("there is ",na_per , " missing value")
train$date <- as.Date(as.character(train$date)) 
 
# this plot shows the mean sales on every weekday
library(dplyr)
ave <- train %>% group_by(day) %>% summarize(sale.mean=mean(sales))
plot(ave$day,ave$sale.mean, type = "b")

# this histogram shows the mean sales per stoore
ave <- train %>% group_by(store) %>% summarize(sale.mean=mean(sales))
plot(ave$store,ave$sale.mean, type = "l")
hist(ave$sale.mean, breaks=30)

train_store <- group_by(train, store)
store_sales<-summarize(train_store, sale.mean=mean(sales))
par(mfrow=c(2,1))
plot(store_sales$store, store_sales$sale.mean)
hist(store_sales$sale.mean, breaks=30)
#==============================================================================================
# reasearch on the whole train data
sun <- train[train$day==7,]
dim(sun[sun$sales==0])[1]/dim(sun)[1] # 98% sales are 0 on day 7
head(sun[sun$sales!=0,])

weekd <- train[train$day!=7,]
dim(weekd[weekd$sales==0,])[1]/dim(weekd)[1] # 3.6% sales are 0 on weekday

close <- train[train$open==0,]
dim(close[close$sales==0,])[1]/dim(close)[1] # 100% sales are 0 when store are not open

state_h <- train[train$state_holiday ==0,]
dim(state_h[state_h$sales==0,])[1]/dim(state_h)[1] # 14% sales are 0 on state holiday.

school_h <- train[train$school_holiday ==0,]
dim(school_h[school_h$sales==0,])[1]/dim(school_h)[1] # 19% sales are 0 on school holiday

sales <- train[train$sales==0,]
sum(sales$open)  # 54, when the store is open, sometime the slales is 0. why? 
sales[sales$open!=0,] # see details. 
#============================================================================================
# conclusion
dim(test[test$open==0,]) #5948 (open==0), sales definitly == 0

#33 stores are open on sunday(open==1,day==7), sales must >0
special_store <- filter(train, open==1, day==7)
unique(special_store$store) 
#  85  122  209  259  262  274  299  310  335  353  423  433  453  494  512  524  530  562
#  578  676  682  732  733  769  863  867  931  948 1045 1081 1097 1099  877

store85 <- filter(train, store==948)
ave <- store85 %>% group_by(day) %>% summarize(sale.mean=mean(sales))
plot(ave$day,ave$sale.mean, type = "l")
hist(ave$sale.mean, breaks=10)
plot(store85$sales)
#============================================================================================
# store1 for exploring
store1 <- filter(train, store==1)
plot(store1[store1$day!=7 ,]$date, store1[store1$day!=7 ,]$sales, type="l")



store1 <- select(store1, -date,-store)
store1[store1$state_holiday==0,]
pairs(store1)

store1$state_holiday <- as.numeric(store1$state_holiday)
cor(store1)

## i am tryting to merge the train and store data set together. 
ntrain <- merge(train,store,by.x = train$store, by.y = test$store )

mean(train$state_holiday=="a")
#[1] 0.01991724
 mean(train$state_holiday=="0")
#[1] 0.9694753
 mean(train$state_holiday=="b")
#[1] 0.00657682
 mean(train$state_holiday=="c")
#[1] 0.004030637
 
length(unique(train$store))
length(unique(test$store))

# i just figured out that there are 11 NA in the "open" columns of test data set
#id sales
#480     480    NA
#1336   1336    NA
#2192   2192    NA
#3048   3048    NA
#4760   4760    NA
#5616   5616    NA
#6472   6472    NA
#7328   7328    NA
#8184   8184    NA
#9040   9040    NA
#10752 10752    NA

## we need to think the methodology to transform the missing value.using what value to replace these missing values? 
test$open[is.na(test$open)] <- 1
sum(is.na(test))


## take a look at test data set, tatolly 856 different stroes are chosen for prediction. 
length(unique(train$store)) #1115
length(unique(test$store)) #856

train1 <- select(train, -customers,-date, -state_holiday, -school_holiday)
test1 <-select(test, -date, -state_holiday, -school_holiday)

lengthoftest<- rep(0,856); j=0
for (i in unique(test$store)) {
    j=j+1
    test_i <- filter(test1, store==i)
    lengthoftest[j] <- dim(test_i)[1]
}
lengthoftest
unique(lengthoftest)
## each store which are chosen in the test data have 48 rows.  

lengthoftest<- rep(0,1115); j=0; sta <- matrix(0,1115,4)
for (i in unique(train$store)) {
    j=j+1
    train_i <- filter(train1, store==i)
    lengthoftest[j] <- dim(train_i)[1]
    sta[j,1] <- i
    sta[j,2] <- length(unique(train_i$day))
    sta[j,3] <- length(unique(train_i$open))
    sta[j,4] <- length(unique(train_i$promo))
}

lengthoftest
unique(lengthoftest)
## each store which are chosen in the test data have 941, 942 or 758 rows.  

sta1 <- data.frame(sta)
names(sta1) <- c("store", "day", "open", "promo")
special <- filter(sta1, day==1 | open==1 | promo==1)
#store day open promo
#1     85   2    1     2
#2    262   2    1     2   in test
#3    335   2    1     2   in test
#4    423   2    1     2
#5    494   2    1     2
#6    562   2    1     2   in test
#7    682   2    1     2
#8    733   2    1     2   in test
#9    769   2    1     2   in test
#10  1097   2    1     2   in test








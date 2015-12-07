tem <- rep(0,dim(traind[2])
a <- c(0,0)
for (i in 1:dim(traind)[2]){
    a<-range(traind[,i])
    tem[i] <- a[2]-a[1]
}
plot(1:1871,tem)
mi1 <- data.frame(tem,names(traind))
na1 <- mi1[mi1[1]>100,]
print(na1)
dim(na1)

library(dplyr)
train_after <- traind
n=dim(traind)[2]-1

for (i in 1:n){
    train_after[,i] <- ntile(traind[,i],10)
}

set.seed(1)
sam <- sample(1:nrow(traind), round(nrow(traind)*8/10))
train <- train_after[sam,]
test <- train_after[-sam,]

## naiveBayes method
model <- naiveBayes(target ~ ., data = train_after)
#pred <- predict(model, train_after[-sam,])
pred_prob <- predict(model, train_after[-sam,], type = "raw")
result_save <- pred_prob


pred_prob[pred_prob>0.5]=1
pred_prob[pred_prob<=0.5]=0

mean(pred_prob[2]== train_after$target[-sam]) 
# the prediction result is so so low , only 0.2340
mean(pred== train_after$target[-sam]) 






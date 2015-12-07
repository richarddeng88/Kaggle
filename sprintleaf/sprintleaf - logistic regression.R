

## ============================= using naivebayes to predict ======================================
library(e1071)
model <- naiveBayes(target ~ ., data = traind)
# pred <- predict(model, testd) 
pred_prob <- predict(model, testd, type = "raw") 

# transfer the result to binary just 0 and 1 
save <- pred_prob
pred_prob[pred_prob>0.5]=1
pred_prob[pred_prob<=0.5]=0

##the result doesn't look good because some prob are too small and some are too large. in another scirpt, 
#i am good tertile all the number into 1:10

## =============================using logistic regressiong============================
# train_after are new matrix, whose number are tranfered into categorical 

## does not work at all
glm.fit<- glm(target~., data=traind,family="binomial")
summary(glm.fit)

glm.probs <- predict(glm.fit,test, type="response")

glm.pred <- rep(0,dim(df)[1])
glm.pred[glm.probs > 0.5] <- c(1)
table(glm.pred, test$target)

mean(glm.pred==df$target)
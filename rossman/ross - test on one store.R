new <- merge(train, store)


store1 <- filter(train, store==1)
store1 <- select(store1, -store,-customers)
library(leaps)
regfit <- regsubsets(sales~., data=store1,nvmax=20)
reg <- summary(regfit)
par(mfrow=c(2,2))
#ploting fo find the best subset selection
plot(reg$rss, xlab = "number of variables", ylab="RSS",type = "l")
which.min(reg$rss)
points(8, reg$rss[8], col="red", cex=2, pch=20)

plot(reg$adjr2, xlab="number of variables", ylab = "Adjusted R square", type = "l")
which.max(reg$adjr2)
points(5,reg$adjr2[5],col="red",cex=2,pch=20)

plot(reg$cp, xlab = "number of variables", ylab="Cp",type = "l")
which.min(reg$cp)
points(4,reg$cp[4],col="red",cex=2,pch=20)

plot(reg$bic, xlab="number of variables", ylab = "BIC", type = "l")
which.min(reg$bic)
points(4,reg$bic[4] ,col="red",cex=2,pch=20)


plot(regfit, scale="r2")
plot(regfit, scale="adjr2")
plot(regfit, scale="Cp")
plot(regfit, scale="bic")



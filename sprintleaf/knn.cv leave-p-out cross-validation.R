library(kernlab)
data(spam)
spam.label <- spam[,58]
spam.data <- spam[,-58]
names(spam.data)
table(spam.label)
n=20
cv_error =rep(0,n)

library(class)
for (i in 1:n){
res <- knn.cv(spam.data, spam.label, k = n, p = 400)
table(res)
cv_error[i] <- mean(res==spam$type)

}
cv_error
plot(1:n, cv_error)
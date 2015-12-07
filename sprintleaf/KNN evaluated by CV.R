data(fgl,package="MASS")
grp=fgl$type
x=scale(fgl[,1:9])
k=length(unique(grp))
dat=data.frame(grp,x)
n=nrow(x)

library(chemometrics)
library(class)
set.seed(123)
train1=sample(1:n,round(n*2/3))
resknn=knnEval(x,grp,train1,knnvec=seq(1,30,by=1),legpos="bottomright")
 




   
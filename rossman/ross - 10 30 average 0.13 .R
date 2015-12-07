options(digits=4)
library(dplyr)
par(mfrow=c(1,1))

train <- read.csv("data/rossman/train.csv")
test <- read.csv("data/rossman/test.csv")
sample <- read.csv("data/rossman/sample_submission.csv")
store <- read.csv("data/rossman/store.csv")

train = train[train$Sales>0,]
preds=c('Store','DayOfWeek','Promo')
mdl = train %>% group_by_(.dots=preds) %>% summarise(PredSales=exp(mean(log(Sales)))) %>% ungroup()
pred = test %>% left_join(mdl,by=preds) %>% select(Id,PredSales) %>% rename(Sales=PredSales)
pred$Sales[is.na(pred$Sales)]=0

write.csv(pred, "kaggle competition/rossman/final_submission1.csv",row.names=F)

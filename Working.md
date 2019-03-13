# **House Prices**
*Predict House Prices*

## Table of contents

- [Introduction](#introduction)
- [Preparation](#preparation)
- [Prediction](#prediction)
- [Conclusion](#conclusion)

## Introduction
I will be doing data cleaning to make a model and predict house prices.

## Preparation
#### Initial works
```
library(Metrics)
library(rpart)
library(ranger)
library(rpart.plot)
train <- read.csv('C:/Data Science/Kaggle/House price/train.csv', na.strings = c("", "NA"), stringsAsFactors =T)
test <- read.csv('C:/Data Science/Kaggle/House price/test.csv', na.strings = c("", "NA"), stringsAsFactors = T)
``` 
#### Data cleaning
I will clean my data to make it clean and tidy to be easier to use for my modeling.
```
Saleprice<-train$SalePrice
train$SalePrice<-NULL
total<-rbind(train,test)

#treating NA
colnames(total)[colSums(is.na(total)) > 0]

much.na<-NA

for (i in 1:ncol(total)) {
  if(sum(is.na(total[,i]))/nrow(total) > 0.3){
    much.na<- c(much.na,i)
  }
}

much.na<- much.na[!is.na(much.na)]

total<- total[,-much.na]

# create mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:ncol(total)) {
  
  if(class(total[,i])=='factor'){
    result<-getmode(total[,i])
    total[,i][is.na(total[,i])]<-result}else{
      result<-mean(total[,i],na.rm = TRUE)
      total[,i][is.na(total[,i])]<-result
    }
  
  
}

total$Id<-NULL
total$MoSold <- as.factor(total$MoSold)
# total$YrSold <- as.factor(total$YrSold)
```
## Prediction
I wil be spliting my train data and trial the model out to see how accurate my model is.
```
#split into train and test
train<- total[1:nrow(train),]
train$Salesprice<-Saleprice
test<-total[(nrow(train)+1):nrow(total),]

#sampling
set.seed(12345)
sam<-sample(1:nrow(train))
train1<-train[sam,]


#split
partition<-round(nrow(train)*0.7)
training<-train1[1:partition,]
testing<-train1[(partition+1):nrow(train1),]

#model
model<-ranger(Salesprice~.,training)
prediction<-predictions(predict(model,testing))
cut.line<-rmse(log(testing$Salesprice),log(prediction))

comparison<-data.frame(col.name=NA,rmse=NA)
for (i in 1:(ncol(training)-1)) {
  
  #feature selection
  training1<-training[,-i]
  model<-ranger(Salesprice~.,training1)
  prediction<-predictions(predict(model,testing))
  comparison1<-data.frame(col.name=i,rmse=rmse(log(testing$Salesprice),log(prediction)))
  print(rmse(log(testing$Salesprice),log(prediction)))
  comparison<-rbind(comparison,comparison1)
}

comparison <- comparison[!is.na(comparison$col.name),]
good.features<-comparison$col.name[comparison$rmse>=cut.line]

training1<-training[,c(good.features,75)]
model<-ranger(Salesprice~.,training1)
prediction<-predictions(predict(model,testing))
rmse(log(testing$Salesprice),log(prediction))
cut.line

#select important features
train<-train[,c(good.features,75)]

#outlier checking
sam<-sample(1:nrow(train))
sampled_train<-train[sam,]
partition<-round(nrow(sampled_train)*0.7)
training<-sampled_train[1:partition,]
testing<-sampled_train[(partition+1):nrow(sampled_train),]

#find error
model <- ranger(Salesprice~.,training)
training$prediction<-predictions(predict(model,training))




#check error
training$error<- abs(training$Salesprice-training$prediction)

comparison<-data.frame(q=NA,rmse=NA)

for (q in 1:100) {
  

training1 <- training[training$error <= quantile(training$error,q*0.01),]
training1$prediction <- NULL
training1$error <- NULL
model <- ranger(Salesprice~., training1)
prediction<-predictions(predict(model,testing))
comparison1<-data.frame(q=q*0.01,rmse=rmse(log(testing$Salesprice),log(prediction)))
print(rmse(log(testing$Salesprice),log(prediction)))
comparison<-rbind(comparison,comparison1)

}

# we know that 0.999 is optimal

training <- training[training$error <= quantile(training$error,0.999),]



model <- ranger(Salesprice~.,train)
train$Prediction <- predictions(predict(model,train))
train$error<- abs(train$Salesprice-train$Prediction)
train <- train[train$error <= quantile(train$error,0.999),]




train$error<-NULL
train$Prediction<-NULL

#create final model
model<-ranger(Salesprice~.,train)
prediction<-predictions(predict(model,test))


submit<-data.frame(Id=1461:2919,SalePrice=prediction)
write.csv(submit,'C:/Data Science/Kaggle/House price/submit.csv',row.names = F)
```
## Conclusion
This was an analysis of what the house prices would be as in estimation in the features we have given. 

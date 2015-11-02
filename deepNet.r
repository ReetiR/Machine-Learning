library(deepnet)
set.seed(1234)
ind <- sample(2, nrow(data_banknote_authentication), replace=TRUE, prob=c(0.7, 0.3))
train <- data_banknote_authentication[ind==1,]
test <- data_banknote_authentication[ind==2,]
x<-data.frame(train[,1:4])
x<-as.matrix(x)
modeln <- dbn.dnn.train(x, train$V5)
summary(modeln)
print(modeln)
x1<-data.frame(test[,1:4])
x1<-as.matrix(x1)
p <- nn.predict(modeln, x1)
summary(p)
print(p)
pred<-data.frame(p)
pd=c()
for (n in 1:397)
{
  if(pred[n,1]<0.492)
    pd[n]<-0
  else
    pd[n]<-1
}

library(caret)
confusionMatrix(pd, test$V5)


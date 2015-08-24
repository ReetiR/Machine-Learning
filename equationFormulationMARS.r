library(earth)
attach(ENB2012_data)
names(ENB2012_data)

set.seed(1234)
ind <- sample(2, nrow(ENB2012_data), replace=TRUE, prob=c(0.9, 0.1))
train <- ENB2012_data[ind==1,]
test <- ENB2012_data[ind==2,]
trainLabels <- ENB2012_data[ind==1, 9]
trainLabels
testLabels <- ENB2012_data[ind==2, 9]
testLabels
model <- earth(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data= train) #Model is created for the given data
#table(predict(model), train$Y1)
summary(model)
plot(model)
attributes(model)
model

yhat<-predict(model)
yhat
y <- train$Y1
y

bx <- model.matrix(model)
head(bx)

head(cat(format(model)))
cat(format(model, style="C"))
cat(format(model, style="bf"))

cat(format(model, style="max")) 


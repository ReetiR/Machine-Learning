library(earth)
attach(winequality.red)
names(winequality.red)

set.seed(1234)
ind <- sample(2, nrow(winequality.red), replace=TRUE, prob=c(0.7, 0.3))
train <- winequality.red[ind==1,]
test <- winequality.red[ind==2,]
trainLabels <- winequality.red[ind==1, 12]
trainLabels
testLabels <- winequality.red[ind==2, 12]
testLabels
model1 <- earth(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar
                + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH 
                + sulphates + alcohol, data= train) #Model is created for the given data
table(predict(model1), train$quality)
summary(model1)
plot(model1)
attributes(model1)

testPred <- predict(model1, newdata = test)
table(testPred, test$quality)

plot(testLabels,testPred)

#evaluation
library(gmodels)
CrossTable(x = testLabels, y = testPred, prop.chisq=TRUE)
#



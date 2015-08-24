library(earth)
attach(ENB2012_data)
names(ENB2012_data)

set.seed(1234)
ind <- sample(2, nrow(ENB2012_data), replace=TRUE, prob=c(0.7, 0.3))
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

testPred <- predict(model, newdata = test)
print (1 - sum((testLabels - testPred)^2) / sum((testLabels - mean(testPred))^2))
table(testPred, test$Y1)
print(summary(earth.model, newdata=test))

plot(testLabels,testPred)


reg1 <- earth(testPred~testLabels)
par(cex=.5)
plot(testLabels,testPred)
abline(0,1)

require("lattice")
xyplot(testLabels ~ testPred, data = ENB2012_data, type = c("p","r"),xlab = "Actual Values",ylab = "Predicted Values", col.line = "red")

#evaluation
library(gmodels)
CrossTable(x = testLabels, y = testPred, prop.chisq=TRUE)
#



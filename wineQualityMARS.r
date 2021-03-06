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
model <- earth(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar
                + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH 
                + sulphates + alcohol, data= train) #Model is created for the given data
table(predict(model), train$quality)
summary(model)
plot(model)
attributes(model)

testPred <- predict(model, newdata = test)
print (1 - sum((testLabels - testPred)^2) / sum((testLabels - mean(testPred))^2))
table(testPred, test$quality)

plot(testLabels,testPred)

reg1 <- earth(testPred~testLabels)
par(cex=.5)
plot(testLabels,testPred)
abline(0,1)

require("lattice")
xyplot(testLabels ~ testPred, data = winequality.red, type = c("p","r"),xlab = "Actual Values",ylab = "Predicted Values", col.line = "red")

#evaluation
library(gmodels)
CrossTable(x = testLabels, y = testPred, prop.chisq=TRUE)
#



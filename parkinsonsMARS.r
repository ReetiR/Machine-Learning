library(earth)
attach(parkinsons.data)
names(parkinsons.data)

set.seed(1234)
ind <- sample(2, nrow(parkinsons.data), replace=TRUE, prob=c(0.7, 0.3))
train <- parkinsons.data[ind==1,]
test <- parkinsons.data[ind==2,]
trainLabels <- parkinsons.data[ind==1, 18]

testLabels <- parkinsons.data[ind==2, 18]

model <- earth(status ~ name + MDVP.Fo.Hz. + MDVP.Fhi.Hz. + MDVP.Flo.Hz.
               + MDVP.Jitter... + MDVP.Jitter.Abs. + MDVP.RAP + MDVP.PPQ + Jitter.DDP 
               + MDVP.Shimmer + MDVP.Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + MDVP.APQ + Shimmer.DDA + NHR + HNR + RPDE + DFA + spread1 + spread2 + D2 + PPE, data= train) #Model is created for the given data
table(predict(model), train$status)
summary(model)
plot(model)
attributes(model)

testPred <- predict(model, newdata = test)
print (1 - sum((testLabels - testPred)^2) / sum((testLabels - mean(testPred))^2))
table(testPred, test$status)

plot(testLabels,testPred)

reg1 <- earth(testPred~testLabels)
par(cex=.5)
plot(testLabels,testPred)
abline(0,1)

require("lattice")
xyplot(testLabels ~ testPred, data = parkinsons.data, type = c("p","r"),xlab = "Actual Values",ylab = "Predicted Values", col.line = "red")

#evaluation
library(gmodels)
CrossTable(x = testLabels, y = testPred, prop.chisq=TRUE)
#


#stepwise reg example

#bring in the data and remove the first column
EXECSAL2<-read.table('EXECSAL2.txt', header = TRUE)
EXECSAL2 <- EXECSAL2[-1]

form <- formula(paste("Y~",paste(names(EXECSAL2)[-1],collapse = "+"),sep=""))


#creating a null model where y is predicted with just the mean
null <- lm(Y~1, data = EXECSAL2)

#import MASS and run stepwise regression
library(MASS)
build <- stepAIC(null, scope = form, direction = "forward")


#creating the best model based upon stepwise
library(leaps)

bestFitEX <- regsubsets(Y~., data = EXECSAL2, nbest = 1, nvmax = 10)
summary(bestFitEX)

#returning rsq, rsq adj, and mallows Cp
summary(bestFitEX)$rsq
summary(bestFitEX)$adjr2
summary(bestFitEX)$cp

#getting MSE and PRESS stats for the models
Model1 <- lm(Y~X1, data = EXECSAL2)
Model2 <- lm(Y~X1 + X3, data = EXECSAL2)
Model3 <- lm(Y~X1 + X3 + X4, data = EXECSAL2)
Model4 <- lm(Y~X1 + X2 + X3 + X4, data = EXECSAL2)
Model5 <- lm(Y~X1 + X2 + X3 + X4 + X5, data = EXECSAL2)
Model6 <- lm(Y~X1 + X2 + X3 + X4 + X5 + X9, data = EXECSAL2)
Model7 <- lm(Y~X1 + X2 + X3 + X4 + X5 + X6 + X9, data = EXECSAL2)
Model8 <- lm(Y~X1 + X2 + X3 + X4 + X5 + X6 + X8 + X9, data = EXECSAL2)
Model9 <- lm(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = EXECSAL2)
Model10 <- lm(Y~., data = EXECSAL2)

#defining a fuction to return MSE of model
MSE <- function(model){
  MSE <- mean(model$residuals^2)
  return(MSE)
  }

#get MSE for model 1 & 10
MSE(Model1)
MSE(Model10)

#import DAAG library
library(DAAG)

#get PRESS for 1 & 10
press(Model1)
press(Model10)
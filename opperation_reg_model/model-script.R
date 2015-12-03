#test model

#read in datatable
file <- "C:\\Users\\Rick\\Documents\\STAT308\\Regressional_Analysis\\opperation_reg_model\\DATA\\EXAM2DATA.txt"
BLOOD <- read.table(file, header = TRUE)

summary(BLOOD)

#plot the dependent variable
par(mfrow=c(3,2), mar=c(2,5,2,1), las=1, bty="n")
plot(BLOOD$Y~BLOOD$X1, main='X1')
abline(lm(BLOOD$Y~BLOOD$X1), col = "red")
plot(BLOOD$Y~BLOOD$X2,main='X2')
abline(lm(BLOOD$Y~BLOOD$X2), col = "red")
plot(BLOOD$Y~BLOOD$X3,main='X3')
abline(lm(BLOOD$Y~BLOOD$X3), col = "red")
plot(BLOOD$Y~BLOOD$X4,main='X4')
abline(lm(BLOOD$Y~BLOOD$X4), col = "red")
plot(BLOOD$Y~BLOOD$X5,main='X5')
abline(lm(BLOOD$Y~BLOOD$X5), col = "red")
plot(BLOOD$Y~BLOOD$X6,main='X6')
abline(lm(BLOOD$Y~BLOOD$X6), col = "red")


#checking the significance of the categorical data
cat <- lm(Y~X8+X9, data = BLOOD)
summary(cat)
anova(cat)

#creating full multi lin model
MLinMod <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9, data = BLOOD)
summary(MLinMod)
par(mfrow=c(2,2))
plot(MLinMod)

#checking the correlation matrix for multicolinearity
M <- cor(BLOOD[,1:9]) # get correlations
library('corrplot') #package corrplot
par(mfrow=c(1,1), mar=c(2,5,2,1), las=1, bty="n")
corrplot(M, method = "circle") #plot matrix

#creating a model minus the highly correlated terms
MLinMod.lesscorr <- lm(Y~X1+X2+X3+X5+X7+X8+X9, data = BLOOD)
summary(MLinMod.lesscorr)
par(mfrow=c(2,2))
plot(MLinMod.lesscorr)

#check influential points using Cooks D

model <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9,data = BLOOD)
plot(cooks.distance(model))

#testing for outliers in the categorical data
library(mvoutlier)
blood_num = BLOOD[,1:6]
blood_num[,'Y'] <- BLOOD$Y
uni.plot(blood_num)

#checking model with first largest outlier removed
MLinMod_NoOut <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9, data = BLOOD[-c(5),])
summary(MLinMod_NoOut)
par(mfrow=c(2,2))
plot(MLinMod_NoOut)

#check with second largest outlier removed
MLinMod_NoOut <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9, data = BLOOD[-c(5,28),])
summary(MLinMod_NoOut)
par(mfrow=c(2,2))
plot(MLinMod_NoOut)

BLOOD <- BLOOD[-c(5),]

#using MASS for stepwise
library(MASS)
form <- Y~X1+X2+X3+X4+X5+X6+X7+X8+X9
null <- lm(Y~1, data = BLOOD)
build <- stepAIC(null, scope = form, direction = "forward")


#using the leaps library
library(leaps)
regsubsets.out <-
  regsubsets(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9, 
             data = BLOOD,
             nbest = 1,
             nvmax = NULL,
             force.in = NULL, force.out = NULL,
             method = "exhaustive")

#show summary data for the multivar models
#shows best variables at each level
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)

#graphing the rsquared values of each model
library(car)
par(mfrow=c(1,2), cex = 0.5)
## Adjusted R2
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
## Mallow Cp
res.legend <-
  subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)


#checking the correlation matrix for multicolinearity
M <- cor(BLOOD[,1:9]) # get correlations

#model with max r-squared.adj
which.max(summary.out$adjr2)
#model with min cp
which.min(summary.out$cp)

library('corrplot') #package corrplot
par(mfrow=c(1,1))
corrplot(M, method = "circle") #plot matrix

mv
#analysis of the best fittng model from stepwise
best <- lm(Y ~ X4 + X3 + X2 + X9 + X1 + X7, data = BLOOD)
summary(best)
par(mfrow=c(2,2))
plot(best)

#including an interaction term
interaction <- lm(Y ~ X4 + X9 + X3 + X2 + X1 + X3*X4, data = BLOOD)
summary(interaction)
plot(interaction)

#second order model
sord <- lm(log(Y) ~  X4 + X3 + X2 + X9 + X1 + X7 + X3*X4, data = BLOOD)
summary(sord)
plot(sord)
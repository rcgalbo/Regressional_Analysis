#example 1.2 

firedam <- "FIREDAM.txt"
FIREDAM <- read.table(firedam, header = TRUE)

#plot the data
plot(FIREDAM$DISTANCE,FIREDAM$DAMAGE, xlab = 'Distance from Firehouse', ylab = 'Damage Caused', main = 'Scatter and Regression of Geese and Digestion Efficiency')
#include the least squares regression
abline(lm(DAMAGE~DISTANCE, data = FIREDAM))

#create the linear model
linearmodel <- lm(DAMAGE~DISTANCE, data = FIREDAM)
#get an analysis of variance and a summary of the variables
anova(linearmodel)
summary(linearmodel)

#give predictions
predict.lm(linearmodel, level= 0.95, interval = "predict", se.fit = TRUE)

#obtain the residules
resid(linearmodel)

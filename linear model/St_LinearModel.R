file <- "P:Regressional_Analysis/DATA/STREETVN.txt"
STREETS <- read.table(file, header=TRUE)
#scatter plott
plot(x = STREETS$Hours, y = STREETS$Earnings, xlim = c(5,12), xlab = 'Hours', ylab = 'earnings', main = 'scatter')
#model regression
LinMod1 <- lm(STREETS$Hours~STREETS$Earnings)

#returns linear model estimates
summary(LinMod1)

#generate anova table
anova(LinMod1)

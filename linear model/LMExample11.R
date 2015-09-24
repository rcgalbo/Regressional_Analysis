file <- "ADSALES.txt"
ADSALES <- read.table(file, header=TRUE)
#scatter plott
plot(x = ADSALES$ADVEXP_X, y = ADSALES$SALES_Y, xlab = 'Advertizing Expenditure', ylab = 'Number of Sales', main = 'Scatter')
#model regression
LinMod1 <- lm(ADSALES$ADVEXP_X~ADSALES$SALES_Y)

#returns linear model estimates
summary(LinMod1)

#generate anova table
anova(LinMod1)

file <- "P:/STAT308/R_Code/DATA/STREETVN.txt"
STREETS <- read.table(file, header=TRUE)
#scatter plott
plot(x = STREETS$Hours, y = STREETS$Earnings, xlim = c(5,12), xlab = 'Hours', ylab = 'earnings', main = 'scatter')
#model regression
LinMod1 <- lm(STREETS$Hours~STREETS$Earnings)
summary(LinMod1)


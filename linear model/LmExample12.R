#example 1.2 

geese <- "SNOWGEESE.txt"
GEESE <- read.table(geese, header = TRUE)

plot(GEESE$WtChange,GEESE$DigEff, xlab = 'Digestion Efficancy', ylab = 'Weight Change', main = 'Scatter and Regression of Geese and Digestion Efficiency')

abline(lm(GEESE$WtChange~GEESE$DigEff))

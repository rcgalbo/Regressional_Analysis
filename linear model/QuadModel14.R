#Quadratic Model with a Quantative Predictor for Immunity and Fitness Data

AEROBIC <- read.table("AEROBIC.txt", header = TRUE)
quadmodel <- lm(IGG~MAXOXY + I(MAXOXY^2), data = AEROBIC)
summary(quadmodel)
anova(quadmodel)

plot(AEROBIC$MAXOXY, AEROBIC$IGG, xlab = "MAXOXY", ylab = "IGG", main = "Scatterplot")
points(AEROBIC$MAXOXY, fitted(quadmodel), col = "red")

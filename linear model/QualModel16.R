#using 3 level qualatative variables to produce model

CARGO <- read.table("CARGO.txt", header = TRUE)
QualModel <- lm(COST ~ X1 + X2, data = CARGO)
summary(QualModel)

anova(QualModel)

contrasts(CARGO$CARGO)

#using the cargo data stream to create a qualmodel
QualModel1 <- lm(COST~ factor(CARGO), data = CARGO)
summary(QualModel1)

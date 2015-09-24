#multiple linear regression
gfclocks <- "GFCLOCKS.txt"
GFCLOCKS <- read.table(gfclocks, header = TRUE)

#create two scatters
par(mfrow = c(1,2))
plot(GFCLOCKS$AGE, GFCLOCKS$PRICE, main = "Scatterplot of Price vs Age")
plot(GFCLOCKS$NUMBIDS, GFCLOCKS$PRICE, main = "Scatterplot of Price vs Number of Bids")

#multi-variable linear model
multimodel <- lm(PRICE~AGE+NUMBIDS, data = GFCLOCKS)

summary(multimodel)
anova(multimodel)

#now including the interaction term
multimodelint <- lm(PRICE~ AGE + NUMBIDS + AGE.BID, data = GFCLOCKS)

summary(multimodelint)
anova(multimodelint)



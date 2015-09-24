#complete second order model

EXPRESS <- read.table("EXPRESS.txt", header = TRUE)
CQuadModel <- lm(Cost~Weight * Distance + I(Weight^2) + I(Distance^2), data = EXPRESS)

summary(CQuadModel)


#comparing nested models (complete vs reduced models)

#calculating f stat
quadmodel <- lm(Cost~Weight*Distance, data = EXPRESS)
anova(quadmodel)

#getting F stat using anova

anova(quadmodel,CQuadModel)
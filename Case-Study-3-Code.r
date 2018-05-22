library(Sleuth3)
library(dplyr)
library(ggformula)
library(pander)
library(knitr)
library(stargazer)
library(car)
library(pander)
library(gridExtra)
library(broom)
library(ggthemes)
library(MASS)
library(leaps)
library(GGally)

nes <- read.csv("http://aloy.rbind.io/data/NES.csv")

head(nes)

summary(nes)

glm.base <- glm(dem ~ year + region + union + income + educ + gender + race + age, data = nes, family = binomial)
summary(glm.base)
glm.basic <- glm(dem ~ 1, data = nes, family = binomial)
stpFwd <- stepAIC(glm.basic, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age), direction = "both", k = log(nrow(nes)))
summary(stpFwd)
stpBk <- stepAIC(glm.base, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age), direction = "both", k = log(nrow(nes)))
summary(stpBk)

glm.square <- glm(dem ~ year + region + union + income + educ + gender + race + age + I(age^2), 
                  data = nes, family = binomial)

summary(glm.square)

glm.inter <- glm(dem ~ year + region + union + income + educ + gender + race + age + 
                   age * year + age * region + age * union + age * income + age * educ + age * gender + age * race, data = nes, family = binomial)
summary(glm.inter)

stp.inter <- stepAIC(glm.inter, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age + I(age^2) +
                                               age * year + age * region + age * union + age * income + age * educ + age * gender + age * race),
                     direction = "both", k = log(nrow(nes)))

summary(glm.inter)

glm.inter <- glm(dem ~ year + region + union + income + educ + gender + race + age + 
                  + age * union + age * income + age * educ + age * race, data = nes, family = binomial)

stp.inter <- stepAIC(glm.inter, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age + I(age^2) +
                                               age * year + age * region + age * union + age * income + age * educ + age * gender + age * race),
                     direction = "both", k = log(nrow(nes)))

summary(stp.inter)

stp.inter.fwd <- stepAIC(glm.basic, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age + I(age^2) +
                                                   age * year + age * region + age * union + age * income + age * educ + age * gender + age * race),
                         direction = "both", k = log(nrow(nes)))

summary(stp.inter.fwd)

residualPlots(stp.inter.fwd, tests = false, type = "response")
residualPlots(stp.inter.fwd, tests = false, type = "pearson")
residualPlots(stp.inter.fwd, tests = false, type = "deviance")
infIndexPlot(stp.inter.fwd, vars = c("hat", "cook"))
anova(stp.inter.fwd, test = "Chisq")

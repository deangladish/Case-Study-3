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
stpFwd <- stepAIC(glm.basic, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age), direction = "both")
summary(stpFwd)
stpBk <- stepAIC(glm.base, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age), direction = "both")
summary(stpBk)

glm.square <- glm(dem ~ year + region + union + income + educ + gender + race + age + 
                    I(year)^2 + I(region)^2 + I(union)^2 + I(income)^2 + I(educ)^2 + I(gender)^2 + I(race)^2 + I(age)^2, 
                  data = nes, family = binomial)

glm.inter <- glm(dem ~ year + region + union + income + educ + gender + race + age + 
                   age * year + age * region + age * union + age * income + age * educ + age * gender + age * race, data = nes, family = binomial)
summary(glm.inter)
stp.inter <- stepAIC(glm.inter, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age + 
                                               age * year + age * region + age * union + age * income + age * educ + age * gender + age * race),
                     direction = "both")

glm.inter <- glm(dem ~ year + region + union + income + educ + gender + race + age + 
                  + age * union + age * income + age * educ + age * race, data = nes, family = binomial)

stp.inter <- stepAIC(glm.inter, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age + 
                                               age * year + age * region + age * union + age * income + age * educ + age * gender + age * race),
                     direction = "both", k = log(nrow(nes)))

summary(stp.inter)

stp.inter.fwd <- stepAIC(glm.basic, scope = list(lower = ~1, upper = ~ year + region + union + income + educ + gender + race + age + 
                                                   age * year + age * region + age * union + age * income + age * educ + age * gender + age * race),
                         direction = "both", k = log(nrow(nes)))

summary(stp.inter.fwd)

### Math Achievement and Socioeconomic Status

library(lme4)

mathach <- read.csv("~/Downloads/NEOMA-LISREL/Multilevel/mathach.csv",header = T)
mathach <- mathach[,1:6]

# Model 1: Intercept-Only Model
model.lmer1 <- lmer(mathach ~ 1 + (1 | school), data = mathach,REML=F)
summary(model.lmer1)

# Model 2: Regression with Means as Outcomes
model.lmer2 <- lmer(mathach ~ 1 + meanses+(1 | school), data = mathach,REML=F)
summary(model.lmer2)

# Model 3: Random Regression Model
model.lmer3 <- lmer(mathach ~ 1 + cses  + (1 + cses | school), data = mathach,REML=F)
summary(model.lmer3)

# Model 4: Intercepts and Slopes as Outcomes
model.lmer4 <- lmer(mathach ~ cses + meanses + sector+meanses *cses+ sector  *cses+
                     (1 + cses| school), 
                   data = mathach,REML=F)
summary(model.lmer4)


## Chi square test
lrt_result <- anova(model.lmer1, model.lmer2, model.lmer3, model.lmer4)
lrt_result



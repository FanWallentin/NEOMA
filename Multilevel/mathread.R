### Math on Reading with Career-Revisited

library(lme4)
urlfile="https://raw.github.com/FanWallentin/NEOMA/main/Multilevel/Math_on_Reading_by_Career.csv"
mathread <- read.csv(urlfile,header = T)
mathread <- mathread[,1:4]

## Simple linear model for overall
model_overall <- lm(Math ~ 1 + Reading, data = mathread)
summary(model_overall)

## For each Career
output <- list()

level <- unique(mathread$Career)
for (i in 1:length(level)){
  model_i <- lm(Math ~ 1 + Reading, data = mathread[mathread$Career==i,])
  output[[i]] <- summary(model_i)
}
output
## Multiple level model
model <- lmer(Math ~ 1 + Reading +(1 + Reading| Career), data = mathread,REML=F,
                    control = lmerControl(optimizer = "bobyqa"))
summary(model)

random_effects <- ranef(model, condVar = TRUE)
print(random_effects)




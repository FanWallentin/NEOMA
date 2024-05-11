### Math on Reading with Career-Revisited

library(lme4)

mathread <- read.csv("~/Downloads/NEOMA-LISREL/Multilevel/Math on Reading by Career.csv",header = T)
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

## Multiple level model
model <- lmer(Math ~ 1 + Reading +(1 + Reading| Career), data = mathread,REML=F,
                    control = lmerControl(optimizer = "bobyqa"))
summary(model)

random_effects <- ranef(model, condVar = TRUE)
print(random_effects)




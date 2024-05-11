####Latent Curve Models

library(lavaan)
library(tidyverse)
library(semPlot)

## input data
psavar <- read.csv("~/Downloads/NEOMA-LISREL/Longitudinal/psavar.csv",header = T)
psavar <- psavar[,1:6]

psavar[psavar==-9] = NA ## missing value

#### Model 1: Linear Growth Curve for psavar Data
model_1a <- '

           ## Measurement model

           a =~ 1*PSA0 + 1*PSA3 + 1*PSA6 + 1*PSA9 + 1*PSA12
           b =~ 0*PSA0 + 3*PSA3 + 6*PSA6 + 9*PSA9 + 12*PSA12
           
           ## Regression

           PSA0 ~~ v1*PSA0
           PSA3 ~~ v1*PSA3
           PSA6 ~~ v1*PSA6
           PSA9 ~~ v1*PSA9
           PSA12 ~~ v1*PSA12


'
fit1 <- growth(model_1a, data=psavar,missing="FIML", 
               likelihood = "wishart")

summary(fit1,standardized=TRUE,rsquare=T,fit.measures=TRUE)

parameterEstimates.mi(aaa)
######## Model 2: Linear Growth Curve with Covariate for psavar Data

model_2a <- '

           a =~ 1*PSA0 + 1*PSA3 + 1*PSA6 + 1*PSA9 + 1*PSA12
           b =~ 0*PSA0 + 3*PSA3 + 6*PSA6 + 9*PSA9 + 12*PSA12
           
           a ~ 1 + Age
           b ~ 1 + 0*Age
           
           PSA0 ~~ v1*PSA0
           PSA3 ~~ v1*PSA3
           PSA6 ~~ v1*PSA6
           PSA9 ~~ v1*PSA9
           PSA12 ~~ v1*PSA12
           a ~~ b

'
fit2 <- growth(model_2a, data=psavar,missing="FIML") 
               #likelihood = "wishart"


summary(fit2,standardized=TRUE,rsquare=T)


######## Model 3
library("mice")
library("semTools")


model_3a <- '

           a =~ 1*PSA0 + 1*PSA3 + 1*PSA6 + 1*PSA9 + 1*PSA12
           b =~ 0*PSA0 + 3*PSA3 + 6*PSA6 + 9*PSA9 + 12*PSA12
           
           a ~ 1 + Age
           b ~ 1 + Age
           
           PSA0 ~~ v1*PSA0
           PSA3 ~~ v1*PSA3
           PSA6 ~~ v1*PSA6
           PSA9 ~~ v1*PSA9
           PSA12 ~~ v1*PSA12
           a ~~ b

'

fit3 = growth.mi(model_3a, data=psavar,m=3, miPackage = "Amelia",
                likelihood = "wishart", meanstructure = TRUE)
fitMeasures(fit3)



### SIMPLEX Model for GPA Model  

library(lavaan)
library(tidyverse)
library(semPlot)

## input data
urlfile="https://raw.github.com/FanWallentin/NEOMA/main/Longitudinal/humphreys.txt"
hump <- read.table(urlfile,header = T)

### Model 1:  humphreys1b.lis       

model_1b <- ' 

  #### "=~" : measurement model
            gpa1 =~ 1*GPA1
            gpa2 =~ 1*GPA2
            gpa3 =~ 1*GPA3
            gpa4 =~ 1*GPA4
            gpa5 =~ 1*GPA5
            gpa6 =~ 1*GPA6
            gpa7 =~ 1*GPA7
            gpa8 =~ 1*GPA8
          
   ### Regression
            gpa2 ~ gpa1
            gpa3 ~ gpa2
            gpa4 ~ gpa3
            gpa5 ~ gpa4
            gpa6 ~ gpa5
            gpa7 ~ gpa6
            gpa8 ~ gpa7
            
  ### Correlated variance          
            GPA1 ~~ v1*GPA1
            GPA2 ~~ v1*GPA2
            GPA3 ~~ v3*GPA3
            GPA4 ~~ v4*GPA4
            GPA5 ~~ v5*GPA5
            GPA6 ~~ v6*GPA6
            GPA7 ~~ v2*GPA7
            GPA8 ~~ v2*GPA8
            
'

GPA_1b <- cfa(model_1b, data = hump,likelihood = "wishart")
summary(GPA_1b, fit.measures=TRUE, rsquare=T,standardized=T)


### Model 2:  humphreys2b.lis

model_2b <- ' 
  #### "=~" : measurement model
            gpa1 =~ 1*GPA1
            gpa2 =~ 1*GPA2
            gpa3 =~ 1*GPA3
            gpa4 =~ 1*GPA4
            gpa5 =~ 1*GPA5
            gpa6 =~ 1*GPA6
            gpa7 =~ 1*GPA7
            gpa8 =~ 1*GPA8
            Hscores =~ 1*HSR
            Hscores =~ ACT
            
    ### Regression
            
            gpa1 ~ Hscores
            gpa2 ~ gpa1
            gpa3 ~ gpa2
            gpa4 ~ gpa3
            gpa5 ~ gpa4
            gpa6 ~ gpa5
            gpa7 ~ gpa6
            gpa8 ~ gpa7
            
            GPA1 ~~ v1*GPA1
            GPA2 ~~ v2*GPA2
            GPA3 ~~ v3*GPA3
            GPA4 ~~ v4*GPA4
            GPA5 ~~ v5*GPA5
            GPA6 ~~ v6*GPA6
            GPA7 ~~ v7*GPA7
            GPA8 ~~ v7*GPA8
            
      
'

GPA_2b <- cfa(model_2b, data = hump,auto.var = TRUE)
summary(GPA_2b, fit.measures=TRUE, rsquare=T,standardized=T)


### Model 3:  humphreys3b.lis

model_3b <- ' 
  #### "=~" : measurement model
            gpa1 =~ 1*GPA1
            gpa2 =~ 1*GPA2
            gpa3 =~ 1*GPA3
            gpa4 =~ 1*GPA4
            gpa5 =~ 1*GPA5
            gpa6 =~ 1*GPA6
            gpa7 =~ 1*GPA7
            gpa8 =~ 1*GPA8
            Hscores =~ 1*HSR
            Hscores =~ ACT
    
    ### Regression
    
            gpa1 ~ Hscores
            gpa2 ~ b1*gpa1
            gpa3 ~ b1*gpa2
            gpa4 ~ b1*gpa3
            gpa5 ~ b1*gpa4
            gpa6 ~ b1*gpa5
            gpa7 ~ b1*gpa6
            gpa8 ~ b1*gpa7
            
    ### Variance
            
            GPA1 ~~ v1*GPA1
            GPA2 ~~ v2*GPA2
            GPA3 ~~ v3*GPA3
            GPA4 ~~ v4*GPA4
            GPA5 ~~ v5*GPA5
            GPA6 ~~ v6*GPA6
            GPA7 ~~ v7*GPA7
            GPA8 ~~ v8*GPA8
            gpa7 ~~ v9*gpa7
            gpa8 ~~ v9*gpa8
      
'

GPA_3b <- cfa(model_3b, data = hump,auto.var = TRUE)
summary(GPA_3b, fit.measures=TRUE, rsquare=T,standardized=T)






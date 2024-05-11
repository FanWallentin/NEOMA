#### Multiple group

library(lavaan)
library(tidyverse)
library(semPlot)


buyer <- read.csv("~/Downloads/NEOMA-LISREL/SEM-MG/BUYER.csv")
seller <- read.csv("~/Downloads/NEOMA-LISREL/SEM-MG/SELLER.csv")
buyer <- buyer[,1:15]
seller <- seller[,1:15]

### 
###
model_1 <- ' 

  #### "=~" : measurement model
            goals =~ q1 + q2 + q3
            coord =~ q28 + q29
            flex =~ q33 + q34
            uncert =~ q39 + q40
            perform =~ q44 + q47 + q48
          
   ### Regression
            coord ~ goals + uncert
            flex ~ goals + uncert
            perform ~ coord + flex
            
'

buyer1 <- cfa(model_1, data = buyer,likelihood = "wishart", estimator = "MLR")
summary(buyer1, fit.measures=TRUE, rsquare=T,standardized=T)

seller1 <- cfa(model_1, data = seller,likelihood = "wishart", estimator = "MLR")
summary(seller1, fit.measures=TRUE, rsquare=T,standardized=T)

a = cbind(buyer,1)
b = cbind(seller,0)
class(a)
names(a)[16] <- "factor"
names(b)[16] <- names(a)[16]  <-  "factor"
group <- rbind(a,b)

### Two groups

G1 <- cfa(model_1, data = group,group = "factor",likelihood = "wishart",
              estimator = "MLR", meanstructure = F)
summary(buyer1, fit.measures=TRUE, rsquare=T,standardized=T)


## G2
model_2 <- ' 

  #### "=~" : measurement model
  group: 1

            goals =~ q1 + q2 + q3
            coord =~ q28 + q29
            flex =~ q33 + q34
            uncert =~ q39 + q40
            perform =~ q44 + q47 + q48
          
   ### Regression
            coord ~ goals + uncert
            flex ~ goals + uncert
            perform ~ coord + flex
  
  group: 2
  
            goals =~ q1 + q2 + q3
            coord =~ q28 + q29
            flex =~ q33 + q34
            uncert =~ q39 + q40
            perform =~ q44 + q47 + q48

            
'

G2 <- cfa(model_2, data = group,group = "factor",likelihood = "wishart",
          estimator = "MLR", meanstructure = F)
summary(G2, fit.measures=TRUE, rsquare=T,standardized=T)


















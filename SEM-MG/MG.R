#### Multiple group

library(lavaan)
library(tidyverse)
library(semPlot)

urlfile_1="https://raw.github.com/FanWallentin/NEOMA/main/SEM-MG/BUYER.csv"
urlfile_2="https://raw.github.com/FanWallentin/NEOMA/main/SEM-MG/SELLER.csv"

buyer <- read.csv(urlfile_1)
seller <- read.csv(urlfile_2)
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

buyer1 <- cfa(model_1, data = buyer,likelihood = "wishart", estimator = "WLSMV",ordered = T)
summary(buyer1, fit.measures=TRUE, rsquare=T,standardized=T)

seller1 <- cfa(model_1, data = seller,likelihood = "wishart", estimator = "WLSMV",ordered = T)
summary(seller1, fit.measures=TRUE, rsquare=T,standardized=T)

buyer = cbind(buyer,1)
seller = cbind(seller,0)
names(seller)[16] <- names(buyer)[16]  <-  "Id"
group <- rbind(buyer,seller)

### Two groups

MG1 <- cfa(model_1, data = group,group = "Id",likelihood = "wishart",
              estimator = "WLSMV", meanstructure = F, ordered=T,
          group.equal = c("loadings","lv.covariances"))
summary(MG1, fit.measures=TRUE, rsquare=T,standardized=T)
#standardizedSolution(MG1)

## G2

MG2 <- cfa(model_1, data = group,group = "Id",likelihood = "wishart",
          estimator = "WLSMV", meanstructure = F, ordered=T,
          group.equal = c("loadings","lv.covariances","regressions"))
summary(MG2, fit.measures=TRUE, rsquare=T,standardized=T)
#standardizedSolution(MG2)

















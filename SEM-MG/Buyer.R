library(lavaan)
library(tidyverse)
library(semPlot)

urlfile_1="https://raw.github.com/FanWallentin/NEOMA/main/SEM-MG/BUYER.csv"
urlfile_2="https://raw.github.com/FanWallentin/NEOMA/main/SEM-MG/SELLER.csv"

buyer <- read.csv(urlfile_1)
buyer <- buyer[,1:15]


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

buyer1 <- cfa(model_1, data = buyer, std.lv=T,
              likelihood = "wishart", estimator = "WLSMV",ordered = T)

summary(buyer1, fit.measures=TRUE, rsquare=T,standardized=T)
standardizedSolution(buyer1)

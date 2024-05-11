library(lavaan)
lower_BA <- '
             281.349
             184.219 182.821
             216.739 171.699 283.289
             198.376 153.201 208.837 246.069

'

mean_BA <- c(262.236, 258.788, 275.630, 269.075)

lower_BNA <- '
              174.485
              134.468 161.869
              129.840 118.836 228.449
              102.194  97.767 136.058 180.460

'
mean_BNA <- c(248.675, 246.896, 258.546, 253.349)

cov_BA = getCov(lower_BA,names = c("READING5", "WRITING5", "READING7", "WRITING7"))
cov_BNA = getCov(lower_BNA,names = c("READING5", "WRITING5", "READING7", "WRITING7"))


######## STEPE

model_E <- '
          Verbal5 =~ READING5+WRITING5
          Verbal7 =~ READING7+WRITING7
          
'


fit_E <- lavaan::cfa(model_E, sample.cov = list(cov_BA,cov_BNA), sample.nobs = c(373,249),
                     sample.mean = list(mean_BA,mean_BNA),likelihood = "wishart",
                     group.equal = c("intercepts","loadings"))

summary(fit_E, standardized=TRUE,rsquare=T)











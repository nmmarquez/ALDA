## ----loadLibraries, warning=FALSE, message=FALSE-------------------------
rm(list=ls())
library(nlme)
options(digits=3) # keep the printed output narrow

## ----attempt1, cache=TRUE, error=TRUE------------------------------------
# change the path for your own computer!
unemployment <- read.table("~/Downloads/unemployment_pp.csv",
                       header=TRUE,
                       sep=",")
head(unemployment, 10)

# How many observations per subject?
length(unique(unemployment$id))
nrow(unemployment)
table(table(unemployment$id))

# Let's build Model D
attempt_1 <- lme(fixed= cesd ~ 1 + unemp*months,
                  data=unemployment,
                  random= ~ 1 + unemp*months | id,
                  method="ML")

## ----attempt2, cache=TRUE, error=TRUE------------------------------------
attempt_2 <- lme(fixed= cesd ~ 1 + unemp + unemp:months,
                  data=unemployment,
                  random= ~ 1 + unemp + unemp:months | id,
                  method="ML",
                  control=lmeControl(maxIter = 5, msMaxIter = 5, niterEM = 5))

## ----attempt3, cache=TRUE, error=TRUE------------------------------------
attempt_3 <- lme(fixed= cesd ~ 1 + unemp + unemp:months,
                  data=unemployment,
                  random= ~ 1 + unemp + unemp:months | id,
                  method="ML",
                  control = lmeControl(maxIter=500)) # default is 50

## ----attempt4, cache=TRUE, error=TRUE------------------------------------
attempt_4 <- lme(fixed= cesd ~ 1 + unemp + unemp:months,
                  data=unemployment,
                  random= ~ 1 + unemp + unemp:months | id,
                  method="ML",
                 control = lmeControl(maxIter=500,
                                      msVerbose=TRUE))

## ----attempt5, cache=TRUE, error=TRUE------------------------------------
attempt_5 <- lme(fixed= cesd ~ 1 + unemp + unemp:months,
                  data=unemployment,
                  random= ~ 1 + unemp + unemp:months | id,
                  method="REML",
                 control = lmeControl(maxIter=500, # default is 50
                                      msMaxIter=500, # default is 50 
                                      msVerbose=5,
                                      msMaxEval=500,
                                      niterEM=100))

######
# Practice!
# Find the default of the `msMaxEval` option. Try raising this higher (by 50 or 100) 
# until you get another error message, numbered (7).
######
ms_max_eval_value <- 300 #[doing this in class]
attempt_6 <- lme(fixed= cesd ~ 1 + unemp + unemp:months,
                  data=unemployment,
                  random= ~ 1 + unemp + unemp:months | id,
                  method="ML",
                 control = lmeControl(maxIter=500,
                                      msMaxIter=500,
                                      msMaxEval=ms_max_eval_value,
                                      msVerbose=10))

## ----attempt7, cache=TRUE, error=TRUE------------------------------------
attempt_7 <- lme(fixed= cesd ~ 1 + unemp + unemp:months,
                  data=unemployment,
                  random= ~ 1 + unemp + unemp:months | id,
                  method="ML",
                 control = lmeControl(maxIter=500,
                                      msMaxIter=500,
                                      msMaxEval=500,
                                      sing.tol=1e-20,
                                      msVerbose=10))
summary(attempt_7)

######
# Practice!
# What is a simpler alternative model we can build to the tortured Model D? 
# Build this one and compare the two models using the appropriate metrics.
######

attempt_8 <- lme(fixed= cesd ~ 1 + unemp + unemp:months,
                 data=unemployment,
                 random= ~ 1 + unemp:months | id,
                 method="ML")

anova(attempt_7, attempt_8)

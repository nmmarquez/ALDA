rm(list=ls())
library(dplyr)
library(lme4)
library(MASS)
library(lmtest)
library(ggplot2)
library(boot)
library(minqa)
library(numDeriv)
library(optimx)

# load in data and construct some variables 
summary(DF <- read.csv("./merged_data.csv") %>% 
            mutate(yearM=(year-max(year)) / 10, logitwp=logit(white_prop + .01)))

testIDs <- table(DF$ID)[table(DF$ID) >= 5] %>% names %>% as.numeric

DF2 <- DF %>% filter(ID %in% testIDs)

# alter the control a bit
mmCtrl <- glmerControl(
    #optimizer = "nloptwrap",
    check.conv.grad = .makeCC("warning", tol = .001, relTol = NULL)
)

# create the formulas to use in model testing
ffsimple <- formula(uc_addmitted ~ 1 + logitwp + yearM + offset(log(count)))
ffintact <- update(ffsimple, ~ . + logitwp*yearM)
ffranint <- update(ffintact, ~ . + (1|ID))

summary(m1_sc <- glmer(ffranint, data=DF2, family=poisson, control=mmCtrl, nAGQ=0))
summary(m1_sc <- glmer(ffranint, data=DF2, family=poisson, control=mmCtrl, nAGQ=1))

derivs1 <- m1_sc@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
dd <- update(m1_sc,devFunOnly=TRUE)
pars <- unlist(getME(m1_sc,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))

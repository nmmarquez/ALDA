rm(list=ls())
library(dplyr)
library(lme4)
library(MASS)
library(lmtest)
library(ggplot2)
library(boot)
library(minqa)

# load in data and construct some variables important to rescale year for
# model fitting otherwise we get gradient issues
summary(DF <- read.csv("./merged_data.csv") %>% 
    mutate(yearM=(year-max(year)) / 10, logitwp=logit(white_prop + .01)))

# alter the control a bit
mmCtrl <- glmerControl(
    # default tol is .001 which is usually fine but the iterative process of
    # the nb model sometime spits out warnings which we can avoid by increasing
    # the value just a bit
    check.conv.grad = .makeCC("warning", tol = .005, relTol = NULL)
)

# create the formulas to use in model testing
ffsimple <- formula(uc_addmitted ~ 1 + logitwp + yearM + offset(log(count)))
ffintact <- update(ffsimple, ~ . + logitwp*yearM)
ffranint <- update(ffintact, ~ . + (1|ID))
ffran <- update(ffintact, ~ . + (1 + yearM|ID))
ffranrwp <- formula(
    uc_addmitted ~ 1 + white_prop + yearM + offset(log(count)) +
        white_prop*yearM + (1 + yearM| ID))

# check out the simple model for need for over dispersion
summary(simpPois <- glm(ffsimple, data=DF, family=poisson()))
summary(simpNB <- glm.nb(ffsimple, data=DF))
lrtest(simpPois, simpNB) # overdispersion adds a lot to the models validity

# add an interaction term and repeat
summary(intactPois <- glm(ffintact, data=DF, family=poisson))
summary(intactNB <- glm.nb(ffintact, data=DF))
lrtest(intactPois, intactNB)


# add random effects for intercept 
summary(ranintPois <- glmer(ffranint, data=DF, family=poisson, control=mmCtrl))
summary(ranintNB <- glmer.nb(ffranint, data=DF, control=mmCtrl))
lrtest(ranintPois, ranintNB)

# add random effects for intercept and slope 
summary(ranPois <- glmer(ffran, data=DF, family=poisson, control=mmCtrl))
summary(ranNB <- glmer.nb(ffran, data=DF, control=mmCtrl))
summary(ranNB2 <- glmer.nb(ffranrwp, data=DF, control=mmCtrl))
lrtest(ranintNB, ranNB)
lrtest(ranNB, ranNB2)

groupPredDF2 <- expand.grid(year=1994:2015, white_prop=c(.7, .3), count=100) %>%
    mutate(yearM=year-max(DF$year)) %>%
    mutate(demog=ifelse(white_prop > .5, "High White", "Low White")) %>%
    mutate(pred=predict(ranNB2, newdata=., re.form=NA))

groupPredDF <- expand.grid(year=1994:2015, white_prop=c(.7, .3), count=1000) %>%
    mutate(yearM=year-max(DF$year), logitwp=logit(white_prop + .01)) %>%
    mutate(demog=ifelse(white_prop > .5, "High White", "Low White")) %>%
    mutate(pred=predict(ranNB, newdata=., re.form=NA))

ggplot(groupPredDF, aes(x=year, y=pred, group=demog, color=demog)) + 
    geom_line()
ggplot(groupPredDF2, aes(x=year, y=pred, group=demog, color=demog)) + 
    geom_line()

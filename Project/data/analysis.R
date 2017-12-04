rm(list=ls())
library(dplyr)
library(lme4)
library(MASS)
library(lmtest)
library(ggplot2)
library(boot)

# load in data and construct some variables important to rescale year for
# model fitting otherwise we get gradient issues
summary(DF <- read.csv("./merged_data.csv") %>% 
    mutate(yearM=(year-max(year)) / 10, accept_rate=uc_addmitted / count))

# alter the control a bit
mmCtrl <- glmerControl(
    # default tol is .001 which is usually fine but the iterative process of
    # the nb model sometime spits out warnings which we can avoid by increasing
    # the value just a bit
    check.conv.grad = .makeCC("warning", tol = .008, relTol = NULL)
)

# create the formulas to use in model testing
ffsimple <- formula(
    uc_addmitted ~ 1 + white_prop + yearM + offset(log(count)))
ffintact <- update(ffsimple, ~ . + white_prop*yearM)
ffranint <- update(ffintact, ~ . + (1|ID))
ffran <- update(ffintact, ~ . + (1 + yearM|ID))

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
lrtest(ranPois, ranNB)
lrtest(ranintPois, ranPois)

## now that we have our main fit lets expand a bit and look at other rates
summary(ranNBcapp <- glmer.nb(
    uc_applied ~ white_prop + yearM + (1 + yearM | ID) + white_prop:yearM + 
        offset(log(count)), data=DF, control=mmCtrl))

summary(ranNBappadm <- glmer.nb(
    uc_addmitted ~ white_prop + yearM + (1 + yearM | ID) + white_prop:yearM + 
        offset(log(uc_applied)), data=DF, control=mmCtrl))

# Doesnt look like there is enough overdispersion in this model so I am going to
# switch to the poisson model for this

summary(ranPappadm <- glmer(
    uc_addmitted ~ white_prop + yearM + (1 + yearM | ID) + white_prop:yearM + 
        offset(log(uc_applied)), data=DF, control=mmCtrl, family=poisson))

groupPredDF <- expand.grid(year=1994:2015, white_prop=c(.7, .3)) %>%
    mutate(yearM=(year-max(DF$year))/10, uc_applied=1, count=1) %>%
    mutate(demog=ifelse(white_prop > .5, "High White", "Low White")) %>%
    mutate(predcadm=exp(predict(ranNB, newdata=., re.form=NA))) %>% 
    mutate(predcapp=exp(predict(ranNBcapp, newdata=., re.form=NA))) %>%
    mutate(predappadm=exp(predict(ranPappadm, newdata=., re.form=NA)))

png("./plots/avgpredcadm.png", width=600)
ggplot(groupPredDF, aes(x=year, y=predcadm*1000, group=demog, color=demog)) + 
    geom_line() + 
    labs(x="Year", y="Rate of Senior Students Admitted to UC (per 1000)",
         title="Total Hispanic Student Rate of Acceptance by School Demography")  + 
    scale_color_discrete(
        name="Demography", 
        labels=c("High White\n(p=.7)", "Low White\n(p=.3)"))
dev.off()

png("./plots/avgpredcapp.png", width=600)
ggplot(groupPredDF, aes(x=year, y=predcapp*1000, group=demog, color=demog)) + 
    geom_line() + 
    labs(x="Year", y="Rate of Students Applying to UC (per 1000)",
         title="Hispanic Student Rate of Application by School Demography") + 
    scale_color_discrete(
        name="Demography", 
        labels=c("High White\n(p=.7)", "Low White\n(p=.3)"))
dev.off()

png("./plots/avgpredappadm.png", width=600)
ggplot(groupPredDF, aes(x=year, y=predappadm*1000, group=demog, color=demog)) + 
    geom_line() + 
    labs(x="Year", y="Rate of Applying Students Admitted to UC (per 1000)",
         title="Applied Hispanic Student Rate of Acceptance by School Demography") + 
    scale_color_discrete(
        name="Demography", 
        labels=c("High White\n(p=.7)", "Low White\n(p=.3)"))
dev.off()

save(DF, ranNB, ranNBcapp, ranPappadm, file="results.Rdata")
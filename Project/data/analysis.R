rm(list=ls())
library(lme4)
library(MASS)
library(dplyr)
library(lmtest)
library(ggplot2)
library(boot)
library(knitr)
library(pander)

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
funcform <- list("0"=list(
    formula(uc_addmitted ~ 1 + yearM * white_prop + offset(log(count))),
    formula(uc_addmitted ~ 1 + yearM * hisp_prop + offset(log(count))),
    formula(uc_addmitted ~ 1 + yearM * white_prop + offset(log(uc_applied))),
    formula(uc_addmitted ~ 1 + yearM * hisp_prop + offset(log(uc_applied))),
    formula(uc_applied ~ 1 + yearM * white_prop + offset(log(count))),
    formula(uc_applied ~ 1 + yearM * hisp_prop + offset(log(count)))))

funcform[["1"]] <- lapply(funcform[["0"]], function(x) update(x, ~ . + (1|ID)))
funcform[["2"]] <- lapply(funcform[["0"]], function(x) 
    update(x, ~ . + (1 + yearM|ID)))

models <- list()

funcsR <- list("P"=function(x) glmer(x, data=DF, family=poisson, control=mmCtrl), 
               "N"=function(x) glmer.nb(x, data=DF, control=mmCtrl))
funcsF <- list("P"=function(x) glm(x, data=DF, family=poisson), 
               "N"=function(x) glm.nb(x, data=DF))

for(k in 1:6){
    for(d in c("P", "N")){
        for(l in as.character(0:2)){
            f_ <- ifelse(as.numeric(l) > 0, funcsR[[d]], funcsF[[d]])
            model_run <- f_(funcform[[l]][[k]])
            print(summary(model_run))
            models[[paste0("Model_", k,l,d)]] <- model_run
        }
    }
}

save(DF, models, file="model_results.Rdata")


for(i in 1:6){
    # Model i comparisons
    anova(models[[paste0("Model_", i, "1P")]], 
          models[[paste0("Model_", i, "0P")]], 
          models[[paste0("Model_", i, "2P")]]) %>%
        as.data.frame() %>% `row.names<-`(NULL) %>% 
        mutate(Model=paste0("Model_",i, 0:2, "P")) %>%
        select(Model, Df, AIC, BIC, Chisq, `Pr(>Chisq)`) %>% pander
    anova(models[[paste0("Model_", i, "2N")]], 
          models[[paste0("Model_", i, "2P")]]) %>%
        as.data.frame() %>% `row.names<-`(NULL) %>% 
        mutate(Model=paste0("Model_",i, "2", c("P", "N"))) %>%
        select(Model, Df, AIC, BIC, Chisq, `Pr(>Chisq)`) %>% pander
}

best_models <- paste0("Model_", 1:6, 2, c("N", "N", "P", "P", "N", "N"))

extract_params <- function(mod){
    fixed <- fixef(mod) 
    fixErr <- summary(mod)$coefficients[,"Std. Error"]
    stars <- ifelse(summary(mod)$coefficients[,"Pr(>|z|)"] < .05, "**", "")
    fixL <- (fixed - 1.96 * fixErr) %>% round(digits=2)
    fixH <- (fixed + 1.96 * fixErr) %>% round(digits=2)
    fixed <- fixed %>% round(digits=2)
    fixstr <- paste0(fixed, stars, "(", fixL, ",", fixH, ")")
    REvars <- (VarCorr(mod)$ID %>% attributes)$stddev**2 %>% round(digits=2)
    rho <- ((VarCorr(mod)$ID %>% attributes)$correlation)[1,2] %>% round(., 2)
    psi <- NA
    if(grepl("Negative", summary(mod)$family)){
        psi <- gsub("[^0-9|^.]", "", summary(mod)$family) %>% as.numeric %>% 
            round(digits=2)
    }
    c(fixstr, REvars, rho, psi) %>% `names<-`(NULL)
}

sapply(models[best_models], extract_params) %>% t %>% as.data.frame %>% pander

groupPredDF <- expand.grid(year=1994:2015, white_prop=c(.7, .3)) %>%
    mutate(yearM=(year-max(DF$year))/10, uc_applied=1, count=1) %>%
    mutate(hisp_prop=1-white_prop) %>%
    mutate(demog=ifelse(hisp_prop > .5, "High Hisp", "Low Hisp")) %>%
    mutate(demogh=ifelse(white_prop > .5, "High White", "Low White")) %>%
    mutate(predcadm=exp(predict(models[["Model_12N"]], newdata=., re.form=NA))) %>% 
    mutate(predcapp=exp(predict(models[["Model_52N"]], newdata=., re.form=NA))) %>%
    mutate(predappadm=exp(predict(models[["Model_32P"]], newdata=., re.form=NA))) %>%
    mutate(hisppredcadm=exp(predict(models[["Model_22N"]], newdata=., re.form=NA))) %>% 
    mutate(hisppredcapp=exp(predict(models[["Model_62N"]], newdata=., re.form=NA))) %>%
    mutate(hisppredappadm=exp(predict(models[["Model_42P"]], newdata=., re.form=NA)))

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

png("./plots/havgpredcadm.png", width=600)
ggplot(groupPredDF, aes(x=year, y=hisppredcadm*1000, group=demogh, color=demogh)) + 
    geom_line() + 
    labs(x="Year", y="Rate of Senior Students Admitted to UC (per 1000)",
         title="Total Hispanic Student Rate of Acceptance by School Demography")  + 
    scale_color_manual(
        name="Demography", 
        labels=c("Low Hispanic\n(p=.3)", "High Hispanic\n(p=.7)"),
        values=c("green", "purple"))
dev.off()

png("./plots/havgpredcapp.png", width=600)
ggplot(groupPredDF, aes(x=year, y=hisppredcapp*1000, group=demogh, color=demogh)) + 
    geom_line() + 
    labs(x="Year", y="Rate of Students Applying to UC (per 1000)",
         title="Hispanic Student Rate of Application by School Demography")  + 
    scale_color_manual(
        name="Demography", 
        labels=c("Low Hispanic\n(p=.3)", "High Hispanic\n(p=.7)"),
        values=c("green", "purple"))
dev.off()

png("./plots/havgpredappadm.png", width=600)
ggplot(groupPredDF, aes(x=year, y=hisppredappadm*1000, group=demogh, color=demogh)) + 
    geom_line() + 
    labs(x="Year", y="Rate of Applying Students Admitted to UC (per 1000)",
         title="Applied Hispanic Student Rate of Acceptance by School Demography") + 
    scale_color_manual(
        name="Demography", 
        labels=c("Low Hispanic\n(p=.3)", "High Hispanic\n(p=.7)"),
        values=c("green", "purple"))
dev.off()

save(DF, ranNB, ranNBcapp, ranPappadm, file="results.Rdata")

rm(list=ls())

library(tidyverse)
library(RecordLinkage)
library(pander)
library(readxl)
library(stringr)
library(lme4)
library(INLA)
setwd("~/Documents/Classes/ALDA/Project/data/")

DF <- read.csv("./merged_data.csv") %>% 
    mutate(yearM=(year-max(year)) / 10, accept_rate=uc_addmitted / count) %>%
    select(-demo_comp) %>%
    mutate(ID2=ID) %>%
    na.exclude

formula1a <- uc_addmitted ~ white_prop + hisp_prop + yearM + 
    white_prop * hisp_prop * yearM + urban + log(mhi) + log(spending) +
    f(ID, model="iid") +
    f(ID2, year, model="iid")

result1a <- inla(formula1a, data=DF, family="poisson", E=DF$count)
summary(result1a)

formula2a <- uc_addmitted ~ white_prop + hisp_prop + yearM + 
    white_prop * hisp_prop * yearM + urban + log(spending) +
    f(ID, model="iid") +
    f(ID2, year, model="iid")

result2a <- inla(formula2a, data=DF, family="poisson", E=DF$count)
summary(result2a)

formula3a <- uc_addmitted ~ white_prop + hisp_prop + yearM + 
    white_prop * hisp_prop * yearM + urban + log(spending) +
    f(ID, model="iid")

result3a <- inla(formula3a, data=DF, family="poisson", E=DF$count)
summary(result3a)

formula4a <- uc_addmitted ~ white_prop + hisp_prop + yearM + 
    white_prop * hisp_prop * yearM + urban + log(spending)

result4a <- inla(formula4a, data=DF, family="poisson", E=DF$count)
summary(result4a)

formula5a <- uc_addmitted ~ white_prop + hisp_prop + yearM + log(spending) +
    urban + white_prop * yearM + hisp_prop * yearM +
    f(ID, model="iid")

result5a <- inla(formula5a, data=DF, family="poisson", E=DF$count)
summary(result5a)

# Applied admitted schema
result1b <- inla(formula1a, data=DF, family="poisson", E=DF$uc_applied)
summary(result1b)

result2b <- inla(formula2a, data=DF, family="poisson", E=DF$uc_applied)
summary(result2b)

result3b <- inla(formula3a, data=DF, family="poisson", E=DF$uc_applied)
summary(result3b)

result4b <- inla(formula4a, data=DF, family="poisson", E=DF$uc_applied)
summary(result4b)

result5b <- inla(formula5a, data=DF, family="poisson", E=DF$uc_applied)
summary(result5b)

# Count applied schema
formula1c <- uc_applied ~ white_prop + hisp_prop + yearM + 
    white_prop * hisp_prop * yearM + urban + log(mhi) + log(spending) +
    f(ID, model="iid") +
    f(ID2, year, model="iid")

result1c <- inla(formula1a, data=DF, family="poisson", E=DF$count)
summary(result1c)

formula2c <- uc_applied ~ white_prop + hisp_prop + yearM + 
    white_prop * hisp_prop * yearM + urban + log(spending) +
    f(ID, model="iid") +
    f(ID2, year, model="iid")

result2c <- inla(formula2a, data=DF, family="poisson", E=DF$count)
summary(result2c)

formula3c <- uc_applied ~ white_prop + hisp_prop + yearM + 
    white_prop * hisp_prop * yearM + urban + log(spending) +
    f(ID, model="iid")

result3c <- inla(formula3c, data=DF, family="poisson", E=DF$count)
summary(result3c)

formula4c <- uc_applied ~ white_prop + hisp_prop + yearM + 
    white_prop * hisp_prop * yearM + urban + log(spending)

result4c <- inla(formula4c, data=DF, family="poisson", E=DF$count)
summary(result4c)

formula5c <- uc_applied ~ white_prop + hisp_prop + yearM + log(spending) +
    urban + white_prop * yearM + hisp_prop * yearM +
    f(ID, model="iid")

result5c <- inla(formula5c, data=DF, family="poisson", E=DF$count)
summary(result5c)

modelsA <-list(result1a, result2a, result3a, result4a, result5a)
modelsB <-list(result1b, result2b, result3b, result4b, result5b)
modelsC <-list(result1c, result2c, result3c, result4c, result5c)

extract_cov <- function(model, cov, model_name){
    cols_ <- c("mean", "0.025quant", "0.975quant")
    DF <- model$summary.fixed[cov, cols_] %>% as.data.frame
    row.names(DF) <- NULL
    colnames(DF) <- c("mean", "lwr", "upr")
    DF$covariate <- cov
    DF$model <- model_name
    DF
}

model_types <- factor(c(
    "Adding MHI and Random Slopes and Intercepts",
    "Adding Random Slopes and Intercepts",
    "Adding Random Intercepts",
    "Base Model",
    "Random Intercepts and No Diversity Index"),
    levels=c(
        "Base Model",
        "Random Intercepts and No Diversity Index",
        "Adding Random Intercepts",
        "Adding Random Slopes and Intercepts",
        "Adding MHI and Random Slopes and Intercepts"
        ))

extractFocusCovs <- function(modelList){
    whiteCOVA <- bind_rows(lapply(1:5, function(i){
        extract_cov(modelList[[i]], "white_prop", model_types[i])
    }))
    
    hispCOVA <- bind_rows(lapply(1:5, function(i){
        extract_cov(modelList[[i]], "hisp_prop", model_types[i])
    }))
    
    diffCovA <- exp(whiteCOVA[,1:3] - hispCOVA[,1:3]) %>% 
        mutate(covariate="White Hispanic Difference", model=model_types)
    
    whiteYCOVA <- bind_rows(lapply(1:5, function(i){
        extract_cov(modelList[[i]], "white_prop:yearM", model_types[i])
    }))
    
    hispYCOVA <- bind_rows(lapply(1:5, function(i){
        extract_cov(modelList[[i]], "hisp_prop:yearM", model_types[i])
    }))
    
    diffYCOVA <- exp(whiteYCOVA[,1:3] - hispYCOVA[,1:3]) %>% 
        mutate(covariate="White Hispanic Difference Over Time", 
               model=model_types)
    
    divCOVA <- bind_rows(lapply(1:4, function(i){
        extract_cov(modelList[[i]], "white_prop:hisp_prop", 
                    model_types[i])})) %>%
        mutate(mean=exp(mean), lwr=exp(lwr), upr=exp(upr)) %>% 
        mutate(covariate="Diversity Index")
    
    divYCOVA <- bind_rows(lapply(1:4, function(i){
        extract_cov(modelList[[i]], "white_prop:hisp_prop:yearM", 
                    model_types[i])})) %>%
        mutate(mean=exp(mean), lwr=exp(lwr), upr=exp(upr)) %>% 
        mutate(covariate="Diversity Index Over Time")
    bind_rows(list(diffCovA, diffYCOVA, divCOVA, divYCOVA))
}


extractFocusCovs(modelsA) %>% 
    ggplot(aes(x=model, y=mean, ymin=lwr, ymax=upr)) + geom_pointrange() + 
    geom_hline(yintercept=1, linetype=2) + 
    facet_wrap(~covariate, scales="free_x") +
    coord_flip() + 
    theme_classic() +
    theme(axis.line.y = element_blank()) +
    labs(x="", y="", title="Parameter Estimates For Count Accepted Model")

extractFocusCovs(modelsB) %>% 
    ggplot(aes(x=model, y=mean, ymin=lwr, ymax=upr)) + geom_pointrange() + 
    geom_hline(yintercept=1, linetype=2) + 
    facet_wrap(~covariate, scales="free_x") +
    coord_flip() + 
    theme_classic() +
    theme(axis.line.y = element_blank()) +
    labs(x="", y="", title="Parameter Estimates For Applied Accepted Model")

extractFocusCovs(modelsC) %>% 
    ggplot(aes(x=model, y=mean, ymin=lwr, ymax=upr)) + geom_pointrange() + 
    geom_hline(yintercept=1, linetype=2) + 
    facet_wrap(~covariate, scales="free_x") +
    coord_flip() + 
    theme_classic() +
    theme(axis.line.y = element_blank()) +
    labs(x="", y="", title="Parameter Estimates For Count Applied Model")

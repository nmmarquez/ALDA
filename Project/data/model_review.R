rm(list=ls())
library(MASS)
library(dplyr)
library(tidyr)
library(lme4)
library(lmtest)
library(ggplot2)
library(boot)
library(mvtnorm)

load(file="./results.Rdata")

DF <- DF %>% 
    mutate(l1pred=predict(ranNB, type="response"), 
           l1resid=uc_addmitted - l1pred,
           l1residstd = l1resid/sd(l1resid),
           l0pred=predict(ranNB, type="response"), 
           l0resid=uc_addmitted - l0pred,
           l0residstd = l0resid/sd(l0resid))
head(DF)

ggplot(data=DF, aes(sample=l1residstd)) +
    stat_qq() +
    theme_bw() +
    xlab("Theoretical normal quantiles") +
    ylab("Standardized level-1 residuals") +
    ggtitle("Q-Q plot of standardized level-1 residuals") +
    # add an identity line y=x to compare equality of distributions
    geom_abline(intercept=0, slope=1)

png("./plots/temporalresiduals.png", width=600)
ggplot(data=DF, aes(x=year, y=l1residstd)) +
    # adding some horizontal jittering so we can see overplotting
    geom_point(position=position_jitter(width=0.1, height=0)) +
    # horizontal reference line
    geom_hline(yintercept=0) +
    theme_bw() +
    xlab("Year") +
    ylab("Standardized level-1 residuals") +
    ggtitle("Level-1 residuals by time")
dev.off()

png("./plots/model1RE.png", width=600)
ranef(ranNB)[[1]] %>%  
    mutate(`(Intercept)`=(`(Intercept)` - mean(`(Intercept)`)) / sd(`(Intercept)`),
           yearM=(yearM - mean(yearM)) / sd(yearM)) %>%
    gather(key=effect, value=estimate, `(Intercept)`, yearM) %>%
    ggplot(aes(sample=estimate)) +
    stat_qq() +
    theme_bw() +
    # facet by effect type to separate out the two
    facet_grid(.~effect) +
    xlab("Theoretical normal quantiles") +
    ylab("Standardized random effects") +
    ggtitle("Q-Q plot of standardized random effects (All-Addmitted Model)") +
    # add an identity line y=x to compare equality of distributions
    geom_abline(intercept=0, slope=1)
dev.off()

png("./plots/model2RE.png", width=600)
ranef(ranNBcapp)[[1]] %>%  
    mutate(`(Intercept)`=(`(Intercept)` - mean(`(Intercept)`)) / sd(`(Intercept)`),
           yearM=(yearM - mean(yearM)) / sd(yearM)) %>%
    gather(key=effect, value=estimate, `(Intercept)`, yearM) %>%
    ggplot(aes(sample=estimate)) +
    stat_qq() +
    theme_bw() +
    # facet by effect type to separate out the two
    facet_grid(.~effect) +
    xlab("Theoretical normal quantiles") +
    ylab("Standardized random effects") +
    ggtitle("Q-Q plot of standardized random effects (All-Applied Model)") +
    # add an identity line y=x to compare equality of distributions
    geom_abline(intercept=0, slope=1)
dev.off()

png("./plots/model3RE.png", width=600)
ranef(ranPappadm)[[1]] %>%  
    mutate(`(Intercept)`=(`(Intercept)` - mean(`(Intercept)`)) / sd(`(Intercept)`),
           yearM=(yearM - mean(yearM)) / sd(yearM)) %>%
    gather(key=effect, value=estimate, `(Intercept)`, yearM) %>%
    ggplot(aes(sample=estimate)) +
    stat_qq() +
    theme_bw() +
    # facet by effect type to separate out the two
    facet_grid(.~effect) +
    xlab("Theoretical normal quantiles") +
    ylab("Standardized random effects") +
    ggtitle("Q-Q plot of standardized random effects (Applied-Addmitted Model)") +
    # add an identity line y=x to compare equality of distributions
    geom_abline(intercept=0, slope=1)
dev.off()

sim_results <- function(data, model){
    data_og <- data
    ognames <- names(data)
    ids <- unique(data$ID)
    ranVcov <-  summary(model)$varcor$ID[1:2,1:2]
    ranDF <- rmvnorm(length(ids), sigma=ranVcov) %>% as.data.frame %>% 
        rename(Intercept=V1, Slope=V2) %>% mutate(ID=ids)
    data <- left_join(data, ranDF, by="ID") %>% 
        mutate(predf=predict(model, newdata=., re.form=NA)) %>%
        mutate(pred= exp(predf + Intercept + Slope*yearM))
    return(data)
        
}

sim_many <- function(n, model){
    groupPred <- expand.grid(year=1994:2015, white_prop=c(.7, .3), count=1) %>%
        mutate(yearM=(year-max(DF$year)) / 10, uc_applied=1) %>%
        mutate(demog=ifelse(white_prop > .5, "High White", "Low White")) %>%
        mutate(ID=ifelse(white_prop > .5, 1, 2))
    
    groupPredSims <- lapply(1:n, function(x)
        (mutate(sim_results(groupPred, model), sim=x))) %>% bind_rows %>%
        mutate(ID = sim*100 + ID)
    
    groupPred <- groupPred %>% 
        mutate(pred=exp(predict(model, newdata=., re.form=NA)))
    
    list(sims=groupPredSims, gmean=groupPred)
}

set.seed(123)
model1 <- sim_many(100, ranNB)
model2 <- sim_many(100, ranNBcapp)
model3 <- sim_many(100, ranPappadm)

png("./plots/simspredcadm.png", width=600)
ggplot(model1$sims, aes(x=year, y=pred*1000, group=ID, color=demog)) + 
    geom_line(alpha=.3, linetype=2) + 
    geom_line(data=model1$gmean, size=2) + 
    labs(x="Year", y="Rate of Senior Students Admitted to UC (per 1000)",
         title="Total Hispanic Student Rate of Acceptance by School Demography")  + 
    scale_color_discrete(
        name="Demography", 
        labels=c("High White\n(p=.7)", "Low White\n(p=.3)"))
dev.off()

png("./plots/simspredcapp.png", width=600)
ggplot(model2$sims, aes(x=year, y=pred*1000, group=ID, color=demog)) + 
    geom_line(alpha=.3, linetype=2) + 
    geom_line(data=model2$gmean, size=2)  + 
    labs(x="Year", y="Rate of Students Applying to UC (per 1000)",
         title="Hispanic Student Rate of Application by School Demography") + 
    scale_color_discrete(
        name="Demography", 
        labels=c("High White\n(p=.7)", "Low White\n(p=.3)"))
dev.off()

png("./plots/simspredappadm.png", width=600)
ggplot(model3$sims, aes(x=year, y=pred*1000, group=ID, color=demog)) + 
    geom_line(alpha=.4, linetype=2) + 
    geom_line(data=model3$gmean, size=2) + 
    labs(x="Year", y="Rate of Applying Students Admitted to UC (per 1000)",
         title="Applied Hispanic Student Rate of Acceptance by School Demography") + 
    scale_color_discrete(
        name="Demography", 
        labels=c("High White\n(p=.7)", "Low White\n(p=.3)"))
dev.off()

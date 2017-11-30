## ----importAlc-----------------------------------------------------------
# change the path for your own computer!
rm(list=ls())
alcohol <- read.table("~/Documents/Classes/ALDA/week9/alcohol1_pp.csv",
                       header=TRUE,
                       sep=",")
head(alcohol, 10)

## ----loadLibraries, warning=FALSE, message=FALSE-------------------------
# install.packages("Amelia")
library(Amelia)
library(dplyr)
library(tidyr)
library(nlme)
library(ggplot2)
options(digits=3) # keep the printed output narrow

## ----fitFull-------------------------------------------------------------
mod_E <- lme(fixed= alcuse ~ coa + age_14*peer,
             data=alcohol,
             random= ~ 1 + age_14 | id,
             method="ML")
summary(mod_E)

sum(is.na(alcohol$alcuse))
table(table(alcohol$id))

## ----makeMCAR------------------------------------------------------------
set.seed(112515)
# sample observations to make MCAR
missing_rows_alcuse <- sample(nrow(alcohol),
                       size=round(0.15*nrow(alcohol)),
                       replace=FALSE)
# copy original data to alcohol_MCAR data
alcohol_MCAR <- alcohol
# replace values with NA for those observations
alcohol_MCAR[missing_rows_alcuse,"alcuse"] <- NA

summary(alcohol_MCAR)

## ----fitdeletion---------------------------------------------------------
# Fitting model with period-wise deletion!
mod_E_deletion <- lme(fixed= alcuse ~ coa + age_14*peer,
                      data=alcohol_MCAR,
                      random= ~ 1 + age_14 | id,
                      method="ML",
                      na.action=na.omit)
summary(mod_E_deletion)

## ------------------------------------------------------------------------
n_missing_subjects <- alcohol_MCAR %>%
    mutate(missing=is.na(alcuse)) %>%
    group_by(id) %>%
    summarize(missing_an_obs=max(missing)) %>%
    ungroup() %>%
    summarize(total_subj=n(), missing_subj=sum(missing_an_obs))
n_missing_subjects

## ----missViz-------------------------------------------------------------
missmap(alcohol_MCAR)

## ----histograms, warning=FALSE, message=FALSE----------------------------
# drop unneeded variables
alcohol_MCAR_subset <- alcohol_MCAR %>%
    select(-age, -male, -ccoa, -cpeer)
# make a long dataset so we can facet over the variables
# (drop id and age_14 since we don't need it for this)
alcohol_MCAR_long <- alcohol_MCAR_subset %>%
    select(-id, -age_14) %>%
    gather(key=Variable, value=Value)
ggplot(alcohol_MCAR_long, aes(x=Value)) +
    geom_histogram() +
    facet_wrap(~ Variable, ncol=3, scales="free") +
    theme_bw() +
    ggtitle("Distributions of variables")

## ----impute1, cache=TRUE-------------------------------------------------
impute_1 <- amelia(x=alcohol_MCAR %>%
                       # remove collinear columns first
                       select(-age, -cpeer, -ccoa),
                   m = 15,
                   cs="id",
                   ts="age_14",
                   polytime=1,
                   logs=c("alcuse", "peer"),
                   noms=c("coa", "male"),
                   empri=.01*nrow(alcohol_MCAR),
                   p2s=2) 
plot(impute_1)

####
# Practice!
####
# Now let's impute in missing values again, but use square root transformations instead of log 
# transformation. Call this `impute_2` and plot it.  Which one looks better?

impute_2 <- amelia(x=alcohol_MCAR %>%
                       # remove collinear columns first
                       select(-age, -cpeer, -ccoa),
                   m = 15,
                   cs="id",
                   ts="age_14",
                   polytime=1,
                   sqrts=c("alcuse", "peer"),
                   noms=c("coa", "male"),
                   empri=.01*nrow(alcohol_MCAR),
                   p2s=1)
plot(impute_2)

## ----impute3, cache=TRUE-------------------------------------------------
impute_3 <- amelia(x=alcohol_MCAR %>%
                       # remove collinear columns first
                       select(-age, -cpeer, -ccoa),
                   m = 15,
                   idvars="id",
                   cs="peer",
                   intercs=TRUE,
                   ts="age_14",
                   polytime=1,
                   sqrts="alcuse",
                   noms=c("coa", "male"),
                   empri=.01*nrow(alcohol_MCAR),
                   p2s=1) 
plot(impute_3)

## ----extract-------------------------------------------------------------
names(impute_2)
names(impute_2$imputations)

## ------------------------------------------------------------------------
head(impute_2[["imputations"]][[3]], 10)

## ----tscsPlot, cache=TRUE------------------------------------------------
# subjects with at least one missing alcuse value
subj_missing <- alcohol_MCAR %>%
    filter(is.na(alcuse)) %>%
    distinct(id) %>%
    select(id)

tscsPlot(impute_2,
         var="alcuse",
         # plot the first 9 subjects with missing values
         cs=subj_missing[1:9,1])
# tscsPlot uses base R plotting functions. you need to reset the grid after it's done
# to show just one plot at a time or else you might get tiny graphs later!
par(mfrow=c(1,1))

## ----overimpute----------------------------------------------------------
overimpute(impute_2, var="alcuse")

## ----looplme, cache=TRUE-------------------------------------------------
# make blank matrices to store estimates from each imputation
# we'll be fitting the same model E but to the imputed data
gamma_imp <- matrix(0,
                     nrow=impute_2$m,
                     ncol=length(fixef(mod_E)))
gamma_SE_imp <- matrix(0,
                     nrow=impute_2$m,
                     ncol=length(fixef(mod_E)))
# set column names for informative output
colnames(gamma_imp) <- colnames(gamma_SE_imp) <- names(fixef(mod_E))

# loop over each imputation in impute_2
for(i in 1:impute_2$m)   {
    temp_mod <- lme(fixed= alcuse ~ coa + age_14*peer,
             data=impute_2[["imputations"]][[i]],
             random= ~ 1 + age_14 | id,
             method="ML",
             # make sure these things converge!
             control=lmeControl(maxIter=500,
                                msMaxIter=500,
                                msMaxEval=500,
                                sing.tol=1e-20))
    # storing fitted values in rows of the matrices
    gamma_imp[i,] <- fixef(temp_mod)
    gamma_SE_imp[i,] <- summary(temp_mod)$tTable[,"Std.Error"]
}

# check out the results
head(gamma_imp)
head(gamma_SE_imp)

## ----mimeld--------------------------------------------------------------
mod_E_impute <- mi.meld(q=gamma_imp,
                        se=gamma_SE_imp,
                        byrow=TRUE)
mod_E_impute

## ------------------------------------------------------------------------
summary(mod_E)$tTable[,1:2]

## ------------------------------------------------------------------------
summary(mod_E_deletion)$tTable[,1:2]


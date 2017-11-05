## ----loadLibraries, warning=FALSE, message=FALSE-------------------------
# load useful libraries for this lab
rm(list=ls())
library(dplyr)
library(tidyr)
library(nlme)
library(ggplot2)

## ----importAlc, warning=FALSE, message=FALSE, cache=TRUE-----------------
# change the path for your own computer!
alcohol <- read.table("~/Documents/Classes/ALDA/week5/alcohol1_pp.csv",
                       header=TRUE,
                       sep=",")
head(alcohol, 10)

## ----modelA--------------------------------------------------------------
# going to use extra iterations here to demo fixing
# convergence issues (this has no problems though)
# see ?lmeControl for more options
mod_A <- lme(fixed= alcuse ~ 1,
             data=alcohol,
             random= ~ 1 | id,
             method="ML",
             control=lmeControl(maxIter=200))
summary(mod_A)

## ----modelB--------------------------------------------------------------
mod_B <- lme(fixed= alcuse ~ age_14,
             data=alcohol,
             random= ~ 1 + age_14 | id,
             method="ML")
summary(mod_B)

## ----modelC--------------------------------------------------------------
mod_C <- lme(fixed= alcuse ~ age_14*coa,
             data=alcohol,
             random= ~ 1 + age_14 | id,
             method="ML")
summary(mod_C)

## ----modelD--------------------------------------------------------------
mod_D <- lme(fixed= alcuse ~ age_14*coa + age_14*peer,
             data=alcohol,
             random= ~ 1 + age_14 | id,
             method="ML")
summary(mod_D)

## ----modelE--------------------------------------------------------------
mod_E <- lme(fixed= alcuse ~ coa + age_14*peer,
             data=alcohol,
             random= ~ 1 + age_14 | id,
             method="ML")
summary(mod_E)

## ----modelF--------------------------------------------------------------
mod_F <- lme(fixed= alcuse ~ ccoa + age_14*cpeer,
             data=alcohol,
             random= ~ 1 + age_14 | id,
             method="ML")
summary(mod_F)

## ----LRT-----------------------------------------------------------------
anova(mod_A, mod_B, mod_C, mod_D, mod_E, mod_F)

## ----LRT2----------------------------------------------------------------
# different order, different comparisons
anova(mod_C, mod_E, mod_A, mod_D, mod_F, mod_B)

## ----OLSlines, fig.height=10---------------------------------------------
# plot fitted OLS trendlines for each subject
ggplot(data=alcohol,
       aes(x=age_14, y=alcuse, group=id)) +
    geom_point() +
    # manually setting x axis breaks to keep it
    # from getting too decimal-y
    scale_x_continuous(breaks=0:2) +
    geom_line(stat="smooth", method="lm", se=FALSE) +
    facet_wrap( ~ id, ncol=8) +
    theme_bw() +
    xlab("Time (years since age 14)") +
    ylab("Alcohol use") +
    ggtitle("OLS trends for each subject\nAlcohol use data")

## ----level1resid---------------------------------------------------------
# take original data and make a new dataframe with residuals
alcohol_resid <- alcohol %>%
    # level=1 means take the residuals after accounting for the random effects
    mutate(l1_resid = resid(mod_F, level=1),
           # also include a column for standardized residuals
           l1_resid_std = l1_resid/sd(l1_resid))

## ----level1QQ------------------------------------------------------------
# note that aes takes sample= rather than x=, y=
# for doing qqplots
ggplot(data=alcohol_resid, aes(sample=l1_resid_std)) +
    stat_qq() +
    theme_bw() +
    xlab("Theoretical normal quantiles") +
    ylab("Standardized level-1 residuals") +
    ggtitle("Q-Q plot of standardized level-1 residuals") +
    # add an identity line y=x to compare equality of distributions
    geom_abline(intercept=0, slope=1)

## ----level1subject-------------------------------------------------------
# plot residuals by subject
ggplot(data=alcohol_resid, aes(x=id, y=l1_resid_std)) +
    geom_point() +
    # horizontal reference line
    geom_hline(yintercept=0) +
    theme_bw() +
    xlab("Subject ID") +
    ylab("Standardized level-1 residuals") +
    ggtitle("Level-1 residuals by subject")

## ----level1time----------------------------------------------------------
# plot residuals by time
ggplot(data=alcohol_resid, aes(x=age_14, y=l1_resid_std)) +
    # adding some horizontal jittering so we can see overplotting
    geom_point(position=position_jitter(width=0.1, height=0)) +
    # horizontal reference line
    geom_hline(yintercept=0) +
    theme_bw() +
    xlab("Time (years since age 14)") +
    ylab("Standardized level-1 residuals") +
    ggtitle("Level-1 residuals by time")

######
# Practice!
######
# Investigate potential heteroscedasticity across parental alcoholism status, 
# peer alcohol use and gender.

## ----level2resid---------------------------------------------------------
alcohol_resid_2 <- alcohol_resid %>%
    # drop the old level 1 residuals
    select(-l1_resid, -l1_resid_std) %>%
    # just keep rows at time zero
    filter(age_14==0) %>%
    # add on the random effects using ranef, make standardized
    mutate(rand_int = ranef(mod_F)[,"(Intercept)"],
           rand_slope = ranef(mod_F)[,"age_14"],
           rand_int_std = rand_int/sd(rand_int),
           rand_slope_std = rand_slope/sd(rand_slope)) %>%
    # drop the unstandardized versions
    select(-rand_int, -rand_slope)
head(alcohol_resid_2)

## ----reshape-------------------------------------------------------------
alcohol_resid_2_long <- alcohol_resid_2 %>%
    # key = name of new dimension to make (in this case, random effect type)
    # value = name of column with the quantity in it
    # rest are the columns to rotate down into the quantity column
    # any variable not mentioned is not affected
    gather(key=Effect, value=estimate_std, rand_int_std, rand_slope_std) %>%
    # make the values for the effect type more English-y
    mutate(Effect=ifelse(Effect=="rand_int_std","Intercept","Slope"))
head(alcohol_resid_2_long)

## ----level2QQint---------------------------------------------------------
ggplot(data=alcohol_resid_2_long, aes(sample=estimate_std)) +
    stat_qq() +
    theme_bw() +
    # facet by effect type to separate out the two
    facet_grid(.~Effect) +
    xlab("Theoretical normal quantiles") +
    ylab("Standardized random effects") +
    ggtitle("Q-Q plot of standardized random effects") +
    # add an identity line y=x to compare equality of distributions
    geom_abline(intercept=0, slope=1)

## ----level2subject-------------------------------------------------------
# plot residuals by subject
ggplot(data=alcohol_resid_2_long, aes(x=id, y=estimate_std)) +
    geom_point() +
    # horizontal reference line
    geom_hline(yintercept=0) +
    facet_grid(.~Effect) +
    theme_bw() +
    xlab("Subject ID") +
    ylab("Standardized level-2 residuals") +
    ggtitle("Level-2 residuals by subject")

######
# Practice!
######
# Investigate potential heteroscedasticity in level-2 residuals  across parental alcoholism 
# status, peer alcohol use and gender.

## ----allTrends, fig.height=10, warning=FALSE-----------------------------
# add predictions for fixed effect trend and random effect trend:
alcohol_all_lines <- alcohol_resid %>%
    select(-l1_resid, -l1_resid_std) %>%
    # population predictions = level 0
    # random effect predictions = level 1
    mutate(pop=predict(mod_F, newdata=., level=0),
           EB=predict(mod_F, newdata=., level=1))
    
# Let's sample out the following IDs
ids.to.select <- c(1, 6, 11, 20, 30, 50, 70, 77)

ggplot(data=alcohol_all_lines %>% filter(id %in% ids.to.select),
       aes(x=age_14, y=alcuse, group=id)) +
    geom_point() +
    scale_x_continuous(breaks=0:2) +
    # OLS lines: fit an lm to each individual trajectory
    # the color aesthetic defines the groups that appear in the legend
    # which is why it's a name and not an actual color
    geom_line(aes(color="OLS"), stat="smooth", method="lm") +
    # population lines: just the fixed effects in Model F
     # added y to the aesthetic to change from alcuse
    geom_line(aes(y=pop, color="Population")) +
    # Empirical Bayes lines: consider random effects in Model F
    geom_line(aes(y=EB, color="Empirical Bayes")) +
    facet_wrap( ~ id, ncol=2) +
    theme_bw() +
    xlab("Time (years since age 14)") +
    ylab("Alcohol use") +
    ylim(0, 4) +
    ggtitle("Comparison of trend estimates for each subject") +
    scale_color_manual(name="Trend type",
                       values=c("springgreen4", "darkgoldenrod1", "steelblue2"),
                       guide = guide_legend(direction = "horizontal")) +
    theme(legend.position=c(0.65, 0.43))


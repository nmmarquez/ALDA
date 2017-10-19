## ----loadLibraries, warning=FALSE, message=FALSE-------------------------
# load useful libraries for this lab
library(dplyr)
library(nlme)
library(pander)
panderOptions('round', 3)
panderOptions('keep.trailing.zeros', TRUE)
library(ggplot2)


## ----importTolerance, warning=FALSE, message=FALSE, cache=TRUE-----------
# change the path for your own computer!
tolerance <- read.table("./tolerance1_pp.csv",
                       header=TRUE,
                       sep=",")
# make an indicator for low/high exposure, based on being above/below the median
median(tolerance$exposure)
# high exposure is greater than median
tolerance <- tolerance %>%
    mutate(high_expo=ifelse(exposure >= median(exposure),1,0))
head(tolerance)

## ----nlme1, cache=TRUE---------------------------------------------------
tolerance_model1 <- lme(fixed= tolerance ~ high_expo*time,
                     data=tolerance,
                     random=reStruct(~ 1 + time | id, pdClass="pdSymm"),
                     method="ML")

## ----summaryLme----------------------------------------------------------
summary(tolerance_model1)

## ----getFixedEffects-----------------------------------------------------
# access fixed effects using fixef function
fixed_model1 <- fixef(tolerance_model1)
fixed_model1

## ----getFullFixedEffects-------------------------------------------------
# access full info on fixed effects with summary
fixed_full_model1 <- summary(tolerance_model1)$tTable
fixed_full_model1

## ----getRandomEffects----------------------------------------------------
# access random effects using ranef function
random_model1 <- ranef(tolerance_model1)
head(random_model1)

## ----subject569----------------------------------------------------------
random_model1["569",]

## ----getVariance---------------------------------------------------------
# access random effects using VarCorr function
var_model1 <- VarCorr(tolerance_model1)
var_model1



## ----strVarCorr----------------------------------------------------------
str(var_model1)

## ----fixBadVariance------------------------------------------------------
# patch up the numeric formatting and keep the matrix shape and names
# note: if you use as.numeric instead if storage.mode,
# the matrix will lose its shape and row/column names
storage.mode(var_model1) <- "numeric"
str(var_model1)
var_model1[2,3]/prod(var_model1[1:2,1])

###### 
# Practice 1! 
###### 
# Create tolerance_model2 by replacing high exposure with gender. 
# Run all the code we've seen so far. 
# Can you comment on the effect of gender on tolerance of deviant behavior? 
# Which variance components are high/low for tolerance_model2?

tolerance_model2 <- lme(fixed= tolerance ~ male*time,
                        data=tolerance,
                        random=reStruct(~ 1 + time | id, pdClass="pdSymm"),
                        method="ML")
summary(tolerance_model2)

## ----makePredictedDataSet------------------------------------------------
# time values we want to look at
time_values <- unique(tolerance$time)
# exposure levels we want to look at
expo_values <- unique(tolerance$high_expo)
# use expand.grid to take all combos
# of time and exposure in a data frame
model_dt <- expand.grid(time=time_values, high_expo=expo_values)
model_dt
model_dt2 <- expand.grid(time=time_values, high_expo=expo_values,
                         id=unique(tolerance$id)) 

## ----addOnPredictions----------------------------------------------------
# now use the model to make predictions at level 0 (fixed effects only)
# newdata=. means use the data piped into it
# if your model needs additional covariates besides what is on model_dt, you'll get an error
model_dt <- model_dt %>%
    # make a column for predicted tolerance
    mutate(pred_tol1=predict(tolerance_model1,
                            newdata=.,
                            level=0))

model_dt2 <- model_dt2 %>%
    # make a column for predicted tolerance
    mutate(pred_tol1=predict(tolerance_model1,
                             newdata=.,
                             level=1))

## ----plotMeans-----------------------------------------------------------
# make a prettier factor variable for exposure (nicer legend)
model_dt <- model_dt %>%
    mutate(Exposure=factor(ifelse(high_expo==1,"High","Low")))

# using the model_dt for plotting, now
# not the original tolerance data
ggplot(data=model_dt,
       aes(x=time, y=pred_tol1, group=Exposure, color=Exposure)) +
    geom_line() +
    ggtitle("Fitted mean trajectories by exposure\nto deviant behavior at age 11") +
    # the \n above means line break, use to manually wrap text
    xlab("Time (years since age 11)") +
    ylab("Average tolerance of deviant behavior") +
    ylim(1,4) # use the range of possible values so as not to distort

###### 
# Practice 2! 
###### 
# Add mean trajectories for the male and female groups in your tolerance_model2.

model2_dt <- expand.grid(time=time_values, male=0:1)
model2_dt

model2_dt <- model2_dt %>%
    # make a column for predicted tolerance
    mutate(pred_tol1=predict(tolerance_model2,
                             newdata=.,
                             level=0))

model2_dt <- model2_dt %>%
    mutate(Sex=factor(ifelse(male==1,"Male","Female")))

ggplot(data=model2_dt,
       aes(x=time, y=pred_tol1, group=Sex, color=Sex)) +
    geom_line() +
    ggtitle("Fitted mean trajectories by Sex\nto deviant behavior at age 11") +
    # the \n above means line break, use to manually wrap text
    xlab("Time (years since age 11)") +
    ylab("Average tolerance of deviant behavior") +
    ylim(1,4) # use the range of possible values so as not to distort

ggplot(data=model2_dt,
       aes(x=time, y=pred_tol1, group=Sex, color=Sex)) +
    geom_line() +
    ggtitle("Fitted mean trajectories by Group\nto deviant behavior at age 11") +
    # the \n above means line break, use to manually wrap text
    xlab("Time (years since age 11)") +
    ylab("Average tolerance of deviant behavior") +
    ylim(1,4) + # use the range of possible values so as not to distort
    geom_line(aes(x=time, y=pred_tol1, group=Exposure, color=Exposure), data=model_dt) +
    scale_colour_discrete(name = "Group")

## ---- panderNotOptimal---------------------------------------------------
pander(var_model1)

## ---- eval=FALSE---------------------------------------------------------
## | **Variance components** | | **Parameter** | **Estimate** |
## |-|-|-|-|
## | Level 1 | Within-person | $\sigma_\varepsilon$ | `r round(var_model1["Residual","StdDev"],2)` |
## | Level 2 | In initial status | $\sigma_0$ | `r round(var_model1["(Intercept)","StdDev"],2)` |
## | | In rate of change | $\sigma_1$ | `r round(var_model1["time","StdDev"],2)` |
## | | Correlation | $\rho_{01}$ | `r round(var_model1["time","Corr"],2)` |

## ----intervalPractice----------------------------------------------------
# 95% intervals for intercepts and slopes for high exposure group
mean_int_high <- fixed_model1["(Intercept)"] + 1*fixed_model1["high_expo"]
sd_int <- var_model1["(Intercept)","StdDev"]
ci_int_high <- c(mean_int_high-1.96*sd_int,
                 mean_int_high+1.96*sd_int)
ci_int_high

mean_slope_high <- fixed_model1["time"] + 1*fixed_model1["high_expo:time"]
sd_slope <- var_model1["time","StdDev"]
ci_slope_high <- c(mean_slope_high-1.96*sd_slope,
                   mean_slope_high+1.96*sd_slope)
ci_slope_high

###### 
# Practice 3! 
###### 
# Make the same two confidence intervals for your model tolerance_model2. 
# Does the confidence interval for the estimated slope for the male group 
# cross zero? How about for the female group?
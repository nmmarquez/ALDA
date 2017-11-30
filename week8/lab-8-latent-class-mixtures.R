## ----loadLibraries, warning=FALSE, message=FALSE-------------------------
# load useful libraries for this lab
library(dplyr)
library(ggplot2)
# install.packages("lcmm")
library(lcmm)
options(digits=3) # keep the printed output narrow

## ----readMontreal, cache=TRUE--------------------------------------------
# note: not a CSV! space delimited
opposition <- read.table("./Montreal.dat", header=TRUE)
head(opposition, 10)

# How many unique cases do we have? Do they all have same number of time points?
length(unique(opposition$caseid))
table(table(opposition$Age))

## ----timeChange, cache=TRUE----------------------------------------------
opposition <- opposition %>%
    mutate(Age_scaled=(Age-10)/10,
           Opposition_jitter=Opposition+rnorm(n=nrow(.), mean=0, sd=0.2))

## ----plotOpp, warning=FALSE, message=FALSE-------------------------------
set.seed(1234)
sample_subjects <- sample(unique(opposition$caseid), 25, replace=FALSE)
ggplot(data=opposition %>% filter(caseid %in% sample_subjects),
       aes(x=Age, y=Opposition, group=caseid)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    facet_wrap(~ caseid, ncol=5) +
    ggtitle("Observed opposition scores by age\nSample subjects")

## ----plotOppSmooth, warning=FALSE, message=FALSE, cache=TRUE-------------
bigger_sample <- sample(unique(opposition$caseid), 100, replace=FALSE)

ggplot(data=opposition %>% filter(caseid %in% bigger_sample),
       aes(x=Age, y=Opposition, group=caseid)) +
    geom_line(stat="smooth",
              method="loess", span=1.5, # increase span to smooth more
              se=FALSE,
              alpha=0.4) +
    theme_bw() +
    ggtitle("Smoothed opposition trajectories\nSample subjects")

## ----class1, cache=TRUE--------------------------------------------------
opp_1 <- hlme(
    fixed = Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
    subject = "caseid",
    ng = 1,
    data = opposition)

summary(opp_1)

## ----lm1-----------------------------------------------------------------
opp_1_lm <- lm(Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
               data = opposition)
summary(opp_1_lm)

## ----class1plot, warning=FALSE, message=FALSE----------------------------
opp_1_preds <- opposition %>%
  # merge on prediction for the class
  mutate(pred_1=predictY(opp_1, newdata=., var.time="Age_scaled")$pred[,1])

# show observed ?sample and predicted pop trajectory
ggplot(data=opp_1_preds %>% filter(caseid %in% bigger_sample),
       aes(x=Age, y=Opposition_jitter, group=caseid)) +
  geom_line(alpha=0.25) +
  geom_line(aes(y=pred_1),
            stat="smooth",
            # fitting cubic will recover our cubic with appropriate scaling
            formula= y ~ x + I(x^2) + I(x^3),
            method="lm",
            se=FALSE,
            alpha=0.8,
            size=2) +
  theme_bw() +
  ggtitle("One group, sample subjects") +
  ylab("Opposition score (jittered)")

## ----class2default, cache=TRUE-------------------------------------------
opp_2_init <- hlme(
    fixed = Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
    subject = "caseid",
    mixture = ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
    ng = 2,
    data = opposition)

summary(opp_2_init)

## ----opp2initplot, warning=FALSE, message=FALSE--------------------------
opp_2_init_preds <- opposition %>%
  # We have two classes, so two columns in the pred matrix from predictY
  mutate(pred_1=predictY(opp_2_init, newdata=., var.time="Age_scaled")$pred[,1],
         pred_2=predictY(opp_2_init, newdata=., var.time="Age_scaled")$pred[,2]) %>%
  # merge the data with a matrix of predicted (posterior) probabilities that each subject is in each class
  left_join(opp_2_init$pprob, by="caseid") %>%
  # report predictY's prediction for most likely class
  mutate(Prediction=ifelse(class==1,pred_1,pred_2)) %>%
  # make class a factor for plotting
  mutate(Class=factor(class))

# facet by predicted class and show predicted trajectories for subjects in bigger sample
plot2 <- ggplot(data=opp_2_init_preds %>% filter(caseid %in% bigger_sample),
                aes(x=Age, y=Opposition_jitter, group=caseid, color=Class)) +
  geom_line(alpha=0.25) +
  geom_line(aes(y=Prediction),
            stat="smooth",
            formula= y ~ x + I(x^2) + I(x^3),
            method="lm",
            se=FALSE,
            alpha=0.5,
            size=2) +
  theme_bw() +
  ggtitle("Two clusters, sample subjects") +
  ylab("Opposition score (jittered)") +
  facet_grid(~class)
plot2

## ----addInEmpMeans, warning=FALSE, message=FALSE-------------------------
# Would we get the same thing if we just fit cubic growth models separately within 
# each class?
plot2 +
  # empirical averages within groups
  geom_line(data=opp_2_init_preds, # use the whole dataset for averages
            aes(group=class),
            color="black",
            stat="smooth",
            formula= y ~ x + I(x^2) + I(x^3),
            method="lm",
            se=FALSE,
            linetype=3,
            alpha=0.7,
            size=2) +
  theme_bw() +
  ggtitle("Two clusters, sample subjects\nWith empirical averages in dotted lines")

## ----showProbs-----------------------------------------------------------
round(head(opp_2_init$pprob),3)

## ----class2, cache=TRUE--------------------------------------------------
# Initializations!
# If we don't initialize, a preliminary mixed model is built to obtain initial values
# before running the latent class mixture model
opp_2 <- hlme(fixed = Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
subject = "caseid",
mixture = ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
ng = 2,
# initial values go in the B argument:
B = c(0, # log-odds class 1 relative to 2, 0=SAME
0, 0, # intercepts
0, 2, # linear terms
0, 0, # quadratic terms
0, 0, # cubic terms
2), # residual SE
data = opposition)

# compare the results for 2 classes
summarytable(opp_2_init, opp_2)

## ----class3, cache=TRUE, warning=FALSE, message=FALSE--------------------
opp_3 <- hlme(fixed = Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
subject = "caseid",
mixture = ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
ng = 3,
B = c(0, 0, # log-odds relative to class 3
0, 3, 5, # intercepts
0, 2, 1, # linear terms
0, -0.5, -0.5, # quadratic terms
0, 0, 0, # cubic terms
2), # residual SE
data = opposition)
summary(opp_3)

opp_3_preds <- opposition %>%
# merge predictions for each class with original data
mutate(pred_1=predictY(opp_3, newdata=., var.time="Age_scaled")$pred[,1],
pred_2=predictY(opp_3, newdata=., var.time="Age_scaled")$pred[,2],
pred_3=predictY(opp_3, newdata=., var.time="Age_scaled")$pred[,3]) %>%
# merge on classifications
left_join(opp_3$pprob, by="caseid") %>%
# report prediction for most likely class
mutate(Prediction=ifelse(class==1, pred_1,
ifelse(class==2, pred_2, pred_3))) %>%
# make class a factor for plotting
mutate(Class=factor(class))

# color trajectories by class prediction and show predicted trajectory
ggplot(data=opp_3_preds %>% filter(caseid %in% bigger_sample),
aes(x=Age, y=Opposition_jitter, group=caseid, color=Class)) +
geom_line(alpha=0.25) +
geom_line(aes(y=Prediction),
stat="smooth",
formula= y ~ x + I(x^2) + I(x^3),
method="lm",
se=FALSE,
alpha=0.8,
size=2) +
theme_bw() +
ggtitle("Three clusters, sample subjects") +
ylab("Opposition score (jittered)") +
facet_grid(~class)

summarytable(opp_1, opp_2, opp_3)

######
# Practice!
######
# Try changing the initialization in model `opp_3` - choose whatever values seem 
# sensible. Rerun the code to plot the three groups again. Compare with the output in 
# the original lab sheet. Has anything changed?

## ----class4, cache=TRUE, warning=FALSE, message=FALSE--------------------
opp_4 <- hlme(fixed = Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
              subject = "caseid",
              mixture = ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
              ng = 4,
              B = c(0, 0, 0, # log odds relative to class 4
                    1, 3, 5, 9, # intercepts
                    0, -1, -1, -1, # linear
                    0, 0, 0, 0, # quadratic
                    0, 0, 0, 0, # cubic
                    1), # residual SE
              data = opposition)
summary(opp_4)

opp_4_preds <- opposition %>%
  # merge predictions for each class with original data
  mutate(pred_1=predictY(opp_4, newdata=., var.time="Age_scaled")$pred[,1],
         pred_2=predictY(opp_4, newdata=., var.time="Age_scaled")$pred[,2],
         pred_3=predictY(opp_4, newdata=., var.time="Age_scaled")$pred[,3],
         pred_4=predictY(opp_4, newdata=., var.time="Age_scaled")$pred[,4]) %>%
  # merge on classifications
  left_join(opp_4$pprob, by="caseid") %>%
  # report prediction for most likely class
  mutate(Prediction=ifelse(class==1, pred_1,
                           ifelse(class==2, pred_2,
                                  ifelse(class==3, pred_3, pred_4)))) %>%
  # make class a factor for plotting
  mutate(Class=factor(class))

# color trajectories by class prediction and show predicted trajectory
ggplot(data=opp_4_preds %>% filter(caseid %in% bigger_sample),
       aes(x=Age, y=Opposition_jitter, group=caseid, color=Class)) +
  geom_line(alpha=0.25) +
  geom_line(aes(y=Prediction),
            stat="smooth",
            formula= y ~ x + I(x^2) + I(x^3),
            method="lm",
            se=FALSE,
            alpha=0.8,
            size=2) +
  theme_bw() +
  ggtitle("Four clusters, sample subjects") +
  ylab("Opposition score (jittered)") +
  facet_wrap(~class, ncol=2)

summarytable(opp_1, opp_2, opp_3, opp_4)

## ----class5, cache=TRUE, warning=FALSE, message=FALSE--------------------
opp_5 <- hlme(fixed = Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
              subject = "caseid",
              mixture = ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
              ng = 5,
              B = c(0, 0, 0, 0, # log-odds rel to class 5
                    0, 1, 3, 5, 9, # intercepts
                    0, 0, -1, -1, -1, # linear
                    0, 0, 0, 0, 0, # quadratic
                    0, 0, 0, 0, 0, # cubic
                    1), # residual SE
              data = opposition)
summary(opp_5)

opp_5_preds <- opposition %>%
    # merge predictions for each class with original data
    mutate(pred_1=predictY(opp_5, newdata=., var.time="Age_scaled")$pred[,1],
           pred_2=predictY(opp_5, newdata=., var.time="Age_scaled")$pred[,2],
           pred_3=predictY(opp_5, newdata=., var.time="Age_scaled")$pred[,3],
           pred_4=predictY(opp_5, newdata=., var.time="Age_scaled")$pred[,4],
           pred_5=predictY(opp_5, newdata=., var.time="Age_scaled")$pred[,5]) %>%
    # merge on classifications
    left_join(opp_5$pprob, by="caseid") %>%
    # report prediction for most likely class
    mutate(Prediction=ifelse(class==1, pred_1,
                      ifelse(class==2, pred_2,
                      ifelse(class==3, pred_3,
                      ifelse(class==4, pred_4, pred_5))))) %>%
    # make class a factor for plotting
    mutate(Class=factor(class))

# color trajectories by class prediction and show predicted trajectory
ggplot(data=opp_5_preds %>% filter(caseid %in% bigger_sample),
        aes(x=Age, y=Opposition_jitter, group=caseid, color=Class)) +
    geom_line(alpha=0.25) +
    geom_line(aes(y=Prediction),
              stat="smooth",
              formula= y ~ x + I(x^2) + I(x^3),
              method="lm",
              se=FALSE,
              alpha=0.8,
              size=2) +
    theme_bw() +
    ggtitle("Five clusters, sample subjects") +
    ylab("Opposition score (jittered)") +
    facet_wrap(~class, ncol=3)

summarytable(opp_1, opp_2, opp_3, opp_4, opp_5)

## ----class6, cache=TRUE, message=FALSE, warning=FALSE--------------------
opp_6 <- hlme(fixed = Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
              subject = "caseid",
              mixture = ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
              ng = 6,
              B = c(0, 0, 0, 0, 0, # log-odds rel to class 6
                    0, 1, 3, 5, 7, 9, # intercepts
                    0, 2, 1, -1, -1, 0, # linear
                    0, 0, 0, 0, 0, 0, # quadratic
                    0, 0, 0, 0, 0, 0, # cubic
                    1), # residual SE
              data = opposition)
summary(opp_6)

opp_6_preds <- opposition %>%
    # merge predictions for each class with original data
    mutate(pred_1=predictY(opp_6, newdata=., var.time="Age_scaled")$pred[,1],
           pred_2=predictY(opp_6, newdata=., var.time="Age_scaled")$pred[,2],
           pred_3=predictY(opp_6, newdata=., var.time="Age_scaled")$pred[,3],
           pred_4=predictY(opp_6, newdata=., var.time="Age_scaled")$pred[,4],
           pred_5=predictY(opp_6, newdata=., var.time="Age_scaled")$pred[,5],
           pred_6=predictY(opp_6, newdata=., var.time="Age_scaled")$pred[,6]) %>%
    # merge on classifications
    left_join(opp_6$pprob, by="caseid") %>%
    # report prediction for most likely class
    mutate(Prediction=ifelse(class==1, pred_1,
                      ifelse(class==2, pred_2,
                      ifelse(class==3, pred_3,
                      ifelse(class==4, pred_4,
                      ifelse(class==5, pred_5, pred_6)))))) %>%
    # make class a factor for plotting
    mutate(Class=factor(class))

# color trajectories by class prediction and show predicted trajectory
ggplot(data=opp_6_preds %>% filter(caseid %in% bigger_sample),
        aes(x=Age, y=Opposition_jitter, group=caseid, color=Class)) +
    geom_line(alpha=0.25) +
    geom_line(aes(y=Prediction),
              stat="smooth",
              formula= y ~ x + I(x^2) + I(x^3),
              method="lm",
              se=FALSE,
              alpha=0.8,
              size=2) +
    theme_bw() +
    ggtitle("Six clusters, sample subjects") +
    ylab("Opposition score (jittered)") +
    facet_wrap(~class, ncol=3)

summarytable(opp_1, opp_2, opp_3, opp_4, opp_5, opp_6)

## ----class4randint, cache=TRUE, message=FALSE, warning=FALSE-------------
opp_4_rint <- hlme(fixed = Opposition ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
                   random=~1,
              subject = "caseid",
              mixture = ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
              ng = 4,
              B = c(0, 0, 0, # log odds relative to class 4
                    1, 3, 5, 9, # intercepts
                    0, -1, -1, -1, # linear
                    0, 0, 0, 0, # quadratic
                    0, 0, 0, 0, # cubic
                    1, # residual int variance
                    1), # residual SE
              data = opposition)

opp_4_rint_preds <- opposition %>%
  # merge on predictions for each class
  mutate(pred_1=predictY(opp_4_rint, newdata=., var.time="Age_scaled")$pred[,1],
         pred_2=predictY(opp_4_rint, newdata=., var.time="Age_scaled")$pred[,2],
         pred_3=predictY(opp_4_rint, newdata=., var.time="Age_scaled")$pred[,3],
         pred_4=predictY(opp_4_rint, newdata=., var.time="Age_scaled")$pred[,4]) %>%
  # merge on classifications
  left_join(opp_4_rint$pprob, by="caseid") %>%
  # report prediction for most likely class
  mutate(Prediction=ifelse(class==1, pred_1,
                           ifelse(class==2, pred_2,
                                  ifelse(class==3, pred_3, pred_4)))) %>%
  # make class a factor for plotting
  mutate(Class=factor(class))

# color trajectories by class prediction and show predicted trajectory
ggplot(data=opp_4_rint_preds %>% filter(caseid %in% bigger_sample),
       aes(x=Age, y=Opposition_jitter, group=caseid, color=Class)) +
  geom_line(alpha=0.25) +
  geom_line(aes(y=Prediction),
            stat="smooth",
            formula= y ~ x + I(x^2) + I(x^3),
            method="lm",
            se=FALSE,
            alpha=0.8,
            size=2) +
  theme_bw() +
  ggtitle("Four clusters, sample subjects") +
  ylab("Opposition score (jittered)") +
  facet_wrap(~class, ncol=2)

summarytable(opp_1, opp_2, opp_3, opp_4, opp_4_rint)

######
# Practice!
######
# Try building a multi-level 4-class model with random intercepts **and slopes**. 
# Change the formula in `opp_4_rint` accordingly and initialize the random slopes 
# variance to 1 and covariance between random slopes and intercepts to 0.5. What 
# differences do you observe in model performance? 

## ----recenter, warning=FALSE, message=FALSE------------------------------
opposition_recenter <- opposition %>%
  # calculate within subject means and merge using caseid
  left_join( opposition %>%
               group_by(caseid) %>%
               summarize(mean_opp=mean(Opposition, na.rm=TRUE)),
             by="caseid") %>%
  mutate(Opposition_diff_mean=Opposition-mean_opp) %>%
  # jitter
  mutate(Opposition_diff_mean_jitter=Opposition_diff_mean+rnorm(n=nrow(.), mean=0, sd=0.1))

# visualize
ggplot(data=opposition_recenter %>% filter(caseid %in% sample_subjects),
       aes(x=Age, y=Opposition_diff_mean, group=caseid)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~ caseid, ncol=5) +
  ggtitle("Observed changes from individual mean opposition scores\nSample subjects") +
  ylab("Difference from individual opposition mean")

## ----class3diffs, cache=TRUE, warning=FALSE, message=FALSE---------------
opp_3_diffed <- hlme(fixed = Opposition_diff_mean ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
subject = "caseid",
mixture = ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3),
ng = 3,
B = c(0, 0, # log-odds relative to class 3
0, 1, -1, # intercepts
0, 0, 0, # linear terms
0, 0, 0, # quadratic terms
0, 0, 0, # cubic terms
2), # residual SE
data = opposition_recenter)
summary(opp_3_diffed)

opp_3_diffed_preds <- opposition_recenter %>%
# merge on predictions for each class
mutate(pred_1=predictY(opp_3_diffed, newdata=., var.time="Age_scaled")$pred[,1],
pred_2=predictY(opp_3_diffed, newdata=., var.time="Age_scaled")$pred[,2],
pred_3=predictY(opp_3_diffed, newdata=., var.time="Age_scaled")$pred[,3]) %>%
# merge on classifications
left_join(opp_3_diffed$pprob, by="caseid") %>%
# report prediction for most likely class
mutate(Prediction=ifelse(class==1, pred_1,
ifelse(class==2, pred_2, pred_3))) %>%
# make class a factor for plotting
mutate(Class=factor(class))

# color trajectories by class prediction and showpredicted trajectory
ggplot(data=opp_3_diffed_preds %>% filter(caseid %in% bigger_sample),
aes(x=Age, y=Opposition_diff_mean_jitter, group=caseid, color=Class)) +
geom_line(alpha=0.25) +
geom_line(aes(y=Prediction),
stat="smooth",
formula= y ~ x + I(x^2) + I(x^3),
method="lm",
se=FALSE,
alpha=0.8,
size=2) +
theme_bw() +
ggtitle("Changes from individual mean opposition score\nThree clusters, sample subjects") +
ylab("Difference from ind. mean opp. score (jittered)") +
facet_grid(~class)


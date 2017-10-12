## ---- message=FALSE------------------------------------------------------
library(dplyr)
library(ggplot2)
# change directories to where this file sits on your own computer!
milk_long <- read.table("~/Documents/!PhD-UW/CSSS-594/ALDA/milk_long.csv",
                        header=TRUE, 
                        sep=",")
head(milk_long)

## ------------------------------------------------------------------------
milk_graph <- ggplot(data=milk_long,
                     aes(x=Week, y=Protein, group=Cow, color=Diet)) +
  geom_point() +
  geom_line() +
  ggtitle("Protein levels in milk by week for each cow") +
  theme_bw() +
  scale_color_manual(values=c("mediumorchid4", "darkgoldenrod1", "deepskyblue"))

milk_graph
# How can we improve this?

## ----milkFacetDiet, warning=FALSE, message=FALSE-------------------------
# We decided that the vertical view was more useful
milk_graph +
    facet_grid(Diet ~ .)

## ----milkFacetDietRandom, warning=FALSE, message=FALSE-------------------
# setting a seed in R makes your "random" number generation reproducible
set.seed(10715)
# sample 20 cow IDs without replacement from the set of cow IDs
sampled_cows <- sample(unique(milk_long$Cow), size=20, replace=FALSE)
sampled_cows

# apply filtering to the data to keep the cows appearing in sampled_cows
# note use of the %in% operator
ggplot(data=milk_long %>%
           filter(Cow %in% sampled_cows),
       aes(x=Week, y=Protein, group=Cow, color=Diet)) +
    geom_line(alpha=0.5) +
    ggtitle("Protein levels in milk by week for sampled cow") +
    theme_bw() +
    scale_color_manual(values=c("mediumorchid4", "darkgoldenrod1", "deepskyblue")) +
    # 5 row, 4 column layout
    facet_wrap( ~ Cow, ncol=4)

## ----regression, message=FALSE, warning=FALSE----------------------------
milk_regression <- milk_long %>%
    group_by(Diet, Cow) %>%
    # use 'do' to run a linear regression with the grouped observations
    # and store the summary object in a variable called indiv_model
    do(indiv_model = summary(lm(Protein ~ Week, data=.))) %>%
    # get the intercept, slope (coefficient of Week), and R^2 from the model objects
    mutate(intercept = indiv_model[["coefficients"]]["(Intercept)","Estimate"],
           slope = indiv_model[["coefficients"]]["Week","Estimate"],
           r2 = indiv_model[["r.squared"]]) %>%
    # drop the indiv_model object for each subject since we're done with it
    select(-indiv_model)

head(milk_regression)

## ----debugDemo-----------------------------------------------------------
# look at just one cow
this_cow <- milk_long %>%
    filter(Cow==5)
head(this_cow)

# fit a regression
this_cow_line <- lm(Protein ~ Week, data=this_cow)

# What is this_cow_line?
class(this_cow_line)
# What's inside that object?
names(this_cow_line)
# I see coefficients but not r squared. How about in the lm summary?
class(summary(this_cow_line))
names(summary(this_cow_line))

## ----stemAndLeaf---------------------------------------------------------
stem(milk_regression$intercept, scale=2)

## ----milkMarginalDists, warning=FALSE, message=FALSE---------------------
# histograms
# note we only have x aesthetic, there is no "y" as this is created by geom_histogram
ggplot(milk_regression, aes(x=intercept)) +
    geom_histogram(fill="magenta", binwidth=0.1) +
    ggtitle("Intercepts from linear regressions fit to each cow")

# density estimates
ggplot(milk_regression, aes(x=intercept)) +
    geom_density(fill="magenta", adjust=1) +
    ggtitle("Intercepts from linear regressions fit to each cow")

# Practice 3.1.1. Try this for the slopes.

## ----intSlopeScatter-----------------------------------------------------
ggplot(data=milk_regression, aes(x=intercept, y=slope)) +
# We could also break these out by diet using color or facets for another view
  geom_point() +
    ggtitle("Slope vs. intercept for level-1 regressions")
# Interpretations of this plot?

## ----level1Lines---------------------------------------------------------
# look at the level 1 regressions faceted out for the sample of cows
ggplot(data=milk_long %>%
           # just the sampled cows using %in%
           filter(Cow %in% sampled_cows),
       aes(x=Week, y=Protein, group=Cow, color=Diet)) +
    geom_point() +
    # adds fitted lines for each group to the plot using lm
    geom_line(stat="smooth", method="lm") +
    ggtitle("Protein levels in milk by week for sampled cow with linear fit") +
    theme_bw() +
    scale_color_manual(values=c("mediumorchid4", "darkgoldenrod1", "deepskyblue")) +
    facet_wrap( ~ Cow, ncol=4)

# Practice 3.3.1. Try a non-parametric smoother here. How does the non-parametric
# fit compare to the linear fit?

## ----level1Superimposed--------------------------------------------------
ggplot(data=milk_long,
       aes(x=Week, y=Protein, group=Cow, color=Diet)) +
    geom_line(stat="smooth", method="lm", alpha=0.75) +
    ggtitle("Linear fit for each cow's trajectory") +
    theme_bw() +
    scale_color_manual(values=c("mediumorchid4", "darkgoldenrod1", "deepskyblue"))

## ----level1Quadratic-----------------------------------------------------
ggplot(data=milk_long,
       aes(x=Week, y=Protein, group=Cow, color=Diet)) +
    # using a formula quadratic in time: y = a + b x + c x^2 + error
    # the I(x^2) tells R to compute x^2 and handle this term separately
    # you use standard R regression syntax in formula
    geom_line(stat="smooth", method="lm",
              formula= y ~ x + I(x^2), alpha=0.75) +
    ggtitle("Quadratic fit for each cow's trajectory") +
    theme_bw() +
    scale_color_manual(values=c("mediumorchid4", "darkgoldenrod1", "deepskyblue"))

## ----level1GroupMeans----------------------------------------------------
ggplot(data=milk_long,
       aes(x=Week, y=Protein, group=Cow, color=Diet)) +
    geom_line(stat="smooth", method="lm", alpha=0.5) +
    # add a layer for average linear models by group, in thicker lines (the size argument)
    # keeping color=Diet in the aes so it uses the same colors as in the individual lines, and adding group=Diet so the models are grouped by diet
    geom_line(stat="smooth", aes(group=Diet, color=Diet), method="lm", size=3, alpha=0.75) +
    # add a layer for the overall mean, in thick (size argument) dashed (linetype argument) line
    # use group="1" in the aes for an overall summary
    geom_line(stat="smooth", aes(group="1"), color="black", method="lm",  size=2, linetype="dashed", alpha=0.75) +
    ggtitle("Linear fit for each cow's trajectory") +
    theme_classic() +
    scale_color_manual(values=c("mediumorchid4", "darkgoldenrod1", "deepskyblue"))

# Check out http://www.r-graph-gallery.com/portfolio/ggplot2-package/

## ----wideToLong, warning=FALSE, message=FALSE----------------------------
# install.packages("tidyr")
library(tidyr)

# wide version of milk data
milk_wide <- read.table("~/Documents/!PhD-UW/CSSS-594/ALDA/milk_wide.csv",
                       header=TRUE,
                       sep=",")
head(milk_wide) # Note the NAs!

## ----wideToLong2, warning=FALSE, message=FALSE---------------------------
milk_wide_to_long <- milk_wide %>%
    # gather takes these arguments:
    # key=name of the longitudinal variable to be created
    # value=name of the variable measured at each occasion
    # the rest are names of what you want to go from individual columns 
  # to values under the longitudinal variable
    # note that any variable not mentioned will be unchanged
    gather(key=Time, value=Protein, Wk1:Wk19) %>%
    # sort it by Cow and then by Time using arrange
    arrange(Cow, Time)
head(milk_wide_to_long)

## ----wideToLongSplit, warning=FALSE, message=FALSE-----------------------
milk_wide_to_long <- milk_wide %>%
    gather(key=Time, value=Protein, Wk1:Wk19) %>%
    # separate takes these arguments:
    # col=column we want to split up into two
    # into=character strings for the names of new columns we want to create
    # sep=the character after which we want to split the col variable
    # (could also split based on punctuation - see ?separate)
    # convert=TRUE means take anything that looks like a number and make it a numeric instead of character
    separate(col=Time, into=c("prefix","Week"), sep=2, convert=TRUE) %>%
    # now drop prefix since we don't need it (note the minus sign for dropping a variable)
    select(-prefix) %>%
    # sort by Cow and then the cleaned Week number
    arrange(Cow, Week)

head(milk_wide_to_long)

## ----dropNA--------------------------------------------------------------
milk_wide_to_long <- milk_wide_to_long %>%
    filter(!is.na(Protein))

## ----spreadDemo----------------------------------------------------------
milk_long_to_wide <- milk_long %>%
    # make a column called week_name that has "Wk"+number, so we get variable names that aren't plain numbers
    # paste0 is a function for concatenating text without adding spaces
    mutate(week_name=paste0("Wk",Week)) %>%
    # we can get rid of Week now because we have the new column
    select(-Week) %>%
    # spread takes these arguments:
    # key=the variable to use for column names for the new columns in wide format,
    # value=the variable to stick in as values for those columns
    spread(key=week_name, value=Protein) %>%
    # the new columns are in alphabetical order, not numeric order
    # (Wk1, Wk10, Wk11, ..., Wk19, Wk2, Wk3, etc.)
    # so use select to reorder them in the order we list
    select(Diet, Cow, Wk1, Wk2:Wk9, Wk10:Wk19)

head(milk_long_to_wide)

## ----minDemo ----------------------------
# What's the minimum value of Wk19 for all cows?
min(milk_long_to_wide$Wk19) # Won't work
min(milk_long_to_wide$Wk19, na.rm=T) # Tell min() how to handle NAs

## ----exampleCor, warning=FALSE, message=FALSE----------------------------
milk_correlation <- milk_wide %>%
    # just look at the first 6 weeks
    select(Wk1:Wk6) %>%
    # there are some missing values, let's use only observations with all 6 present ("complete.obs"). (Use this only when you know you don't need all observations!)
    cor(x=., use="complete.obs")

# correlation matrix formatted nicely with pander and a caption
library(pander)
# putting in some options to keep the decimal points under control
panderOptions('round', 2)
panderOptions('keep.trailing.zeros', TRUE)

pander(milk_correlation,
       # Don't forget to mention we're only using the complete observations!
       caption="Correlation among milk protein levels in first 6 weeks, complete observations only")
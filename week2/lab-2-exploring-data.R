# CSSS, Lab 2: Exploring longitudinal data. R Code.
# Author: Max Schneider
# Date: October 4, 2017

## ----importData----------------------------------------------------------
# change directories to your own computer if needed!
milk_long <- read.table("~/Documents/logdata/week2/milk_long.csv", header=TRUE, 
                        sep=",")
head(milk_long)
str(milk_long)

## ----dplyrSimpleDemo, warning=FALSE, message=FALSE-----------------------
# Install dplyr package (as necessary) and load it in
# install.packages("dplyr")
library(dplyr)

milk_long_modified <- milk_long %>%
  # make flag for cows with an even ID number (i.e. ID mod 2 is 0, %% is the mod operator)
  mutate( is_even = ifelse(Cow %% 2 == 0, 1, 0) )
head(milk_long_modified, 25)
str(milk_long_modified)

## ----baseSimpleDemo, warning=FALSE, message=FALSE------------------------
# copy milk_long to a new data frame
milk_long_modified2 <- milk_long
# make flag for cows with an even ID number (i.e. ID mod 2 is 0, %% is the mod operator)
milk_long_modified2$is_even <- ifelse(milk_long_modified2$Cow %% 2 == 0, 1, 0)
head(milk_long_modified2)
str(milk_long_modified2)

## ----dplyrChainDemo------------------------------------------------------
milk_long_subset <- milk_long %>%
  # make a dataset that's just even cow ID #s for week 1,
  # who are NOT having the barley only diet;
  mutate( is_even = ifelse(Cow %% 2 ==0, 1, 0) ) %>%
  filter( is_even==1 & Week==1 & Diet != "Barley") %>%
  # keep only the Diet, Cow, and Protein columns...
  select( Diet, Cow, Protein ) %>%
  # ...and sort by Protein in descending order.
  arrange( desc(Protein) )
head(milk_long_subset)

## ----groupByDemo---------------------------------------------------------
cow_diet_weeks <- milk_long %>%
  # first at level of cow and diet, how many weeks (measurement occasions) are there?
  group_by(Diet, Cow) %>%
  summarize(n_weeks=n()) %>%
  # then at the diet level, how many cows and min and max weeks observed?
  group_by(Diet) %>%
  summarize(n_cows=n_distinct(Cow),
            min_weeks=min(n_weeks),
            max_weeks=max(n_weeks))
cow_diet_weeks

# store specific numbers to report inline for cows on the mixed diet
# use as.numeric to convert to a single number instead of a 1x1 data frame
n_mixed <- cow_diet_weeks %>%
  filter(Diet=="Mixed") %>%
  select(n_cows) %>%
  as.numeric()
n_mixed

min_mixed <- cow_diet_weeks %>%
  filter(Diet=="Mixed") %>%
  select(min_weeks) %>%
  as.numeric()
min_mixed

max_mixed <- cow_diet_weeks %>%
  filter(Diet=="Mixed") %>%
  select(max_weeks) %>%
  as.numeric()

## ----eval=FALSE----------------------------------------------------------
## $n=$ `r n_mixed`
## ----eval=FALSE----------------------------------------------------------
## `r min_mixed`

## ----eval=FALSE----------------------------------------------------------
## `r max_mixed`

## ----milkNoFacet, warning=FALSE, message=FALSE---------------------------
library(ggplot2)
ggplot(data=milk_long,
       aes(x=Week, y=Protein, group=Cow, color=Diet)) +
  geom_point() +
  geom_line() +
  ggtitle("Protein levels in milk by week for each cow")

## ----milkScatterOnly, warning=FALSE, message=FALSE-----------------------
ggplot(data=milk_long,
       aes(x=Week, y=Protein, shape=Diet)) +
  geom_point(size=5, alpha=0.5) +
  ggtitle("Protein levels in milk by week, all cows")

## ----milkNoFacetRev, warning=FALSE, message=FALSE------------------------
# storing the graph to an object -- I can add more layers later!
milk_graph <- ggplot(data=milk_long,
                     aes(x=Week, y=Protein, group=Cow, color=Diet)) +
  geom_line(alpha=0.5) +
  ggtitle("Protein levels in milk by week for each cow") +
  theme_bw() +
  scale_color_manual(values=c("mediumorchid4", "darkgoldenrod1", "deepskyblue"))

milk_graph

## ----milkFacetDiet, warning=FALSE, message=FALSE-------------------------
milk_graph +
  facet_grid(. ~ Diet)
milk_graph +
  facet_grid(Diet ~ .)
## ----milkFacetCow, warning=FALSE, message=FALSE, fig.height=8, fig.width=10----
milk_graph +
  # will make a graph for each cow wrapping across columns
  facet_wrap( ~ Cow, ncol=10)

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
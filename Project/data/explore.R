rm(list=ls())

library(tidyverse)
library(RecordLinkage)
setwd("~/Documents/Classes/ALDA/Project/data/")

senior_files <- list.files("./CAHSData", full.names=TRUE) %>% 
    grep("senior", ., value=TRUE)

eligable_files <- list.files("./CAHSData", full.names=TRUE) %>% 
    grep("elig", ., value=TRUE)

# the way that you are identifying unique observations is different for
# different parts of the process look at your last select statement here and 
# be more careful
senior_data_wide <- rbind_list(lapply(senior_files, function(x) 
    read.delim(x, sep="\t") %>% set_names(x=., nm=tolower(names(.))))) %>%
    mutate(Year=as.Date(substring(year, 1, 2), "%y")) %>% select(-year) %>%
    mutate(year=as.numeric(as.character(Year, format="%Y"))) %>% 
    select(-c(Year)) %>% mutate(school=tolower(school))

uc_data <- read_csv("./UCdata/all_uc_data.csv") %>% 
    set_names(x=., nm=tolower(names(.))) %>%
    mutate(school=tolower(`school name`)) %>% select(-`school name`) %>%
    rename(county=`county/state/ territory`) %>%
    mutate(no_match=!(school %in% unique(senior_data_wide$school))) %>%
    mutate(new_school=sub("high school", "high", school)) %>%
    mutate(school=replace(school, no_match, new_school[no_match])) %>%
    select(-no_match, -new_school, -x1, -calculation1)

senior_data_long_unstruct <- senior_data_wide %>%
    gather(ethnicity, count, am_ind:not_reported)

senior_data_long <- senior_data_long_unstruct %>% filter(ethnicity=="total") %>%
    select(-ethnicity) %>% rename(total=count) %>% 
    right_join(senior_data_long_unstruct) %>% filter(ethnicity!="total")

### how may rows are we losing to bad data practices? ~ 650
nrow(senior_data_long %>% filter(!is.na(count) & total < count))

senior_data_long <- senior_data_long %>% filter(is.na(count) | total >= count)

white_pop <- senior_data_long %>% filter(ethnicity == "white") %>%
    mutate(white_prop=count/total) %>%
    select(-ethnicity, -count, -total)

senior_data_long <- left_join(senior_data_long, white_pop)
cor(senior_data_long$white_prop, senior_data_long$count, use="pair")
cor(senior_data_long$white_prop, senior_data_long$total, use="pair")

senior_data_long <- senior_data_long %>% mutate(demo_comp=as.factor(NA)) %>%
    mutate(demo_comp=ifelse(white_prop > .7, "High White Pop.", NA)) %>%
    mutate(demo_comp=ifelse(white_prop < .3, "Low White Pop.", demo_comp)) %>%
    mutate(demo_comp=as.factor(demo_comp))

hisp_data_senior <- senior_data_long %>% 
    filter(ethnicity == "hispanic" & !is.na(count) & !is.na(total))

# This is over counting admissions because some students get in multple places
# use the all data set later
hisp_data_uc <- uc_data %>% 
    filter(`uad uc ethn 6 cat`=="Hispanic/ Latino" & `measure names`=="adm") %>%
    group_by(city, county, year, schooltype, school) %>%
    summarize(uc_addmitted=sum(`measure values`, na.rm=T))

names(hisp_data_senior)
names(hisp_data_uc)

merged_adm_data <- inner_join(hisp_data_senior, hisp_data_uc)

set.seed(123)
samp1 <- merged_adm_data %>% 
    filter(year==2015 & demo_comp == "Low White Pop.") %>% 
    select(school, county, demo_comp) %>% unique %>% sample_n(8)
samp2 <- merged_adm_data %>%
    filter(year==2015 & demo_comp == "High White Pop.") %>% 
    select(school, county, demo_comp) %>% unique %>% sample_n(8) 

sub_merged_data <- rbind(samp1, samp2) %>% mutate(keep=1) %>% 
    right_join(merged_adm_data %>% select(-demo_comp)) %>% 
    mutate(keep=ifelse(is.na(keep), 0, 1)) %>%
    filter(keep==1)

write_csv(sub_merged_data, "./subset_data.csv")

ggplot(sub_merged_data, aes(x=year, y=uc_addmitted/count, color=demo_comp)) + 
    geom_point() + 
    geom_smooth(method="lm", se=F) +
    facet_wrap(~school) + 
    labs(title="Linear Model Trajectories of Hispanic Student Admission") + 
    ylim(0,1)

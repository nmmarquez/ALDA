rm(list=ls())

library(tidyverse)
library(RecordLinkage)
library(pander)
setwd("~/Documents/Classes/ALDA/Project/data/")

senior_files <- list.files("./CAHSData", full.names=TRUE) %>% 
    grep("senior", ., value=TRUE)

eligable_files <- list.files("./CAHSData", full.names=TRUE) %>% 
    grep("elig", ., value=TRUE)

# 
senior_data_wide <- rbind_list(lapply(senior_files, function(x) 
    read.delim(x, sep="\t") %>% set_names(x=., nm=tolower(names(.))))) %>%
    mutate(year=sprintf("%04d", year)) %>%
    mutate(Year=as.Date(substring(year, 1, 2), "%y")) %>% select(-year) %>%
    mutate(year=as.numeric(as.character(Year, format="%Y"))) %>% 
    select(-c(Year)) %>% mutate(school=tolower(school)) %>% 
    as.data.frame

senior_data_wide %>% select(county, district, school) %>% unique %>% nrow
senior_data_wide %>% select(county, district, school, cds_code) %>% unique %>% nrow

senior_data_wide %>% select(white, hispanic, total, year) %>% group_by(year) %>% 
    summarize(Hispanic=sum(hispanic), White=sum(white), Total=sum(total)) %>% 
    as.data.frame %>% t %>% pander

uc_files <- list.files("./UCdata", full.names=TRUE) %>% 
    grep("Universitywide", ., value=T)
uc_data <- bind_rows(lapply(uc_files, read.csv, stringsAsFactors=F)) %>% 
    set_names(x=., nm=tolower(names(.))) %>%
    mutate(school=tolower(`school.name`)) %>% select(-`school.name`) %>%
    rename(county=`county.state..territory`) %>%
    mutate(no_match=!(school %in% unique(senior_data_wide$school))) %>%
    mutate(new_school=sub("high school", "high", school)) %>%
    mutate(school=replace(school, no_match, new_school[no_match])) %>%
    select(-no_match, -new_school, -x, -calculation1)

senior_data_long_unstruct <- senior_data_wide %>%
    gather(ethnicity, count, am_ind:not_reported)

senior_data_long <- senior_data_long_unstruct %>% filter(ethnicity=="total") %>%
    select(-ethnicity) %>% rename(total=count) %>% 
    right_join(senior_data_long_unstruct) %>% filter(ethnicity!="total")

### how may rows are we losing to bad data practices?
nrow(senior_data_long %>% filter(!is.na(count) & total < count))

senior_data_long <- senior_data_long %>% filter(is.na(count) | total >= count)

white_pop <- senior_data_long %>% filter(ethnicity == "white") %>%
    mutate(white_prop=count/total) %>%
    select(-ethnicity, -count, -total)

hisp_pop <- senior_data_long %>% filter(ethnicity == "hispanic") %>%
    mutate(hisp_prop=count/total) %>%
    select(-ethnicity, -count, -total)

senior_data_long <- left_join(senior_data_long, white_pop) %>%
    left_join(hisp_pop)
cor(senior_data_long$white_prop, senior_data_long$count, use="pair")
cor(senior_data_long$white_prop, senior_data_long$total, use="pair")
cor(senior_data_long$hisp_prop, senior_data_long$count, use="pair")
cor(senior_data_long$hisp_prop, senior_data_long$total, use="pair")

senior_data_long <- senior_data_long %>% mutate(demo_comp=as.factor(NA)) %>%
    mutate(demo_comp=ifelse(white_prop > .7, "High White Pop.", NA)) %>%
    mutate(demo_comp=ifelse(white_prop < .3, "Low White Pop.", demo_comp)) %>%
    mutate(demo_comp=as.factor(demo_comp))

hisp_data_senior <- senior_data_long %>% 
    filter(ethnicity == "hispanic" & !is.na(count) & !is.na(total))

#
hisp_data_uc <- uc_data %>% 
    filter(`uad.uc.ethn.6.cat`=="Hispanic/ Latino" & `measure.names`=="adm") %>%
    group_by(city, county, year, schooltype, school) %>%
    summarize(uc_addmitted=sum(`measure.values`, na.rm=T)) %>% as.data.frame

hisp_data_uc <- uc_data %>% 
    filter(`uad.uc.ethn.6.cat`=="Hispanic/ Latino" & `measure.names`=="app") %>%
    group_by(city, county, year, schooltype, school) %>%
    summarize(uc_applied=sum(`measure.values`, na.rm=T)) %>% as.data.frame %>%
    select(-schooltype) %>%
    right_join(hisp_data_uc, by=c("city", "county", "year", "school")) %>%
    select(-city, -schooltype)

# make sure there isnt any weird records in the uc system
sum(hisp_data_uc$uc_applied < hisp_data_uc$uc_addmitted)

# data points in HS data
select(hisp_data_senior, county, school) %>% unique %>% nrow
# data points in UC data
select(hisp_data_uc, county, school) %>% unique %>% nrow
# data points in joined data over UC data points
inner_join((select(hisp_data_senior, county, school) %>% unique %>% mutate(cat1=1)),
           (select(hisp_data_uc, county, school) %>% unique %>% mutate(cat2=1))) %>%
    nrow %>% `/`(select(hisp_data_uc, county, school) %>% unique %>% nrow)

merged_adm_data <- inner_join(hisp_data_senior, hisp_data_uc)
merged_adm_data <- merged_adm_data %>% select(county, school) %>%
    unique %>% mutate(., ID=1:nrow(.)) %>% right_join(merged_adm_data) %>%
    filter(count != 0) %>% mutate(year0=year-min(year))

hisp_data_senior %>% filter(hisp_prop != 0) %>%
    ggplot(aes(x=hisp_prop)) + geom_density() + facet_wrap(~year)
merged_adm_data %>% filter(hisp_prop != 0) %>%
    ggplot(aes(x=hisp_prop)) + geom_histogram() + facet_wrap(~year)

hisp_data_senior %>% filter(hisp_prop != 0) %>% 
    mutate(Data="Full Data") %>%
    bind_rows((merged_adm_data %>% mutate(Data="Merged Data"))) %>%
    filter(year %in% 1996:2015) %>%
    ggplot(aes(x=hisp_prop, group=Data, fill=Data)) + geom_density(alpha=.3) + 
    facet_wrap(~year) + 
    labs(y="Density", x="Proportion of Hispanic Students", 
         title="Yearly Densities of Demographic Proportions: Hispanic")

hisp_data_senior %>% mutate(Data="Full Data") %>%
    bind_rows((merged_adm_data %>% mutate(Data="Merged Data"))) %>%
    filter(year %in% 1996:2015) %>%
    ggplot(aes(x=white_prop, group=Data, fill=Data)) + geom_density(alpha=.3) + 
    facet_wrap(~year) + 
    labs(y="Density", x="Proportion of White Students", 
         title="Yearly Densities of Demographic Proportions: White")

hisp_data_senior %>% filter(hisp_prop != 0 & year %in% 1996:2015) %>%
    ggplot(aes(x=hisp_prop)) + geom_density(fill="red", alpha=.3) + 
    geom_density(data=merged_adm_data, fill="blue", alpha=.3) + 
    facet_wrap(~year) + 
    scale_fill_manual(name="Test", labels=c("Full Data", "Matched Data"))

# number of weird records we need to address in the HS data set
sum(merged_adm_data$uc_applied > merged_adm_data$count)


merged_adm_data[merged_adm_data$uc_applied > merged_adm_data$count, "count"] <- 
    merged_adm_data[merged_adm_data$uc_applied > merged_adm_data$count, "uc_applied"]

# Now we are good
sum(merged_adm_data$uc_applied > merged_adm_data$count)
sum(merged_adm_data$uc_addmitted > merged_adm_data$count)

write.csv(merged_adm_data, "./merged_data.csv", row.names=F)

merged_adm_data %>%
    ggplot(aes(x=count, y=uc_addmitted, color=white_prop)) + geom_point() + 
    geom_smooth(method="lm", color="red")
merged_adm_data %>%
    ggplot(aes(x=count, y=uc_applied, color=white_prop)) + geom_point() + 
    geom_smooth(method="lm", color="red")
merged_adm_data %>%
    ggplot(aes(x=uc_applied, y=uc_addmitted, color=white_prop)) + geom_point() + 
    geom_smooth(method="lm", color="red")

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

png("./plots/sample_plot.png", width=600)
ggplot(sub_merged_data, aes(x=year, y=uc_addmitted/count, color=demo_comp)) + 
    geom_point() + 
    geom_smooth(method="lm", se=F) +
    facet_wrap(~school) + 
    labs(title="Linear Model Trajectories of Hispanic Student Admission",
         y="Rate of Admission to UC", x="Year") +
    scale_color_discrete(
        name="Demography", 
        labels=c("High White\n(p>.7)", "Low White\n(p<.3)")) +
    ylim(0,1)
dev.off()

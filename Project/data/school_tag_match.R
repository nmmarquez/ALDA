rm(list=ls())
library(httr)
library(tidyverse)
library(jsonlite)
library(XML)
library(RCurl)

senior_files <- list.files("./CAHSData", full.names=TRUE) %>% 
    grep("senior", ., value=TRUE)

HS_data_tags <- bind_rows(lapply(senior_files, function(x) 
    read.delim(x, sep="\t") %>% set_names(x=., nm=tolower(names(.))))) %>%
    mutate(year=sprintf("%04d", year)) %>%
    mutate(Year=as.Date(substring(year, 1, 2), "%y")) %>% select(-year) %>%
    mutate(year=as.numeric(as.character(Year, format="%Y"))) %>% 
    select(-c(Year)) %>% mutate(school=tolower(school)) %>% 
    as.data.frame %>% select(county, district, school) %>% unique

uc_files <- list.files("./UCdata", full.names=TRUE) %>% 
    grep("Universitywide", ., value=T)
UC_data_tags <- bind_rows(lapply(uc_files, read.csv, stringsAsFactors=F)) %>% 
    set_names(x=., nm=tolower(names(.))) %>%
    mutate(school=tolower(`school.name`)) %>% select(-`school.name`) %>%
    rename(county=`county.state..territory`) %>% 
    select(city, county, school) %>% unique

search_wikipedia <- function(search_string, n=5){
    url_ <- "https://en.wikipedia.org/w/api.php"
    query_ <- list(
        "action" = "query", "list" = "search", 
        "srsearch" = search_string, "format" = "json"
    )
    
    jsonResults <- GET(url = url_, query = query_) %>% 
        content(as = "text") %>%
        fromJSON 
    
    return(jsonResults$query$search[1:n, ] %>% select(title, pageid))
}

search_duckduckgo <- function(search_string, n=5){
    url_ <- "http://api.duckduckgo.com/"
    query_ <- list("q" = search_string, "format" = "json")
    
    jsonResults <- GET(url = url_, query = query_) %>% 
        content(as = "text") %>%
        fromJSON 
    
    return(jsonResults)
}

search_nces <- function(name, city, district, county){
    url_ <- "https://nces.ed.gov/ccd/schoolsearch/school_list.asp?"
    url_ <- paste(paste0(url_, "Search=1"),
        paste0("InstName=", gsub(" ", "+", name)), "SchoolID=", "Address=", 
        paste0("City=", gsub(" ", "+", city)), "State=06", "Zip=", "Miles=", 
        paste0("County=", gsub(" ", "+", county)), "Phone=", "PhoneAreaCode=", 
        paste0("DistrictName=", gsub(" ", "+", district)), "DistrictID=", 
        "SchoolType=1", "SchoolType=2", "SchoolType=3", "SchoolType=4", 
        "SpecificSchlTypes=all", "IncGrade=-1", "LoGrade=-1", "HiGrade=-1",
        sep="&")
    full_html <- getURL(url_)
    fresult <- readHTMLTable(full_html)[[4]][3,2] %>% as.character
    return(fresult)
}

test_UC <- UC_data_tags %>% filter(city=="Santa Ana") %>% 
    mutate(search=paste0(school, ", ", city, ", ", county))

for(i in 1:nrow(test_UC)){
    cat(test_UC$search[i], "\n")
    cat(search_nces(test_UC$school[i], test_UC$city[i], "", test_UC$county[i]))
    cat("\n\n")
}

test_HS <-HS_data_tags %>% filter(district=="Santa Ana Unified") %>% 
    mutate(search=paste(school, district, county, sep=", "))

search_duckduckgo(test_UC$search[1])

test_UC

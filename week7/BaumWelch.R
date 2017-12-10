rm(list=ls())

source("./")

log_sum <- function(loga, logb){
    loga_ <- ifelse(loga >= logb, loga, logb)
    logb_ <- ifelse(loga >= logb, logb, loga)
    loga_ + log1p(exp(logb_ - loga_))
}
setwd("F:/Anol/Compas Analysis/COMPAS-Analysis")
library(DataExplorer)
library(dplyr)
library(tidyverse)
rawdata <-  read.csv("F:/Anol/Compas Analysis/compas-scores-two-years.csv")
head(rawdata)
str(rawdata)
nrow(rawdata)

#Checking which all columns has missing values and extent of it
missingv <- as.data.frame(DataExplorer::profile_missing(rawdata));missingv
missingv[order(missingv$pct_missing,decreasing = TRUE),]

#Dropping ineffictive columns
rawdata[,c("priors_count.1","decile_score.1","violent_recid")] <- NULL

#Calculating difference between different dates
rawdata$jailstay <- as.numeric(as.Date(rawdata$c_jail_out) - as.Date(rawdata$c_jail_in)) 
rawdata$r_jailstay <- as.numeric(as.Date(rawdata$r_jail_out) - as.Date(rawdata$r_jail_in)) 

#
rawdata %>%
  count(race,sex)%>%
  group_by(sex)
  
a <- read.csv("F:/Anol/Compas Analysis/compas-scores-two-years.csv")

summary(a)
  
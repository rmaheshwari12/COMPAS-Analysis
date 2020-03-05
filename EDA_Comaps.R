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

#Deriving More Variables from existing data

#Jail stay
rawdata$c_jailstay <- as.numeric(as.Date(rawdata$c_jail_out) - as.Date(rawdata$c_jail_in)) 

#Total Juv_count
rawdata <- 
  rawdata %>%
  mutate(juv_total = select(.,c(juv_fel_count,juv_misd_count,juv_other_count))%>% rowSums(na.rm = TRUE)) 

#select(.data = rawdata,c(juv_fel_count,juv_misd_count,juv_other_count,juv_total))



rawdata %>% select(days_b_screening_arrest) %>% summary()
rawdata %>%
  count(race,sex)%>%
  group_by(sex)
  
a <- read.csv("F:/Anol/Compas Analysis/compas-scores-two-years.csv")


df <- dplyr::select(a, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')
summary(df)


df1 <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor,ref = 1))%>%
  mutate(race_factor = factor(race)) %>%
  within(race_factor <- relevel(race_factor, ref = 3))%>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(score_text == "Low", labels = c("LowScore","HighScore")))
  


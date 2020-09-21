install.packages("RSQLite")
install.packages("blob")
install.packages("stargazer")
install.packages("ggplot2")
library(stargazer)
library(RSQLite)
library(ggplot2)
library(DataExplorer)
library(lattice)

# setwd("E:/Anol/Compass Analysis/COMPAS-Analysis")
# 
# filename <- "compas.db"
# sqlite.driver <- dbDriver("SQLite")
# db <- dbConnect(sqlite.driver,
#                 dbname = "compas.db")
# 
# dbListTables(db)
# 
# compas <- dbReadTable(db,"compas")
# people <- dbReadTable(db,"people")
# charge <- dbReadTable(db,"charge")
# prisonhistory <- dbReadTable(db,"prisonhistory")
# jailhistory <- dbReadTable(db,"jailhistory")
# casearrest <- dbReadTable(db,"casearrest")
# summary <- dbReadTable(db,"summary")
# 
# #Small comment
# 
# #EDA
# 
# install.packages("DataExplorer") 
# library(DataExplorer)
# 
# 
# str(compas)
# str(people)
# people$sex <- as.factor(people$sex)
# people$sex <- relevel(x = people$sex,ref = "Male")
# 
# #Merging the people and compas data set to include 'race' column in the comaps score
# a = merge(compas,people,by = "person_id") 
# 
# #Finding distribution of compas score for african-american people
# a_blck = subset(a, a$type_of_assessment == "Risk of Recidivism" & a$race == "African-American" )
# plot(density(a_blck$decile_score.x), lwd = 3, col = "red", xlim = c(0,12), main = "Plot")
# par(new = TRUE)
# 
# #Finding distribution of compas score for native american and other people
# a_white = subset(a, a$type_of_assessment == "Risk of Recidivism" & a$race != "African-American" )
# plot(density(a_white$decile_score.x), add = TRUE, col = "blue", lwd =3, xlim = c(0,12), main = "Plot")
# 
# summary(a_white$race)
# summary(a_blck$race)
# #The plot shows that african american people have more high risk numbers by COMPAS compared to other races.
# 
# #Replicating Compass Analysis
# setwd("F:/Anol/Rdata_CompasDB")
# library(rio)
# compas = import("Compas.Rdata")
# prisonhistory = import("prisonhistory.Rdata")
# people = import("people.Rdata")
# charge = import("charge.Rdata")
# jailhistory = import("jailhistory.Rdata")
# casearrest = import("casearrest.Rdata")
# 
# 
# write.csv(compas,"Compas_data.csv")
# write.csv(prisonhistory,"prisonhistory_data.csv")
# write.csv(people,"people_data.csv")
# write.csv(charge,"charge_data.csv")
# write.csv(jailhistory,"jailhistory_data.csv")
# write.csv(casearrest,"casearrest_data.csv")
# 

#Linear Regression for the Compass Decile score prediction

setwd("C:/Users/praga/OneDrive/Desktop/COMPAS_Prof.Anol.B")

d <- read.csv("Corrected_compas_score.csv")
attach(d)
detach(d)
summary(d)
head(d)

hist(log(d$decile_score),breaks = 8)

plot()

colnames(d)
unwanted_cols = c("X",'"c_jail_in","c_jail_out","c_case_number","c_offense_date","c_arrest_date","c_days_from_compas","c_charge_degree","c_charge_desc","r_days_from_arrest"')
d = drop_columns(data = d,ind = c("X",'"c_jail_in","c_jail_out","c_case_number","c_offense_date","c_arrest_date","c_days_from_compas","c_charge_degree","c_charge_desc","r_days_from_arrest"'))

#Relevel the race
d$race <- relevel(d$race, ref = "African-American")

#Correlation matrix
cordf = cor(d[,unlist(lapply(d, is.numeric))])

corrplot::corrplot(cordf)

#ScatterPlots
plot(d$decile_score,d$age)
plot(d$decile_score,d$priors_count)
pairs(~age+decile_score+race+priors_count+juv_fel_count+juv_misd_count+score_text,data=d,main="Simple Scatterplot Matrix")


#Simple LR model with age, juv count, sex and race
m0_decile_all = lm(log(decile_score) ~ age + juv_fel_count + juv_misd_count + sex + priors_count + race ,d)
summary(m0_decile_all)
plot(m0_decile_all)

#Excluding Race and Sex Interaction
m0_decile_all_interaction = lm(log(decile_score) ~ age + juv_fel_count + juv_misd_count + sex*race + priors_count, d)
summary(m0_decile_all_interaction)
plot(m0_decile_all_interaction)

#Checking the effect of crime factors on decile score
m0_decile_crime_factors = lm(log(decile_score) ~ juv_fel_count + juv_misd_count + priors_count ,d)
summary(m0_decile_crime_factors)
plot(m0_decile_crime_factors)

#Checking the effect of Race and Sex interaction term in predicting Decile score
m0_decile_raceandsex = lm(log(decile_score) ~ sex*race, d)
summary(m0_decile_raceandsex)
plot(m0_decile_raceandsex)

stargazer(m0_decile_all,m0_decile_all_interaction,m0_decile_crime_factors,m0_decile_raceandsex,type = 'text')

#################################################################################################################3

#checking if Decile score is a good predictor of Recidivism
hist(d$is_recid)
d$is_recid <- factor(d$is_recid)


#using all other factors just to predict recidivism

m1_recid_no_decile = glm(is_recid ~ age + juv_fel_count + juv_misd_count + sex + priors_count + race , family =binomial , data = d)
summary(m1_recid_no_decile)
plot(m1_recid_no_decile)


#using all the crime factors to predict the recidivism
m1_recid_crime_factors = glm(is_recid ~ juv_fel_count + juv_misd_count + priors_count, family =binomial , data = d)
summary(m1_recid_crime_factors)
plot(m1_recid_crime_factors)


#using decile_score along with other factors
m1_recid_all = glm(is_recid ~ age + juv_fel_count + juv_misd_count + sex + priors_count + race + decile_score, family =binomial , data = d)
summary(m1_recid_all)
plot(m1_recid_all)


#using decile score as predictor of recidivism
m1_recid_decilescore = glm(is_recid ~ decile_score, family =binomial , data = d)
summary(m1_recid_decilescore)
plot(m1_recid_decilescore)


#using race and sex alone as a predictor of recidivism
m1_recid_sex_race = glm(is_recid ~ sex*race, family =binomial , data = d)
summary(m1_recid_sex_race)
plot(m1_recid_sex_race)


stargazer(m1_recid_no_decile,m1_recid_crime_factors,m1_recid_all,m1_recid_decilescore,m1_recid_sex_race,type = 'text' )



g <- ggplot(d, aes(race))
g + geom_bar(aes(decile_score))


counts <- table(d$decile_score, d$race)
barplot(d$decile_score,d$race, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red","green","cyan","yellow"),
        legend = rownames(counts), beside=TRUE)

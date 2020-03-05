install.packages("RSQLite")
install.packages("blob")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("scales")
library(stargazer)
library(RSQLite)
library(ggplot2)
library(DataExplorer)
library(lattice)
library(scales)
library(dplyr)

setwd("E:/Anol/Compass Analysis/COMPAS-Analysis")

filename <- "compas.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = "compas.db")

dbListTables(db)

compas <- dbReadTable(db,"compas")
people <- dbReadTable(db,"people")
charge <- dbReadTable(db,"charge")
prisonhistory <- dbReadTable(db,"prisonhistory")
jailhistory <- dbReadTable(db,"jailhistory")
casearrest <- dbReadTable(db,"casearrest")
summary <- dbReadTable(db,"summary")

#Small comment

#EDA

install.packages("DataExplorer") 
library(DataExplorer)


str(compas)
str(people)
people$sex <- as.factor(people$sex)
people$sex <- relevel(x = people$sex,ref = "Male")

#Merging the people and compas data set to include 'race' column in the comaps score
a = merge(compas,people,by = "person_id") 

#Finding distribution of compas score for african-american people
a_blck = subset(a, a$type_of_assessment == "Risk of Recidivism" & a$race == "African-American" )
plot(density(a_blck$decile_score.x), lwd = 3, col = "red", xlim = c(0,12), main = "Plot")
par(new = TRUE)

#Finding distribution of compas score for native american and other people
a_white = subset(a, a$type_of_assessment == "Risk of Recidivism" & a$race != "African-American" )
plot(density(a_white$decile_score.x), add = TRUE, col = "blue", lwd =3, xlim = c(0,12), main = "Plot")

summary(a_white$race)
summary(a_blck$race)
#The plot shows that african american people have more high risk numbers by COMPAS compared to other races.

#Replicating Compass Analysis
setwd("F:/Anol/Rdata_CompasDB")
library(rio)
compas = import("Compas.Rdata")
prisonhistory = import("prisonhistory.Rdata")
people = import("people.Rdata")
charge = import("charge.Rdata")
jailhistory = import("jailhistory.Rdata")
casearrest = import("casearrest.Rdata")


write.csv(compas,"Compas_data.csv")
write.csv(prisonhistory,"prisonhistory_data.csv")
write.csv(people,"people_data.csv")
write.csv(charge,"charge_data.csv")
write.csv(jailhistory,"jailhistory_data.csv")
write.csv(casearrest,"casearrest_data.csv")


#Linear Regression for the Compass Decile score prediction

setwd("C:/USF - BAIS/Anol/Compass Analysis")

d <- read.csv("compas_with_drug_details.csv")
attach(d)
detach(d)
summary(d)
head(d)

#There are some negative values (-1) in decile score, typically indicator of missing data. We will remove this data from the analysis
d <- filter(d,decile_score != -1 )

hist(log(d$decile_score),breaks = 8)

#removing unwanted columns:

colnames(d)
unwanted_cols = c('age_cat','days_b_screening_arrest',"c_jail_in","c_jail_out","c_case_number","c_offense_date","c_arrest_date","c_days_from_compas",
                  'type_of_assessment', 'v_type_of_assessment','screening_date')

d = drop_columns(data = d, ind = c('X','Unnamed..0'))
#Relevel the race
d$race <- relevel(d$race, ref = "Caucasian")

#Correlation matrix
cordf = cor(d[,unlist(lapply(d, is.numeric))])
corrplot::corrplot(cordf)

#ScatterPlots
library(ggplot2)

ggplot(data = d, aes(decile_score,age)) + 
geom_point(color = 'steelblue') + 
ggtitle(" Decile score vs Age") +
geom_smooth(method = 'lm', color = 'red')

ggplot(data = d, aes(decile_score,priors_count)) + 
geom_point(color = 'steelblue') + 
ggtitle(" Decile score vs Priors Count") +
geom_smooth(method = 'gam', color = 'red', formula = y ~ s(x, bs = "cs"))


plot(d$decile_score,d$age)


plot(d$decile_score,d$priors_count)


pairs(~age+decile_score+race+priors_count+juv_fel_count+juv_misd_count+score_text,data=d,main="Simple Scatterplot Matrix")


#Series of Models


#Simple LR model withouth Interaction
# all variables that might generate decile score i.e. age, juv count, sex and race

m0_decile_all = lm(log(decile_score) ~ age + juv_fel_count + juv_misd_count + as.factor(druginvolvment) + sex + priors_count + race ,d)
summary(m0_decile_all)
plot(m0_decile_all)

#Simple LR model with Race and Sex Interaction

m0_decile_all_interaction = lm(log(decile_score) ~ age + juv_fel_count + juv_misd_count + as.factor(druginvolvment) +sex*race + priors_count, d)
summary(m0_decile_all_interaction)
plot(m0_decile_all_interaction)

#Checking the effect of crime factors on decile score
m0_decile_crime_factors = lm(log(decile_score) ~ juv_fel_count + juv_misd_count + priors_count + as.factor(druginvolvment),d) #normality fails
summary(m0_decile_crime_factors)
plot(m0_decile_crime_factors)

m0_decile_crime_factors_squareterms = lm(log(decile_score) ~ juv_fel_count + juv_misd_count + priors_count + I(priors_count^2) + as.factor(druginvolvment),d)
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


#GLM model to predict recidivism using using all other factors withouth interaction

m1_recid_no_decile = glm(is_recid ~ age + juv_fel_count + juv_misd_count + sex + priors_count + race + as.factor(druginvolvment) + length_of_stay, family =binomial , data = d)
summary(m1_recid_no_decile)
plot(m1_recid_no_decile)


#GLM model to predict recidivism using all the crime related factors 
m1_recid_crime_factors = glm(is_recid ~ juv_fel_count + juv_misd_count + priors_count + as.factor(druginvolvment) + length_of_stay, family =binomial , data = d)
summary(m1_recid_crime_factors)
plot(m1_recid_crime_factors)


#GLM model to predict the using decile score as predictor of recidivism
m1_recid_decilescore = glm(is_recid ~ decile_score, family =binomial , data = d)
summary(m1_recid_decilescore)
plot(m1_recid_decilescore)


#using race and sex alone as a predictor of recidivism
m1_recid_sex_race = glm(is_recid ~ sex*race, family =binomial , data = d)
summary(m1_recid_sex_race)
plot(m1_recid_sex_race)


stargazer(m1_recid_no_decile,m1_recid_crime_factors,m1_recid_all,m1_recid_decilescore,m1_recid_sex_race,type = 'text' )


################################################################################################################

#Computing a fair score matrix by considering only the crime factors as per the model m0_decile_crime_factors!


d$Newdecilescore = exp(0.969811) + exp(0.13694)*d$juv_fel_count + exp(0.11234)*d$juv_misd_count + exp(0.06106)*d$priors_count + exp(0.25013)*d$druginvolvment

summary(d$Newdecilescore)
summary(d$decile_score)

hist(d$decile_score, col = c1)
hist(d$Newdecilescore, col = c2, add = TRUE)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
d$scaleddecilescore = scale(d$Newdecilescore)
summary(d$scaleddecilescore)


hist(d$scaleddecilescore, col = c2)
hist(d$decile_score, col = c1,add = TRUE)


d_africanamerica = subset(d, race == 'African-American', select = c('decile_score','scaleddecilescore'))
hist(d_africanamerica$scaleddecilescore, col= c2)
hist(d_africanamerica$decile_score, col = c1, add =  TRUE)

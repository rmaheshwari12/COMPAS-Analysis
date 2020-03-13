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
#detach(d)
summary(d)
head(d)
dim(d)

#histogram of decile score
hist(log(d$decile_score),breaks = 8)

#removing unwanted columns:

colnames(d)
unwanted_cols = c('id','key','age_cat','dob','r_days_from_arrest','days_b_screening_arrest',"c_case_number",
                  "c_offense_date","c_arrest_date","c_days_from_compas",'type_of_assessment', 'v_type_of_assessment',
                  'screening_date','num_vr_cases','num_r_cases', 'decile_score.1')

d = drop_columns(data = d, ind = unwanted_cols)

#Data Manipulation

d$race <- relevel(d$race, ref = "Caucasian") #Relevel the race to caucasian
d$is_recid <- factor(d$is_recid) #factorize the is_recid flag
d$is_violent_recid <- factor(d$is_violent_recid) #factorize is_voilent_recid flag
d$length_of_stay = d$length_of_stay + 1 #adding 1 to all the records as some preprocessing has computed release on same day as -1 or release day after as 0 and so on. 

boxplot(d$length_of_stay)
IQR(d$length_of_stay)

d = subset(d, d$length_of_stay <= 500)#removing records where length of stay is greater than 500


#Charge Degree_factors

table(d$charge_degree_fact)

#removing Charge degree F5,F6,F7 as there are only 6 records of them combined in the data and no significance was found online about these charge degrees

d = d[!((d$charge_degree_fact == 'F5') | (d$charge_degree_fact == 'F6') | (d$charge_degree_fact == 'F7')),]
dim(d)


#Correlation matrix
cordf = cor(d[,unlist(lapply(d, is.numeric))])
corrplot::corrplot(cordf)

#ScatterPlots
library(ggplot2)

ggplot(data = d, aes(decile_score)) +
  geom_bar(aes(fill= d$race)) +
  ggtitle("Decile Score by Race") +
  xlab(" Decile Score for Risk of Recividism ") +
  ylab("Frequency")

ggplot(data = d, aes(decile_score,age)) + 
geom_point(color = 'steelblue') + 
ggtitle(" Decile score vs Age") +
geom_smooth(method = 'lm', color = 'red')

ggplot(data = d, aes(decile_score,priors_count)) + 
geom_point(color = 'steelblue') + 
ggtitle(" Decile score vs Priors Count") +
geom_smooth(method = 'gam', color = 'red', formula = y ~ s(x, bs = "cs"))

ggplot(data = d, aes(charge_degree_fact)) +
geom_bar(color = 'steelblue') + 
ggtitle ("Chrage Degree Distribution")

pairs(~age+decile_score+race+priors_count+juv_fel_count+juv_misd_count+score_text,data=d,main="Simple Scatterplot Matrix")


#################################################################################################################################

###########################
# Predicting Decile Score #
###########################


#Simple LR model withouth Interaction
# all variables that might generate decile score i.e. age, juv count, sex and race

m0_decile_all = lm(log(decile_score) ~ age + juv_fel_count + juv_misd_count + sex + priors_count + race ,d)
summary(m0_decile_all)
plot(m0_decile_all)
#-------------------------------

#Simple LR model with Race and Sex Interaction

m0_decile_all_interaction = lm(log(decile_score) ~ age + juv_fel_count + juv_misd_count + sex*race + priors_count, d)
summary(m0_decile_all_interaction)
plot(m0_decile_all_interaction)
#-------------------------------

#Checking the effect of crime factors on decile score
m0_decile_crime_factors = lm(log(decile_score) ~ juv_fel_count + juv_misd_count + priors_count ,d) #normality fails
summary(m0_decile_crime_factors)
plot(m0_decile_crime_factors)

m0_decile_crime_factors_squareterms = lm(log(decile_score) ~ juv_fel_count + juv_misd_count + priors_count + I(priors_count^2) ,d)
summary(m0_decile_crime_factors)
plot(m0_decile_crime_factors)

#------------------------------

#Checking the effect of Race and Sex interaction term in predicting Decile score
m0_decile_raceandsex = lm(log(decile_score) ~ sex*race, d)
summary(m0_decile_raceandsex)
plot(m0_decile_raceandsex)

stargazer(m0_decile_all,m0_decile_all_interaction,m0_decile_crime_factors,m0_decile_raceandsex,type = 'text')

#################################################################################################################3

#############################
# Predicting the Recidivism #
#############################


#checking if Decile score is a good predictor of Recidivism

set.seed(101)
install.packages("caret")
library(caret)
train.index <- createDataPartition(d$is_recid, p = .7, list = FALSE)
train <- d[ train.index,]
test  <- d[-train.index,]



#GLM model to predict recidivism using using all other factors withouth interaction

m1_recid_no_decile = glm(is_recid ~ age + juv_fel_count + juv_misd_count + priors_count +as.factor(druginvolvment) + length_of_stay + sex + race, control = control, family =binomial , data = d)
summary(m1_recid_no_decile)
plot(m1_recid_no_decile)

#------------------------------

#GLM model to predict recidivism using all the crime related factors 
m1_recid_crime_factors = glm(is_recid ~ juv_fel_count + juv_misd_count + priors_count + as.factor(druginvolvment) + length_of_stay, family =binomial , data = d)
summary(m1_recid_crime_factors)
plot(m1_recid_crime_factors)

#------------------------------

#GLM mode using decile score alone as predictor of recidivism
m1_recid_decilescore = glm(is_recid ~ decile_score, family =binomial , data = d)
summary(m1_recid_decilescore)
plot(m1_recid_decilescore)

#------------------------------

#GLM model using race and sex alone as a predictor of recidivism
m1_recid_sex_race = glm(is_recid ~ sex*race, family =binomial , data = d)
summary(m1_recid_sex_race)
plot(m1_recid_sex_race)

#------------------------------

stargazer(m1_recid_no_decile,m1_recid_crime_factors,m1_recid_decilescore,m1_recid_sex_race,type = 'text' )


#RMSE
pred1 =m1_recid_no_decile %>% predict(test)
rmse(test$is_recid,pred)

pred2 =m1_recid_crime_factors %>% predict(test)
rmse(test$is_recid,pred)

pred3 =m1_recid_decilescore %>% predict(test)
rmse(test$is_recid,pred)

pred4 =m1_recid_sex_race %>% predict(test)
rmse(test$is_recid,pred)

plot(test$is_recid,pred, xlab="Actual", ylab = "Predicted") +abline(0,1,col="red")

actuals.pred = data.frame(cbind(actuals=test$is_recid, predicted=pred1)) 
head(actuals.pred)
library(corrplot)
cor.values=as.matrix(cor(actuals.pred))
print(cor.values)
corrplot(cor.values, method="circle", type="upper")
#tail(pred1)
#head(test$is_recid)



################################################################################################################

#Running the models to determine the violent recidivism based on all the factors like above:

#Shukla Starts here...

hist(d$is_recid)
set.seed(101)

install.packages("caret")
library(caret)
train.index <- createDataPartition(d$is_recid, p = .7, list = FALSE)
train <- d[ train.index,]
test  <- d[-train.index,]
#++++++++++++++

#checking if Decile score is a good predictor of Recidivism



#GLM model to predict recidivism using using all other factors withouth interaction

m1_recid_no_decile = glm(is_recid ~ age + juv_fel_count + juv_misd_count + priors_count +as.factor(druginvolvment) + length_of_stay + sex + race+charge_degree_fact, family =binomial , data = train)
summary(m1_recid_no_decile)
plot(m1_recid_no_decile)


#GLM model to predict recidivism using all the crime related factors 
m1_recid_crime_factors = glm(is_recid ~ juv_fel_count + juv_misd_count + priors_count + as.factor(druginvolvment) + length_of_stay+ charge_degree_fact, family =binomial , data = train)
summary(m1_recid_crime_factors)
plot(m1_recid_crime_factors)


#GLM model to predict the using decile score as predictor of recidivism
m1_recid_decilescore = glm(is_recid ~ decile_score, family =binomial , data = train)
summary(m1_recid_decilescore)
plot(m1_recid_decilescore)


#using race and sex alone as a predictor of recidivism
m1_recid_sex_race = glm(is_recid ~ sex*race, family =binomial , data = train)
summary(m1_recid_sex_race)
plot(m1_recid_sex_race)


stargazer(m1_recid_no_decile,m1_recid_crime_factors,m1_recid_decilescore,m1_recid_sex_race,type = 'text' )

#RMSE
pred =m1_recid_no_decile %>% predict(test)
rmse(test$is_recid,pred)

pred =m1_recid_crime_factors %>% predict(test)
rmse(test$is_recid,pred)

pred =m1_recid_decilescore %>% predict(test)
rmse(test$is_recid,pred)

pred =m1_recid_sex_race %>% predict(test)
rmse(test$is_recid,pred)

plot(test$is_recid,pred, xlab="Actual", ylab = "Predicted") +abline(0,1,col="red")

actuals.pred = data.frame(cbind(actuals=test$is_recid, predicted=pred)) 
head(actuals.pred)
library(corrplot)
cor.values=as.matrix(cor(actuals.pred))
print(cor.values)
corrplot(cor.values, method="circle", type="upper")
#tail(pred1)
#head(test$is_recid)






################################################################################################################



#Computing a fair score matrix by considering only the crime factors as per the model m0_decile_crime_factors!

m1_recid_crime_factors$coefficients[6]

d$myscore = round(exp(m1_recid_crime_factors$coefficients[1]) + exp(m1_recid_crime_factors$coefficients[2])*d$juv_fel_count + exp(m1_recid_crime_factors$coefficients[3])*d$juv_misd_count + exp(m1_recid_crime_factors$coefficients[4])*d$priors_count + exp(m1_recid_crime_factors$coefficients[5])*d$druginvolvment + exp(m1_recid_crime_factors$coefficients[6])*d$length_of_stay)
summary(d$myscore)
d$myscore = d$myscore + 2
#having rounded of values of -1 and 0 ?
hist(d$myscore, breaks = 10)

d$scaledmyscore = round(rescale(d$myscore,to= c(1,10)))
summary(d$scaledmyscore)
hist(d$scaledmyscore)

summary(d$decile_score)
hist(d$decile_score, breaks = 10)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

d$scaledmyscore <- ifelse(d$scaledmyscore == 0 ,d$scaledmyscore+1,d$scaledmyscore) 
summary(d$scaledmyscore)

hist(d$decile_score, breaks = 10, col = c1, ylim = c(0,6500),main = NULL)
hist(d$scaledmyscore,breaks = 10, col = c2, add = TRUE)
title("Histogram of Comaps-Decile score and My score")
legend("topright", c("Decile Score", "My Score"), col=c(c1, c2), lwd=10)


ggplot(data = d, aes(sort(charge_degree_fact, decreasing = TRUE))) +
geom_bar(color = 'red') +
ggtitle ("Chrage Degree Distribution") + 
xlab(" Charge Degrees")+
ylab("Frequency")


summary(d$scaleddecilescore)



d_africanamerica = subset(d, race == 'African-American', select = c('decile_score','myscore','scaledmyscore'))
hist(d_africanamerica$decile_score, col= c2, ylim=c(0,3000))
hist(d_africanamerica$scaledmyscore, col = c1, add =  TRUE)

dd <- scale(d$decile_score)
mm <- scale(d$myscore)

summary(dd)
summary(mm)


hist(dd, breaks = 10, col = c1, ylim = c(0,6500))
hist(mm ,breaks = 10, col = c2, add = TRUE)
hist(dd)
hist(mm)

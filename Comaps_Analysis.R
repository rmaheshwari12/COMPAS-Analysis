# install.packages("blob")
# install.packages("stargazer")
# install.packages("ggplot2")
# install.packages("scales")
#install.packages("ROCR")
library(ROCR)
library(stargazer)
library(ggplot2)
library(DataExplorer)
library(lattice)
library(scales)
library(dplyr)
library(ModelMetrics)
library(caret)

#Linear Regression for the Compass Decile score prediction

setwd("C:/USF - BAIS/Anol/Compass Analysis")

d <- read.csv("compas_with_drug_details.csv")
attach(d)
dim(d)
head(d)
summary(d)

#histogram of decile score
hist(d$decile_score,breaks = 8, main = "Histogram of decile_score", col = 'steelblue')
hist(log(d$decile_score),breaks = 8, main = "Histogram of decile_score", col = 'steelblue')

#removing unwanted columns:

colnames(d)
unwanted_cols = c('id','key','age_cat','dob','r_days_from_arrest','days_b_screening_arrest',"c_case_number",
                  "c_offense_date","c_arrest_date","c_days_from_compas",'type_of_assessment', 'v_type_of_assessment',
                  'screening_date','num_vr_cases','num_r_cases', 'decile_score.1')

d = drop_columns(data = d, ind = unwanted_cols)

#Data Manipulation
d$race <- relevel(d$race, ref = "Caucasian") #Relevel the race to caucasian
d$is_recid <- factor(d$is_recid,) #factorize the is_recid flag, 0 = NO , 1 = Yes
d$is_violent_recid <- factor(d$is_violent_recid) #factorize is_voilent_recid flag, 0 = NO , 1 = Yes
d$druginvolvment <- factor(d$druginvolvment) #factorize drug involvment flag, 0 = NO , 1 = Yes
d$length_of_stay = d$length_of_stay + 1 #adding 1 to all the records as some preprocessing has computed release on same day as -1 or release day after as 0 and so on. 

#Charge Degree_factors correction
table(d$charge_degree_fact)

#removing Charge degree F5,F6,F7 as there are only 6 records of them combined in the data and no significance was found online about these charge degrees
d = d[!((d$charge_degree_fact == 'F5') | (d$charge_degree_fact == 'F6') | (d$charge_degree_fact == 'F7')),]
table(d$charge_degree_fact)
dim(d)
#Removing oe record with c_jail_out date '1/1/2020'. This is an erroneous record as our analysis is based on data till April 2016. 
d = d[!(d$c_jail_out=='1/1/2020 0:00' ),]

#DATA SPLIT INTO TRAIN TEST
set.seed(101)
train.index <- caret::createDataPartition(d$is_recid, p = .7, list = FALSE)
train <- d[ train.index,]
test  <- d[-train.index,]

table(train$is_recid,train$druginvolvment)
table(test$is_recid, test$druginvolvment)


############### Exploratory Analysis ##################

#Correlation matrix
cordf = cor(d[,unlist(lapply(d, is.numeric))])
corrplot::corrplot(cordf)

#ScatterPlots
ggplot(data = d, aes(decile_score)) +
  geom_bar(aes(fill= d$race)) +
  ggtitle("Decile Score by Race") +
  xlab(" Decile Score for Risk of Recividism ") +
  ylab("Frequency")

ggplot(data = d, aes(decile_score,age)) + 
  geom_point(color = 'steelblue') + 
  ggtitle(" Decile score vs Age") +
  geom_smooth(method = 'auto', color = 'red')

ggplot(data = d, aes(decile_score,juv_fel_count)) + 
  geom_point(color = 'steelblue') + 
  ggtitle(" Decile score vs Juvenile Felony Count") +
  geom_smooth(method = 'auto', color = 'red')

ggplot(data = d, aes(decile_score,juv_misd_count)) + 
  geom_point(colour = 'steelblue') + 
  ggtitle(" Decile score vs Juvenile misconduct Count") +
  geom_smooth(method = 'auto', color = 'red')

ggplot(data = d, aes(decile_score,priors_count)) + 
  geom_point(color = 'steelblue') + 
  ggtitle(" Decile score vs Priors Count") +
  geom_smooth(method = 'auto', color = 'red', formula = y ~ s(x, bs = "cs"))

ggplot(data = d, aes(charge_degree_fact)) +
  geom_bar(color = 'steelblue') + 
  ggtitle ("Chrage Degree Distribution")

ggplot(data = d, aes(sort(charge_degree_fact, decreasing = TRUE))) +
  geom_bar(color = 'red') +
  ggtitle ("Chrage Degree Distribution") + 
  xlab(" Charge Degrees")+
  ylab("Frequency")

pairs(~age+decile_score+race+priors_count+juv_fel_count+juv_misd_count+score_text,data=d,main="Simple Scatterplot Matrix")

#################################################################################################################################

###########################
# Predicting Decile Score #
###########################


#Simple LR model withouth Interaction
# all variables that might generate decile score i.e. age, juv count, sex and race

m0_decile_all = lm(log(decile_score) ~ age + juv_fel_count + juv_misd_count + sex + priors_count + race +charge_degree ,d)
summary(m0_decile_all)
plot(m0_decile_all)
#-------------------------------

#Simple LR model with Race and Sex Interaction

m0_decile_all_interaction = lm(log(decile_score) ~ age + juv_fel_count + juv_misd_count + sex*race + priors_count+charge_degree, d)
summary(m0_decile_all_interaction)
plot(m0_decile_all_interaction)
#-------------------------------

#Checking the effect of crime factors on decile score
m0_decile_crime_factors = lm(log(decile_score) ~ juv_fel_count + juv_misd_count + priors_count + charge_degree_fact + as.factor(druginvolvment),d) #normality fails
summary(m0_decile_crime_factors)
plot(m0_decile_crime_factors)

#------------------------------

#Checking the effect of Race and Sex interaction term in predicting Decile score
m0_decile_raceandsex = lm(log(decile_score) ~ sex*race, d)
summary(m0_decile_raceandsex)
plot(m0_decile_raceandsex)

stargazer(m0_decile_all,m0_decile_all_interaction,m0_decile_crime_factors,m0_decile_crime_factors_charge_degree,m0_decile_raceandsex,type = 'text')
stargazer::stargazer(m0_decile_crime_factors, type= 'text')

#################################################################################################################

#Function to generate F1 score
f1score <- function(x1,x2) {
  f1scorenum = 2*x1*x2/(x1+x2)
  return(f1scorenum)
}
#################################################################################################################

#############################
# Predicting the Recidivism #
#############################

#GLM model to predict recidivism using using all other factors withouth interaction

m1_recid_no_decile = glm(is_recid ~ age + juv_fel_count + juv_misd_count + priors_count  + druginvolvment+charge_degree + sex + race, family =binomial , data = train)
summary(m1_recid_no_decile)
plot(m1_recid_no_decile)


#Evaluation matrix - m1_recid_no_decile
pred_no_decile = m1_recid_no_decile %>% predict.glm(test,type="response") %>% {if_else(.> 0.5 , 1,0)} %>% as.factor(.)
caret::confusionMatrix(test$is_recid,pred_no_decile,positive='1')
pred_no_decile_precision = caret::precision(test$is_recid,pred_no_decile)
pred_no_decile_precision
pred_no_decile_recall = caret::recall(test$is_recid,pred_no_decile)
pred_no_decile_recall
f1score(pred_no_decile_precision,pred_no_decile_recall)
p1=m1_recid_no_decile %>% predict.glm(test,type="response")
pred1=prediction(p1,test$is_recid)
rocv1=performance(pred1,"tpr","fpr")
plot(rocv1,colorize=T, main="Violent score ROC curve", ) #ROC
aucv1=performance(pred1,"auc")
aucv1=round(unlist(slot(aucv1,"y.values")),4)
legend(0.6,0.3,aucv1,title = "AUC")

#------------------------------

#GLM model to predict recidivism using all the crime related factors 
m1_recid_crime_factors = glm(is_recid ~ juv_fel_count + juv_misd_count + priors_count + as.factor(druginvolvment)  +charge_degree, family =binomial , data = train)
summary(m1_recid_crime_factors)
plot(m1_recid_crime_factors)

#Evaluation matrix - m1_recid_crime_factors
pred_crime_fact = m1_recid_crime_factors %>% predict.glm(test,type="response") %>% {if_else(.> 0.5 , 1,0)} %>% as.factor(.)
caret::confusionMatrix(test$is_recid,pred_crime_fact,positive='1')
pred_crime_fact_precision = caret::precision(test$is_recid,pred_crime_fact)
pred_crime_fact_recall = caret::recall(test$is_recid,pred_crime_fact)
f1score(pred_crime_fact_precision,pred_crime_fact_recall)
p1=m1_recid_crime_factors %>% predict.glm(test,type="response")
pred1=prediction(p1,test$is_recid)
rocv1=performance(pred1,"tpr","fpr")
plot(rocv1,colorize=T, main="Violent score ROC curve", ) #ROC
aucv1=performance(pred1,"auc")
aucv1=round(unlist(slot(aucv1,"y.values")),4)
legend(0.6,0.3,aucv1,title = "AUC")

#------------------------------

#GLM mode using decile score alone as predictor of recidivism
m1_recid_decilescore = glm(is_recid ~ decile_score, family =binomial , data = train)
summary(m1_recid_decilescore)
plot(m1_recid_decilescore)


#Evaluation matrix - m1_recid_decilescore
pred_m1_recid_decilescore = m1_recid_decilescore %>% predict.glm(test,type="response") %>% {if_else(.> 0.5 , 1,0)} %>% as.factor(.)
caret::confusionMatrix(test$is_recid,pred_m1_recid_decilescore,positive='1')
pred_m1_recid_decilescore_precision = caret::precision(test$is_recid,pred_m1_recid_decilescore)
pred_m1_recid_decilescore_recall = caret::recall(test$is_recid,pred_m1_recid_decilescore)
f1score(pred_m1_recid_decilescore_precision,pred_m1_recid_decilescore_recall)
p1=m1_recid_decilescore %>% predict.glm(test,type="response")
pred1=prediction(p1,test$is_recid)
rocv1=performance(pred1,"tpr","fpr")
plot(rocv1,colorize=T, main="Violent score ROC curve", ) #ROC
aucv1=performance(pred1,"auc")
aucv1=round(unlist(slot(aucv1,"y.values")),4)
legend(0.6,0.3,aucv1,title = "AUC")
#------------------------------

#GLM model using race and sex alone as a predictor of recidivism
m1_recid_sex_race = glm(is_recid ~ sex*race, family =binomial , data = train)
summary(m1_recid_sex_race)
plot(m1_recid_sex_race)

#Evaluation matrix - m1_recid_sex_race
pred_m1_recid_sex_race = m1_recid_sex_race %>% predict.glm(test,type="response") %>% {if_else(.> 0.5 , 1,0)} %>% as.factor(.)
caret::confusionMatrix(test$is_recid,pred_m1_recid_sex_race,positive='1')
pred_m1_recid_sex_race_precision = caret::precision(test$is_recid,pred_m1_recid_sex_race)
pred_m1_recid_sex_race_precision
pred_m1_recid_sex_race_recall = caret::recall(test$is_recid,pred_m1_recid_sex_race)
pred_m1_recid_sex_race_recall
f1score(pred_m1_recid_sex_race_precision,pred_m1_recid_sex_race_recall)
p1=m1_recid_sex_race %>% predict.glm(test,type="response")
pred1=prediction(p1,test$is_recid)
rocv1=performance(pred1,"tpr","fpr")
plot(rocv1,colorize=T, main="Violent score ROC curve", ) #ROC
aucv1=performance(pred1,"auc")
aucv1=round(unlist(slot(aucv1,"y.values")),4)
legend(0.6,0.3,aucv1,title = "AUC")
#------------------------------

library(stargazer)
stargazer(m1_recid_no_decile,m1_recid_crime_factors,m1_recid_decilescore,m1_recid_sex_race,type = 'text')

################################################################################################################

#Running the models to determine the violent recidivism based on all the factors like above:

#GLM model to predict Voilent recidivism using using all other factors withought interaction

Vtrain.index <- caret::createDataPartition(d$is_violent_recid, p = .7, list = FALSE)
Vtrain <- d[ train.index,]
Vtest  <- d[-train.index,]

#model 1 - without decile score

m1_Vrecid_no_decile = glm(is_violent_recid ~ age + juv_fel_count + juv_misd_count + priors_count +as.factor(druginvolvment)  + sex + race+charge_degree_fact, family =binomial , data = Vtrain)
summary(m1_Vrecid_no_decile)
plot(m1_recid_no_decile)

#Evaluation matrix - m1_Vrecid_no_decile
Vpred_no_decile = m1_Vrecid_no_decile %>% predict.glm(Vtest,type="response") %>% {if_else(.>0.5,1,0)} %>% as.factor(.)
caret::confusionMatrix(Vtest$is_violent_recid,Vpred_no_decile,positive='1')
Vpred_no_decile_precision = caret::precision(Vtest$is_recid,Vpred_no_decile)
Vpred_no_decile_precision
Vpred_no_decile_recall = caret::recall(Vtest$is_recid,Vpred_no_decile)
Vpred_no_decile_recall
f1score(Vpred_no_decile_precision,Vpred_no_decile_recall)
p1=m1_Vrecid_no_decile %>% predict.glm(Vtest,type="response")
pred1=prediction(p1,Vtest$is_recid)
rocv1=performance(pred1,"tpr","fpr")
plot(rocv1,colorize=T, main="Violent score ROC curve", ) #ROC
aucv1=performance(pred1,"auc")
aucv1=round(unlist(slot(aucv1,"y.values")),4)
legend(0.6,0.3,aucv1,title = "AUC")

#------------------------------

#GLM model to predict recidivism using all the crime related factors 
m1_Vrecid_crime_factors = glm(is_violent_recid ~ juv_fel_count + juv_misd_count + priors_count + as.factor(druginvolvment) + charge_degree_fact, family =binomial , data = Vtrain)
summary(m1_Vrecid_crime_factors)
plot(m1_Vrecid_crime_factors)

#Evaluation matrix - m1_Vrecid_crime_factors
Vpred_crime_factor = m1_Vrecid_crime_factors %>% predict.glm(Vtest,type="response" ) %>% {if_else(.>0.5,1,0)} %>% as.factor(.)
caret::confusionMatrix(Vtest$is_violent_recid,Vpred_crime_factor,positive='1')
Vpred_crime_factor_precision = caret::precision(Vtest$is_recid,Vpred_crime_factor)
Vpred_crime_factor_precision
Vpred_crime_factor_recall = caret::recall(Vtest$is_recid,Vpred_crime_factor)
Vpred_crime_factor_recall
f1score(Vpred_crime_factor_precision,Vpred_crime_factor_recall)
p1=m1_Vrecid_crime_factors %>% predict.glm(Vtest,type="response")
pred1=prediction(p1,Vtest$is_recid)
rocv1=performance(pred1,"tpr","fpr")
plot(rocv1,colorize=T, main="Violent score ROC curve", ) #ROC
aucv1=performance(pred1,"auc")
aucv1=round(unlist(slot(aucv1,"y.values")),4)
legend(0.6,0.3,aucv1,title = "AUC")
#------------------------------

#GLM model to predict the using decile score as predictor of recidivism
m1_Vrecid_decilescore = glm(is_violent_recid ~ v_decile_score, family =binomial , data = Vtrain)
summary(m1_Vrecid_decilescore)
plot(m1_Vrecid_decilescore)


#Evaluation matrix - m1_Vrecid_decilescore
Vpred_decile = m1_Vrecid_decilescore %>% predict.glm(Vtest,type="response" ) %>% {if_else(.>0.2,1,0)} %>% as.factor(.) #since max probability is 0.25978 (calculated using summary before converting into factor)
caret::confusionMatrix(Vtest$is_violent_recid,Vpred_decile,positive='1')
Vpred_decile_precision = caret::precision(Vtest$is_recid,Vpred_decile)
Vpred_decile_precision
Vpred_decile_recall = caret::recall(Vtest$is_recid,Vpred_decile)
Vpred_decile_recall
f1score(Vpred_decile_precision,Vpred_decile_recall)
p1=m1_Vrecid_decilescore %>% predict.glm(Vtest,type="response")
pred1=prediction(p1,Vtest$is_recid)
rocv1=performance(pred1,"tpr","fpr")
plot(rocv1,colorize=T, main="Violent score ROC curve", ) #ROC
aucv1=performance(pred1,"auc")
aucv1=round(unlist(slot(aucv1,"y.values")),4)
legend(0.6,0.3,aucv1,title = "AUC")

#------------------------------

#using race and sex alone as a predictor of recidivism
m1_Vrecid_sex_race = glm(is_violent_recid ~ sex*race, family =binomial , data = Vtrain)
summary(m1_Vrecid_sex_race)
plot(m1_Vrecid_sex_race)


#Evaluation matrix - m1_Vrecid_sex_race
Vpred_sexandRace = m1_Vrecid_sex_race %>% predict.glm(Vtest,type="response") %>% {if_else(.>0.3,1,0)} %>% as.factor(.) #since max probability is 0.25978 (calculated using summary before converting into factor)
caret::confusionMatrix(Vtest$is_recid,Vpred_sexandRace,positive='1')
Vpred_sexandRace_precision = caret::precision(Vtest$is_recid,Vpred_sexandRace)
Vpred_sexandRace_precision
Vpred_sexandRace_recall = caret::recall(Vtest$is_recid,Vpred_sexandRace)
Vpred_sexandRace_recall
f1score(Vpred_sexandRace_precision,Vpred_sexandRace_recall)
p1=m1_Vrecid_sex_race %>% predict.glm(Vtest,type="response")
pred1=prediction(p1,Vtest$is_recid)
rocv1=performance(pred1,"tpr","fpr")
plot(rocv1,colorize=T, main="Violent score ROC curve", ) #ROC
aucv1=performance(pred1,"auc")
aucv1=round(unlist(slot(aucv1,"y.values")),4)
legend(0.6,0.3,aucv1,title = "AUC")
#------------------------------

library(stargazer)
stargazer(m1_Vrecid_no_decile,m1_Vrecid_crime_factors,m1_Vrecid_decilescore,m1_Vrecid_sex_race,type = 'text' )


################################################################################################################
#Computing a fair score matrix by considering only the crime factors as per the model m0_decile_crime_factors!
#Including all the factors is generating NAs.

plot(m0_decile_crime_factors_charge_degree)

#with drug involvment
d$myscore = round(exp(m0_decile_crime_factors$coefficients[1]) 
                  + exp(m0_decile_crime_factors$coefficients[2])*d$juv_fel_count 
                  + exp(m0_decile_crime_factors$coefficients[3])*d$juv_misd_count 
                  + exp(m0_decile_crime_factors$coefficients[4])*d$priors_count 
                  + exp(m0_decile_crime_factors$coefficients[5])*as.numeric(d$charge_degree_fact)
                  +exp(m0_decile_crime_factors$coefficients[6])*as.numeric(d$charge_degree_fact)
                  +exp(m0_decile_crime_factors$coefficients[7])*as.numeric(d$charge_degree_fact)
                  +exp(m0_decile_crime_factors$coefficients[8])*as.numeric(d$charge_degree_fact)
                  +exp(m0_decile_crime_factors$coefficients[9])*as.numeric(d$charge_degree_fact)
                  +exp(m0_decile_crime_factors$coefficients[10])*as.numeric(d$druginvolvment))


#My score summary stats
summary(d$myscore)
summary(m1_recid_crime_factors)
hist(d$myscore, breaks = 10)

#scaling my score in the range of 1-10 so it can be compared with decile score
d$scaledmyscore = round(rescale(d$myscore,to= c(1,10)))


summary(d$scaledmyscore)
hist(d$scaledmyscore)

summary(d$decile_score)
hist(d$decile_score)

###################Comparison of Fair score and Decile Score ####################

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

hist(d$scaledmyscore, col = c1, ylim = c(0,3000),main = NULL, breaks = 10, xlab = "Risk Score (1-10)")
hist(d$decile_score, col = c2, add = TRUE, breaks = 10)
title("Histogram of Fair score and Comaps-Decile score")
legend("topright", c("Fair Score", "Decile Score"), col=c(c1, c2), lwd=10)


d_africanamerica = subset(d, race == 'African-American', select = c('decile_score','myscore','scaledmyscore'))
hist(d_africanamerica$scaledmyscore, col= c1, ylim=c(0,1570), main = NULL, breaks = 10, xlab = "Risk Score (1-10)")
hist(d_africanamerica$decile_score, col = c2, add =  TRUE, breaks = 10)
title(" Comparing Fair and decile score for african-american population")
legend("topright", c("Fair Score", "Decile Score"), col=c(c1, c2), lwd=10)


d_caucasian = subset(d, race == 'Caucasian', select = c('decile_score','myscore','scaledmyscore'))
hist(d_caucasian$scaledmyscore, col= c1, ylim=c(0,1270),xlim = c(1,10),main = NULL, breaks = 10, xlab = "Risk Score (1-10)")
hist(d_caucasian$decile_score, col = c2, add =  TRUE, breaks = 10)
title(" Comparing Fair and decile score for caucasian population")
legend("topright", c("Fair Score", "Decile Score"), col=c(c1, c2), lwd=10)


summary(d$scaledmyscore)
summary(d_africanamerica$scaledmyscore)
summary(d_caucasian$scaledmyscore)

hist(d$scaledmyscore)

summary(d$decile_score)
summary(d_africanamerica$decile_score)
summary(d_caucasian$decile_score)
hist(d$decile_score)

##########################################################################################################

#Checking the records where decile score is same as fair score

table(d$decile_score, d$scaledmyscore)

d$same_score  = if_else(d$decile_score == d$scaledmyscore,"Yes","No")
table(d$same_score)

#so only records where decile score is 1,2,3 (i.e. low score) is matching with My_score since my score is very skewed towards lower scores.

#Checking percent difference in decile score and my score

d$score_diff_percent = ((((d$decile_score) - (d$scaledmyscore)) / mean(d$decile_score))*100)
summary(d$score_diff_percent)


####################################################################################################

#Checking if the new scaled my score is a good predictor of recidivism

myscore_train.index = caret::createDataPartition(d$is_recid, p = 0.7, list = FALSE)
myscore_train = d[myscore_train.index,]
myscore_test = d[-myscore_train.index,]

table(myscore_test$is_recid)

#GLM to test the myscore against recidivism

myscore_recid = glm(is_recid ~ scaledmyscore, family = binomial, data = myscore_train)
decile_recid = glm(is_recid ~ decile_score, family = binomial, data = myscore_train)

stargazer(myscore_recid,decile_recid, type = "text")

#MODEL Performance evaluation
pred_myscore_recid = myscore_recid %>% predict.glm(myscore_test,type="response") %>% {if_else(.>0.43,1,0)} %>% as.factor(.) #since min probability is 0.2078 (calculated using summary before converting into factor)
###
caret::confusionMatrix(myscore_test$is_recid,pred_myscore_recid,positive='1')
pred_myscore_recid_precision = caret::precision(myscore_test$is_recid,pred_myscore_recid)
pred_myscore_recid_precision
pred_myscore_recid_recall = caret::recall(myscore_test$is_recid,pred_myscore_recid)
pred_myscore_recid_recall
f1score(pred_myscore_recid_precision,pred_myscore_recid_recall)
pr=myscore_recid %>% predict.glm(myscore_test,type="response")
pre=prediction(pr,myscore_test$is_recid)
roc=performance(pre,"tpr","fpr")
plot(roc,colorize=T, main="Fair score ROC curve", ) #ROC
aucf=performance(pre,"auc")
aucf=round(unlist(slot(aucf,"y.values")),4)
legend(0.6,0.3,aucf,title = "AUC")
###########

#For Decile score
pred_decile_recid = decile_recid %>% predict.glm(myscore_test,type="response") %>% {if_else(.>0.43,1,0)} %>% as.factor(.) #since min probability is 0.2078 (calculated using summary before converting into factor)
caret::confusionMatrix(myscore_test$is_recid,pred_decile_recid,positive='1')
pred_decile_recid_precision = caret::precision(myscore_test$is_recid,pred_decile_recid)
pred_decile_recid_precision
pred_decile_recid_recall = caret::recall(myscore_test$is_recid,pred_decile_recid)
pred_decile_recid_recall
f1score(pred_decile_recid_precision,pred_decile_recid_recall)
pr1=decile_recid %>% predict.glm(myscore_test,type="response")
pre1=prediction(pr1,myscore_test$is_recid)
rocd=performance(pre1,"tpr","fpr")
plot(rocd,colorize=T, main="Decile score ROC curve", ) #ROC
aucd=performance(pre1,"auc")
aucd=round(unlist(slot(aucd,"y.values")),4)
legend(0.6,0.3,aucd,title = "AUC")

##ROC Fair score and Decile score combined#######
roc=performance(pre,"tpr","fpr")
plot(roc, col="red",main="ROC curve")
pr1=decile_recid %>% predict.glm(myscore_test,type="response")
pre1=prediction(pr1,myscore_test$is_recid)
roc1=performance(pre1,"tpr","fpr")
plot(roc1,add=TRUE,col="green", main="ROC curve Comparison")
abline(a=0,b=1)  


####################################################

d$difference = d$decile_score-d$scaledmyscore
summary(d$difference)
summary(d$decile_score)
summary(d$scaledmyscore)
percent_inflation = (mean(d$decile_score)-mean(d$scaledmyscore));percent_inflation
percent_inflation/mean(d$decile_score)


###############################################


#----------------------------------------------------------------------------------------------------------------------------#
#                                     Using Data Mining for Bank Direct Marketing                                            #
#                                     Data: bank-full.csv                                                                    #                                                                    #
#                                     Author : Guo Yang,Wang Longling,Yu hefei,Zheng Siya                                    #
#                                     Date : Oct,2017                                                                        #
#----------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------set working directory----------------------------------------------------#
setwd("/Users/mdt000mbp/Desktop/My Doc/Data Mining/Group Project/bank_data_analysis-master");


##----------------------------------------------------Insatll packages used for analyzing--------------------------------------------#
install.packages("ggplot2", dependencies = TRUE)
install.packages("Rcpp",dependencies = TRUE)
install.packages("ggthemes")
install.packages("glmnet")
install.packages("corrplot")
install.packages("rpart")
install.packages("randomForest")
install.packages("e1071")
install.packages("party")
install.packages("gbm")
install.packages("class")
install.packages("MASS")
install.packages("car",dependencies = TRUE)
install.packages("caret",dependencies = TRUE)
install.packages("mccr")
install.packages("caretEnsemble")
install.packages("DMwR")
install.packages("e1071")
install.packages("RWeka")
install.packages("h2o")
install.packages("ggmosaic")
install.packages("tidyverse")
install.packages("ipred")



#----------------------------------------------------------------load library--------------------------------------------------------#
library(Rcpp)
library(ggplot2) # Data visualization
library(ggthemes) # Data visualization
library(caret) # Streamline the process of creating random variable.
library(rpart) # Decision tree utils
library(randomForest) # Random Forest
library(e1071) # SVM
library(party) # Conditional inference trees
library(ggmosaic)
library(tidyverse)
library(gbm) # AdaBoost
library(class) # KNN
library(corrplot)
library(car)
library(MASS)
library(mccr)
library(caret)
library(DMwR)
library(caretEnsemble)
library("DMwR")
library("rpart")
library(nnet) #neural network
library("e1071") #svm
library("RWeka") # J48
library(h2o)  # deep learning, you need to install java jdk 8 first(not jdk9).
library("ipred")



# Common function to calculating MCC using 5-fold cross validation for lda, rpart, neural network,deep learning,random forest and so on.
source("my_mcc.R")

#----------------------------------------------------Exploring and preparing the data-------------------------------------------------#
# Collecting Data
bank<-read.table("bank-full.csv",sep=";",head=TRUE)
str(bank)
summary(bank)
is.na(bank) ## no null values


# Feature Engineering

## Impact of age

ggplot(bank, aes(age, color=y)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=2)  + 
  labs(title = "How Age impact success", x = "age", y = "Count", fill = "success")

## Impact of job
table(bank$job, bank$y)

bank <- bank %>%
  mutate(y=factor(y,levels=c("yes","no"),ordered=TRUE))
ggplot(data = bank) + geom_mosaic(aes(x = product(job), fill = y)) + 
  labs(x = "Job", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of marital
ggplot(data = bank) + geom_mosaic(aes(x = product(marital), fill = y)) + 
  labs(x = "Marital status", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of education
ggplot(data = bank) + geom_mosaic(aes(x = product(education), fill = y)) + 
  labs(x = "Education", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of default
ggplot(data = bank) + geom_mosaic(aes(x = product(default), fill = y)) + 
  labs(x = "Default", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of balance
breaks=c(-8020, 200, 400,600,800,1000,2000,5000,102127)
bank$balancecut<-cut(bank$balance,breaks)
levels(bank$balancecut)
ggplot(data = bank) + geom_mosaic(aes(x = product(balancecut), fill = y)) + 
  labs(x = "Balance Group", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of housing
ggplot(data = bank) + geom_mosaic(aes(x = product(housing), fill = y)) + 
  labs(x = "Housing", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of loan
ggplot(data = bank) + geom_mosaic(aes(x = product(loan), fill = y)) + 
  labs(x = "Loan", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of contact
ggplot(data = bank) + geom_mosaic(aes(x = product(contact), fill = y)) + 
  labs(x = "Contact", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of day
breaks=c(-1,0,seq(0:31))
bank$daycut<-cut(bank$day,breaks)
levels(bank$daycut)
ggplot(data = bank) + geom_mosaic(aes(x = product(daycut), fill = y)) + 
  labs(x = "Day", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of month
bank <- bank %>%
  mutate(month=factor(month,levels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),ordered=TRUE))
ggplot(data = bank) + geom_mosaic(aes(x = product(month), fill = y)) + 
  labs(x = "Month", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of duration
breaks=c(-1,100,200,300,400,500,1000,60000)
bank$durationcut<-cut(bank$duration,breaks)
levels(bank$durationcut)
ggplot(data = bank) + geom_mosaic(aes(x = product(durationcut), fill = y)) + 
  labs(x = "Duration(s)", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of campaign
breaks=c(0,2,5,10,20,30,70)
bank$campaigncut<-cut(bank$campaign,breaks)
levels(bank$campaigncut)
ggplot(data = bank) + geom_mosaic(aes(x = product(campaigncut), fill = y)) + 
  labs(x = "Campaign", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of pdays
breaks=c(-2,-1,100,200,300,500,2000)
bank$pdayscut<-cut(bank$pdays,breaks)
levels(bank$pdayscut)
ggplot(data = bank) + geom_mosaic(aes(x = product(pdayscut), fill = y)) + 
  labs(x = "Pdays", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of previous
breaks=c(-1,0,1,2,5,8,10,500)
bank$previouscut<-cut(bank$previous,breaks)
levels(bank$previouscut)
ggplot(data = bank) + geom_mosaic(aes(x = product(previouscut), fill = y)) + 
  labs(x = "Previous", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

## Impact of poutcome
ggplot(data = bank) + geom_mosaic(aes(x = product(poutcome), fill = y)) + 
  labs(x = "Poutcome", y = "Subscribe Ratio") + 
  theme(panel.background = NULL,axis.text.x = element_text(angle=90, vjust=1))

#Transformation of Categorical Variable to Numerical Variable
## Transfer job categories to numeric values
bank$job_admin_numeric[bank$job=="admin."]<-"1"
bank$job_admin_numeric[!bank$job=="admin."]<-"0"
bank$job_admin_numeric<-as.numeric(bank$job_admin_numeric)
bank$job_bluecollar_numeric[bank$job=="blue-collar"]<-"1"
bank$job_bluecollar_numeric[!bank$job=="blue-collar"]<-"0"
bank$job_bluecollar_numeric<-as.numeric(bank$job_bluecollar_numeric)
bank$job_entrepreneur_numeric[bank$job=="entrepreneur"]<-"1"
bank$job_entrepreneur_numeric[!bank$job=="entrepreneur"]<-"0"
bank$job_entrepreneur_numeric<-as.numeric(bank$job_entrepreneur_numeric)
bank$job_housmaid_numeric[bank$job=="housemaid"]<-"1"
bank$job_housmaid_numeric[!bank$job=="housemaid"]<-"0"
bank$job_housmaid_numeric<-as.numeric(bank$job_housmaid_numeric)
bank$job_management_numeric[bank$job=="management"]<-"1"
bank$job_management_numeric[!bank$job=="management"]<-"0"
bank$job_management_numeric<-as.numeric(bank$job_management_numeric)
bank$job_retired_numeric[bank$job=="retired"]<-"1"
bank$job_retired_numeric[!bank$job=="retired"]<-"0"
bank$job_retired_numeric<-as.numeric(bank$job_retired_numeric)
bank$job_selfemployed_numeric[bank$job=="self-employed"]<-"1"
bank$job_selfemployed_numeric[!bank$job=="self-employed"]<-"0"
bank$job_selfemployed_numeric<-as.numeric(bank$job_selfemployed_numeric)
bank$job_services_numeric[bank$job=="services"]<-"1"
bank$job_services_numeric[!bank$job=="services"]<-"0"
bank$job_services_numeric<-as.numeric(bank$job_services_numeric)
bank$job_student_numeric[bank$job=="student"]<-"1"
bank$job_student_numeric[!bank$job=="student"]<-"0"
bank$job_student_numeric<-as.numeric(bank$job_student_numeric)
bank$job_technician_numeric[bank$job=="technician"]<-"1"
bank$job_technician_numeric[!bank$job=="technician"]<-"0"
bank$job_technician_numeric<-as.numeric(bank$job_technician_numeric)
bank$job_unemployed_numeric[bank$job=="unemployed"]<-"1"
bank$job_unemployed_numeric[!bank$job=="unemployed"]<-"0"
bank$job_unemployed_numeric<-as.numeric(bank$job_unemployed_numeric)
bank$job_unknown_numeric[bank$job=="unknown"]<-"1"
bank$job_unknown_numeric[!bank$job=="unknown"]<-"0"
bank$job_unknown_numeric<-as.numeric(bank$job_unknown_numeric)
#Transfer month categories to numeric values
bank$month_jan_numeric[bank$month=="jan"]<-"1"
bank$month_jan_numeric[!bank$month=="jan"]<-"0"
bank$month_jan_numeric<-as.numeric(bank$month_jan_numeric)
bank$month_feb_numeric[bank$month=="feb"]<-"1"
bank$month_feb_numeric[!bank$month=="feb"]<-"0"
bank$month_feb_numeric<-as.numeric(bank$month_feb_numeric)
bank$month_mar_numeric[bank$month=="mar"]<-"1"
bank$month_mar_numeric[!bank$month=="mar"]<-"0"
bank$month_mar_numeric<-as.numeric(bank$month_mar_numeric)
bank$month_apr_numeric[bank$month=="apr"]<-"1"
bank$month_apr_numeric[!bank$month=="apr"]<-"0"
bank$month_apr_numeric<-as.numeric(bank$month_apr_numeric)
bank$month_may_numeric[bank$month=="may"]<-"1"
bank$month_may_numeric[!bank$month=="may"]<-"0"
bank$month_may_numeric<-as.numeric(bank$month_may_numeric)
bank$month_jun_numeric[bank$month=="jun"]<-"1"
bank$month_jun_numeric[!bank$month=="jun"]<-"0"
bank$month_jun_numeric<-as.numeric(bank$month_jun_numeric)
bank$month_jul_numeric[bank$month=="jul"]<-"1"
bank$month_jul_numeric[!bank$month=="jul"]<-"0"
bank$month_jul_numeric<-as.numeric(bank$month_jul_numeric)
bank$month_aug_numeric[bank$month=="aug"]<-"1"
bank$month_aug_numeric[!bank$month=="aug"]<-"0"
bank$month_aug_numeric<-as.numeric(bank$month_aug_numeric)
bank$month_sep_numeric[bank$month=="sep"]<-"1"
bank$month_sep_numeric[!bank$month=="sep"]<-"0"
bank$month_sep_numeric<-as.numeric(bank$month_sep_numeric)
bank$month_oct_numeric[bank$month=="oct"]<-"1"
bank$month_oct_numeric[!bank$month=="oct"]<-"0"
bank$month_oct_numeric<-as.numeric(bank$month_oct_numeric)
bank$month_nov_numeric[bank$month=="nov"]<-"1"
bank$month_nov_numeric[!bank$month=="nov"]<-"0"
bank$month_nov_numeric<-as.numeric(bank$month_nov_numeric)
bank$month_dec_numeric[bank$month=="dec"]<-"1"
bank$month_dec_numeric[!bank$month=="dec"]<-"0"
bank$month_dec_numeric<-as.numeric(bank$month_dec_numeric)
## Transfer marital categories to numeric values
bank$marital_divorced_numeric[bank$marital=="divorced"]<-"1"
bank$marital_divorced_numeric[!bank$marital=="divorced"]<-"0"
bank$marital_divorced_numeric<-as.numeric(bank$marital_divorced_numeric)
bank$marital_married_numeric[bank$marital=="married"]<-"1"
bank$marital_married_numeric[!bank$marital=="married"]<-"0"
bank$marital_married_numeric<-as.numeric(bank$marital_married_numeric)
bank$marital_single_numeric[bank$marital=="single"]<-"1"
bank$marital_single_numeric[!bank$marital=="single"]<-"0"
bank$marital_single_numeric<-as.numeric(bank$marital_single_numeric)
## Transfer education categories to numeric values
bank$education_unknown_numeric[bank$education=="unknown"]<-"1"
bank$education_unknown_numeric[!bank$education=="unknown"]<-"0"
bank$education_unknown_numeric<-as.numeric(bank$education_unknown_numeric)
bank$education_primary_numeric[bank$education=="primary"]<-"1"
bank$education_primary_numeric[!bank$education=="primary"]<-"0"
bank$education_primary_numeric<-as.numeric(bank$education_primary_numeric)
bank$education_secondary_numeric[bank$education=="secondary"]<-"1"
bank$education_secondary_numeric[!bank$education=="secondary"]<-"0"
bank$education_secondary_numeric<-as.numeric(bank$education_secondary_numeric)
bank$education_tertiary_numeric[bank$education=="tertiary"]<-"1"
bank$education_tertiary_numeric[!bank$education=="tertiary"]<-"0"
bank$education_tertiary_numeric<-as.numeric(bank$education_tertiary_numeric)
## Transfer contact categories to numeric values
bank$contact_cellular_numeric[bank$contact=="cellular"]<-"1"
bank$contact_cellular_numeric[!bank$contact=="cellular"]<-"0"
bank$contact_cellular_numeric<-as.numeric(bank$contact_cellular_numeric)
bank$contact_telephone_numeric[bank$contact=="telephone"]<-"1"
bank$contact_telephone_numeric[!bank$contact=="telephone"]<-"0"
bank$contact_telephone_numeric<-as.numeric(bank$contact_telephone_numeric)
bank$contact_unknown_numeric[bank$contact=="unknown"]<-"1"
bank$contact_unknown_numeric[!bank$contact=="unknown"]<-"0"
bank$contact_unknown_numeric<-as.numeric(bank$contact_unknown_numeric)
## Transfer contact categories to numeric values
bank$contact_cellular_numeric[bank$contact=="cellular"]<-"1"
bank$contact_cellular_numeric[!bank$contact=="cellular"]<-"0"
bank$contact_cellular_numeric<-as.numeric(bank$contact_cellular_numeric)
bank$contact_telephone_numeric[bank$contact=="telephone"]<-"1"
bank$contact_telephone_numeric[!bank$contact=="telephone"]<-"0"
bank$contact_telephone_numeric<-as.numeric(bank$contact_telephone_numeric)
bank$contact_unknown_numeric[bank$contact=="unknown"]<-"1"
bank$contact_unknown_numeric[!bank$contact=="unknown"]<-"0"
bank$contact_unknown_numeric<-as.numeric(bank$contact_unknown_numeric)
## Transfer default categories to numeric values
bank$default_numeric[bank$default=="no"]<-"0"
bank$default_numeric[bank$default=="yes"]<-"1"
bank$default_numeric<-as.numeric(bank$default_numeric)
## Transfer loan categories to numeric values
bank$loan_numeric[bank$loan=="no"]<-"0"
bank$loan_numeric[bank$loan=="yes"]<-"1"
bank$loan_numeric<-as.numeric(bank$loan_numeric)
## Transfer housing categories to numeric values
bank$housing_numeric[bank$housing=="no"]<-"0"
bank$housing_numeric[bank$housing=="yes"]<-"1"
bank$housing_numeric<-as.numeric(bank$housing_numeric)
#Transfer poutcome categories to numeric values
bank$poutcome_unknown_numeric[bank$poutcome=="unknown"]<-"1"
bank$poutcome_unknown_numeric[!bank$poutcome=="unknown"]<-"0"
bank$poutcome_unknown_numeric<-as.numeric(bank$poutcome_unknown_numeric)
bank$poutcome_failure_numeric[bank$poutcome=="failure"]<-"1"
bank$poutcome_failure_numeric[!bank$poutcome=="failure"]<-"0"
bank$poutcome_failure_numeric<-as.numeric(bank$poutcome_failure_numeric)
bank$poutcome_success_numeric[bank$poutcome=="success"]<-"1"
bank$poutcome_success_numeric[!bank$poutcome=="success"]<-"0"
bank$poutcome_success_numeric<-as.numeric(bank$poutcome_success_numeric)
bank$poutcome_other_numeric[bank$poutcome=="other"]<-"1"
bank$poutcome_other_numeric[!bank$poutcome=="other"]<-"0"
bank$poutcome_other_numeric<-as.numeric(bank$poutcome_other_numeric)

## Check the correlation between the attributes. Values above 0.75 or below -0.75 are indicative of high positive or high negative correlation. 
str(bank)
str(bank[c(24:64)])
cor(bank[c(24:64)])
## as it is shown in the correlation form, there is no significant correlation between any two of the variables
## write.csv(bank, file = "newbank.csv", row.names = FALSE) # try to use weka to select attributes



## we could choose two ways for discriminants methods:using original non-categorical attributes or using all transformed numeric attributes
## bank_originalDiscriminants<-bank[c(1,6,10,12,13,14,15,17)]
## bank_Discriminants<-bank[c(17,1,6,10,12,13,14,15,25:64)]
## we choose bank_Discriminants<-bank[c(17:65)] first
bank_originalDiscriminants<-bank[c(1,6,10,12,13,14,15,17)]
bank_Discriminants<-bank[c(17,1,6,10,12,13,14,15,24:64)]
bank2=bank[1:17]

#------------------------------------------------Train models on the data and eveluate model performance---------------------------------------#

#**************************************************************************************
#
#    	Linear Discriminants (LDA)
#
#**************************************************************************************
# LDA---Using originaldata
#[1] 0.291677 to 0.3486922
bank_lda<-bank_originalDiscriminants
mccrs_lda<- calc_seed_mcc('lda',bank_lda,5,5,"original")
mean_mcc_lda <- mean(mccrs_lda)   
lci <- mean(mccrs_lda) - sd(mccrs_lda) * 1.96
uci <- mean(mccrs_lda) + sd(mccrs_lda) * 1.96
mccrs_lda
#[1] 0.3207995 0.3301442 0.3132280 0.3133025 0.3207107 0.3269868 0.2995016 0.3043949 0.3226610 0.3404311 0.3227879 0.3363438 0.3255491 0.3204734
#[15] 0.3009576 0.3109323 0.3385516 0.3054439 0.3135248 0.3348233 0.3249288 0.3128876 0.3459170 0.2849928 0.3343417
mean_mcc_lda #[1] 0.3201846
lci  #[1] 0.291677
uci  #[1] 0.3486922


#LDA---Using Transferred data
bank_lda_2<-bank_Discriminants
mccrs_lda_2<- calc_seed_mcc('lda',bank_lda_2,5,5,"original")
mean_mcc_lda_2 <- mean(mccrs_lda_2)   
lci <- mean(mccrs_lda_2) - sd(mccrs_lda_2) * 1.96
uci <- mean(mccrs_lda_2) + sd(mccrs_lda_2) * 1.96
mccrs_lda_2
#[1] 0.4521571 0.4668396 0.4701956 0.4387505 0.4770019 0.4595849 0.4436271 0.4490420 0.4824845 0.4672310 0.4692227 0.4572482 0.4543464 0.4730705
#[15] 0.4467868 0.4430062 0.4621063 0.4646641 0.4669283 0.4748430 0.4647767 0.4637291 0.4583456 0.4407654 0.4829639
mean_mcc_lda_2 #[1] 0.4611887
lci  #[1] 0.436352
uci  #[1] 0.4860254


#LDA--Using Pre-scale
bank_lda_3<-bank_Discriminants
mccrs_lda_3<- calc_seed_mcc('lda_prescale',bank_lda_3,5,5,"pre_scale")
mean_mcc_lda_3 <- mean(mccrs_lda_3)   
lci <- mean(mccrs_lda_3) - sd(mccrs_lda_3) * 1.96
uci <- mean(mccrs_lda_3) + sd(mccrs_lda_3) * 1.96
mccrs_lda_3
#[1] 0.4521571 0.4668396 0.4701956 0.4387505 0.4770019 0.4595849 0.4436271 0.4490420 0.4824845 0.4672310 0.4692227 0.4572482 0.4543464 0.4730705
#[15] 0.4467868 0.4430062 0.4621063 0.4646641 0.4669283 0.4748430 0.4647767 0.4637291 0.4583456 0.4407654 0.4829639
mean_mcc_lda_3 #[1] 0.4611887
lci  #[1] 0.436352
uci  #[1] 0.4860254


#LDA--Using smote
bank_lda_4<-bank_Discriminants
mccrs_lda_4<- calc_seed_mcc('lda',bank_lda_4,5,5,"smote")
mean_mcc_lda_4 <- mean(mccrs_lda_4)   
lci <- mean(mccrs_lda_4) - sd(mccrs_lda_4) * 1.96
uci <- mean(mccrs_lda_4) + sd(mccrs_lda_4) * 1.96
mccrs_lda_4
#[1] 0.5057237 0.5358802 0.5125829 0.5147682 0.5377336 0.5140125 0.5238962 0.5120820 0.5287302 0.5290162 0.5253840 0.5183143 0.5168788 0.5331001
#[15] 0.5114884 0.5098902 0.5324968 0.5276735 0.5227282 0.5112307 0.5179829 0.5137604 0.5133638 0.5125786 0.5430263
mean_mcc_lda_4 
#[1] 0.5209729
lci  #[1] 0.5011294
uci  #[1] 0.5408164


#LDA--Using Pre-scale and smote
bank_lda_5<-bank_Discriminants
mccrs_lda_5<- calc_seed_mcc('lda_prescale',bank_lda_5,5,5,"smote")
mean_mcc_lda_5 <- mean(mccrs_lda_5)   
lci <- mean(mccrs_lda_5) - sd(mccrs_lda_5) * 1.96
uci <- mean(mccrs_lda_5) + sd(mccrs_lda_5) * 1.96
mccrs_lda_5
mean_mcc_lda_5
lci  
uci  


#LDA--using UpSample
bank_lda_6<-bank_Discriminants
mccrs_lda_6<- calc_seed_mcc('lda',bank_lda_6,5,5,"original")
mean_mcc_lda_6 <- mean(mccrs_lda_6)   
lci <- mean(mccrs_lda_6) - sd(mccrs_lda_6) * 1.96
uci <- mean(mccrs_lda_6) + sd(mccrs_lda_6) * 1.96
mccrs_lda_6
mean_mcc_lda_6 
lci 
uci  



#**************************************************************************************
#
#    	GLM - Logistic Regression
#
#**************************************************************************************
#GLM  --Oringinal data 
bank_glm<-bank[c(1:17)]
mccrs_glm<- calc_seed_mcc('glm',bank_glm,5,5,"original")
mean_mcc_glm <- mean(mccrs_glm)   
lci <- mean(mccrs_glm) - sd(mccrs_glm) * 1.96
uci <- mean(mccrs_glm) + sd(mccrs_glm) * 1.96
mccrs_glm
#[1] 0.4259230 0.4192280 0.4269814 0.4178725 0.4460781 0.4140615 0.4393081 0.4062543 0.4407730 0.4221875 0.4273253 0.4293791 0.4197902 0.4437536
#[15] 0.4107440 0.4077109 0.4416592 0.4202375 0.4263179 0.4448994 0.4346050 0.4402594 0.4238931 0.3982519 0.4455904
mean_mcc_glm #[1] 0.4269234
lci  # [1] 0.4002572
uci #[1] 0.4535896

#GLM -- SMOTE 
bank_glm<-bank[c(1:17)]
mccrs_glm<- calc_seed_mcc('glm',bank_glm,5,5,"smote")
mean_mcc_glm <- mean(mccrs_glm)   
lci <- mean(mccrs_glm) - sd(mccrs_glm) * 1.96
uci <- mean(mccrs_glm) + sd(mccrs_glm) * 1.96
mccrs_glm
# [1] 0.4839108 0.4916402 0.4734592 0.4672472 0.5001552 0.4874802 0.4820082 0.4719961 0.4878759 0.4954404 0.4865021 0.4823496 0.4731533 0.4900782
# [15] 0.4922587 0.4703768 0.4867273 0.4988428 0.4831213 0.4931408 0.4834947 0.4894104 0.4836675 0.4722864 0.4957366
mean_mcc_glm # [1] 0.4848944
lci  #[1] 0.4668628
uci  #[1] 0.502926

#*********************************************************
#
#    	Trees using rpart
#
#*********************************************************
# Rpart with Gini---Original data
bank_rpart<-bank[c(1:17)]
mccrs_rpart<- calc_seed_mcc('rpart_gini',bank_rpart,5,5,"original")
mean_mccrs_rpart <- mean(mccrs_rpart)   
lci <- mean(mccrs_rpart) - sd(mccrs_rpart) * 1.96
uci <- mean(mccrs_rpart) + sd(mccrs_rpart) * 1.96
mccrs_rpart
#[1] 0.4133525 0.4202375 0.4245689 0.4084608 0.4360400 0.4253406 0.4087494 0.4036673 0.4613244 0.4175818 0.4202302 0.4774040 0.4234965 0.4397171
#[15] 0.4038155 0.3939032 0.4041234 0.4293356 0.4394479 0.4642124 0.4299124 0.4179947 0.4486197 0.4423483 0.4600022
mean_mccrs_rpart #[1] 0.4285555
lci  #[1] 0.3864164
uci #[1] 0.4706945

# Rpart with Gini---smote
bank_rpart<-bank[c(1:17)]
mccrs_rpart<- calc_seed_mcc('rpart_gini',bank_rpart,5,5,"smote")
mean_mccrs_rpart <- mean(mccrs_rpart)   
lci <- mean(mccrs_rpart) - sd(mccrs_rpart) * 1.96
uci <- mean(mccrs_rpart) + sd(mccrs_rpart) * 1.96
mccrs_rpart
#[1] 0.4595268 0.4638021 0.4349366 0.4587634 0.4726365 0.4657650 0.4411305 0.4462239 0.4830695 0.4665721 0.4540439 0.4739301 0.4252453 0.4589345
#[15] 0.4532072 0.4541447 0.4148415 0.4751487 0.4601162 0.4754766 0.4558213 0.4638640 0.4560957 0.4369162 0.4355917
mean_mccrs_rpart  #[1] 0.4554322
lci  #[1] 0.4228298
uci  #[1] 0.4880345

# Rpart with entropy---Original data
bank_rpart<-bank[c(1:17)]
mccrs_rpart<- calc_seed_mcc('rpart_entropy',bank_rpart,5,5,"original")
mean_mccrs_rpart <- mean(mccrs_rpart)   
lci <- mean(mccrs_rpart) - sd(mccrs_rpart) * 1.96
uci <- mean(mccrs_rpart) + sd(mccrs_rpart) * 1.96
mccrs_rpart
#[1] 0.4023708 0.4125274 0.4373151 0.4003154 0.4236033 0.3979921 0.4038159 0.4142404 0.4469243 0.4332278 0.3913213 0.4409280 0.4178128 0.4210991
#[15] 0.3806802 0.3849214 0.4037152 0.4181772 0.4318895 0.4300399 0.4050753 0.4087553 0.4020511 0.4038751 0.4705865
mean_mccrs_rpart #[1] 0.4153304
lci  #[1] 0.3748346
uci  #[1] 0.4558262

# Rpart with entropy---smote
bank_rpart<-bank[c(1:17)]
mccrs_rpart<- calc_seed_mcc('rpart_entropy',bank_rpart,5,5,"smote")
mean_mccrs_rpart <- mean(mccrs_rpart)   
lci <- mean(mccrs_rpart) - sd(mccrs_rpart) * 1.96
uci <- mean(mccrs_rpart) + sd(mccrs_rpart) * 1.96
mccrs_rpart
#[1] 0.4602405 0.4628837 0.4394154 0.4464318 0.4645440 0.4636182 0.4451782 0.4275400 0.4830695 0.4665721 0.4569542 0.4653926 0.4373068 0.4614380
#[15] 0.4550583 0.4423888 0.4524029 0.4716840 0.4601162 0.4530221 0.4523145 0.4638640 0.4483484 0.4335682 0.4102785
mean_mccrs_rpart #[1] 0.4529452
lci  #[1] 0.4225601
uci #[1] 0.4833304

#*********************************************************
#
#    	Neural  network
#
#*********************************************************
# Neural  network---original data
bank_nnet<-bank[c(1:17)]
mccrs_nnet<- calc_seed_mcc('nnet',bank_nnet,5,5,"original")
mean_mccrs_nnet <- mean(mccrs_nnet)   
lci <- mean(mccrs_nnet) - sd(mccrs_nnet) * 1.96
uci <- mean(mccrs_nnet) + sd(mccrs_nnet) * 1.96
mccrs_nnet
#[1] 0.4650317 0.4628008 0.4902883 0.4725380 0.5184531 0.5028947 0.4899105 0.4428252 0.5171173 0.4789072 0.4809129 0.4856402 0.4596779 0.4578068
#[15] 0.4748290 0.4758927 0.4914627 0.4984622 0.4708275 0.4988161 0.4840845 0.5037326 0.4781587 0.4494139 0.5237456
mean_mccrs_nnet #[1] 0.4829692
lci  #[1] 0.4416858
uci #[1] 0.5242526

# Neural  network---smote
bank_nnet<-bank[c(1:17)]
mccrs_nnet<- calc_seed_mcc('nnet',bank_nnet,5,5,"smote")
mean_mccrs_nnet <- mean(mccrs_nnet)   
lci <- mean(mccrs_nnet) - sd(mccrs_nnet) * 1.96
uci <- mean(mccrs_nnet) + sd(mccrs_nnet) * 1.96
mccrs_nnet
#[1] 0.5007131 0.5107681 0.5063438 0.5064154 0.5523042 0.5011690 0.5176282 0.5164732 0.5004295 0.5164452 0.5002831 0.5361265 0.5093438 0.5141441
#[15] 0.5049800 0.5007065 0.5515906 0.5356397 0.5256260 0.5215374 0.5211079 0.5425571 0.5087914 0.5102130 0.5231836
mean_mccrs_nnet #[1] 0.5173808
lci  #[1] 0.4866986
uci #1] 0.5480631


#*********************************************************
#
#    	SVM
#
#*********************************************************
## SVM -smote
bank_svm<-bank_Discriminants
mccrs_svm<- calc_seed_mcc('svm',bank_svm,5,5,"smote")
mean_mccrs_svm <- mean(mccrs_svm)   
lci <- mean(mccrs_svm) - sd(mccrs_svm) * 1.96
uci <- mean(mccrs_svm) + sd(mccrs_svm) * 1.96
mccrs_svm
#[1]0.5320938 0.5473141 0.5146044 0.5268694 0.5406574 0.5233268 0.5256306 0.5174377 0.5331968
#[10] 0.5323514 0.5420244 0.5317606 0.5259909 0.5107265 0.5242834 0.5135417 0.5392996 0.5325879
#[19] 0.5362675 0.5260671 0.5372245 0.5416720 0.5239903 0.5152524 0.5394985
mean_mccrs_svm 
lci 
uci 


#*********************************************************
#
#    	J48
#
#*********************************************************
#j48---smote
bank_j48<-bank[c(1:17)]
mccrs_j48<- calc_seed_mcc('j48',bank_j48,5,5,"smote")
mean_mccrs_j48<- mean(mccrs_j48)   
lci <- mean(mccrs_j48) - sd(mccrs_j48) * 1.96
uci <- mean(mccrs_j48) + sd(mccrs_j48) * 1.96
mccrs_j48
#[1] 0.4882120 0.5192823 0.4748539 0.5012115 0.4926749 0.5146454 0.5096833 0.4866307
#[9] 0.5092497 0.4871050 0.5000817 0.5021093 0.5012061 0.4915230 0.4773220 0.4962933
#[17] 0.4980623 0.4938213 0.5037263 0.4914459 0.4912532 0.5059779 0.4954057 0.4905960
#[25] 0.5185054
mean_mccrs_j48 #[1] 0.4976351
lci  #[1] 0.4753729
uci #[1] 0.5198974


#*********************************************************
#
#    	Bagging
#
#*********************************************************
## bagging --smote
bank_bagging<-bank[c(1:17)]
mccrs_bagging<- calc_seed_mcc('bagging',bank_bagging,5,5,"smote")
mean_mccrs_bagging <- mean(mccrs_bagging)   
lci <- mean(mccrs_bagging) - sd(mccrs_bagging) * 1.96
uci <- mean(mccrs_bagging) + sd(mccrs_bagging) * 1.96
mccrs_bagging
# [1] 0.5338562 0.5427192 0.5369815 0.5252097 0.5420349 0.5308378 0.5398521 0.4993542
#[9] 0.5700129 0.5505280 0.5405577 0.5312099 0.5336702 0.5290721 0.5297638 0.5368277
#[17] 0.5246267 0.5360419 0.5365027 0.5325270 0.5375063 0.5611712 0.5383873 0.5159348
#[25] 0.5380628
mean_mccrs_bagging #[1] 0.5357299
lci  #[1] 0.5095531
uci #[1] 0.5619068

#*********************************************************
#
#    	Random Forest 
#
#*********************************************************
#Ramdom Forest -- smote
bank_random_forest<-bank[c(1:17)]
mccrs_random_forest<- calc_seed_mcc('random_forest_no_mtry',bank_random_forest,5,5,"smote")
mean_mccrs_random_forest <- mean(mccrs_random_forest)   
lci <- mean(mccrs_random_forest) - sd(mccrs_random_forest) * 1.96
uci <- mean(mccrs_random_forest) + sd(mccrs_random_forest) * 1.96
mccrs_random_forest
mean_mccrs_random_forest 
lci  
uci 


#Ramdom Forest -- smote and mtry=6

bank_random_forest_6<-bank[c(1:17)]
mccrs_random_forest_6<- calc_seed_mcc('random_forest_mtry',bank_random_forest_6,5,5,"smote")
mean_mccrs_random_forest_6 <- mean(mccrs_random_forest_6)   
lci <- mean(mccrs_random_forest_6) - sd(mccrs_random_forest_6) * 1.96
uci <- mean(mccrs_random_forest_6) + sd(mccrs_random_forest_6) * 1.96
mccrs_random_forest_6
mean_mccrs_random_forest_6 
lci  
uci
#1] 0.5615410 0.5676606 0.5554811 0.5628095
#[5] 0.5710931 0.5564018 0.5476907 0.5257697
#[9] 0.5739036 0.5745236 0.5615351 0.5618830
#[13] 0.5537137 0.5615786 0.5667066 0.5566924
#[17] 0.5555912 0.5678207 0.5636385 0.5636760
#[21] 0.5577405 0.5619803 0.5526831 0.5394576
#[25] 0.5818014



#*********************************************************
#
#    	Deep Learning with Random Forest 
#
#*********************************************************

localH2O = h2o.init()
# Make all original data as factor or numeric 
bank2$y <- as.factor(bank2$y)
bank2$age <- as.numeric(bank2$age)
bank2$balance <- as.numeric(bank2$balance)
bank2$day <- as.numeric(bank2$day)
bank2$duration <- as.numeric(bank2$duration)
bank2$campaign <- as.numeric(bank2$campaign)
bank2$pdays <- as.numeric(bank2$pdays)
bank2$previous <- as.numeric(bank2$previous)
bank2$y<- factor(bank2$y, ordered = FALSE )
bank2$month<- factor(bank2$month, ordered = FALSE )

#Differentiate the y and other variables
feature_names <- setdiff(names(bank2), "y")


#Try one sample and connect to h2o server
folds <- createFolds(bank2$y, k = 5 , list = TRUE, returnTrain = TRUE)
prostate.hex<-as.h2o(bank2[folds[[1]],], destination_frame="train.hex")
prostate.dl = h2o.deeplearning(x = feature_names, training_frame = prostate.hex,
                               autoencoder = TRUE,
                               reproducible = T,
                               seed = 1234,
                               hidden = c(6,5,6), epochs = 50,
                               max_w2 = 10,
                               l1=1e-5)
#Get the anomaly list
anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=FALSE)
head(anon)
err <- as.data.frame(anon)
#Plot the MSE error VS index
plot(sort(err$Reconstruction.MSE), main='Reconstruction Error')
#From the plot, we make 0.15 as a cut point for clean and unclean data. 
#Train the clean model
model1 <- randomForest(y~., data = na.exclude(SMOTE(y~., bank2[folds[[1]],][err$Reconstruction.MSE < 0.15,])),mtry=6)
#Train the unclean model
model2 <- randomForest(y~., data = na.exclude(SMOTE(y~., bank2[folds[[1]],][err$Reconstruction.MSE >= 0.15,])),mtry=6)
#Test the weightage of each variable for clean model 
importance    <- importance(model1)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour= 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
#Test the weightage of each variable for unclean model 
importance    <- importance(model1)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour= 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
#---------------------------------------------------------------------------------
#Using the MSE<=0.15 training data to test all data
k.rfdp1.folds <- function(k) {
  folds <- createFolds(bank2$y, k = 5 , list = TRUE, returnTrain = TRUE)
  for (i in 1:k) {
    prostate.hex<-as.h2o(bank2[folds[[i]],], destination_frame="train.hex")
    prostate.dl = h2o.deeplearning(x = feature_names, training_frame = prostate.hex,
                                   autoencoder = TRUE,
                                   reproducible = T,
                                   seed = 1234,
                                   hidden = c(6,5,6), epochs = 50,
                                   max_w2 = 10,
                                   l1=1e-5)
    anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=FALSE)
    head(anon)
    err <- as.data.frame(anon)
    model1 <- randomForest(y~., data = na.exclude(SMOTE(y~., bank2[folds[[i]],][err$Reconstruction.MSE < 0.15,])),mtry=6)
    predictions1 <- predict(model1,newdata = bank2[-folds[[i]],],type="prob")
    predictions1 <- as.data.frame(predictions1)
    mccrs = c(mccrs, mccr(as.integer(as.logical(bank2[-folds[[i]], ]$y=='yes')),
                          as.integer(as.logical(predictions1[,1]>0.5))))[]
  }
  mccrs
}

mccrs = c()
mccrsRFDP1 = c()
for (i in 1:5){
  set.seed(i)
  mccrsRFDP1 = c(mccrsRFDP1, k.rfdp1.folds(5))
}
mccrsRFDP1
#[1] 0.5479941 0.5696207 0.5524086 0.5555989 0.5644433 0.5647296 0.5509343 0.5364237 0.5697691
#[10] 0.5756156 0.5606194 0.5668157 0.5593198 0.5610943 0.5652848 0.5519216 0.5614933 0.5683468
#[19] 0.5594077 0.5601601 0.5725006 0.5648585 0.5594456 0.5360799 0.5768963
#mean is 0.5604713

#Using both the MSE>0.15 and the MSE <=0.15 training data to predict the model 
k.rfdp2.folds <- function(k) {
  folds <- createFolds(bank2$y, k = 5 , list = TRUE, returnTrain = TRUE)
  for (i in 1:k) {
    prostate.hex<-as.h2o(bank2[folds[[i]],], destination_frame="train.hex")
    prostate.dl = h2o.deeplearning(x = feature_names, training_frame = prostate.hex,
                                   autoencoder = TRUE,
                                   reproducible = T,
                                   seed = 1234,
                                   hidden = c(6,5,6), epochs = 50,
                                   max_w2 = 10,
                                   l1=1e-5)
    anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=FALSE)
    head(anon)
    err <- as.data.frame(anon)
    model1 <- randomForest(y~., data = na.exclude(SMOTE(y~., bank2[folds[[i]],][err$Reconstruction.MSE < 0.15,])),mtry=6)
    model2 <- randomForest(y~., data = na.exclude(SMOTE(y~., bank2[folds[[i]],][err$Reconstruction.MSE >= 0.15,])),mtry=6)
    predictions1 <- predict(model1,newdata = bank2[err$Reconstruction.MSE < 0.15,][-folds[[i]],],type="prob")
    predictions2 <- predict(model2,newdata = bank2[err$Reconstruction.MSE >= 0.15,][-folds[[i]],],type="prob")
    predictions1 <- as.data.frame(predictions1)
    predictions2 <- as.data.frame(predictions2)
    mccrs = c(mccrs, mccr(as.integer(as.logical(c(bank2[err$Reconstruction.MSE >= 0.15,][-folds[[i]],]$y=='yes',bank2[err$Reconstruction.MSE < 0.15,][-folds[[i]],]$y=='yes'))),
                          as.integer(as.logical(c(predictions2[,1]>0.5,predictions1[,1]>0.5)))))
    
  }
  mccrs
}

mccrs = c()
mccrsRFDP2 = c()
for (i in 1:5){
  set.seed(i)
  mccrsRFDP2 = c(mccrsRFDP2, k.rfdp2.folds(5))
}
mccrsRFDP2
# Sometimes, the returned value is nagetive. This is due to the set up difference among different computers.
#[1] 0.7364632 0.7256823 0.7432317 0.7218834 0.7273801 0.7389395 0.7360560 0.7110721 0.7339643
#[10] 0.7376203 0.6872357 0.7156129 0.7363467 0.7487934 0.7656341 0.7764117 0.7192398 0.7401311
#[19] 0.7463806 0.7671850 0.7024806 0.7562011 0.7389934 0.7135333 0.7466120
#------------------------------------------------------------------------------------

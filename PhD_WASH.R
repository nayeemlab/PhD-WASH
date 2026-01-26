
###Diarrhea Vs Years######

setwd('F:\\PhD Thesis\\WASH\\Dataset_raw\\')
#WASH_data <- read.csv("Translated_Data_All_Append_noclean.csv")
WASH_data <- read.csv("Translated_Data_All_Append_noclean.csv", na.strings = c("", "NA"))

WASH_data$Date_of_intervention   <- factor(WASH_data$Date_of_intervention)                                 
NROW(WASH_data$Date_of_intervention)
#View(WASH_data)

WASH_data_nomiss <- subset(WASH_data, !is.na(Date_of_intervention))
NROW(WASH_data_nomiss)

WASH_data_nomiss$SL_Com <- factor(WASH_data_nomiss$SL_Com)                                     
WASH_data_nomiss$SL_Com

WASH_data_nomiss$SL_Visit  <- factor( WASH_data_nomiss$SL_Visit)                                            
WASH_data_nomiss$SL_Visit

WASH_data_nomiss$SL_Name  <- factor(WASH_data_nomiss$SL_Name )                                         
WASH_data_nomiss$SL_Name

WASH_data_nomiss$Community_name  <- factor(WASH_data_nomiss$Community_name)                                         
WASH_data_nomiss$Community_name

x <- table(WASH_data_nomiss$Community_name)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Community_name)

WASH_data_nomiss$Family_name  <- factor(WASH_data_nomiss$Family_name)                                        
WASH_data_nomiss$Family_name

WASH_data_nomiss$Participant_code  <- factor(WASH_data_nomiss$Participant_code)                                      
WASH_data_nomiss$Participant_code

WASH_data_nomiss$Full_name  <- factor(WASH_data_nomiss$Full_name)                                           
WASH_data_nomiss$Full_name

WASH_data_nomiss$Date_of_birth  <- factor(WASH_data_nomiss$Date_of_birth)                                         
WASH_data_nomiss$Date_of_birth

WASH_data_nomiss$Sex  <- factor(WASH_data_nomiss$Sex)                                
WASH_data_nomiss$Sex

WASH_data_nomiss$Sex_cat  <- factor(WASH_data_nomiss$Sex,levels=c(0,1),labels = c('Female','Male'))                                
WASH_data_nomiss$Sex_cat 

x <- table(WASH_data_nomiss$Sex_cat)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Sex_cat)


WASH_data_nomiss$educational_level_years <- factor(WASH_data_nomiss$educational_level_years)                              
WASH_data_nomiss$educational_level_years

WASH_data_nomiss$educational_level_years[WASH_data_nomiss$educational_level_years == -1] <- NA                              
WASH_data_nomiss$educational_level_years

x <- table(WASH_data_nomiss$educational_level_years)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$educational_level_years)


WASH_data_nomiss$Visit  <- factor(WASH_data_nomiss$Visit)                                   
WASH_data_nomiss$Visit

WASH_data_nomiss$Date_of_intervention   <- factor(WASH_data_nomiss$Date_of_intervention)                                 
WASH_data_nomiss$Date_of_intervention

WASH_data_nomiss$Year <- factor(WASH_data_nomiss$Year)                                  
WASH_data_nomiss$Year

summary(WASH_data_nomiss$Year)

x <- table(WASH_data_nomiss$Year)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Year)

WASH_data_nomiss$Age_years_or_months <- as.numeric(WASH_data_nomiss$Age_years_or_months)                  
WASH_data_nomiss$Age_years_or_months

library(DescTools)
summary(WASH_data_nomiss$Age_years_or_months)
Desc(WASH_data_nomiss$Age_years_or_months, na.rm = T)

WASH_data_nomiss$Age_years_or_months_cat[WASH_data_nomiss$Age_years_or_months < 5]  = 0
WASH_data_nomiss$Age_years_or_months_cat[WASH_data_nomiss$Age_years_or_months >= 5] = 1

WASH_data_nomiss$Age_years_or_months_cat <- factor(WASH_data_nomiss$Age_years_or_months_cat,levels=c(0,1),labels = c('<5','>=5'))
WASH_data_nomiss$Age_years_or_months_cat

x <- table(WASH_data_nomiss$Age_years_or_months_cat)
x
round(prop.table(x),4)*100

WASH_data_nomiss$Positive_for_ascaris_feces_sample  <- factor(WASH_data_nomiss$Positive_for_ascaris_feces_sample)                 
WASH_data_nomiss$Positive_for_ascaris_feces_sample

WASH_data_nomiss$Positive_for_ascaris_feces_sample  <- factor(WASH_data_nomiss$Positive_for_ascaris_feces_sample,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Positive_for_ascaris_feces_sample

x <- table(WASH_data_nomiss$Positive_for_ascaris_feces_sample)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Positive_for_ascaris_feces_sample)


WASH_data_nomiss$Positive_for_blastocystis_feces_sample   <- factor(WASH_data_nomiss$Positive_for_blastocystis_feces_sample)               
WASH_data_nomiss$Positive_for_blastocystis_feces_sample

WASH_data_nomiss$Positive_for_blastocystis_feces_sample  <- factor(WASH_data_nomiss$Positive_for_blastocystis_feces_sample,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Positive_for_blastocystis_feces_sample

x <- table(WASH_data_nomiss$Positive_for_blastocystis_feces_sample)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Positive_for_blastocystis_feces_sample)

WASH_data_nomiss$Positive_for_giardia_feces_sample   <- factor(WASH_data_nomiss$Positive_for_giardia_feces_sample)                   
WASH_data_nomiss$Positive_for_giardia_feces_sample

WASH_data_nomiss$Positive_for_giardia_feces_sample  <- factor(WASH_data_nomiss$Positive_for_giardia_feces_sample,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Positive_for_giardia_feces_sample

x <- table(WASH_data_nomiss$Positive_for_giardia_feces_sample)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Positive_for_giardia_feces_sample)

WASH_data_nomiss$Positive_for_trichuris_feces_sample   <- factor(WASH_data_nomiss$Positive_for_trichuris_feces_sample)                  
WASH_data_nomiss$Positive_for_trichuris_feces_sample

WASH_data_nomiss$Positive_for_trichuris_feces_sample  <- factor(WASH_data_nomiss$Positive_for_trichuris_feces_sample,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Positive_for_trichuris_feces_sample

x <- table(WASH_data_nomiss$Positive_for_trichuris_feces_sample)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Positive_for_trichuris_feces_sample)

WASH_data_nomiss$Positive_for_uncinaria_feces_sample   <- factor(WASH_data_nomiss$Positive_for_uncinaria_feces_sample)               
WASH_data_nomiss$Positive_for_uncinaria_feces_sample

WASH_data_nomiss$Positive_for_uncinaria_feces_sample  <- factor(WASH_data_nomiss$Positive_for_uncinaria_feces_sample,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Positive_for_uncinaria_feces_sample

x <- table(WASH_data_nomiss$Positive_for_uncinaria_feces_sample)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Positive_for_uncinaria_feces_sample)

WASH_data_nomiss$Positive_for_strongyloides_feces_sample     <- factor(WASH_data_nomiss$Positive_for_strongyloides_feces_sample)        
WASH_data_nomiss$Positive_for_strongyloides_feces_sample

WASH_data_nomiss$Positive_for_strongyloides_feces_sample  <- factor(WASH_data_nomiss$Positive_for_strongyloides_feces_sample,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Positive_for_strongyloides_feces_sample

x <- table(WASH_data_nomiss$Positive_for_strongyloides_feces_sample)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Positive_for_strongyloides_feces_sample)



WASH_data_nomiss$Positive_for_enterobius_present_feces_sample   <- factor(WASH_data_nomiss$Positive_for_enterobius_present_feces_sample)         
WASH_data_nomiss$Positive_for_enterobius_present_feces_sample

WASH_data_nomiss$Positive_for_enterobius_present_feces_sample  <- factor(WASH_data_nomiss$Positive_for_enterobius_present_feces_sample,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Positive_for_enterobius_present_feces_sample

x <- table(WASH_data_nomiss$Positive_for_enterobius_present_feces_sample)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Positive_for_enterobius_present_feces_sample)

WASH_data_nomiss$Positive_for_hymenolepis_nana_feces_sample   <- factor(WASH_data_nomiss$Positive_for_hymenolepis_nana_feces_sample)         
WASH_data_nomiss$Positive_for_hymenolepis_nana_feces_sample

WASH_data_nomiss$Positive_for_hymenolepis_nana_feces_sample  <- factor(WASH_data_nomiss$Positive_for_hymenolepis_nana_feces_sample,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Positive_for_hymenolepis_nana_feces_sample

x <- table(WASH_data_nomiss$Positive_for_hymenolepis_nana_feces_sample)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Positive_for_hymenolepis_nana_feces_sample)

WASH_data_nomiss$Trematodo <- factor(WASH_data_nomiss$Trematodo)                                        
WASH_data_nomiss$Trematodo 

WASH_data_nomiss$Trematodo  <- factor(WASH_data_nomiss$Trematodo,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Trematodo

x <- table(WASH_data_nomiss$Trematodo)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Trematodo)

WASH_data_nomiss$Capillaria <- factor(WASH_data_nomiss$Capillaria)                                           
WASH_data_nomiss$Capillaria

WASH_data_nomiss$Capillaria  <- factor(WASH_data_nomiss$Capillaria,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Capillaria

x <- table(WASH_data_nomiss$Capillaria)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Capillaria)

WASH_data_nomiss$Trichomonas <- factor(WASH_data_nomiss$Trichomonas)                                     
WASH_data_nomiss$Trichomonas

WASH_data_nomiss$Trichomonas  <- factor(WASH_data_nomiss$Trichomonas,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Trichomonas

x <- table(WASH_data_nomiss$Trichomonas)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Trichomonas)

WASH_data_nomiss$Hymenolepis.nana <- factor(WASH_data_nomiss$Hymenolepis.nana )                               
WASH_data_nomiss$Hymenolepis.nana

WASH_data_nomiss$Hymenolepis.nana  <- factor(WASH_data_nomiss$Hymenolepis.nana,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Hymenolepis.nana

x <- table(WASH_data_nomiss$Hymenolepis.nana)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Hymenolepis.nana)


WASH_data_nomiss$Hymenolepis.diminuta   <- factor(WASH_data_nomiss$Hymenolepis.diminuta)                                 
WASH_data_nomiss$Hymenolepis.diminuta

WASH_data_nomiss$Hymenolepis.diminuta  <- factor(WASH_data_nomiss$Hymenolepis.diminuta,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Hymenolepis.diminuta

x <- table(WASH_data_nomiss$Hymenolepis.diminuta)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Hymenolepis.diminuta)


WASH_data_nomiss$Taenia <- factor(WASH_data_nomiss$Taenia)                                       
WASH_data_nomiss$Taenia

WASH_data_nomiss$Taenia  <- factor(WASH_data_nomiss$Taenia,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Taenia

x <- table(WASH_data_nomiss$Taenia)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Taenia)


WASH_data_nomiss$other_parasite <- factor(WASH_data_nomiss$other_parasite)                                       
WASH_data_nomiss$other_parasite

WASH_data_nomiss$other_parasite  <- factor(WASH_data_nomiss$other_parasite,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$other_parasite

x <- table(WASH_data_nomiss$other_parasite)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$other_parasite)



WASH_data_nomiss$Weight_kg  <- as.numeric(WASH_data_nomiss$Weight_kg)                                             
WASH_data_nomiss$Weight_kg

WASH_data_nomiss$Weight_kg[WASH_data_nomiss$Weight_kg == -1] <- NA                              
WASH_data_nomiss$Weight_kg

summary(WASH_data_nomiss$Weight_kg)
Desc(WASH_data_nomiss$Weight_kg, na.rm = T)

WASH_data_nomiss$Height_cm  <- as.numeric(WASH_data_nomiss$Height_cm )                                
WASH_data_nomiss$Height_cm

WASH_data_nomiss$Height_cm[WASH_data_nomiss$Height_cm == -1] <- NA                              
WASH_data_nomiss$Weight_kg

summary(WASH_data_nomiss$Height_cm)
Desc(WASH_data_nomiss$Height_cm, na.rm = T)

WASH_data_nomiss$Weight_for_age_z_score <- as.numeric(WASH_data_nomiss$Weight_for_age_z_score)                   
WASH_data_nomiss$Weight_for_age_z_score

summary(WASH_data_nomiss$Weight_for_age_z_score)
Desc(WASH_data_nomiss$Weight_for_age_z_score, na.rm = T)

WASH_data_nomiss$Weight_for_age_z_score_description <- factor(WASH_data_nomiss$Weight_for_age_z_score_description)                 
WASH_data_nomiss$Weight_for_age_z_score_description

x <- table(WASH_data_nomiss$Weight_for_age_z_score_description)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Weight_for_age_z_score_description)

WASH_data_nomiss$Height_for_age_z_score <- as.numeric(WASH_data_nomiss$Height_for_age_z_score)                     
WASH_data_nomiss$Height_for_age_z_score

summary(WASH_data_nomiss$Height_for_age_z_score)
Desc(WASH_data_nomiss$Height_for_age_z_score, na.rm = T)

WASH_data_nomiss$Height_for_age_z_score_description <- factor(WASH_data_nomiss$Height_for_age_z_score_description)                   
WASH_data_nomiss$Height_for_age_z_score_description

x <- table(WASH_data_nomiss$Height_for_age_z_score_description)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Height_for_age_z_score_description)

WASH_data_nomiss$BMI_for_age_z_score <- as.numeric(WASH_data_nomiss$BMI_for_age_z_score)                     
WASH_data_nomiss$BMI_for_age_z_score

summary(WASH_data_nomiss$BMI_for_age_z_score)
Desc(WASH_data_nomiss$BMI_for_age_z_score, na.rm = T)

WASH_data_nomiss$BMI_for_age_z_score_description <- factor(WASH_data_nomiss$BMI_for_age_z_score_description)                     
WASH_data_nomiss$BMI_for_age_z_score_description

x <- table(WASH_data_nomiss$BMI_for_age_z_score_description)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$BMI_for_age_z_score_description)

WASH_data_nomiss$IMC_form <- as.numeric(WASH_data_nomiss$IMC_form)                                           
WASH_data_nomiss$IMC_form

summary(WASH_data_nomiss$IMC_form)
Desc(WASH_data_nomiss$IMC_form, na.rm = T)

WASH_data_nomiss$Hemoglobin <- as.numeric(WASH_data_nomiss$Hemoglobin)                           
WASH_data_nomiss$Hemoglobin

WASH_data_nomiss$Hemoglobin[WASH_data_nomiss$Hemoglobin == -1] <- NA                              
WASH_data_nomiss$Hemoglobin

summary(WASH_data_nomiss$Hemoglobin)
Desc(WASH_data_nomiss$Hemoglobin, na.rm = T)

WASH_data_nomiss$Anemia_classified_age_sex <- factor(WASH_data_nomiss$Anemia_classified_age_sex)                           
WASH_data_nomiss$Anemia_classified_age_sex

WASH_data_nomiss$Anemia_classified_age_sex[WASH_data_nomiss$Anemia_classified_age_sex == -1] <- NA                              
WASH_data_nomiss$Anemia_classified_age_sex

WASH_data_nomiss$Anemia_classified_age_sex  <- factor(WASH_data_nomiss$Anemia_classified_age_sex,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Anemia_classified_age_sex

x <- table(WASH_data_nomiss$Anemia_classified_age_sex)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Anemia_classified_age_sex)

WASH_data_nomiss$Hto_EQ.hemocue.. <- as.numeric(WASH_data_nomiss$Hto_EQ.hemocue..)                                      
WASH_data_nomiss$Hto_EQ.hemocue..

summary(WASH_data_nomiss$Hto_EQ.hemocue..)
Desc(WASH_data_nomiss$Hto_EQ.hemocue.., na.rm = T)

WASH_data_nomiss$Hto.capilar..  <- as.numeric(WASH_data_nomiss$Hto.capilar..)                
WASH_data_nomiss$Hto.capilar..

WASH_data_nomiss$Hto.capilar..[WASH_data_nomiss$Hto.capilar.. == -1] <- NA                              
WASH_data_nomiss$Hto.capilar..

summary(WASH_data_nomiss$Hto.capilar..)
Desc(WASH_data_nomiss$Hto.capilar.., na.rm = T)

WASH_data_nomiss$Received_anti_parasite_treatment_today  <- factor(WASH_data_nomiss$Received_anti_parasite_treatment_today)
WASH_data_nomiss$Received_anti_parasite_treatment_today

WASH_data_nomiss$Received_anti_parasite_treatment_today[WASH_data_nomiss$Received_anti_parasite_treatment_today == -1] <- NA                              
WASH_data_nomiss$Received_anti_parasite_treatment_today

WASH_data_nomiss$Received_anti_parasite_treatment_today  <- factor(WASH_data_nomiss$Received_anti_parasite_treatment_today,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Received_anti_parasite_treatment_today

x <- table(WASH_data_nomiss$Received_anti_parasite_treatment_today)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Received_anti_parasite_treatment_today)


WASH_data_nomiss$last_antiparasitic_treatment_received_today_health_post <- factor(WASH_data_nomiss$last_antiparasitic_treatment_received_today_health_post)
WASH_data_nomiss$last_antiparasitic_treatment_received_today_health_post


WASH_data_nomiss$Received_anti_parasite_treatment_ever <- factor(WASH_data_nomiss$Received_anti_parasite_treatment_ever)
WASH_data_nomiss$Received_anti_parasite_treatment_ever

WASH_data_nomiss$Received_anti_parasite_treatment_ever[WASH_data_nomiss$Received_anti_parasite_treatment_ever == -1] <- NA                              
WASH_data_nomiss$Received_anti_parasite_treatment_ever

WASH_data_nomiss$Received_anti_parasite_treatment_ever  <- factor(WASH_data_nomiss$Received_anti_parasite_treatment_ever,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Received_anti_parasite_treatment_ever

x <- table(WASH_data_nomiss$Received_anti_parasite_treatment_ever)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Received_anti_parasite_treatment_ever)

WASH_data_nomiss$Date_anti_parasite_treatment_today <- factor(WASH_data_nomiss$Date_anti_parasite_treatment_today)   
WASH_data_nomiss$Date_anti_parasite_treatment_today 

WASH_data_nomiss$Received_anti_parasite_treatment_this_year <- factor(WASH_data_nomiss$Received_anti_parasite_treatment_this_year)            
WASH_data_nomiss$Received_anti_parasite_treatment_this_year

WASH_data_nomiss$last_antiparasitic_treatment_how_long_ago <- as.numeric(WASH_data_nomiss$last_antiparasitic_treatment_how_long_ago)             
WASH_data_nomiss$last_antiparasitic_treatment_how_long_ago


summary(WASH_data_nomiss$last_antiparasitic_treatment_how_long_ago)
Desc(WASH_data_nomiss$last_antiparasitic_treatment_how_long_ago, na.rm = T)

WASH_data_nomiss$kinship <- factor(WASH_data_nomiss$kinship )                                        
WASH_data_nomiss$kinship

WASH_data_nomiss$Has_sink <- factor(WASH_data_nomiss$Has_sink)                                         
WASH_data_nomiss$Has_sink

WASH_data_nomiss$Has_sink  <- factor(WASH_data_nomiss$Has_sink,levels=c(0, 1, 2),labels = c('No','Yes', 'Shared'))                                
WASH_data_nomiss$Has_sink

x <- table(WASH_data_nomiss$Has_sink)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Has_sink)


WASH_data_nomiss$Sink_works <- factor(WASH_data_nomiss$Sink_works)                                     
WASH_data_nomiss$Sink_works

WASH_data_nomiss$Sink_works  <- factor(WASH_data_nomiss$Sink_works,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Sink_works

x <- table(WASH_data_nomiss$Sink_works)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Sink_works)

WASH_data_nomiss$Safe_sink_water <- factor(WASH_data_nomiss$Safe_sink_water)
WASH_data_nomiss$Safe_sink_water

WASH_data_nomiss$Safe_sink_water[WASH_data_nomiss$Safe_sink_water == -1 | WASH_data_nomiss$Safe_sink_water == 2 | WASH_data_nomiss$Safe_sink_water == 99] <- NA                              
WASH_data_nomiss$Safe_sink_water

WASH_data_nomiss$Safe_sink_water  <- factor(WASH_data_nomiss$Safe_sink_water,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Safe_sink_water

x <- table(WASH_data_nomiss$Safe_sink_water)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Safe_sink_water)

WASH_data_nomiss$Sink_without_water_recently <- factor(WASH_data_nomiss$Sink_without_water_recently)        
WASH_data_nomiss$Sink_without_water_recently

WASH_data_nomiss$Sink_without_water_recently[WASH_data_nomiss$Sink_without_water_recently == -1 | WASH_data_nomiss$Sink_without_water_recently == 2 | WASH_data_nomiss$Sink_without_water_recently == 99] <- NA                              
WASH_data_nomiss$Sink_without_water_recently

WASH_data_nomiss$Sink_without_water_recently  <- factor(WASH_data_nomiss$Sink_without_water_recently,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Sink_without_water_recently

x <- table(WASH_data_nomiss$Sink_without_water_recently)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Sink_without_water_recently)

WASH_data_nomiss$If.yes_for_how_long_days_Sink_without_water <- as.numeric(WASH_data_nomiss$If.yes_for_how_long_days_Sink_without_water)          
WASH_data_nomiss$If.yes_for_how_long_days_Sink_without_water

WASH_data_nomiss$For_how_long_Sink_without_water <- factor(WASH_data_nomiss$For_how_long_Sink_without_water)                       
WASH_data_nomiss$For_how_long_Sink_without_water

WASH_data_nomiss$No_sink_spring <- factor(WASH_data_nomiss$No_sink_spring)                                     
WASH_data_nomiss$No_sink_spring

WASH_data_nomiss$No_sink_spring[WASH_data_nomiss$No_sink_spring == -1 | WASH_data_nomiss$No_sink_spring == 2 | WASH_data_nomiss$No_sink_spring == 99] <- NA                              
WASH_data_nomiss$No_sink_spring

WASH_data_nomiss$No_sink_spring  <- factor(WASH_data_nomiss$No_sink_spring,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$No_sink_spring

x <- table(WASH_data_nomiss$No_sink_spring)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$No_sink_spring)

WASH_data_nomiss$No_sink_brook <- factor(WASH_data_nomiss$No_sink_brook)                                        
WASH_data_nomiss$No_sink_brook

WASH_data_nomiss$No_sink_brook[WASH_data_nomiss$No_sink_brook == -1 | WASH_data_nomiss$No_sink_brook == 2 | WASH_data_nomiss$No_sink_brook == 99] <- NA                              
WASH_data_nomiss$No_sink_brook

WASH_data_nomiss$No_sink_brook  <- factor(WASH_data_nomiss$No_sink_brook,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$No_sink_brook

x <- table(WASH_data_nomiss$No_sink_brook)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$No_sink_brook)

WASH_data_nomiss$No_sink_river <- factor(WASH_data_nomiss$No_sink_river)                                        
WASH_data_nomiss$No_sink_river

WASH_data_nomiss$No_sink_river[WASH_data_nomiss$No_sink_river == -1 | WASH_data_nomiss$No_sink_river == 2 | WASH_data_nomiss$No_sink_river == 99] <- NA                              
WASH_data_nomiss$No_sink_river

WASH_data_nomiss$No_sink_river  <- factor(WASH_data_nomiss$No_sink_river,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$No_sink_river

x <- table(WASH_data_nomiss$No_sink_river)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$No_sink_river)

WASH_data_nomiss$No_sink_tank <- factor(WASH_data_nomiss$No_sink_tank)                                         
WASH_data_nomiss$No_sink_tank

WASH_data_nomiss$No_sink_tank[WASH_data_nomiss$No_sink_tank == -1 | WASH_data_nomiss$No_sink_tank == 2 | WASH_data_nomiss$No_sink_tank == 99] <- NA                              
WASH_data_nomiss$No_sink_tank

WASH_data_nomiss$No_sink_tank  <- factor(WASH_data_nomiss$No_sink_tank,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$No_sink_tank

x <- table(WASH_data_nomiss$No_sink_tank)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$No_sink_tank)

WASH_data_nomiss$No_sink_other <- factor(WASH_data_nomiss$No_sink_other)                           
WASH_data_nomiss$No_sink_other

WASH_data_nomiss$No_sink_other[WASH_data_nomiss$No_sink_other == -1 | WASH_data_nomiss$No_sink_other == 2 | WASH_data_nomiss$No_sink_other == 99] <- NA                              
WASH_data_nomiss$No_sink_other

WASH_data_nomiss$No_sink_other  <- factor(WASH_data_nomiss$No_sink_other,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$No_sink_other

x <- table(WASH_data_nomiss$No_sink_other)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$No_sink_other)

WASH_data_nomiss$last_clean_water_tank_days <- as.numeric(WASH_data_nomiss$last_clean_water_tank_days)             
WASH_data_nomiss$last_clean_water_tank_days

WASH_data_nomiss$last_clean_water_tank_days[WASH_data_nomiss$last_clean_water_tank_days == -1] <- NA                              
WASH_data_nomiss$last_clean_water_tank_days

summary(WASH_data_nomiss$last_clean_water_tank_days)
Desc(WASH_data_nomiss$last_clean_water_tank_days, na.rm = T)


WASH_data_nomiss$Drinks_water_outside_home_without_boiling <- factor(WASH_data_nomiss$Drinks_water_outside_home_without_boiling)             
WASH_data_nomiss$Drinks_water_outside_home_without_boiling

WASH_data_nomiss$Drinks_water_outside_home_without_boiling  <- factor(WASH_data_nomiss$Drinks_water_outside_home_without_boiling,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Drinks_water_outside_home_without_boiling

x <- table(WASH_data_nomiss$Drinks_water_outside_home_without_boiling)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Drinks_water_outside_home_without_boiling)

WASH_data_nomiss$Drinks_water_outside_home_with_boils <- factor(WASH_data_nomiss$Drinks_water_outside_home_with_boils)               
WASH_data_nomiss$Drinks_water_outside_home_with_boils

WASH_data_nomiss$Drinks_water_outside_home_with_boils  <- factor(WASH_data_nomiss$Drinks_water_outside_home_with_boils,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$Drinks_water_outside_home_with_boils

x <- table(WASH_data_nomiss$Drinks_water_outside_home_with_boils)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$Drinks_water_outside_home_with_boils)

WASH_data_nomiss$Drinks_water_outside_home_purchased <- factor(WASH_data_nomiss$Drinks_water_outside_home_purchased)                 
WASH_data_nomiss$Drinks_water_outside_home_purchased


WASH_data_nomiss$Drinks_water_outside_home_others <- factor(WASH_data_nomiss$Drinks_water_outside_home_others)     
WASH_data_nomiss$Drinks_water_outside_home_others

WASH_data_nomiss$Drinks_water_outside_home_boiled_without_boiled <- factor(WASH_data_nomiss$Drinks_water_outside_home_boiled_without_boiled)      
WASH_data_nomiss$Drinks_water_outside_home_boiled_without_boiled 

WASH_data_nomiss$Where_you_drink_water_when_thirsty <- factor(WASH_data_nomiss$Where_you_drink_water_when_thirsty)                   
WASH_data_nomiss$Where_you_drink_water_when_thirsty

WASH_data_nomiss$Where_else_water_used <- factor(WASH_data_nomiss$Where_else_water_used)                        
WASH_data_nomiss$Where_else_water_used

WASH_data_nomiss$When_you_last_drink_spring <- factor(WASH_data_nomiss$When_you_last_drink_spring)                          
WASH_data_nomiss$When_you_last_drink_spring

WASH_data_nomiss$When_you_last_drink_spring  <- factor(WASH_data_nomiss$When_you_last_drink_spring,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$When_you_last_drink_spring

x <- table(WASH_data_nomiss$When_you_last_drink_spring)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$When_you_last_drink_spring)

WASH_data_nomiss$When_you_last_drink_stream <- factor(WASH_data_nomiss$When_you_last_drink_stream)              
WASH_data_nomiss$When_you_last_drink_stream

WASH_data_nomiss$When_you_last_drink_stream[WASH_data_nomiss$When_you_last_drink_stream == -1] <- NA                              
WASH_data_nomiss$When_you_last_drink_stream

WASH_data_nomiss$When_you_last_drink_stream  <- factor(WASH_data_nomiss$When_you_last_drink_stream,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$When_you_last_drink_stream

x <- table(WASH_data_nomiss$When_you_last_drink_stream)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$When_you_last_drink_stream)

WASH_data_nomiss$When_you_last_drink_river <- factor(WASH_data_nomiss$When_you_last_drink_river)    
WASH_data_nomiss$When_you_last_drink_river

WASH_data_nomiss$When_you_last_drink_river[WASH_data_nomiss$When_you_last_drink_river == -1] <- NA                              
WASH_data_nomiss$When_you_last_drink_river

WASH_data_nomiss$When_you_last_drink_river  <- factor(WASH_data_nomiss$When_you_last_drink_river,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$When_you_last_drink_river

x <- table(WASH_data_nomiss$When_you_last_drink_river)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$When_you_last_drink_river)

WASH_data_nomiss$Days_since_you_drink_water <- as.numeric(WASH_data_nomiss$Days_since_you_drink_water)                           
WASH_data_nomiss$Days_since_you_drink_water

WASH_data_nomiss$Days_since_you_drink_water[WASH_data_nomiss$Days_since_you_drink_water == -1] <- NA                              
WASH_data_nomiss$Days_since_you_drink_water

summary(WASH_data_nomiss$Days_since_you_drink_water)
Desc(WASH_data_nomiss$Days_since_you_drink_water, na.rm = T)

WASH_data_nomiss$Do_you_drink_chapo <- factor(WASH_data_nomiss$Do_you_drink_chapo)                                 
WASH_data_nomiss$Do_you_drink_chapo

WASH_data_nomiss$Do_you_drink_lemonade <- factor(WASH_data_nomiss$Do_you_drink_lemonade)                                 
WASH_data_nomiss$Do_you_drink_lemonade

WASH_data_nomiss$Do_you_drink_soda <- factor(WASH_data_nomiss$Do_you_drink_soda)                                    
WASH_data_nomiss$Do_you_drink_soda

WASH_data_nomiss$Do_you_drink_masato <- factor(WASH_data_nomiss$Do_you_drink_masato)                                   
WASH_data_nomiss$Do_you_drink_masato

WASH_data_nomiss$Do_you_drink_others <- factor(WASH_data_nomiss$Do_you_drink_others)                                   
WASH_data_nomiss$Do_you_drink_others

WASH_data_nomiss$consumes_tap_water <- factor(WASH_data_nomiss$consumes_tap_water)                                 
WASH_data_nomiss$consumes_tap_water

WASH_data_nomiss$consumes_boiled_water <- factor(WASH_data_nomiss$consumes_boiled_water)                                 
WASH_data_nomiss$consumes_boiled_water

WASH_data_nomiss$consumes_river_water <- factor(WASH_data_nomiss$consumes_river_water)                                   
WASH_data_nomiss$consumes_river_water

WASH_data_nomiss$water_source_tank <- factor(WASH_data_nomiss$water_source_tank)                                      
WASH_data_nomiss$water_source_tank

WASH_data_nomiss$water_source_river <- factor(WASH_data_nomiss$water_source_river )                                    
WASH_data_nomiss$water_source_river

WASH_data_nomiss$water_source_stream <- factor(WASH_data_nomiss$water_source_stream)                                    
WASH_data_nomiss$water_source_stream

WASH_data_nomiss$water_source_spring <- factor(WASH_data_nomiss$water_source_spring)                                    
WASH_data_nomiss$water_source_spring

WASH_data_nomiss$water_source_other <- factor(WASH_data_nomiss$water_source_other)                  
WASH_data_nomiss$water_source_other

WASH_data_nomiss$Source_of_water_used_to_prepare_masato <- factor(WASH_data_nomiss$Source_of_water_used_to_prepare_masato)                 
WASH_data_nomiss$Source_of_water_used_to_prepare_masato

WASH_data_nomiss$Last_time_drank_masato_days <- factor(WASH_data_nomiss$Last_time_drank_masato_days)                            
WASH_data_nomiss$Last_time_drank_masato_days

WASH_data_nomiss$How_long_drank_masato_days <- factor(WASH_data_nomiss$How_long_drank_masato_days)                             
WASH_data_nomiss$How_long_drank_masato_days

WASH_data_nomiss$Did_you_go_with_children. <- factor(WASH_data_nomiss$Did_you_go_with_children.)                              
WASH_data_nomiss$Did_you_go_with_children.

WASH_data_nomiss$Where_do_you_defecate <- factor(WASH_data_nomiss$Where_do_you_defecate)                 
WASH_data_nomiss$Where_do_you_defecate

WASH_data_nomiss$When_you_last_drink_river[WASH_data_nomiss$When_you_last_drink_river == -1] <- NA                              
WASH_data_nomiss$When_you_last_drink_river

WASH_data_nomiss$When_you_last_drink_river  <- factor(WASH_data_nomiss$When_you_last_drink_river,levels=c(0, 1),labels = c('No','Yes'))                                
WASH_data_nomiss$When_you_last_drink_river

x <- table(WASH_data_nomiss$When_you_last_drink_river)
x
round(prop.table(x),4)*100

summary(WASH_data_nomiss$When_you_last_drink_river)

#summary(WASH_data_nomiss$Days_since_you_drink_water)
#Desc(WASH_data_nomiss$Days_since_you_drink_water, na.rm = T)

WASH_data_nomiss$Do_you_wash_your_hands_after_defecate. <- factor(WASH_data_nomiss$Do_you_wash_your_hands_after_defecate.)                 
WASH_data_nomiss$Do_you_wash_your_hands_after_defecate.

WASH_data_nomiss$handwash_with_soap_after_defecate. <- factor(WASH_data_nomiss$handwash_with_soap_after_defecate.)                    
WASH_data_nomiss$handwash_with_soap_after_defecate.

WASH_data_nomiss$handwash_with_others_after_defecate. <- factor(WASH_data_nomiss$handwash_with_others_after_defecate.)                   
WASH_data_nomiss$handwash_with_others_after_defecate.

WASH_data_nomiss$Nothing_after_Defecates <- factor(WASH_data_nomiss$Nothing_after_Defecates)                                
WASH_data_nomiss$Nothing_after_Defecates

WASH_data_nomiss$Defecates_field <- factor(WASH_data_nomiss$Defecates_field)                                       
WASH_data_nomiss$Defecates_field

WASH_data_nomiss$Defecates_latrine <- factor(WASH_data_nomiss$Defecates_latrine)                                      
WASH_data_nomiss$Defecates_latrine

WASH_data_nomiss$Buries_feces <- factor(WASH_data_nomiss$Buries_feces)                                  
WASH_data_nomiss$Buries_feces

WASH_data_nomiss$Observed_washing_hands <- factor(WASH_data_nomiss$Observed_washing_hands)                                 
WASH_data_nomiss$Observed_washing_hands

WASH_data_nomiss$Frequency_washes_hands <- factor(WASH_data_nomiss$Frequency_washes_hands)                                 
WASH_data_nomiss$Frequency_washes_hands

WASH_data_nomiss$Location_of_wash_hands <- factor(WASH_data_nomiss$Location_of_wash_hands)                                 
WASH_data_nomiss$Location_of_wash_hands

WASH_data_nomiss$Wash_hands_soap <- factor(WASH_data_nomiss$Wash_hands_soap)                                        
WASH_data_nomiss$Wash_hands_soap

WASH_data_nomiss$Wash_hands_other <- factor(WASH_data_nomiss$Wash_hands_other)                
WASH_data_nomiss$Wash_hands_other

WASH_data_nomiss$handwash_before_eating_or_after_bathroom <- factor(WASH_data_nomiss$handwash_before_eating_or_after_bathroom)               
WASH_data_nomiss$handwash_before_eating_or_after_bathroom

WASH_data_nomiss$hand_washing_moments <- factor(WASH_data_nomiss$hand_washing_moments)                                  
WASH_data_nomiss$hand_washing_moments

WASH_data_nomiss$handwash_notuse_reason <- factor(WASH_data_nomiss$handwash_notuse_reason )                         
WASH_data_nomiss$handwash_notuse_reason

WASH_data_nomiss$observed_clean_bathroom_toilet <- factor(WASH_data_nomiss$observed_clean_bathroom_toilet)                         
WASH_data_nomiss$observed_clean_bathroom_toilet

WASH_data_nomiss$bathroom_Paper_covered <- factor(WASH_data_nomiss$bathroom_Paper_covered)                                 
WASH_data_nomiss$bathroom_Paper_covered

WASH_data_nomiss$presence_of_rodents <- factor(WASH_data_nomiss$presence_of_rodents)                                    
WASH_data_nomiss$presence_of_rodents

WASH_data_nomiss$Play_with_pets <- factor(WASH_data_nomiss$Play_with_pets)                                         
WASH_data_nomiss$Play_with_pets

WASH_data_nomiss$What_pet <- factor(WASH_data_nomiss$Play_with_pets)                     
WASH_data_nomiss$What_pet

WASH_data_nomiss$After_plays_with_pets_washes_hands <- factor(WASH_data_nomiss$After_plays_with_pets_washes_hands)                     
WASH_data_nomiss$After_plays_with_pets_washes_hands

WASH_data_nomiss$There_are_rats <- factor(WASH_data_nomiss$There_are_rats)                                         
WASH_data_nomiss$There_are_rats

WASH_data_nomiss$contact_mice <- factor(WASH_data_nomiss$contact_mice )                              
WASH_data_nomiss$contact_mice

WASH_data_nomiss$Last_time_ate_picuro_days <- factor(WASH_data_nomiss$Last_time_ate_picuro_days )                             
WASH_data_nomiss$Last_time_ate_picuro_days

WASH_data_nomiss$Last_time_ate_roncoso_days <- factor(WASH_data_nomiss$Last_time_ate_roncoso_days )                            
WASH_data_nomiss$Last_time_ate_roncoso_days

WASH_data_nomiss$How_did_you_hunt <- factor(WASH_data_nomiss$How_did_you_hunt )                                      
WASH_data_nomiss$How_did_you_hunt

WASH_data_nomiss$Has_dogs <- factor(WASH_data_nomiss$Has_dogs )                                              
WASH_data_nomiss$Has_dogs

WASH_data_nomiss$Has_cats <- factor(WASH_data_nomiss$Has_cats )                                      
WASH_data_nomiss$Has_cats

WASH_data_nomiss$Has_animal_others <- factor(WASH_data_nomiss$Has_animal_others)                                      
WASH_data_nomiss$Has_animal_others

WASH_data_nomiss$Raises_animals <- factor(WASH_data_nomiss$Raises_animals )                                  
WASH_data_nomiss$Raises_animals

WASH_data_nomiss$observed_animals_type <- factor(WASH_data_nomiss$observed_animals_type  )                               
WASH_data_nomiss$observed_animals_type

WASH_data_nomiss$frequency_ate_yesterday <- factor(WASH_data_nomiss$frequency_ate_yesterday )                               
WASH_data_nomiss$frequency_ate_yesterday

WASH_data_nomiss$what_ate_yesterday <- factor(WASH_data_nomiss$what_ate_yesterday )                                    
WASH_data_nomiss$what_ate_yesterday

WASH_data_nomiss$ate_other_yesterday <- factor(WASH_data_nomiss$ate_other_yesterday )                                 
WASH_data_nomiss$ate_other_yesterday

WASH_data_nomiss$Last_time_you_ate_fish <- factor(WASH_data_nomiss$Last_time_you_ate_fish )                            
WASH_data_nomiss$Last_time_you_ate_fish

WASH_data_nomiss$Last_time_you_ate_What.fish <- factor(WASH_data_nomiss$Last_time_you_ate_What.fish)                
WASH_data_nomiss$Last_time_you_ate_What.fish

WASH_data_nomiss$How_did_you_get_fished_Last_time_you_ate <- factor(WASH_data_nomiss$How_did_you_get_fished_Last_time_you_ate)               
WASH_data_nomiss$How_did_you_get_fished_Last_time_you_ate

WASH_data_nomiss$Last_time_you_ate_wild_meat <- factor(WASH_data_nomiss$Last_time_you_ate_wild_meat)                            
WASH_data_nomiss$Last_time_you_ate_wild_meat

WASH_data_nomiss$Last_time_you_ate_What_meat <- factor(WASH_data_nomiss$Last_time_you_ate_What_meat)                  
WASH_data_nomiss$Last_time_you_ate_What_meat

WASH_data_nomiss$How_did_you_get_meat_Last_time_you_ate <- factor(WASH_data_nomiss$How_did_you_get_meat_Last_time_you_ate)                 
WASH_data_nomiss$How_did_you_get_meat_Last_time_you_ate

WASH_data_nomiss$Had_diarrhea <- factor(WASH_data_nomiss$Had_diarrhea)                                        
WASH_data_nomiss$Had_diarrhea

WASH_data_nomiss$Episode_diarrhea <- factor(WASH_data_nomiss$Episode_diarrhea )                                      
WASH_data_nomiss$Episode_diarrhea 

WASH_data_nomiss$had_diarrhea_days <- factor(WASH_data_nomiss$had_diarrhea_days )                                    
WASH_data_nomiss$had_diarrhea_days

WASH_data_nomiss$still_sick_diarrhea <- factor(WASH_data_nomiss$still_sick_diarrhea)                                    
WASH_data_nomiss$still_sick_diarrhea

WASH_data_nomiss$watery_diarrhea <- factor(WASH_data_nomiss$watery_diarrhea)                                     
WASH_data_nomiss$watery_diarrhea

WASH_data_nomiss$dysenteric_diarrhea <- factor(WASH_data_nomiss$dysenteric_diarrhea )                                   
WASH_data_nomiss$dysenteric_diarrhea

WASH_data_nomiss$had_diarrhea_mucus <- factor(WASH_data_nomiss$had_diarrhea_mucus )                              
WASH_data_nomiss$had_diarrhea_mucus

WASH_data_nomiss$Went_health_post_diarrhea <- factor(WASH_data_nomiss$Went_health_post_diarrhea )                             
WASH_data_nomiss$Went_health_post_diarrhea

WASH_data_nomiss$Severe_diarrhea <- factor(WASH_data_nomiss$Severe_diarrhea      )                           
WASH_data_nomiss$Severe_diarrhea

WASH_data_nomiss$Date_treatment_diarrhea <- factor(WASH_data_nomiss$Date_treatment_diarrhea )                               
WASH_data_nomiss$Date_treatment_diarrhea

WASH_data_nomiss$had_diarrhea_bloody <- factor(WASH_data_nomiss$had_diarrhea_bloody)                                    
WASH_data_nomiss$had_diarrhea_bloody

WASH_data_nomiss$diarrhea_vomiting <- factor(WASH_data_nomiss$diarrhea_vomiting)                                      
WASH_data_nomiss$diarrhea_vomiting

WASH_data_nomiss$diarrhea_nausea <- factor(WASH_data_nomiss$diarrhea_nausea )                                       
WASH_data_nomiss$diarrhea_nausea

WASH_data_nomiss$diarrhea_fever <- factor(WASH_data_nomiss$diarrhea_fever)                                        
WASH_data_nomiss$diarrhea_fever

WASH_data_nomiss$diarrhea_unknown <- factor(WASH_data_nomiss$diarrhea_unknown )                                      
WASH_data_nomiss$diarrhea_unknown

WASH_data_nomiss$diarrhea_cause <- factor(WASH_data_nomiss$diarrhea_cause )                               
WASH_data_nomiss$diarrhea_cause

WASH_data_nomiss$Number_children_diarrhea <- factor(WASH_data_nomiss$Number_children_diarrhea)                      
WASH_data_nomiss$Number_children_diarrhea

WASH_data_nomiss$Went_health_post_children_diarrhea <- factor(WASH_data_nomiss$Went_health_post_children_diarrhea)                     
WASH_data_nomiss$Went_health_post_children_diarrhea

WASH_data_nomiss$Number_adults_diarrhea <- factor(WASH_data_nomiss$Number_adults_diarrhea )                        
WASH_data_nomiss$Number_adults_diarrhea

WASH_data_nomiss$Went_health_post_adult_diarrhea <- factor(WASH_data_nomiss$Went_health_post_adult_diarrhea )                       
WASH_data_nomiss$Went_health_post_adult_diarrhea

WASH_data_nomiss$days_since_last_cough <- factor(WASH_data_nomiss$days_since_last_cough)                                  
WASH_data_nomiss$days_since_last_cough

WASH_data_nomiss$cough_duration_days <- factor(WASH_data_nomiss$cough_duration_days )                                 
WASH_data_nomiss$cough_duration_days

WASH_data_nomiss$Went_health_post_cough <- factor(WASH_data_nomiss$Went_health_post_cough )                                
WASH_data_nomiss$Went_health_post_cough

WASH_data_nomiss$Number_children_cough <- factor(WASH_data_nomiss$Number_children_cough )                        
WASH_data_nomiss$Number_children_cough

WASH_data_nomiss$Went_health_post_children_cough <- factor(WASH_data_nomiss$Went_health_post_children_cough)                        
WASH_data_nomiss$Went_health_post_children_cough

WASH_data_nomiss$Number_adults_cough <- factor(WASH_data_nomiss$Number_adults_cough )                           
WASH_data_nomiss$Number_adults_cough

WASH_data_nomiss$Went_health_post_adult_cough <- factor(WASH_data_nomiss$Went_health_post_adult_cough)                           
WASH_data_nomiss$Went_health_post_adult_cough

WASH_data_nomiss$Had_covid <- factor(WASH_data_nomiss$Had_covid )                                         
WASH_data_nomiss$Had_covid

WASH_data_nomiss$Confirmed_covid <- factor(WASH_data_nomiss$Confirmed_covid)                                   
WASH_data_nomiss$Confirmed_covid

WASH_data_nomiss$had_any_other_illness <- factor(WASH_data_nomiss$had_any_other_illness)                                  
WASH_data_nomiss$had_any_other_illness

WASH_data_nomiss$which_illness <- factor(WASH_data_nomiss$which_illness)                                  
WASH_data_nomiss$which_illness

WASH_data_nomiss$Received_treatment_ARI <- factor(WASH_data_nomiss$Received_treatment_ARI)                                 
WASH_data_nomiss$Received_treatment_ARI

WASH_data_nomiss$Observations <- factor(WASH_data_nomiss$Observations)                                 
WASH_data_nomiss$Observations

WASH_data_nomiss$Had_respiratory_illness <- factor(WASH_data_nomiss$Had_respiratory_illness)                        
WASH_data_nomiss$Had_respiratory_illness

WASH_data_nomiss$Freq_episode_respiratory_illness <- factor(WASH_data_nomiss$Freq_episode_respiratory_illness)                  
WASH_data_nomiss$Freq_episode_respiratory_illness

WASH_data_nomiss$How_long_respiratory_illness_last_days <- factor(WASH_data_nomiss$How_long_respiratory_illness_last_days )                
WASH_data_nomiss$How_long_respiratory_illness_last_days

WASH_data_nomiss$Went_health_post_respiratory_illness <- factor(WASH_data_nomiss$Went_health_post_respiratory_illness)                   
WASH_data_nomiss$Went_health_post_respiratory_illness

WASH_data_nomiss$Date_treatment_respiratory_illness <- factor(WASH_data_nomiss$Date_treatment_respiratory_illness)                     
WASH_data_nomiss$Date_treatment_respiratory_illness

WASH_data_nomiss$still_sick_respiratory <- factor(WASH_data_nomiss$still_sick_respiratory)                              
WASH_data_nomiss$still_sick_respiratory

WASH_data_nomiss$Severe_respiratory_illness <- factor(WASH_data_nomiss$Severe_respiratory_illness)                     
WASH_data_nomiss$Severe_respiratory_illness

WASH_data_nomiss$had_respiratory_illness_sore_throat <- factor(WASH_data_nomiss$had_respiratory_illness_sore_throat)                    
WASH_data_nomiss$had_respiratory_illness_sore_throat

WASH_data_nomiss$had_respiratory_illness_fever <- factor(WASH_data_nomiss$had_respiratory_illness_fever)                          
WASH_data_nomiss$had_respiratory_illness_fever

WASH_data_nomiss$had_respiratory_illness_cough <- factor(WASH_data_nomiss$had_respiratory_illness_cough)                          
WASH_data_nomiss$had_respiratory_illness_cough

WASH_data_nomiss$had_skin_disease <- factor(WASH_data_nomiss$had_skin_disease)                                   
WASH_data_nomiss$had_skin_disease

WASH_data_nomiss$had_skin_disease_days <- factor(WASH_data_nomiss$had_skin_disease_days)                           
WASH_data_nomiss$had_skin_disease_days

WASH_data_nomiss$Went_health_post_skin_disease <- factor(WASH_data_nomiss$Went_health_post_skin_disease)                          
WASH_data_nomiss$Went_health_post_skin_disease

WASH_data_nomiss$number_living_at_home <- factor(WASH_data_nomiss$number_living_at_home)
WASH_data_nomiss$number_living_at_home

WASH_data_nomiss$mother_years_old <- factor(WASH_data_nomiss$mother_years_old )                                      
WASH_data_nomiss$mother_years_old

WASH_data_nomiss$father_years_old <- factor(WASH_data_nomiss$father_years_old )                                      
WASH_data_nomiss$father_years_old

WASH_data_nomiss$household_task <- factor(WASH_data_nomiss$household_task)                                         
WASH_data_nomiss$household_task

WASH_data_nomiss$hunt_father <- factor(WASH_data_nomiss$hunt_father)                                          
WASH_data_nomiss$hunt_father

WASH_data_nomiss$fishing_father <- factor(WASH_data_nomiss$fishing_father )                                     
WASH_data_nomiss$fishing_father

WASH_data_nomiss$Cultivation_father <- factor(WASH_data_nomiss$Cultivation_father )                                    
WASH_data_nomiss$Cultivation_father

WASH_data_nomiss$wash_river_father <- factor(WASH_data_nomiss$wash_river_father )                             
WASH_data_nomiss$wash_river_father

WASH_data_nomiss$collect_from_jungle_father <- factor(WASH_data_nomiss$collect_from_jungle_father  )                           
WASH_data_nomiss$collect_from_jungle_father

WASH_data_nomiss$raise.feed_animals_father <- factor(WASH_data_nomiss$raise.feed_animals_father)                              
WASH_data_nomiss$raise.feed_animals_father

WASH_data_nomiss$construction_father <- factor(WASH_data_nomiss$construction_father)                                    
WASH_data_nomiss$construction_father

WASH_data_nomiss$kitchen_father <- factor(WASH_data_nomiss$kitchen_father)                                         
WASH_data_nomiss$kitchen_father

WASH_data_nomiss$Other_father <- factor(WASH_data_nomiss$Other_father )                                     
WASH_data_nomiss$Other_father

WASH_data_nomiss$Which_other_father <- factor(WASH_data_nomiss$Which_other_father  )                                   
WASH_data_nomiss$Which_other_father

WASH_data_nomiss$hunt_mother <- factor(WASH_data_nomiss$hunt_mother )                                         
WASH_data_nomiss$hunt_mother

WASH_data_nomiss$fishing_mother <- factor(WASH_data_nomiss$fishing_mother  )                                    
WASH_data_nomiss$fishing_mother

WASH_data_nomiss$Cultivation_mother <- factor(WASH_data_nomiss$Cultivation_mother )                                    
WASH_data_nomiss$Cultivation_mother

WASH_data_nomiss$wash_river_mother <- factor(WASH_data_nomiss$wash_river_mother )                             
WASH_data_nomiss$wash_river_mother

WASH_data_nomiss$collect_from_jungle_mother <- factor(WASH_data_nomiss$collect_from_jungle_mother)                             
WASH_data_nomiss$collect_from_jungle_mother

WASH_data_nomiss$raise.feed_animals_mother <- factor(WASH_data_nomiss$raise.feed_animals_mother )                             
WASH_data_nomiss$raise.feed_animals_mother

WASH_data_nomiss$construction_mother <- factor(WASH_data_nomiss$construction_mother )                                   
WASH_data_nomiss$construction_mother

WASH_data_nomiss$kitchen_mother <- factor(WASH_data_nomiss$kitchen_mother )                                       
WASH_data_nomiss$kitchen_mother

WASH_data_nomiss$Other_mother <- factor(WASH_data_nomiss$Other_mother )                                         
WASH_data_nomiss$Other_mother

WASH_data_nomiss$Which_other_mother <- factor(WASH_data_nomiss$Which_other_mother )                                    
WASH_data_nomiss$Which_other_mother

WASH_data_nomiss$Activity_home_child <- factor(WASH_data_nomiss$Activity_home_child )                                    
WASH_data_nomiss$Activity_home_child

WASH_data_nomiss$hunt_child <- factor(WASH_data_nomiss$hunt_child )                                          
WASH_data_nomiss$hunt_child

WASH_data_nomiss$fishing_child <- factor(WASH_data_nomiss$fishing_child)                                
WASH_data_nomiss$fishing_child

WASH_data_nomiss$collect_from_jungle_child <- factor(WASH_data_nomiss$collect_from_jungle_child)                              
WASH_data_nomiss$collect_from_jungle_child

WASH_data_nomiss$raise.feed_animals_child <- factor(WASH_data_nomiss$raise.feed_animals_child)                               
WASH_data_nomiss$raise.feed_animals_child

WASH_data_nomiss$child_education <- factor(WASH_data_nomiss$child_education)                                    
WASH_data_nomiss$child_education

WASH_data_nomiss$child_plays_in_river <- factor(WASH_data_nomiss$child_plays_in_river)                                   
WASH_data_nomiss$child_plays_in_river

WASH_data_nomiss$child_helps_on_farm <- factor(WASH_data_nomiss$child_helps_on_farm)                           
WASH_data_nomiss$child_helps_on_farm

WASH_data_nomiss$child_helps_with_construction <- factor(WASH_data_nomiss$child_helps_with_construction)                          
WASH_data_nomiss$child_helps_with_construction

WASH_data_nomiss$child_helps_wash_clothes <- factor(WASH_data_nomiss$child_helps_wash_clothes)                               
WASH_data_nomiss$child_helps_wash_clothes

WASH_data_nomiss$child_helps_cooking <- factor(WASH_data_nomiss$child_helps_cooking)                                    
WASH_data_nomiss$child_helps_cooking

WASH_data_nomiss$child_activity_other <- factor(WASH_data_nomiss$child_activity_other)                              
WASH_data_nomiss$child_activity_other

WASH_data_nomiss$child_activity_other_which <- factor(WASH_data_nomiss$child_activity_other_which)                             
WASH_data_nomiss$child_activity_other_which

WASH_data_nomiss$river_activity <- factor(WASH_data_nomiss$river_activity)                                   
WASH_data_nomiss$river_activity

WASH_data_nomiss$Which.river.activity. <- factor(WASH_data_nomiss$Which.river.activity.)                                  
WASH_data_nomiss$Which.river.activity.

WASH_data_nomiss$river_activity_fishing <- factor(WASH_data_nomiss$river_activity_fishing)                                 
WASH_data_nomiss$river_activity_fishing

WASH_data_nomiss$river_activity_bathing <- factor(WASH_data_nomiss$river_activity_bathing)                           
WASH_data_nomiss$river_activity_bathing

WASH_data_nomiss$river_activity_washing_cloths <- factor(WASH_data_nomiss$river_activity_washing_cloths)                          
WASH_data_nomiss$river_activity_washing_cloths

WASH_data_nomiss$river_activity_other <- factor(WASH_data_nomiss$river_activity_other)                              
WASH_data_nomiss$river_activity_other

WASH_data_nomiss$contact_with_animals_home <- factor(WASH_data_nomiss$contact_with_animals_home)                           
WASH_data_nomiss$contact_with_animals_home

WASH_data_nomiss$contact_with_dogs_home <- factor(WASH_data_nomiss$contact_with_dogs_home)                               
WASH_data_nomiss$contact_with_dogs_home

WASH_data_nomiss$contact_with_chicken_home <- factor(WASH_data_nomiss$contact_with_chicken_home)                              
WASH_data_nomiss$contact_with_chicken_home

WASH_data_nomiss$contact_with_monkey_home <- factor(WASH_data_nomiss$contact_with_monkey_home)                               
WASH_data_nomiss$contact_with_monkey_home 

WASH_data_nomiss$contact_with_parrots_home  <- factor(WASH_data_nomiss$contact_with_parrots_home)                        
WASH_data_nomiss$contact_with_parrots_home 

WASH_data_nomiss$contact_with_other_animals_home <- factor(WASH_data_nomiss$contact_with_other_animals_home)                    
WASH_data_nomiss$contact_with_other_animals_home

WASH_data_nomiss$contact_with_other_animals_home_what <- factor(WASH_data_nomiss$contact_with_other_animals_home_what)                   
WASH_data_nomiss$contact_with_other_animals_home_what

WASH_data_nomiss$contact_with_animals_neighbor <- factor(WASH_data_nomiss$contact_with_animals_neighbor)                          
WASH_data_nomiss$contact_with_animals_neighbor

WASH_data_nomiss$contact_with_dogs_neighbor <- factor(WASH_data_nomiss$contact_with_dogs_neighbor)                           
WASH_data_nomiss$contact_with_dogs_neighbor

WASH_data_nomiss$contact_with_chicken_neighbor <- factor(WASH_data_nomiss$contact_with_chicken_neighbor)                          
WASH_data_nomiss$contact_with_chicken_neighbor

WASH_data_nomiss$contact_with_monkey_neighbor <- factor(WASH_data_nomiss$contact_with_monkey_neighbor )                          
WASH_data_nomiss$contact_with_monkey_neighbor

WASH_data_nomiss$contact_with_parrots_neighbor <- factor(WASH_data_nomiss$contact_with_parrots_neighbor)                     
WASH_data_nomiss$contact_with_parrots_neighbor

WASH_data_nomiss$contact_with_other_animals_neighbor <- factor(WASH_data_nomiss$contact_with_other_animals_neighbor  )              
WASH_data_nomiss$contact_with_other_animals_neighbor

WASH_data_nomiss$contact_with_other_animals_neighbor_what <- factor(WASH_data_nomiss$contact_with_other_animals_neighbor_what)               
WASH_data_nomiss$contact_with_other_animals_neighbor_what

WASH_data_nomiss$contact_with_rodents <- factor(WASH_data_nomiss$contact_with_rodents )                              
WASH_data_nomiss$contact_with_rodents

WASH_data_nomiss$contact_with_rodents_home <- factor(WASH_data_nomiss$contact_with_rodents_home)                              
WASH_data_nomiss$contact_with_rodents_home

WASH_data_nomiss$contact_with_rodents_farm <- factor(WASH_data_nomiss$contact_with_rodents_farm)                              
WASH_data_nomiss$contact_with_rodents_farm

WASH_data_nomiss$breakfast_this_morning <- factor(WASH_data_nomiss$breakfast_this_morning)                                 
WASH_data_nomiss$breakfast_this_morning

WASH_data_nomiss$what_breakfast  <- factor(WASH_data_nomiss$what_breakfast)                                      
WASH_data_nomiss$what_breakfast

WASH_data_nomiss$dinner_last_night <- factor(WASH_data_nomiss$dinner_last_night)                                      
WASH_data_nomiss$dinner_last_night

WASH_data_nomiss$what_dinner <- factor(WASH_data_nomiss$what_dinner)
WASH_data_nomiss$what_dinner





WASH_data_nomiss <- subset(WASH_data_nomiss, Year %in% c(2020, 2021, 2022, 2023, 2024))

WASH_data_nomiss <- subset(WASH_data_nomiss, Had_diarrhea %in% c(0, 1))

WASH_data_nomiss$Had_diarrhea_cat <- factor(WASH_data_nomiss$Had_diarrhea,levels=c(0,1),labels = c('No','Yes'))
WASH_data_nomiss$Had_diarrhea_cat 

summary(WASH_data_nomiss$Had_diarrhea_cat)
x <- table(WASH_data_nomiss$Had_diarrhea_cat)
x
round(prop.table(x),4)*100
#3645 + 496


x <- table(WASH_data_nomiss$Year)
x
round(prop.table(x),4)*100
#145  + 115 + 1787 + 1071 + 1023  

diatab <- table( WASH_data_nomiss$Had_diarrhea_cat, WASH_data_nomiss$Year)
diatab

round(prop.table(diatab,2)*100,2)

chisq.test(diatab)







###Respiratory Illness Vs Years######

setwd('F:\\PhD Thesis\\WASH\\Dataset_raw\\')
WASH_data <- read.csv("Translated_Data_All_Append.csv")

WASH_data <- subset(WASH_data, Year %in% c(2020, 2021, 2022, 2023, 2024))

WASH_data <- subset(WASH_data, Had_respiratory_illness %in% c(0, 1))

WASH_data$Had_respiratory_illness_cat <- factor(WASH_data$Had_respiratory_illness,levels=c(0,1),labels = c('No','Yes'))
WASH_data$Had_respiratory_illness_cat 

summary(WASH_data$Had_respiratory_illness_cat )
x <- table(WASH_data$Had_respiratory_illness_cat )
x
round(prop.table(x),4)*100
#1522  + 634


x <- table(WASH_data$Year)
x
round(prop.table(x),4)*100
#145 + 1662  + 283   + 66  

diatab <- table(WASH_data$Year, WASH_data$Had_respiratory_illness_cat)
diatab

round(prop.table(diatab,1)*100,2)

chisq.test(diatab)

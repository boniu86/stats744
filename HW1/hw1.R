library(readr)
library(tidyverse)


##load dataset
heart_data<-read_csv("heart.csv",col_names =FALSE)

names(heart_data)<-c("age","sex","chest_pain_type", "resting_blood_pressure",
                     "serum_cholestoral_in_mg/dl " ,    
       "fasting_blood_sugar>120(mg/dl)",      
      "resting_electrocardiographic_results" ,
      "maximum_heart_rate_achieved", 
      "exercise_induced_angina",   
      "oldpeak","the_slope_of_oldpeak",    
      "number_of_major_vessels_colored_by_flourosopy",        
      "thal","results"   
)
#no N/A (mentioned on dataset folder),also can be checked by:
any(is.na(heart_data)) #[1] FALSE

##all var r int, so correct them into factors and numbers; 
heart_data[,c(2,3,6,7,9,11,13,14)]<-lapply(heart_data[,c(2,3,6,7,9,11,13,14)], as.factor)
heart_data[,-c(2,3,6,7,9,11,13,14)]<-lapply(heart_data[,-c(2,3,6,7,9,11,13,14)], as.numeric)

##get basic idea of dataset; 
summary(heart_data)
##age range from yong 29 to older 77

##sex 2 types, sex (1 = male; 0 = female) 

##pain type, 4 types, 
 #cp: chest pain type 
 #Value 1: typical angina 
 #Value 2: atypical angina 
 #Value 3: non-anginal pain 
 #Value 4: asymptomatic 

##resting blood pressure, google said normal is 120/80~140/90; 
 #the resting blood pressure looksl ike recorded systolic blood pressure. 
 #(The highest pressure when your heart beats and pushes the blood round your body.) 

##serum_cholestoral ranges from min 126 to max 564; 
 #again,based on google resuts
 #Less than 200mg/dL	Desirable
 #200-239 mg/dL	Borderline high
 #240mg/dL and above	High
 #thoes patients tends to have high serum cholestoral;

##fasting_blood_sugar if larger than 120mg/dl
 #A fasting blood sugar level less than 100 mg/dL (5.6 mmol/L) is normal.
 #A fasting blood sugar level from 100 to 125 mg/dL (5.6 to 6.9 mmol/L) is considered prediabetes. 
 #If it's 126 mg/dL (7 mmol/L) or higher on two separate tests, you have diabetes.
 #0 indicates lower and 1 otherwise
 #most patients are normal and prediabetes level

##resting_electrocardiographic_results
 #Value 0: normal 
 #Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) 
 #Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria 

##maximum_heart_rate_achieved
 #min from 71 to max 201, 

##exercise_induced_angina
 #(1 = yes; 0 = no) 
 #few ppl have angina when exercise
 
##oldpeak:ST depression induced by exercise relative to rest   
 #It is often a sign of myocardial ischemia

##slope of ST depression
 #Value 1: upsloping 
 #Value 2: flat 
 #Value 3: downsloping 

##number_of_major_vessels_colored_by_flourosopy

##thal:3 = normal; 6 = fixed defect; 7 = reversable defect     

##results: Absence (1) or presence (2) of heart disease


##simple plot
gg1<-ggplot(heart_data,aes(x=age,y=maximum_heart_rate_achieved,colour=results))
print(gg1+geom_point())

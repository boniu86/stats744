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


theme_set(theme_bw())
##simple plot

#may interested in gender difference in resting blood pressure with ages caused by heart diseaes;
gg1<-(ggplot(heart_data,aes(x=age,y=resting_blood_pressure,colour=results)))+geom_point()+facet_wrap(~sex)
print(gg1)
##conclusion: 0 is female,1 is male; colour 1 is non-heart-disease, otherwise 2;
 #blood pressure does not differ with gender;
 #the number of female herat disease patients are obviously less than male's;
 #for female, almost all heart diseaes patients are over 50 ys old; also resting blood pressure are higher in heart diseaes patients than other;
 #for male, heart diseaes patintes ages across all the age range, from 35 to 70; blood pressure are range all across of heart diseaes patients and non-heart diseaes;
 #seems like female heart diseaes risk increase with ages, this phenomenon only occurs in here, may wrong due to lack to data collection;
 

##ad smooth line
gg2<-gg1+geom_smooth(method="lm",aes(color=results,fill=results))
print(gg2)


##make more complicated plot;
gg0 <- ggplot(heart_data,aes(age,exercise_induced_angina))+geom_point()
print(gg0)
##no sence of this plots

(heart_data
  ## collapse age by year
  %>% mutate(f_age=cut(age,breaks=29:77,labels=seq(29.5,76.5)),
             ## then turn it back into a number
             f_age=as.numeric(as.character(f_age)))
  ## means by age group/heart diseaes vs non-heart disease/number of chest pain type
  %>% group_by(f_age,results,thal)
  ## compute proportion, n, standard error
  %>% summarise(prop=mean(as.numeric(exercise_induced_angina)-1),
                n=n(),
                se=sqrt(prop*(1-prop)/n))
) -> heart_sum

gg4 <- ggplot(heart_sum,aes(f_age,prop,colour=results)) +
  geom_point(aes(size=n))+
  geom_linerange(aes(ymin=prop-1*se,ymax=prop+1*se))+
  facet_wrap(~thal,labeller=label_both)+
  scale_colour_brewer(palette="Dark2")
print(gg4)

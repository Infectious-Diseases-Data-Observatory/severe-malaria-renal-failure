## multiple imputation using random forests
library(tidyverse)
library(conflicted)
library(missForest)
library(doParallel)
library(tictoc)

RUN_IMPUTE_MODEL = F
sm_data = readr::read_csv('Data/Renal_Analysis_Data.csv')
key_cols=c("STUDY","COUNTRY","SITEID","SEX","Age","Died",
           "Heart Rate_beats_min","Height_cm","Mid-Upper Arm Circumference_cm",
           "Respiratory Rate_breaths_min",
           "Systolic Blood Pressure_mmHg","Temperature_C",
           "Weight_kg","Oxygen Saturation_%","Diastolic Blood Pressure_mmHg",
           "Coma_Final",
           "Respiratory distress","Shock","Hypoglycaemia","Seizure","Jaundice",
           "Anemia","Hyperparasitaemia",
           "GCS_tot","BCS_tot","para_ul","Glucose_mmol_L","Hemoglobin_g_L",
           "Hematocrit_%", "Lactic Acid_mmol_L","Base Excess_mmol_L",
           "Creatinine_umol_L","BUN", "Urea Nitrogen_mmol_L")
sm_data = sm_data[, key_cols] %>%
  mutate(across(c(STUDY,COUNTRY,SITEID,SEX,Hyperparasitaemia,Seizure,Jaundice,Shock,Hypoglycaemia), as.factor))
sm_data = as.data.frame(sm_data)
str(sm_data)
summary(sm_data)
registerDoParallel(cores=6)

f_name = 'RData/imputed_dataset.RData'
if(RUN_IMPUTE_MODEL){
  tic(); sm_impute = missForest(xmis = sm_data, parallelize = 'forests',ntree = 50); toc();
  save(sm_impute, file = f_name)
} else {
  load(f_name)
}


x_imp = sm_impute$ximp

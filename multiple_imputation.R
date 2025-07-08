## multiple imputation using random forests
library(tidyverse)
library(conflicted)
library(missForest)
library(doParallel)
library(tictoc)
library(naniar)
library(mice)
library(doParallel)
library(foreach)
library(ranger)


RUN_IMPUTE_MODEL = T
sm_data = readr::read_csv('Data/Analysis_data.csv')
key_cols=c("STUDY","SEX","Age","Died",'Mid-Upper Arm Circumference_cm',
           "Weight_kg","Hb", "Lactate","Height_cm",'Anemia',
           "Base Excess_mmol_L","Bicarbonate_mEq_L",
           "Creatinine_umol_L","BUN")
sm_data = sm_data[, key_cols ] %>%
  mutate(across(c(STUDY,SEX,Anemia,Died), as.factor),
         # across(c(`Respiratory distress`,Shock,Hypoglycaemia,Seizure,
         #          Jaundice,Hyperparasitaemia,Anemia), as.numeric),
         log10_BUN = log10(BUN),
         log10_Creatinine = log10(Creatinine_umol_L))

imp_list = mice(sm_data, m = 5,  printFlag = T, maxit = 15)

f_name = 'RData/imputed_dataset.RData'
save(imp_list, file = f_name)




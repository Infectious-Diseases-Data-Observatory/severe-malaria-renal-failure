## multiple imputation using random forests
library(tidyverse)
library(conflicted)
library(missForest)
library(doParallel)
library(tictoc)
library(naniar)


RUN_IMPUTE_MODEL = T
sm_data = readr::read_csv('Data/Analysis_data.csv')
key_cols=c("STUDY","COUNTRY","SITEID","SEX","Age","Died",
           "Heart Rate_beats_min","Height_cm","Mid-Upper Arm Circumference_cm",
           "Respiratory Rate_breaths_min",
           "Systolic Blood Pressure_mmHg","Temperature_C",
           "Weight_kg","Oxygen Saturation_%","Diastolic Blood Pressure_mmHg",
           "Coma_Final","Platelets_10_9_L",
           "Respiratory distress","Shock","Hypoglycaemia","Seizure","Jaundice",
           "Anemia","Hyperparasitaemia",
           "para_ul","Glucose_mmol_L","Hb",
           "HCT", "Lactate","Base Excess_mmol_L",
           "Creatinine_umol_L","BUN")
sm_data = sm_data[, c('USUBJID', key_cols) ] %>%
  mutate(across(c(STUDY,COUNTRY,SITEID,SEX), as.factor),
         across(c(`Respiratory distress`,Shock,Hypoglycaemia,Seizure,
                  Jaundice,Hyperparasitaemia,Anemia), as.numeric))%>%
  arrange(STUDY)
sm_data = as.data.frame(sm_data)
str(sm_data)
vis_miss(sm_data %>% select(all_of(key_cols)) )
summary(sm_data)


registerDoParallel(cores=6)

test_indices = sample(1:nrow(sm_data),size = 5000,replace = F)

f_name = 'RData/imputed_dataset.RData'
if(RUN_IMPUTE_MODEL){
  tic();
  sm_impute = missForest(xmis = sm_data[, -1], parallelize = 'forests',ntree = 100);
  toc();
  save(sm_impute, file = f_name)
} else {
  load(f_name)
}


x_imp = sm_impute$ximp
x_imp %>% ggplot(aes(x=BUN, y = Creatinine_umol_L))+geom_point()+scale_x_log10()+scale_y_log10()+
  facet_wrap(vars(STUDY))

sm_data %>% ggplot(aes(x=BUN, y = Creatinine_umol_L))+geom_point()+scale_x_log10()+scale_y_log10()+
  geom_smooth()+theme_minimal()+
  facet_wrap(vars(STUDY))


xx = sm_data %>% filter(STUDY=='TRACT')


xx$BUN_Creat_ratio = ifelse((log2(xx$Creatinine_umol_L) - log2(xx$BUN) <1) |
                              (log2(xx$Creatinine_umol_L) - log2(xx$BUN) > 6), T, F)
xx %>% ggplot(aes(x=BUN, y = Creatinine_umol_L, color = BUN_Creat_ratio))+
  geom_point()+scale_x_log10()+scale_y_log10()+
  theme_minimal()

sm_data %>% ggplot(aes(x=log2(Creatinine_umol_L) - log2(BUN)))+
  geom_histogram()+geom_vline(xintercept = c(0.5,6.5))+
  theme_minimal()

sm_data %>% ggplot(aes(x=BUN, y = Creatinine_umol_L, color = STUDY))+
  geom_point()+scale_x_log10()+scale_y_log10()+
  theme_minimal()

xx %>% ggplot(aes(x=log2(Creatinine_umol_L) - log2(BUN)))+
  geom_histogram()+geom_vline(xintercept = c(1,6))+
  theme_minimal()

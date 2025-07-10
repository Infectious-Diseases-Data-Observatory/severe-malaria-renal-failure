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
           "Creatinine_umol_L","BUN", 'Creat_BUN_ratio')
p1=vis_miss(sm_data %>% select(all_of(key_cols)), sort_miss = T)
p1
p2=vis_miss(sm_data %>% dplyr::filter(INCLUDE_CC_ANALYSIS) %>% select(all_of(key_cols)), sort_miss = T)
p2
ggsave(filename = 'Missing_key_data.pdf', plot = p1)
ggsave(filename = 'Missing_key_data_analysis_pop.pdf', plot = p2)

sm_data = sm_data[, key_cols ] %>%
  mutate(across(c(STUDY,SEX,Anemia,Died), as.factor),
         # across(c(`Respiratory distress`,Shock,Hypoglycaemia,Seizure,
         #          Jaundice,Hyperparasitaemia,Anemia), as.numeric),
         log10_BUN = log10(BUN),
         log10_Creatinine = log10(Creatinine_umol_L),
         log10_Creat_BUN_ratio = log10(Creat_BUN_ratio),
         log10_Creat_BUN_ratio = ifelse(log10_Creat_BUN_ratio<0 | log10_Creat_BUN_ratio>2, NA, log10_Creat_BUN_ratio)) %>%
  select(-Creatinine_umol_L, -BUN, -Creat_BUN_ratio)

p1=sm_data %>% ggplot(aes(x=Age, y = Height_cm, colour = STUDY))+geom_point()+theme(legend.position = 'none')
p2=sm_data %>% ggplot(aes(x=Age, y = `Mid-Upper Arm Circumference_cm`, colour = STUDY))+geom_point()+theme(legend.position = 'none')
p3=sm_data %>% ggplot(aes(x=Age, y = Hb, colour = STUDY))+geom_point()+theme(legend.position = 'none')
p4=sm_data %>% ggplot(aes(x=Bicarbonate_mEq_L, y = Lactate, colour = STUDY))+geom_point()+theme(legend.position = 'none')
gridExtra::grid.arrange(p1,p2,p3,p4)

colnames(sm_data)
set.seed(5237)
imp_list = mice(sm_data, m = 5,  printFlag = T, maxit = 15,
                method = c("","","rf","","rf","rf","norm","rf","rf","rf","norm","norm",'norm',"norm","norm"))
xx = complete(imp_list,action = 1)
numeric_df <- xx %>% select(where(is.numeric))
p1=xx %>% ggplot(aes(x=Age, y = Height_cm, colour = STUDY))+geom_point()+geom_smooth(aes(group=NA))
p2=xx %>% ggplot(aes(x=Age, y = `Mid-Upper Arm Circumference_cm`, colour = STUDY))+geom_point()+geom_smooth(aes(group=NA))
p3=xx %>% ggplot(aes(x=Hb, y = Lactate, colour = STUDY))+geom_point()+geom_smooth(aes(group=NA))
p4=xx %>% ggplot(aes(x=Age, y = Hb, colour = STUDY))+geom_point()+geom_smooth(aes(group=NA))
gridExtra::grid.arrange(p1,p2,p3,p4)

# MUAC imputation does not work for adults!!!
# Lactate imputation has some serious problems (use of linear regression)

# Create pairwise plots
GGally::ggpairs(numeric_df %>% select(Age, `Mid-Upper Arm Circumference_cm`, Weight_kg, Height_cm, Hb),
                mapping = ggplot2::aes(color = as.factor(Age<15)))
GGally::ggpairs(numeric_df %>% select(Age, Lactate,Hb,`Base Excess_mmol_L`,Bicarbonate_mEq_L,log10_BUN,log10_Creatinine),
                mapping = ggplot2::aes(color = as.factor(Age<15)))

f_name = 'RData/imputed_dataset.RData'
save(imp_list, file = f_name)




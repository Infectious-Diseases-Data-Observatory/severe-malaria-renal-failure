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
           "para_ul","Glucose_mmol_L","Hb","HCT",
           "Lactate","Base Excess_mmol_L","Bicarbonate_mEq_L",'pH_',
           "Creatinine_umol_L","BUN")
sm_data = sm_data[, key_cols ] %>%
  mutate(across(c(STUDY,COUNTRY,SITEID,SEX), as.factor),
         across(c(`Respiratory distress`,Shock,Hypoglycaemia,Seizure,
                  Jaundice,Hyperparasitaemia,Anemia), as.numeric),
         log10_BUN = log10(BUN),
         log10_Creatinine = log10(Creatinine_umol_L),
         log10_Platelets = log10(Platelets_10_9_L),)%>%
  arrange(STUDY)
# sm_data = as.data.frame(sm_data)
# str(sm_data)
# vis_miss(sm_data %>% select(all_of(key_cols)) )
# summary(sm_data)
#

registerDoParallel(cores=6)


n_cores <- parallel::detectCores() - 1  # Leave one core free
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Number of imputations
m <- 5
test_indices = sample(1:nrow(sm_data),size = 1000,replace = F)
xmis = sm_data %>% select(-Platelets_10_9_L, -BUN, - Creatinine_umol_L,-HCT)
# Perform imputations in parallel
imp_list <- foreach(i = 1:m, .packages = "mice", .combine = ibind) %dopar% {
  mice(xmis, m = 1, method='rf', seed = i, printFlag = FALSE, maxit = 15)
}

# Stop the cluster
stopCluster(cl)


completed_data <- complete(imp_list, action = 1)
plot_x_y_imputed = function(xmis, completed_data, x, y){
  # Logical vectors indicating which values were imputed
  x_imp <- is.na(xmis[,x])
  y_imp <- is.na(xmis[,y])
completed_data$x = completed_data[,x]
completed_data$y = completed_data[,y]

# Create a combined indicator: "none", "x", "y", or "both"
  imp_status <- ifelse(x_imp & y_imp, "both",
                       ifelse(x_imp, "x",
                              ifelse(y_imp, "y", "none")))

  # Add this to the completed data
  completed_data$imp_status <- factor(imp_status, levels = c("none", "x", "y", "both"))
  completed_data %>% ggplot(aes(x = x, y = y, color = imp_status)) +
    geom_point(alpha = 0.5, size = 1.5) +
    scale_color_manual(values = c("none" = "black", "x" = "blue", "y" = "red", "both" = "purple")) +
    labs(color = "Imputation status") +
    theme_minimal()
}
plot_x_y_imputed(xmis = xmis, completed_data = completed_data,
                 x = 'Bicarbonate_mEq_L',y = "Base Excess_mmol_L")

f_name = 'RData/imputed_dataset.RData'
save(imp_list, file = f_name)


# if(RUN_IMPUTE_MODEL){
#   tic();
#   sm_impute = missForest(, parallelize = 'forests',ntree = 100);
#   imp = mice(sm_data, m=2, method = 'rf', maxit=10, printFlag=TRUE)
#
#   toc();
#   save(sm_impute, file = f_name)
# } else {
#   load(f_name)
# }


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

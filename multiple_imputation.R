## multiple imputation using random forests
library(tidyverse)
# library(conflicted)
library(missForest)
library(doParallel)
library(tictoc)
library(naniar)
library(mice)
library(doParallel)
library(foreach)
library(ranger)


f_name = 'RData/imputed_dataset.RData'
sm_data$log10_para_ul = log10(sm_data$para_ul+50)
key_cols=c("STUDY","SEX","Age","Died",'Mid-Upper Arm Circumference_cm',
           "Weight_kg","Hb", "Lactate","Height_cm",'Anemia',
           "Base Excess_mmol_L","Bicarbonate_mEq_L",'Potassium_mmol_L',
           "Creatinine_umol_L","BUN", 'Creat_BUN_ratio',
           'Seizure', 'Coma', 
           'Respiratory distress','Glucose_mmol_L',
           'Prostration','log10_para_ul','Heart Rate_beats_min')


###### Visualise missingness

# --- 1. Prepare missingness matrix ---
xx <- sm_data %>% filter(INCLUDE_CC_ANALYSIS) %>%
  select(STUDY, all_of(key_cols)) %>%
  arrange(STUDY) %>%
  mutate(.row_id = row_number()) 

miss_data = xx %>%
  pivot_longer(
    cols = all_of(key_cols),
    names_to  = "variable",
    values_to = "value",
    values_transform = is.na
  ) %>%
  mutate(
    variable = factor(variable, levels = key_cols),
    is_missing = value
  )

# --- 2. Study boundary positions for dividing lines ---
study_boundaries <- xx %>%
  group_by(STUDY) %>%
  summarise(
    start = min(.row_id),
    end   = max(.row_id),
    mid   = (min(.row_id) + max(.row_id)) / 2,
    .groups = "drop"
  )

# --- 3. Main missingness heatmap ---
p <- ggplot(miss_data, aes(x = variable, y = .row_id, fill = is_missing)) +
  geom_raster() +
  scale_fill_manual(
    values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
    labels = c("FALSE" = "Present", "TRUE"  = "Missing"),
    name   = "Data status"
  ) +
  # Study dividing lines
  geom_hline(
    data        = study_boundaries[-nrow(study_boundaries), ],
    aes(yintercept = end + 0.5),
    colour      = "white",
    linewidth   = 0.8
  ) +
  # Study labels on y-axis
  scale_y_continuous(
    breaks = study_boundaries$mid,
    labels = study_boundaries$STUDY,
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    title    = "Missingness patterns by study",
    subtitle = sprintf("%d rows × %d variables", nrow(sm_data), length(key_cols)),
    x        = NULL,
    y        = "Study"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y      = element_text(size = 9),
    panel.grid       = element_blank(),
    legend.position  = "top",
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey40")
  )


# ggsave(filename = 'Missing_key_data.pdf', plot = p1)
ggsave(filename = '~/Dropbox/Apps/Overleaf/Severe malaria renal failure/SupplementaryFigures/Missing_key_data_analysis_pop.pdf', plot = p)

sm_data_for_imputation = sm_data[, key_cols ] %>%
  mutate(across(c(STUDY,SEX,Anemia,Died), as.factor),
         # across(c(`Respiratory distress`,Shock,Hypoglycaemia,Seizure,
         #          Jaundice,Hyperparasitaemia,Anemia), as.numeric),
         log10_BUN = log10(BUN),
         log10_Creatinine = log10(Creatinine_umol_L),
         log10_Creat_BUN_ratio = log10(Creat_BUN_ratio),
         log10_Creat_BUN_ratio = ifelse(log10_Creat_BUN_ratio<0 | log10_Creat_BUN_ratio>2,
                                        NA, log10_Creat_BUN_ratio)) %>%
  select(-Creatinine_umol_L, -BUN, -Creat_BUN_ratio)

p1=sm_data_for_imputation %>% ggplot(aes(x=Age, y = Height_cm, colour = STUDY))+geom_point()+theme(legend.position = 'none')
p2=sm_data_for_imputation %>% ggplot(aes(x=Age, y = `Mid-Upper Arm Circumference_cm`, colour = STUDY))+geom_point()+theme(legend.position = 'none')
p3=sm_data_for_imputation %>% ggplot(aes(x=Age, y = Hb, colour = STUDY))+geom_point()+theme(legend.position = 'none')
p4=sm_data_for_imputation %>% ggplot(aes(x=Bicarbonate_mEq_L, y = Lactate, colour = STUDY))+geom_point()+theme(legend.position = 'none')
gridExtra::grid.arrange(p1,p2,p3,p4)

colnames(sm_data)

if(RUN_IMPUTE_MODEL | !file.exists(f_name)){
  
  set.seed(946)
  imp_list = mice(sm_data_for_imputation, m = 5,  printFlag = T, maxit = 15,
                  method = "rf")
  save(imp_list, file = f_name)
} else {
  load(file = f_name)
}
xx = complete(imp_list,action = 1)
numeric_df <- xx %>% select(where(is.numeric))

numeric_df$Age_missing = ifelse(is.na(sm_data_for_imputation$Age), 'Yes','No')
numeric_df$Weight_missing = ifelse(is.na(sm_data_for_imputation$Weight_kg), 'Yes','No')
numeric_df$Height_missing = ifelse(is.na(sm_data_for_imputation$Height_cm), 'Yes','No')
vals_missing = is.na(sm_data_for_imputation$Height_cm) + is.na(sm_data_for_imputation$Weight_kg)

numeric_df$Weight_Height_missing = ifelse(vals_missing==0,'none','both')
numeric_df$Weight_Height_missing[vals_missing==1] ='one'
table(numeric_df$Weight_Height_missing)
table(numeric_df$Weight_Height_missing )

p1= numeric_df %>% ggplot(aes(x=Age, y = Height_cm, colour = Height_missing))+geom_jitter(alpha=0.5)+theme_minimal()
p2= numeric_df %>% ggplot(aes(x=Age, y = Weight_kg, colour = Weight_missing))+geom_jitter(alpha=0.5)+theme_minimal()
p3= numeric_df %>% ggplot(aes(x=Height_cm, y = Weight_kg, colour = Weight_Height_missing))+geom_jitter(alpha=0.5)+theme_minimal()
p = gridExtra::grid.arrange(p1,p2,p3)
ggsave(filename = '~/Dropbox/Apps/Overleaf/Severe malaria renal failure/SupplementaryFigures/Height_Weight_imputation.pdf', plot = p)


p1=xx %>% ggplot(aes(x=Age, y = Height_cm, colour = STUDY))+geom_point()+geom_smooth(aes(group=NA))+theme(legend.position = 'none')
p2=xx %>% ggplot(aes(x=Age, y = `Mid-Upper Arm Circumference_cm`, colour = STUDY))+geom_point()+geom_smooth(aes(group=NA))+theme(legend.position = 'none')
p3=xx %>% ggplot(aes(x=Hb, y = Lactate, colour = STUDY))+geom_point()+geom_smooth(aes(group=NA))+theme(legend.position = 'none')
p4=xx %>% ggplot(aes(x=Age, y = Hb, colour = STUDY))+geom_point()+geom_smooth(aes(group=NA))+theme(legend.position = 'none')
gridExtra::grid.arrange(p1,p2,p3,p4)

xx %>% ggplot(aes(x=Potassium_mmol_L, y = log10_BUN, colour = STUDY))+
  geom_point()+geom_smooth(aes(group=NA))

# MUAC imputation does not work for adults!!!
# Lactate imputation has some serious problems (use of linear regression)

# Create pairwise plots
GGally::ggpairs(numeric_df %>% select(Age, `Mid-Upper Arm Circumference_cm`, Weight_kg, Height_cm, Hb),
                mapping = ggplot2::aes(color = as.factor(Age<15)))
GGally::ggpairs(numeric_df %>% select(Age, Lactate,Hb,`Base Excess_mmol_L`,Bicarbonate_mEq_L,log10_BUN,log10_Creatinine),
                mapping = ggplot2::aes(color = as.factor(Age<15)))






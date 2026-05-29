sm_data = readr::read_csv('Data/Combined_dataset.csv')
library(dplyr)
library(tidyr)
library(ggplot2)

summary_table <- sm_data %>%
  group_by(Continent, STUDY) %>%
  summarise(
    n_patients = n(),
    years_running = paste0(
      min(year_admission, na.rm = TRUE), 
      "–", 
      max(year_admission, na.rm = TRUE)
    ),
    first_year = min(year_admission, na.rm = TRUE),
    SM=round(100*mean(Malaria_Positive,na.rm=T)),
    median_age_iqr = paste0(
      round(median(Age, na.rm = TRUE), 1),
      " (",
      round(quantile(Age, 0.25, na.rm = TRUE), 1),
      "–",
      round(quantile(Age, 0.75, na.rm = TRUE), 1),
      ")"
    ),
    mean_mortality_rate = round(100*mean(Died_28D, na.rm = TRUE)),
    n_Creatinine = sum(!is.na(Creatinine_umol_L)),
    n_BUN = sum(!is.na(BUN)),
    n_Lactate = sum(!is.na(Lactate))
  ) %>%
  arrange(Continent, first_year, STUDY) %>%
  select(-first_year)
summary_table


lab_vars <- c(
  # Laboratory: haematology
  "Hb",
  "Leukocytes_10_9_L",
  "Platelets_10_9_L",
  "Eosinophils_Leukocytes_%",
  "Lymphocytes_Leukocytes_%",
  "Monocytes_Leukocytes_%",
  "Neutrophils_Leukocytes_%",
  "Reticulocytes_Erythrocytes_%",
  
  # Laboratory: glucose / lactate
  "Glucose_mmol_L",
  "Lactate",
  
  # Laboratory: renal / electrolytes
  "Creatinine_umol_L",
  "BUN",
  "Sodium_mmol_L",
  "Potassium_mmol_L",
  "Chloride_mmol_L",
  
  # Laboratory: liver
  "Bilirubin_umol_L",
  "Direct Bilirubin_umol_L",

  # Laboratory: inflammation
  "C Reactive Protein_mg_L",
  
  # Laboratory: acid-base / blood gas
  "pH_",
  "Bicarbonate_mEq_L",
  "Carbon Dioxide_mmol_L",
  "Partial Pressure Carbon Dioxide_mmHg",
  "Base Excess_mmol_L",
  "Anion Gap_mmol_L"
 
)

c(
  
  # Outcomes
  "Died_28D",
  "Time_to_death",
  # Study / site
  "STUDY",
  "SITEID",
  "COUNTRY",
  "Continent",
  "year_admission",
  
  # Demographics / anthropometry
  "SEX",
  "Age",
  "Age_category",
  "Weight_kg",
  "Height_cm",
  "Head Circumference_cm",
  "Mid-Upper Arm Circumference_cm",
  
  # Clinical: vital signs / bedside assessment
  "Temperature_C",
  "Respiratory Rate_breaths_min",
  "Heart Rate_beats_min",
  "Systolic Blood Pressure_mmHg",
  "Diastolic Blood Pressure_mmHg",
  "Oxygen Saturation_%",
  "Capillary Refill Time_sec",
  
  # Clinical: neurologic / severity features
  "BCS_tot",
  "Coma",
  "Prostration",
  "Seizure",
  "Respiratory distress",
  "Shock",
  "Anuria",
  "Hypoglycaemia",
  
  # Clinical: other signs / complications
  "Blood in urine",
  "Epilepsy",
  "Fever",
  "Jaundice",
  "Cyanosis",
  "Edema",
  "Sepsis",
  "Bleeding",
  "Dehydration",
  
  # Laboratory: malaria-specific
  "Malaria_Positive",
  "para_ul"
)

dat_plot = sm_data[sm_data$STUDY!='SMAC', lab_vars] 
prop_missing_order <- dat_plot %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "prop_missing"
  ) %>%
  arrange(prop_missing) %>%
  pull(variable)


df_plot <- dat_plot %>% 
  mutate(patient_id = row_number()) %>%
  mutate(across(-patient_id, is.na)) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = factor(variable, levels = prop_missing_order))

ggplot(df_plot, aes(x = variable, y = patient_id, fill = value)) +
  geom_raster() +
  scale_fill_manual(values = c("FALSE" = "grey90", "TRUE" = "firebrick")) +
  labs(x = NULL, y = "", fill = "Missing") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )+ theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


sm_data %>% filter(!is.na(Creatinine_umol_L), 
                   !is.na(Age),
                   !is.na(year_admission), STUDY=='KEMRI') %>%
  ggplot(aes(x=year_admission, y = Creatinine_umol_L, colour = Age_category))+
  geom_jitter()+geom_smooth(aes(group='NA'))+xlab('')+
  scale_y_log10()+theme_minimal()

sm_data %>% filter(!is.na(Hb), !is.na(Age)) %>%
  ggplot(aes(x=Age, y = Hb, colour = `Died within 28 days`))+geom_point(alpha=0.2)+
  scale_x_sqrt() + facet_wrap(~STUDY) + geom_smooth(aes(group=NA))+
  theme_minimal()


sm_data %>% filter(!is.na(Hb), !is.na(Lactate), Hb<16) %>%
  ggplot(aes(x=Hb, y = Lactate, colour = `Died within 28 days`))+geom_point(alpha=0.2)+
  facet_wrap(~STUDY) + geom_smooth(aes(group=NA))+
  theme_minimal() + geom_vline(xintercept = 5)

sm_data = sm_data %>% filter(Malaria_Positive) %>% mutate(BE=`Base Excess_mmol_L`)
dim(sm_data)


sm_data %>% filter(!is.na(Bicarbonate_mEq_L), !is.na(`Base Excess_mmol_L`)) %>%
  ggplot(aes(x=Bicarbonate_mEq_L, y = `Base Excess_mmol_L`, colour = STUDY))+geom_jitter()+
  theme_minimal()+xlab('Bicarbonate (mEq/L)')+
  ylab('Base Excess (mmol/L)')+
  geom_hline(yintercept = -8)+
  geom_vline(xintercept = 15)+geom_smooth(aes(group=NA),se = F)

mod_BE = mgcv::gam(BE ~ s(Bicarbonate_mEq_L), data = sm_data)
sm_data$BE_imputed = predict(mod_BE, newdata = sm_data)
mod_BC = mgcv::gam(Bicarbonate_mEq_L ~ s(BE), data = sm_data)
sm_data$BC_imputed = predict(mod_BC, newdata = sm_data)

sm_data$`Base Excess_mmol_L (imputed)` = ifelse(is.na(sm_data$`Base Excess_mmol_L`), sm_data$BE_imputed, sm_data$`Base Excess_mmol_L`)

sm_data$`Bicarbonate_mEq_L (imputed)` = ifelse(is.na(sm_data$Bicarbonate_mEq_L), sm_data$BC_imputed, sm_data$Bicarbonate_mEq_L)


sm_data %>% filter(!is.na(`Base Excess_mmol_L (imputed)`),
                   !is.na(Lactate), `Base Excess_mmol_L (imputed)`<10,
                   !is.na(Hb)) %>%
  mutate(`Anemia (Hb<5g/dL)` = (Hb<=5)) %>%
  ggplot(aes(x=`Base Excess_mmol_L (imputed)`, y = Lactate, colour = `Anemia (Hb<5g/dL)`))+
  geom_point(alpha=0.5)+theme_minimal()+
  xlab('Base Excess (mmol/L)')



mod_Lactate_children = mgcv::gam(Died ~ s(Lactate))

sm_data = read_csv('Data/Renal_Analysis_Data.csv')
sm_data = sm_data %>% arrange(STUDY, COUNTRY, SITEID, RFSTDTC) %>%
  mutate(ID = 1:n())

apply(sm_data, 2, function(x) round(100*mean(is.na(x))))

key_cols=c("ID","STUDY","COUNTRY","SITEID","SEX","Age","Died",
  "Heart Rate_beats_min","Height_cm","Mid-Upper Arm Circumference_cm",
  "Respiratory Rate_breaths_min",
  "Systolic Blood Pressure_mmHg","Temperature_C",
  "Weight_kg","Oxygen Saturation_%","Diastolic Blood Pressure_mmHg",
  "Coma_Final",
  "Respiratory distress","Shock","Hypoglycaemia","Seizure","Jaundice",
  "Blood in urine","Anemia","Hyperparasitaemia",
  "GCS_tot","BCS_tot","para_ul","Glucose_mmol_L","Hemoglobin_g_L",
  "Hematocrit_%", "Lactic Acid_mmol_L","Base Excess_mmol_L",
  "Creatinine_umol_L","BUN", "Urea Nitrogen_mmol_L")


# Convert to long format
df_long <- sm_data[,key_cols] %>%
  mutate(across(-c(ID,STUDY,COUNTRY,SITEID), as.character)) %>%
  pivot_longer(-c(ID,STUDY,COUNTRY,SITEID), names_to = "Variable", values_to = "Value") %>%
  mutate(Missing = ifelse(is.na(Value), "Missing", "Not Missing")) %>%
  arrange(ID)

# Plot missing data matrix
custom_y_pos = which(!duplicated(sm_data$STUDY)) + diff(c(which(!duplicated(sm_data$STUDY)), nrow(sm_data)))/2
custom_y_vals = unique(sm_data$STUDY)
p_missing=ggplot(df_long, aes(x = Variable, y = ID, fill = Missing)) +
  geom_tile() +
  scale_fill_manual(values = c("Missing" = "red", "Not Missing" = "lightgrey")) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks=custom_y_pos,labels=custom_y_vals)
ggsave(filename = 'missing_data.pdf',plot = p_missing,width = 12,height = 12)

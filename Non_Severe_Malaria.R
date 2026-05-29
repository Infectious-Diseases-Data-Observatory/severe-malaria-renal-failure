library(tidyverse)
source('setup_parameters.R')
# load(file = 'RData/All_data_Severe_malaria.RData')
load('RData/Analysis_dataset.RData')

my_function = function(x,dp=1) c(round(100*mean(x),dp), sum(x), length(x))
# sm_data_analysis = sm_data_analysis %>% 
#   filter(!is.na(PfHRP2_ng_ml)|!is.na(Platelets_10_9_L), !is.na(para_ul))
# table(sm_data_analysis$STUDY)


### Data from children that is complete
xx_full = sm_data_analysis %>% filter(!is.na(Age), 
                                      !is.na(Prostration), 
                                      !is.na(Coma), 
                                      !is.na(`Respiratory distress`),
                                      !is.na(Hb),
                                      !is.na(Glucose_mmol_L),
                                      !is.na(Jaundice),
                                      Age>15) %>%
  mutate(
    Hyperparasitaemia = ifelse(!is.na(para_ul), para_ul>100000, Hyperparasitaemia),
    Severe_Anaemia=Hb <= 5,
    Hypoglycaemia = Glucose_mmol_L<3,
    Category_SM = case_when(
      !Prostration & Coma=='Yes' & !`Respiratory distress` & !Severe_Anaemia & !Hypoglycaemia & !Jaundice ~ 'Coma only',
      Prostration & Coma=='No' & !`Respiratory distress` & !Severe_Anaemia & !Hypoglycaemia & !Jaundice ~ 'Prostrate only',
      !Prostration & Coma=='No' & `Respiratory distress` & !Severe_Anaemia & !Hypoglycaemia & !Jaundice ~ 'Respiratory distress only',
      !Prostration & Coma=='No' & !`Respiratory distress` & Severe_Anaemia & !Hypoglycaemia & !Jaundice ~ 'Severe anaemia only',
      !Prostration & Coma=='No' & !`Respiratory distress` & !Severe_Anaemia & Hypoglycaemia & !Jaundice~ 'Hypoglycaemia only',
      !Prostration & Coma=='No' & !`Respiratory distress` & !Severe_Anaemia & !Hypoglycaemia & Jaundice~ 'Jaundice only',
      T ~ 'Multiple categories'
    ))
table(xx_full$Category_SM,xx_full$STUDY)
aggregate(Died_28D ~ Category_SM, data = xx_full, FUN = my_function)

                                      

### Data from children that is complete
xx_full = sm_data_analysis %>% filter(!is.na(Age), 
                                      !is.na(Prostration), 
                                      !is.na(Coma), 
                                      !is.na(`Respiratory distress`),
                                      !is.na(Hb),
                                      !is.na(Glucose_mmol_L),
                                      !is.na(Jaundice),
                                      # !is.na(Shock),
                                      # !is.na(Seizure),
                                      Age<15) %>%
  mutate(
    Hyperparasitaemia = ifelse(!is.na(para_ul), para_ul>100000, Hyperparasitaemia),
    Severe_Anaemia=Hb <= 5,
    Hypoglycaemia = Glucose_mmol_L<3,
    Category_SM = case_when(
    !Prostration & Coma=='Yes' & !`Respiratory distress` & !Severe_Anaemia & !Hypoglycaemia & !Jaundice ~ 'Coma only',
    Prostration & Coma=='No' & !`Respiratory distress` & !Severe_Anaemia & !Hypoglycaemia & !Jaundice ~ 'Prostrate only',
    !Prostration & Coma=='No' & `Respiratory distress` & !Severe_Anaemia & !Hypoglycaemia & !Jaundice ~ 'Respiratory distress only',
    !Prostration & Coma=='No' & !`Respiratory distress` & Severe_Anaemia & !Hypoglycaemia & !Jaundice ~ 'Severe anaemia only',
    !Prostration & Coma=='No' & !`Respiratory distress` & !Severe_Anaemia & Hypoglycaemia & !Jaundice~ 'Hypoglycaemia only',
    !Prostration & Coma=='No' & !`Respiratory distress` & !Severe_Anaemia & !Hypoglycaemia & Jaundice~ 'Jaundice only',
    T ~ 'Multiple categories'
  ))

table(xx_full$Category_SM,xx_full$STUDY)
aggregate(Died_28D ~ Category_SM, data = xx_full, FUN = my_function)
table(xx_full$Category_SM, xx_full$Hyperparasitaemia)

sa = xx_full %>% filter(Category_SM=='Severe anaemia only')
table(sa$Died_28D, sa$Hyperparasitaemia,useNA = 'ifany')

table(xx_full$STUDY)
dim(xx_full)
mod=lme4::glmer(Died_28D ~ Age_category + Prostration + Coma + Severe_Anaemia + 
                  Hypoglycaemia + `Respiratory distress` + Jaundice + (1|STUDY/SITEID), 
                family = 'binomial', data = xx_full)
summary(mod)
library(broom.mixed)
library(dplyr)

coef_df <- broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE) %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  )
coef_df
coef_df <- coef_df %>% filter(term!='(Intercept)')%>% 
  mutate(term = recode(term,
                       'Age_category(2,5]'='Age: (2,5]',
                       "Age_category(5,15]"='Age: (5,15]',
                       "ProstrationTRUE" = "Prostration",
                       "ComaYes" = "Coma",
                       "Severe_AnaemiaTRUE" = "Severe Anaemia",
                       "JaundiceTRUE" = "Jaundice",
                       "HypoglycaemiaTRUE" = "Hypoglycaemia",
                       "Glucose_mmol_L < 3TRUE" = "Glucose < 3 mmol/L",
                       "`Respiratory distress`TRUE" = "Respiratory distress"
  ))
ggplot(coef_df, aes(x = OR, y = term)) +
   scale_x_log10() +
  geom_point(size = 3) +
   geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), height = 0.15) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
  ylab("") + xlab('Odds ratio for death')+
  theme_bw(base_size = 14)
######### PROSTRATION ############ß
prostration = sm_data_analysis %>% filter(!is.na(Prostration), Age<12)
table(prostration$STUDY)
quantile(prostration$Age)
mod=lme4::glmer(Died_28D ~ Age + Prostration + Coma + (1|STUDY), family = 'binomial', data = prostration)
summary(mod)
aggregate(Died_28D ~ Prostration + Coma, data = prostration, my_function)

########## BCS ###################
bcs = sm_data_analysis %>% filter(!is.na(BCS_tot))
aggregate(Died_28D ~ BCS_tot, data = bcs, my_function)


########## SEIZURES ###################
convulsions = sm_data_analysis %>% filter(!is.na(Seizure), Age<15)
table(convulsions$STUDY)/nrow(convulsions)
mod=lme4::glmer(Died_28D ~ Age + Seizure + Coma + (1|STUDY), family = 'binomial', data = convulsions)
summary(mod)
aggregate(Died_28D ~ Seizure + Coma, data = convulsions, my_function)

          
########## HB ###################
hb = sm_data_analysis %>% filter(!is.na(Hb), Age<15) %>% mutate(STUDY=as.factor(STUDY))
round(100*table(hb$STUDY)/nrow(hb))
hb$hb_cat = cut(hb$Hb, breaks = c(0,3,4,5,6,7,10,20))
table(hb$hb_cat)
aggregate(Died_28D ~ hb_cat, data = hb, my_function)

mod=lme4::glmer(Died_28D ~ Age + (Hb<5)+(Hb<4)+(Hb<3) + (1|STUDY), family = 'binomial', data = hb)
summary(mod)
mod=mgcv::gam(Died_28D ~ s(Age) + s(Hb) + s(STUDY, bs='re'), family = 'binomial', data = hb %>% filter(Hb<10, Coma=='No'))
mod=mgcv::gam(Died_28D ~ s(Age) + s(Hb) + Coma + s(STUDY, bs='re'), family = 'binomial', data = hb %>% filter(Hb<10))
summary(mod)
xx = tidygam::predict_gam(mod, series = 'Hb',tran_fun = boot::inv.logit) %>% filter(STUDY == 'AQUAMAT')
xx %>% ggplot(aes(x=Hb, y=100*Died_28D))+geom_line()+xlab('Haemoglobin (g/dL)')+
  ylab('Predicted mortality (%)')+theme_bw()+ylim(0,15)+xlim(2,10)+
  geom_ribbon(aes(ymin = 100*lower_ci, ymax = 100*upper_ci), alpha = 0.2) 


########## HB ADULTS ###################
hb = sm_data_analysis %>% filter(!is.na(Hb), Age>15) %>% mutate(STUDY=as.factor(STUDY))
round(100*table(hb$STUDY)/nrow(hb))
hb$hb_cat = cut(hb$Hb, breaks = c(0,3,4,5,6,7,10,20))
table(hb$hb_cat)
aggregate(Died_28D ~ hb_cat, data = hb, my_function)


########## GLUCOSE ###################
glucose = sm_data_analysis %>% filter(!is.na(Glucose_mmol_L),  Glucose_mmol_L<10, Age<15) %>% mutate(STUDY=as.factor(STUDY))
round(100*table(glucose$STUDY)/nrow(glucose))
mod=mgcv::gam(Died_28D ~ s(Age) + s(Glucose_mmol_L, k=10) + Coma + s(STUDY, bs='re'), 
              family = 'binomial', data = glucose %>% filter(Glucose_mmol_L<10))
summary(mod)
# plot(mod)

xx = tidygam::predict_gam(mod, series = 'Glucose_mmol_L',tran_fun = boot::inv.logit) %>% filter(STUDY == 'AQUAMAT', Coma=='No')
xx %>% ggplot(aes(x=Glucose_mmol_L, y=100*Died_28D))+geom_line()+xlab('Glucose (mmol/L)')+
  ylab('Predicted mortality (%)')+theme_bw()+ylim(0,15)+xlim(0,10)+
  geom_ribbon(aes(ymin = 100*lower_ci, ymax = 100*upper_ci), alpha = 0.2) 



######### Potassium ##########
sm_potassium = sm_data_analysis %>%
  filter(!is.na(Potassium_mmol_L), Potassium_mmol_L<10) %>%
  mutate(Potassium_levels = cut(Potassium_mmol_L,breaks=c(0,3,7.5, 10)))
dim(sm_potassium)
table(sm_potassium$STUDY)

sm_potassium %>% 
  ggplot(aes(x=Potassium_mmol_L, y = Sodium_mmol_L))+geom_point(alpha=0.2)+
  scale_x_sqrt()+ylab('Sodium (mmol/L)')+
  theme_bw(base_size = 20)+
  geom_smooth()+
  geom_vline(xintercept = c(2,5,15), linetype='dashed')

p0=sm_potassium %>% 
  ggplot(aes(x=Age, y = Potassium_mmol_L))+geom_point(alpha=0.2)+
  scale_x_sqrt()+ylab('Potassium (mmol/L)')+
  theme_bw(base_size = 20)+geom_hline(yintercept = c(3.5, 7.5),color='red')+
  geom_smooth()+
  geom_vline(xintercept = c(2,5,15), linetype='dashed')

table(sm_potassium$Age_category, sm_potassium$Potassium_levels)

p1=sm_potassium %>% 
  ggplot(aes(x=Bicarbonate_mEq_L, y = Potassium_mmol_L))+geom_point(alpha=0.2)+
  ylab('Potassium (mmol/L)')+xlab('Bicarbonate (mEq/L)')+
  theme_bw(base_size = 20)+geom_hline(yintercept = c(3.5, 7.5),color='red')+
  geom_smooth()

p2=sm_potassium %>% 
  ggplot(aes(x=Lactate, y = Potassium_mmol_L))+geom_point(alpha=0.2)+
  ylab('Potassium (mmol/L)')+xlab('Lactate (mmol/L)')+
  theme_bw(base_size = 20)+geom_hline(yintercept = c(3.5, 7.5),color='red')+
  geom_smooth()

p3=sm_potassium %>% 
  ggplot(aes(x=BUN, y = Potassium_mmol_L))+geom_point(alpha=0.2)+
  ylab('Potassium (mmol/L)')+xlab('BUN (mmol/L)')+
  scale_x_log10(breaks=breaks, minor_breaks=minor_breaks)+
  theme_bw(base_size = 20)+geom_hline(yintercept = c(3.5, 7.5),color='red')+
  geom_smooth()

sm_potassium %>% 
  ggplot(aes(x=Creatinine_umol_L, y = Potassium_mmol_L))+geom_point(alpha=0.2)+
  ylab('Potassium (mmol/L)')+scale_x_log10()+
  theme_bw(base_size = 20)+geom_hline(yintercept = c(3.5, 7.5),color='red')+
  geom_smooth()
sm_potassium %>% 
  ggplot(aes(x=PfHRP2_ng_ml+1, y = Potassium_mmol_L))+geom_point(alpha=0.2)+
  ylab('Potassium (mmol/L)')+scale_x_log10()+
  theme_bw(base_size = 20)+geom_hline(yintercept = c(3.5, 7.5),color='red')+
  geom_smooth()
sm_potassium %>% 
  ggplot(aes(x=para_ul+100, y = Potassium_mmol_L))+geom_point(alpha=0.2)+
  ylab('Potassium (mmol/L)')+scale_x_log10()+
  theme_bw(base_size = 20)+geom_hline(yintercept = c(3.5, 7.5),color='red')+
  geom_smooth()

mod_potassium = mgcv::gam((Potassium_mmol_L>7.5) ~ s(Age) + s(Bicarbonate_mEq_L) + as.numeric(Lactate<5)+ log10(BUN) +SITE_STUDY,
                          data = sm_potassium)
summary(mod_potassium)
grid.arrange(p0,p1,p2,p3)

############ TRUE SM? #######
sm_data_analysis = sm_data_analysis %>%
  filter(!is.na(PfHRP2_ng_ml)|!is.na(Platelets_10_9_L), !is.na(para_ul)) %>%
  mutate(Classification = case_when(
    PfHRP2_ng_ml>1000  ~ 'True Severe Malaria',
    PfHRP2_ng_ml<500 ~ 'Not Severe Malaria',
    Platelets_10_9_L<150  ~ 'True Severe Malaria',
    Platelets_10_9_L>200 ~ 'Not Severe Malaria',
    para_ul>10^5 ~ 'True Severe Malaria',
    para_ul<100 ~ 'Not Severe Malaria',
    T ~ 'Uncertain'
  )) %>% filter(Classification != 'Uncertain') 
table(sm_data_analysis$Classification, sm_data_analysis$STUDY)
round(100*table(sm_data_analysis$Classification)/nrow(sm_data_analysis))

sm_data_analysis %>% ggplot(aes(x=BUN, y = PfHRP2_ng_ml+1, colour = as.character(para_ul<1000)))+
  geom_point()+scale_x_log10()+scale_y_log10()+
  geom_smooth(se=F)

ind = sm_data_analysis$BUN>10 & sm_data_analysis$PfHRP2_ng_ml>1000
table(sm_data_analysis$para_ul[ind]<1000, sm_data_analysis$STUDY[ind])

sm_bun = sm_data_analysis %>% filter(!is.na(BUN))
sm_bun %>% 
  ggplot(aes(x=Age, y = BUN, colour = Classification))+geom_point(alpha=0.3)+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks,limits=c(1,100))+
  scale_x_sqrt()+ylab('Blood Urea Nitrogen (mmol/L)')+
  annotation_logticks(sides='l')+
  theme_bw(base_size = 20)+geom_hline(yintercept = 30)
aggregate( BUN > 30 ~ Classification+(Age<15), data = sm_data_analysis,FUN = my_function)


sm_potassium = sm_data_analysis %>% filter(!is.na(Potassium_mmol_L), !is.na(BUN))
dim(sm_potassium)
table(sm_potassium$Age<15)
table(sm_potassium$STUDY)
sm_potassium %>% 
  ggplot(aes(x=Age, y = Potassium_mmol_L, colour = Classification))+geom_point(alpha=0.2)+
  scale_x_sqrt()+ylab('Potassium (mmol/L)')+
  theme_bw(base_size = 20)+geom_hline(yintercept = 7.5)+
  geom_smooth()

sm_potassium %>% 
  ggplot(aes(x=BUN, y = Potassium_mmol_L, colour = Classification))+
  geom_point(alpha=0.3)+
  scale_x_log10(breaks=breaks, minor_breaks=minor_breaks)+ylab('Potassium (mmol/L)')+
  theme_bw(base_size = 20)+geom_hline(yintercept = 7.5)+geom_smooth()

sm_potassium %>% 
  ggplot(aes(x=BUN, y = Potassium_mmol_L, colour = STUDY))+
  geom_point(alpha=0.5)+
  scale_x_log10()+ylab('Potassium (mmol/L)')+
  xlab('Blood Urea Nitrogen (mmol/L)')+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks,limits=c(1,100))+
  theme_bw(base_size = 20)+geom_hline(yintercept = 7.5)+geom_smooth(se = F)

aggregate( Potassium_mmol_L > 7.5 ~ Classification+(Age<15), data = sm_data_analysis,FUN = my_function)




sm_data_analysis %>% 
  filter(SM_category != 'Uncertain') %>%
  ggplot(aes(x=Age, y = Creatinine_umol_L, colour = SM_category))+scale_y_log10()+geom_point()
sm_data_analysis %>% 
  filter(SM_category != 'Uncertain') %>%
  ggplot(aes(x=Age, y = Creatinine_umol_L, colour = STUDY))+scale_y_log10()+geom_point()

table(Creat_100 = sm_data_analysis$Creatinine_umol_L > 500, sm_data_analysis$SM_category)
aggregate( Creatinine_umol_L > 100 ~ SM_category, data = sm_data_analysis,FUN = mean)

s

sm_data_analysis %>% 
  filter(SM_category != 'Uncertain') %>%
  ggplot(aes(x=Age, y = Potassium_mmol_L, colour = SM_category))+geom_point()


sm_data_analysis %>% 
  filter(SM_category != 'Uncertain') %>%
  ggplot(aes(x=Age, y = BUN, colour = STUDY))+scale_y_log10()+geom_point()

table(Creat_100 = sm_data_analysis$BUN > 20, sm_data_analysis$SM_category)

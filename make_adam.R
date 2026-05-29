## This makes an analysis dataset for the severe malaria renal failure analysis

library(conflicted)
library(tidyverse)
library(tibble)
library(dplyr)
library(kableExtra)
library(readr)
library(RColorBrewer)

rm(list=ls())

conflicted::conflicts_prefer(dplyr::filter)
col_ids = c("STUDYID","USUBJID")

## Dataset 1: baseline parameters (clinical and lab) and outcome (mortality) as binary and time to death
f_path = function(domain, extra=' 2025-03-19',folder='Data/DATA 2025-03-21/'){
  paste0(folder,domain,extra,'.csv')
}

# get study names from TS domain
ts_SM = read_csv(file = f_path('TS'),na = "\\N")
unique(ts_SM$STUDYID)
ts_SM %>% filter(TSPARMCD=='TITLE') %>% select(STUDYID, TSVAL)

# get demographics from DM domain - includes time of death for some studies
dm_SM = read_csv(f_path('DM'),na = "\\N")
dm_SM = dm_SM %>%
  mutate(Age = case_when(
    AGEU=='MONTHS' ~ AGE,
    AGEU=='YEARS' ~ AGE*12),
    Died = as.numeric(DTHFL=='Y'),
    Time_to_death =
      case_when(
        !is.na(DTHHR)&DTHFL=='Y'~DTHHR,
        !is.na(DTHDY)&DTHFL=='Y'~DTHDY*24,
        T~NA
      ),
    Time_to_death = ifelse(STUDYID=='UUJKO', NA, Time_to_death)) %>%
  select(STUDYID, USUBJID,RFSTDTC,SITEID,ARM,COUNTRY,SEX,Age,Died,Time_to_death)

any(duplicated(dm_SM$USUBJID))
table(dm_SM$STUDYID)
table(dm_SM$COUNTRY)

## Get mortality outcomes from DS domain
ds_SM = read_csv(f_path(domain = 'DS'),na = "\\N")

ds_SM = ds_SM%>%
  arrange(STUDYID,USUBJID) %>%
  group_by(USUBJID)%>%
  filter(DSDECOD %in% c('DEATH','DIED')) %>%
  mutate(n_res=n(),
         # Died = any(DSDECOD=='DEATH'),
         Time_to_death =
           case_when(
             !is.na(DSSTHR) ~ DSSTHR,
             !is.na(DSCDSTHR) ~ DSCDSTHR,
             !is.na(DSHR) ~ DSHR,
             !is.na(DSDY) ~ DSDY*24,
             T ~ NA)) %>%
  distinct(USUBJID, .keep_all = T) %>% select(STUDYID,USUBJID, Time_to_death)
any(duplicated(ds_SM$USUBJID))


# tv_SM = read_csv('Data/TV 2024-11-08.csv',na = "\\N")

vs_SM = read_csv(f_path('VS'),na = "\\N")
vs_SM$VSTEST[vs_SM$VSTEST=='Pulse Rate']='Heart Rate'
vs_SM$VSTEST[vs_SM$VSTEST=='Body Length']='Height'

table(vs_SM$STUDYID, vs_SM$EPOCH, useNA = 'ifany')
baseline_vs_SM = vs_SM %>% filter(EPOCH %in% c('BASELINE','SCREENING')) %>%
  filter(VSSTRESN != '') %>%
  arrange(STUDYID, USUBJID, VSTEST) %>%
  group_by(USUBJID, VSTEST) %>% mutate(n=length(USUBJID))
max(baseline_vs_SM$n)
baseline_vs_SM_wide = baseline_vs_SM %>%
  pivot_wider(id_cols = c(USUBJID, STUDYID),
              names_from = c(VSTEST, VSSTRESU), values_from = VSSTRESN, values_fn = mean)


sa_SM = read_csv(f_path('SA'),na = "\\N")
sa_SM$EPOCH[sa_SM$STUDYID=='MOLFZ']='BASELINE'
sa_SM$EPOCH[which(sa_SM$STUDYID=='JTGNG' & sa_SM$SADECOD=='Coma')]='BASELINE'
sa_SM$EPOCH[which(sa_SM$STUDYID=='JTGNG' & sa_SM$SATERM=='Cerebral malaria')]='BASELINE'
sa_SM$SADECOD[which(sa_SM$STUDYID=='JTGNG' & sa_SM$SATERM=='Cerebral malaria')]='Coma'

remove_cols = which(apply(sa_SM, 2, function(x) mean(is.na(x)))==1)
sa_SM = sa_SM[, -remove_cols]
sa_SM = sa_SM %>% filter(EPOCH %in% c('BASELINE','SCREENING')) %>%
  mutate(SADECOD = ifelse(SADECOD=='Cerebral malaria', 'Coma', SADECOD),
         SADECOD = ifelse(SADECOD=='Urine looks dark', 'Blood in urine', SADECOD))

table(sa_SM$SADECOD)
sort(table(sa_SM$SATERM[sa_SM$SADECOD=='']))
sa_SM$SADECOD[grep(pattern = 'prostra',x = sa_SM$SATERM,ignore.case = T)]='Prostration'

sa_SM$SADECOD[grep(pattern = 'sickle cell',x = sa_SM$SATERM,ignore.case = T)]='Sickling disorder due to hemoglobin S'
sa_SM$SADECOD[grep(pattern = 'black water|blackwater|haemoglobinurea',x = sa_SM$SATERM,ignore.case = T)]='Blood in urine'
sa_SM$SADECOD[grep(pattern = 'COCA-COLA',x = sa_SM$SATERM,ignore.case = T)]='Blood in urine'
sa_SM$SADECOD[grep(pattern = 'DARK URINE SYNDROME|HAEMOLYTIC ANAEMIA|HEAMOLYTIC ANAEMIA',x = sa_SM$SATERM,ignore.case = T)]='Blood in urine'
sa_SM$SADECOD[grep(pattern = 'hypoglyc',x = sa_SM$SATERM,ignore.case = T)]='Hypoglycaemia'
sa_SM$SADECOD[grep(pattern = 'hyperpara',x = sa_SM$SATERM,ignore.case = T)]='Hyperparasitaemia'
sa_SM$SADECOD[grep(pattern = 'acido',x = sa_SM$SATERM,ignore.case = T)]='Respiratory distress'
sa_SM$SADECOD[grep(pattern = 'kussmaul',x = sa_SM$SATERM,ignore.case = T)]='Respiratory distress'
sa_SM$SADECOD[grep(pattern = 'respiratory distress',x = sa_SM$SATERM,ignore.case = T)]='Respiratory distress'
sa_SM$SADECOD[sa_SM$SADECOD=='Difficulty breathing']='Respiratory distress'
sa_SM$SADECOD[grep(pattern = 'anaemia|anemia',x = sa_SM$SATERM,ignore.case = T)]='Anemia'

select_cols = c("Respiratory distress","Fever","Vomiting","Diarrhea","Blood in urine",
                "Jaundice","Seizure","Epilepsy","Coma","Anuria","Anemia","Cyanosis",
                "Edema","Shock","Human immunodeficiency virus infection","Meningitis",
                "Prostration","Sepsis","Hypoglycaemia","Bleeding",
                "Sickling disorder due to hemoglobin S","Dehydration","Splenomegaly",
                "Hyperparasitaemia")
sa_SM = sa_SM %>% filter(#SAPRESP=='Y',
  SADECOD %in% select_cols)
baseline_sa_SM_wide = sa_SM %>%
  arrange(STUDYID, USUBJID, SADECOD) %>%
  group_by(USUBJID, SADECOD) %>% mutate(n=length(USUBJID)) %>%
  pivot_wider(id_cols = c(USUBJID,STUDYID),
              names_from = c(SADECOD), values_from = SAOCCUR,
              values_fn = function(x) any(x=='Y'))


rs_SM = read_csv(f_path('RS'),na = "\\N")
remove_cols = which(apply(rs_SM, 2, function(x) mean(is.na(x)))==1)
rs_SM$RSTEST = tolower(rs_SM$RSTEST)
unique(rs_SM$RSTEST)

rs_SM$EPOCH[which(rs_SM$STUDYID=='FSOUE' & rs_SM$RSTEST=='bcs01-total score')]='BASELINE'
bcs_cols = c('bcs01-best eye response','bcs01-motor response','bcs01-verbal response')
gcs_cols = c('gcs01-best eye response','gcs01-motor response','gcs01-verbal response')

rs_SM = rs_SM %>% filter(EPOCH %in% c('BASELINE','SCREENING')) %>%
  group_by(USUBJID,RSTEST) %>%
  mutate(n=n()) %>%
  pivot_wider(id_cols = c(USUBJID,STUDYID),
              names_from = c(RSTEST), values_from = RSSTRESN,values_fn = mean)
rs_SM$BCS_total = rowSums(rs_SM[, bcs_cols])
rs_SM$GCS_total = rowSums(rs_SM[, gcs_cols])
any(duplicated(rs_SM$USUBJID))

# ind = !is.na(rs_SM$BCS_total) & !is.na(rs_SM$`bcs01-total score`)
# all(rs_SM$BCS_total[ind]==rs_SM$`bcs01-total score`[ind])
# ind = !is.na(rs_SM$GCS_total) & !is.na(rs_SM$`gcs01-total score`)
# sum(rs_SM$GCS_total[ind]!=rs_SM$`gcs01-total score`[ind])

rs_SM = rs_SM %>%
  mutate(GCS_tot = ifelse(is.na(`gcs01-total score`), GCS_total, `gcs01-total score`),
         BCS_tot = ifelse(is.na(`bcs01-total score`), BCS_total, `bcs01-total score`),
         coma_RS = case_when(
           !is.na(GCS_tot) & GCS_tot<=10 ~ 1,
           !is.na(GCS_tot) & GCS_tot>10 ~ 0,
           !is.na(BCS_tot) & BCS_tot<=2 ~ 1,
           !is.na(BCS_tot) & BCS_tot>2 ~ 0 )) %>%
  select(USUBJID,STUDYID, GCS_tot, BCS_tot, coma_RS)

table(rs_SM$coma_RS, rs_SM$STUDYID, useNA = 'ifany')

clinical_baseline = merge(baseline_sa_SM_wide, rs_SM, by = col_ids, all = T)
table(coma=clinical_baseline$Coma, coma_RS=clinical_baseline$coma_RS, useNA = 'ifany')

clinical_baseline = clinical_baseline %>%
  mutate(Coma_Final = case_when(
    !is.na(coma_RS) & coma_RS==1 ~ 1,
    !is.na(Coma) & Coma ~ 1,
    !is.na(coma_RS) & coma_RS==0 ~ 0,
    !is.na(Coma) & !Coma ~ 0,
    T ~ NA
  ))
table(clinical_baseline$Coma_Final, clinical_baseline$STUDYID,useNA = 'ifany')
colnames(clinical_baseline)
clinical_baseline = clinical_baseline %>%
  mutate(Coma=Coma_Final) %>%
  select(-Coma_Final, -coma_RS)

## Get lab data
lb_SM = read_csv(f_path('LB'),na = "\\N")
remove_cols = which(apply(lb_SM, 2, function(x) mean(is.na(x)))==1)
lb_SM = lb_SM[, -remove_cols]
table(lb_SM$STUDYID, lb_SM$EPOCH, useNA = 'ifany')
lb_SM$EPOCH[lb_SM$LBTEST=='Creatinine' & lb_SM$STUDYID=='AWPDN']='BASELINE'
# View(lb_SM %>% filter(EPOCH==''))
# lb_SM$LBSTRESN[lb_SM$STUDYID=='NLSSA' & lb_SM$LBTEST=='Creatinine']= NA

## manual corrections
ind = which(lb_SM$LBTEST=='Leukocytes'&lb_SM$STUDYID=='ITYCK'&lb_SM$LBSTRESU=="10^6/L")
lb_SM$LBSTRESN[ind] = as.numeric(lb_SM$LBSTRESN[ind])/10^3
lb_SM$LBSTRESU[ind] = "10^9/L"

ind = which(lb_SM$LBTEST=='Potassium'&lb_SM$STUDYID=='ZQEVB'&lb_SM$LBSTRESN>999)
lb_SM$LBSTRESN[ind]=NA

ind = which(lb_SM$LBTEST=='Urea'&lb_SM$STUDYID=='ITYCK')
lb_SM$LBTEST[ind] = 'Urea Nitrogen'
lb_SM$LBSTRESU[ind] = 'mmol/L'

ind = which(lb_SM$LBTEST=='Bicarbonate'&lb_SM$LBSTRESU=='mmol/L')
lb_SM$LBSTRESU[ind]='mEq/L'

## manual corrections
ids_blantyre = unique(dm_SM$USUBJID[dm_SM$SITEID=='Blantyre' & dm_SM$STUDYID=='ZQEVB'])
ind = which(lb_SM$STUDYID=='ZQEVB' & lb_SM$LBTEST=='Urea Nitrogen' &
              lb_SM$LBORRESU=='mg/dL' & lb_SM$USUBJID %in% ids_blantyre)
ind2 = which(lb_SM$STUDYID=='ZQEVB' & lb_SM$LBTEST=='Urea Nitrogen' &
               lb_SM$LBORRESU=='mg/dL' & !lb_SM$USUBJID %in% ids_blantyre)
median(as.numeric(lb_SM$LBORRES[ind]))
median(as.numeric(lb_SM$LBORRES[ind2]))
median(as.numeric(lb_SM$LBSTRESN[ind]))
median(as.numeric(lb_SM$LBSTRESN[ind2]))

lb_SM$LBSTRESN[ind] = as.numeric(lb_SM$LBORRES[ind])/0.0555
lb_SM$LBSTRESN[ind2] = as.numeric(lb_SM$LBORRES[ind2])
lb_SM$LBSTRESN = as.numeric(lb_SM$LBSTRESN)

ind = which(lb_SM$STUDYID=='ITYCK' & lb_SM$LBTEST=='Glucose')
lb_SM$LBSTRESN[ind] = lb_SM$LBSTRESN[ind]/18.0182

## Manual correction of units issue!!!
lb_SM %>% filter(LBTEST=='Urea Nitrogen') %>%
  mutate(LBORRES = as.numeric(LBORRES)) %>%
  ggplot(aes(x=LBORRES, y = LBSTRESN, colour = LBORRESU))+
  geom_point()
lb_SM %>% filter(LBTEST=='Urea Nitrogen') %>%
  mutate(LBORRES = as.numeric(LBORRES)) %>%
  ggplot(aes(x=LBORRES, y = LBSTRESN, colour = as.numeric(STUDYID=='NLSSA')))+
  geom_point()

lb_SM = lb_SM %>% filter(!(as.numeric(LBORRES)>10^6 & LBTESTCD=='CREAT'))

lb_SM %>% filter(LBTEST=='Creatinine') %>%
  mutate(LBORRES = as.numeric(LBORRES)) %>%
  ggplot(aes(x=log10(LBORRES), y = log10(LBSTRESN), colour = LBORRESU))+
  geom_point()

table(lb_SM$LBTEST,lb_SM$LBSPEC)
# remove baseline WBC from CSF or urine
lb_SM=lb_SM%>% filter(!(LBSPEC%in%c('CEREBROSPINAL FLUID','URINE') & LBTEST=='Leukocytes'))

## Manual edits
lb_SM$LBSTRESU[lb_SM$LBTEST=='Creatinine' & lb_SM$LBSTRESU=='mmol/L']='umol/L'
lb_SM$LBSTRESN[lb_SM$LBTEST=='Creatinine' & lb_SM$LBSTRESN > 14*88]=NA


unique(lb_SM$EPOCH)
xx = lb_SM %>% filter(is.na(EPOCH) | EPOCH=='')
table(xx$STUDYID, xx$LBTEST)

unique(lb_SM$LBTEST)
ind = which(lb_SM$STUDYID=='HRLGX' & lb_SM$LBTEST=='Lactic Acid' & is.na(lb_SM$LBSTRESN))
lb_SM$LBSTRESN[ind]=lb_SM$LBORRES[ind]

ind = which(lb_SM$STUDYID=='ZQEVB' & lb_SM$LBTEST=='Creatinine' & is.na(lb_SM$LBSTRESN) & lb_SM$LBSTRESU=='umol/L')
lb_SM$LBSTRESN[ind]=lb_SM$LBORRES[ind]

ind = which(lb_SM$LBTEST=='pH' & is.na(lb_SM$LBSTRESN))
lb_SM$LBSTRESN[ind]=lb_SM$LBORRES[ind]

ind = which(lb_SM$LBTEST=='Carbon Dioxide' & is.na(lb_SM$LBSTRESN) & lb_SM$STUDYID=='JTGNG')
lb_SM$LBSTRESN[ind]=lb_SM$LBORRES[ind]

lb_SM$LBSTRESN = as.numeric(lb_SM$LBSTRESN)
renal_FUP = lb_SM %>% filter(LBTEST %in% c('Creatinine','Urea Nitrogen'),
                             !EPOCH %in% c('BASELINE','SCREENING'),
                             STUDYID=='ZQEVB', VISIT %in% c("28d","90d","180d")) %>%
  group_by(USUBJID, LBTEST) %>%
  mutate(
    outlier_bun = ifelse(LBSTRESN>10 & LBTEST== 'Urea Nitrogen',T,F),
    outlier_creat = ifelse(LBSTRESN>100 & LBTEST== 'Creatinine',T,F),
    baseline_mean = mean(LBSTRESN))
xx = renal_FUP %>% filter(outlier_bun | outlier_creat)
ids_exclude = unique(xx$USUBJID)
renal_FUP = renal_FUP %>% filter(!USUBJID %in% ids_exclude) %>%
  distinct(USUBJID, .keep_all = T) %>%
  select(USUBJID,baseline_mean, LBTEST)%>%
  pivot_wider(id_cols = USUBJID,names_from = LBTEST,values_from = baseline_mean,names_prefix = 'Baseline_')


# key_baseline_vars = c('Hematocrit','Hemoglobin',"Base Excess","Lactic Acid",
# "Glucose","Platelets","Creatinine","Leukocytes","Urea Nitrogen")
baseline_lb_SM = lb_SM %>% filter(EPOCH %in% c('BASELINE','SCREENING'),
                                  # LBTEST %in% key_baseline_vars,
                                  !LBSPEC %in% c('CSF','CEREBROSPINAL FLUID')) %>%
  arrange(STUDYID, USUBJID, LBTEST) %>%
  group_by(USUBJID, STUDYID, LBTEST) %>%
  mutate(n=length(USUBJID))

ind = which(baseline_lb_SM$STUDYID=='JTGNG' &
              baseline_lb_SM$n>2 &
              (baseline_lb_SM$LBCDSTHR>1 | baseline_lb_SM$LBDY>1) &
              !baseline_lb_SM$LBCDSTHR==0)
# View(baseline_lb_SM[ind, ])
baseline_lb_SM = baseline_lb_SM[-ind, ]


## Need to check repeated measures at baseline!!
xx=baseline_lb_SM %>% filter(n>1) %>% arrange(USUBJID)
unique(xx$LBTEST)
# View(xx %>% filter(LBTEST=='Creatinine'))
baseline_lb_SM %>% filter(LBTEST=='Creatinine') %>%
  ggplot(aes(x=as.numeric(LBSTRESN), color=LBSTRESU)) + geom_histogram()+
  scale_x_log10()
baseline_lb_SM$LBSTRESU[baseline_lb_SM$LBTEST=='Creatinine'&baseline_lb_SM$LBSTRESU=='mmol/L']='umol/L'
ind = which(baseline_lb_SM$LBTEST=='Creatinine'&baseline_lb_SM$STUDYID=='ITYCK' & baseline_lb_SM$LBSTRESN<10)
baseline_lb_SM$LBSTRESN[ind] = as.numeric(baseline_lb_SM$LBSTRESN[ind])*88.4

baseline_lb_SM_wide = baseline_lb_SM %>%
  pivot_wider(id_cols = c(USUBJID, STUDYID),
              names_from = c(LBTEST, LBSTRESU),
              values_from = LBSTRESN, values_fn = mean)
ind = which(baseline_lb_SM_wide$STUDYID=='JTGNG' &
              !is.na(baseline_lb_SM_wide$`Creatinine_umol/L`) &
              !is.na(baseline_lb_SM_wide$`Urea Nitrogen_mmol/L`) &
              baseline_lb_SM_wide$`Creatinine_umol/L`>400 &
              baseline_lb_SM_wide$`Urea Nitrogen_mmol/L` < 10)
baseline_lb_SM_wide$`Creatinine_umol/L`[ind]=baseline_lb_SM_wide$`Creatinine_umol/L`[ind]/10


#####***** MB data *******#######
mb_SM = read_csv(f_path('MB'),na = "\\N")
table(mb_SM$STUDYID, mb_SM$EPOCH, useNA = 'ifany')

# get HRP2 data
mb_SM_hrp2 = mb_SM %>%
  filter(MBTEST %in%
           c("Plasmodium Histidine Rich Protein 2"),
         EPOCH=='BASELINE') %>%
  group_by(USUBJID) %>%
  mutate(PfHRP2_ng_ml = as.numeric(max(c(MBSTRESN,MBORRES), na.rm = T))) %>%
  distinct(USUBJID, .keep_all = T) %>%
  select(USUBJID,STUDYID,PfHRP2_ng_ml)


mb_SM$EPOCH[mb_SM$EPOCH=='' & mb_SM$MBCDSTHR==0]='BASELINE'
mb_SM$EPOCH[mb_SM$STUDYID=='MOLFZ']='BASELINE'

mb_positive = mb_SM %>% filter(MBTSTDTL=='DETECTION', MBTESTCD %in% c('PFALCIP','PFALCIPA','PFALCIPS','PLSMDM')) %>%
  rename(Malaria_RDT = MBSTRESC) %>% select(USUBJID, STUDYID, Malaria_RDT) %>%
  group_by(USUBJID) %>%
  mutate(Malaria_RDT = any(Malaria_RDT %in% c("POSITIVE",'Y')))%>%
  distinct(USUBJID, .keep_all = T)
any(duplicated(mb_positive$USUBJID))

mb_SM = mb_SM%>% filter(MBTSTDTL=='QUANTIFICATION') %>% select(-MBSEQ, -DOMAIN, -MBORRES, -MBORRESU)
mb_SM_para = mb_SM %>%
  filter(MBTEST %in%
           c('Plasmodium falciparum, Asexual',
             "Plasmodium falciparum" ,
             "Plasmodium falciparum, Sexual",
             'Plasmodium, Asexual'),
         EPOCH=='BASELINE', MBSTRESN != '', !is.na(MBSTRESN)) %>%
  group_by(USUBJID) %>%
  mutate(para_ul = max(MBSTRESN, na.rm = T)) %>%
  distinct(USUBJID, .keep_all = T) %>%
  select(USUBJID,STUDYID,para_ul)



mb_SM_para = merge(mb_SM_para, mb_positive, all = T)
mb_SM_para = merge(mb_SM_para, mb_SM_hrp2, all = T)

mb_SM_para = mb_SM_para %>%
  mutate(Malaria_Positive = case_when(
    !is.na(PfHRP2_ng_ml) & PfHRP2_ng_ml>0 ~ T,
    !is.na(para_ul) & para_ul>0 ~ T,
    !is.na(Malaria_RDT) & Malaria_RDT ~ T,
    
    !is.na(PfHRP2_ng_ml) & PfHRP2_ng_ml==0 ~ F,
    !is.na(para_ul) & para_ul==0 ~ F,
    !is.na(Malaria_RDT) & !Malaria_RDT ~ F,
    
    T ~ NA
  ))
# Malaria_Positive = ifelse(STUDYID=='UUJKO', T, Malaria_Positive))
table(mb_SM_para$STUDYID, mb_SM_para$Malaria_Positive,useNA = 'ifany')

#####***** Make merged dataset *******#######
#####*
#####*
#####*

dat_all = merge(dm_SM, ds_SM, by = col_ids, all = T) %>%
  mutate(Time_to_death = case_when(
    !is.na(Time_to_death.x) & is.na(Time_to_death.y) ~ Time_to_death.x,
    is.na(Time_to_death.x) & !is.na(Time_to_death.y) ~ Time_to_death.y,
    !is.na(Time_to_death.x) & !is.na(Time_to_death.y) & Time_to_death.x==Time_to_death.y ~ Time_to_death.x,
    !is.na(Time_to_death.x) & !is.na(Time_to_death.y) & Time_to_death.x!=Time_to_death.y ~ Time_to_death.x,
    T ~ NA
  ))
if(any(dat_all$Time_to_death<0)) writeLines('NEGATIVE TIMES TO DEATH!!!!')
# View(dat_all %>% filter(Time_to_death<0))
dat_all = dat_all %>% select(-Time_to_death.x, -Time_to_death.y)
dat_all$Time_to_death[dat_all$Time_to_death<0]=NA

dat_all = merge(dat_all, baseline_vs_SM_wide, by = col_ids, all = T)
dat_all = merge(dat_all, clinical_baseline, by = col_ids, all = T)
dat_all = merge(dat_all, mb_SM_para, by = col_ids, all = T)
dat_all = merge(dat_all, baseline_lb_SM_wide, by = col_ids, all = T)
### Add the normal creatinines in the TRACT study
dat_all = merge(dat_all, renal_FUP, by = 'USUBJID', all = T)

dat_all_final = dat_all%>%
  mutate(
    STUDY = case_when(
      STUDYID=='AWPDN' ~ 'FEAST',
      STUDYID=='UUJKO' ~ 'AQUAMAT',
      STUDYID=='CCFRW' ~ 'AQ Vietnam',
      STUDYID=='ZCZCV' ~ 'SEAQUAMAT',
      STUDYID=='HRLGX' ~ 'AQ Gambia',
      STUDYID=='ZEYRR' ~ 'AAV',
      STUDYID=='GEZHR' ~ 'NO Uganda',
      STUDYID=='JTGNG' ~ 'Thailand cohort',
      STUDYID=='RHKEF' ~ 'CQ Gambia',
      STUDYID=='ZQEVB' ~ 'TRACT',
      STUDYID=='MOLFZ' ~ 'Kilifi cohort',
      STUDYID=='ITYCK' ~ 'Dong Nai',
      STUDYID=='FSOUE' ~ 'Namazzi 2022',
      STUDYID=='NLSSA' ~ 'Conroy 2019',
      T ~ STUDYID
    )
  )
table(dat_all_final$STUDY)
# dat_all_final = dat_all_final[permute::shuffle(nrow(dat_all_final)), ]

table(dat_all_final$STUDY, dat_all_final$COUNTRY)

dat_all_final$Continent = ifelse(dat_all_final$COUNTRY %in% c('VNM','BGD','IDN','IND','MMR','THA'),'ASIA','AFRICA')
table(dat_all_final$Continent, dat_all_final$STUDY)

## Get rid of implausible values for age and weight
dat_all_final$Age[dat_all_final$Age<0]=NA
dat_all_final$Age[dat_all_final$Age==0]=0.5
dat_all_final$Weight_kg[dat_all_final$Age>100 & dat_all_final$Weight_kg<1]=NA

colnames(dat_all_final) = gsub(pattern = '/',replacement = '_',fixed = T,x = colnames(dat_all_final))
colnames(dat_all_final) = gsub(pattern = '^',replacement = '_',fixed = T,x = colnames(dat_all_final))
table(dat_all_final$STUDY, dat_all_final$Malaria_Positive, useNA = 'ifany')
dat_all_final$Malaria_Positive[is.na(dat_all_final$Malaria_Positive) & dat_all_final$STUDY=='Thailand cohort']=TRUE

dat_all_final$BUN=dat_all_final$`Urea Nitrogen_mmol_L`
dat_all_final$BUN = ifelse(dat_all_final$BUN==0,NA, dat_all_final$BUN)
dat_all_final$Creatinine_umol_L =
  ifelse(dat_all_final$Creatinine_umol_L==0,NA, dat_all_final$Creatinine_umol_L)


dat_all_final$Age = dat_all_final$Age/12
dat_all_final$Age_category = cut(dat_all_final$Age, breaks = c(0, 2, 5, 15, 100))
dat_all_final = dat_all_final %>%
  mutate(Age_category = recode(Age_category,
                               "(0,2]" = "≤2",
                               "(2,5]" = "(2,5]",
                               "(5,15]" = "(5,15]",
                               "(15,100]" = ">15"))

dat_all_final$Age_category_pch = as.numeric(dat_all_final$Age_category)

dat_all_final %>% ggplot(aes(x=Age, y=Weight_kg, colour = STUDY))+geom_point()+
  geom_smooth(aes(group=NA))

dat_all_final$`Hematocrit_%`[dat_all_final$`Hematocrit_%`>100 | dat_all_final$Hemoglobin_g_L>250 | dat_all_final$`Hematocrit_%`==0]=NA

ind = dat_all_final$STUDY=='KEMRI' &
  !is.na(dat_all_final$`Hematocrit_%`) &
  dat_all_final$Hemoglobin_g_L > 5* dat_all_final$`Hematocrit_%`
dat_all_final$`Hematocrit_%`[ind] = dat_all_final$`Hematocrit_%`[ind]*10
dat_all_final$`Hematocrit_%`[dat_all_final$`Hematocrit_%`>100]=NA
dat_all_final$Hemoglobin_g_L[dat_all_final$Hemoglobin_g_L>200]=NA

dat_all_final %>%
  ggplot(aes(x=`Hematocrit_%`, y=Hemoglobin_g_L, colour = STUDY))+geom_point()

dat_all_final$COUNTRY=as.factor(dat_all_final$COUNTRY)
# mod_smooth = mgcv::gam(Weight_kg ~ s(Age,k=4) + s(COUNTRY,bs='re'), data = dat_all_final)
# ind_pred = is.na(dat_all_final$Weight_kg) & !is.na(dat_all_final$Age)
# dat_all_final$Weight_kg[ind_pred] =
#   predict(mod_smooth,
#           newdata = dat_all_final[ind_pred, c('Age','COUNTRY')], exclude="s(COUNTRY,bs='re')")

# dat_all_final$Weight_category = cut(dat_all_final$Weight_kg, breaks = c(0, 30, 100))
# dat_all_final = dat_all_final %>%
#   mutate(Weight_category = recode(Weight_category,
#                                   "(0,30]" = "≤30",
#                                   "(30,100]" = ">30"))
dat_all_final$STUDY_color = brewer.pal(n = 12, name = 'Paired')[as.numeric(as.factor(dat_all_final$STUDY))]


dat_all_final %>% ggplot(aes(x=Creatinine_umol_L, y=BUN, colour = STUDY))+
  geom_point()+
  scale_x_log10() + scale_y_log10()+geom_vline(xintercept = 5)+
  geom_hline(yintercept = .5) + facet_wrap(~STUDY)
dat_all_final %>% ggplot(aes(x=Weight_kg, y=BUN, colour = STUDY))+
  geom_point()+
  scale_y_log10() + facet_wrap(~STUDY)
dat_all_final %>% ggplot(aes(x=Weight_kg, y=Creatinine_umol_L, colour = STUDY))+
  geom_point()+
  scale_y_log10()
dat_all_final %>% ggplot(aes(x=Age, y=BUN, colour = STUDY))+
  geom_point()+
  scale_y_log10()
dat_all_final %>% ggplot(aes(x=Age, y=Creatinine_umol_L, colour = STUDY))+
  geom_point()+
  scale_y_log10()



dat_all_final = dat_all_final %>%
  mutate(Creatinine_umol_L = ifelse(Creatinine_umol_L<8, 8, Creatinine_umol_L),
         BUN = ifelse(BUN<.5, .5, BUN),
         BUN = ifelse(BUN>100, 100, BUN),
         Creat_BUN_ratio = Creatinine_umol_L/BUN)
dat_all_final %>% ggplot(aes(x=log10(Creat_BUN_ratio)))+
  geom_histogram()+ facet_wrap(~STUDY)
p1=dat_all_final %>% ggplot(aes(x=Age, Creat_BUN_ratio, colour = STUDY))+geom_point()+scale_y_log10()+scale_x_sqrt()+geom_hline(yintercept = c(1,100))
ggsave(plot = p1,filename = 'BUN_Creatinine_outliers_removal.pdf')
ind = which(dat_all_final$Creat_BUN_ratio<1 | dat_all_final$Creat_BUN_ratio>100)
dat_all_final$STUDY[ind]
dat_all_final$Creatinine_umol_L[ind]=NA
dat_all_final$BUN[ind]=NA

# Manual correction time to death in AQUAMAT
ind = which(dat_all_final$Time_to_death > 8000 & dat_all_final$STUDY=='AQUAMAT')
dat_all_final$Time_to_death[ind]=10*24

# Remove 999 height values in TRACT
ind = which(dat_all_final$Height_cm > 200 & dat_all_final$STUDY=='TRACT')
dat_all_final$Height_cm[ind]=NA


table(is.na(dat_all_final$Hypoglycaemia) & !is.na(dat_all_final$Glucose_mmol_L))
dat_all_final = dat_all_final %>%
  mutate(Hypoglycaemia = case_when(
    is.na(Hypoglycaemia) & !is.na(Glucose_mmol_L) & Glucose_mmol_L<=2.2 ~ T,
    is.na(Hypoglycaemia) & !is.na(Glucose_mmol_L) & Glucose_mmol_L>2.2 ~ F,
    T ~ Hypoglycaemia
  ),
  Glucose_mmol_L = ifelse(Glucose_mmol_L>3 & STUDY=='AQUAMAT' & !is.na(Hypoglycaemia) & Hypoglycaemia, NA, Glucose_mmol_L),
  `Died within 28 days` = Died & (is.na(Time_to_death) | Time_to_death < (28*24)),
  `Died within 28 days` = ifelse(`Died within 28 days`, 'Yes', 'No'))

table(is.na(dat_all_final$RFSTDTC), dat_all_final$STUDY)

dat_all_final$date_admission = ym(dat_all_final$RFSTDTC)
dat_all_final$year_admission = year(dat_all_final$date_admission)
table(is.na(dat_all_final$year_admission), dat_all_final$STUDY)
dat_all_final$year_admission[dat_all_final$STUDY=='Conroy 2019']=2010
dat_all_final$year_admission[dat_all_final$STUDY=='Dong Nai']=1992
dat_all_final$year_admission[dat_all_final$STUDY=='Namazzi 2022']=2015

## Manual correction of base excess values in KEMRI
ind = which(dat_all_final$STUDY=='KEMRI' &
              dat_all_final$date_admission>as.Date('1998-01-01') &
              dat_all_final$`Base Excess_mmol_L` > -2)
dat_all_final$`Base Excess_mmol_L`[ind] = -1*dat_all_final$`Base Excess_mmol_L`[ind]
dat_all_final$`Base Excess_mmol_L` = ifelse(dat_all_final$`Base Excess_mmol_L`< -35, NA, dat_all_final$`Base Excess_mmol_L`)
hist(dat_all_final$`Base Excess_mmol_L`, breaks = 200)

## Missing ages in AQ Vietnam study (CCFRW)?? This seems like a data merge issue because there are more (569) patients in the data relative to the study (560)

## MUAC outliers
dat_all_final %>% filter(!is.na(`Mid-Upper Arm Circumference_cm`)) %>%
  ggplot(aes(x=Age, y = `Mid-Upper Arm Circumference_cm`, colour = STUDY))+
  geom_point()+theme_minimal()+
  geom_abline(intercept = 8, slope = .5)+
  geom_abline(intercept = 19, slope = .5)+geom_vline(xintercept = 15)+
  geom_smooth(method = lm)


dat_all_final %>% filter(!is.na(`Mid-Upper Arm Circumference_cm`), Age<5) %>%
  ggplot(aes(x=`Mid-Upper Arm Circumference_cm`, color = STUDY))+
  geom_histogram()+theme_minimal()


ind = which((dat_all_final$Age>15  |
               (dat_all_final$`Mid-Upper Arm Circumference_cm` < (dat_all_final$Age*0.5 + 8)) |
               (dat_all_final$`Mid-Upper Arm Circumference_cm` > (dat_all_final$Age*0.5 + 19))) &
              !is.na(dat_all_final$`Mid-Upper Arm Circumference_cm`))
dat_all_final$`Mid-Upper Arm Circumference_cm`[ind]=NA
## Manual correction of base excess values in FEAST
ind = which(dat_all_final$STUDY=='FEAST' &
              dat_all_final$`Base Excess_mmol_L` > 5 & dat_all_final$Bicarbonate_mEq_L<20)
dat_all_final$`Base Excess_mmol_L`[ind] = -1*dat_all_final$`Base Excess_mmol_L`[ind]
hist(dat_all_final$`Base Excess_mmol_L`, breaks = 200)


# Remove Creatinine values in GEZHR - issues with measurement
# ind = which(dat_all_final$STUDYID == 'GEZHR')
# dat_all_final$Creatinine_umol_L[ind]=NA

dat_all_final$Lactate = dat_all_final$`Lactic Acid_mmol_L`
dat_all_final = dat_all_final%>% select(-`Lactic Acid_mmol_L`)
dat_all_final$Lactate = ifelse(dat_all_final$Lactate > 30, NA, dat_all_final$Lactate)
dat_all_final$Lactate = ifelse(dat_all_final$Lactate > 20, 20, dat_all_final$Lactate)
hist(dat_all_final$Lactate, breaks = 200)

# remove wrong Sodium values in the Kilifi study which were swapped with the Potassium
ind = which(dat_all_final$Sodium_mmol_L > 500 )
dat_all_final$Sodium_mmol_L[ind] = NA
ind_swapped = dat_all_final$Sodium_mmol_L<15 & dat_all_final$Potassium_mmol_L>100

plot(dat_all_final$Sodium_mmol_L, dat_all_final$Potassium_mmol_L, col=as.numeric(ind_swapped)+1)

na = dat_all_final$Sodium_mmol_L[which(ind_swapped)]
dat_all_final$Sodium_mmol_L[which(ind_swapped)] = dat_all_final$Potassium_mmol_L[which(ind_swapped)]
dat_all_final$Potassium_mmol_L[which(ind_swapped)] = na

dat_all_final$Potassium_mmol_L[which(dat_all_final$Potassium_mmol_L > 30)]=NA
dat_all_final$Sodium_mmol_L[which(dat_all_final$Sodium_mmol_L < 75 | dat_all_final$Sodium_mmol_L > 200)]=NA

dat_all_final$Potassium_mmol_L[which(dat_all_final$Potassium_mmol_L >= 9.9 & dat_all_final$STUDY=='AQUAMAT')]=NA
dat_all_final$Potassium_mmol_L[which(dat_all_final$Potassium_mmol_L >= 9.9 & dat_all_final$STUDY=='Namazzi 2022')]=NA
dat_all_final$Potassium_mmol_L[which(dat_all_final$Potassium_mmol_L >= 9.9 & dat_all_final$STUDY=='TRACT')]=NA

sum(!is.na(dat_all_final$`Partial Pressure Carbon Dioxide_mmHg`))

dat_all_final = dat_all_final %>% select(-`Partial Pressure Carbon Dioxide_`, -`Partial Pressure Oxygen_mmHg`,-`Partial Pressure Carbon Dioxide_mmol_L`)

source('hb_data_imputation.R')
dat_all_final = hb_hct_impute(dat_all_final)


write_csv(dat_all_final, file = 'Data/adam_out.csv')
dat_all_final = dat_all_final %>% select(-USUBJID, -RFSTDTC)



### NOT YET CURATED ####
load('~/Dropbox/MORU/Severe malaria/Pigment_Neutrophils/Malaria_Pigment_Prognosis/RData/SMAC_data.RData')
smac = myMergedData %>% 
  mutate(
    STUDY='SMAC',
    Age = AGE/12,
    Age_category = cut(Age, breaks = c(0, 2, 5, 15, 100)),
    SEX = ifelse(SEX==1,'F','M'),
    Died = OUTCOME,
    `Died within 28 days` = ifelse(Died==1, 'Yes', 'No'),
    para_ul = as.numeric(PARASIT),
    Malaria_Positive=ifelse(!is.na(para_ul) & para_ul>0, T, F),
    SITEID = country_names,
    COUNTRY = case_when(SITEID=='The Gambia' ~ 'GMB',
                        SITEID=='Kenya' ~ 'KEN',
                        SITEID=='Ghana' ~ 'GHA',
                        SITEID=='Malawi' ~ 'MLW',
                        SITEID=='Gabon (Lambarene)' ~ 'GBN',
                        SITEID=='Gabon (Libreville)' ~ 'GBN'
    ),
    Continent='Africa',
    Weight_kg = as.numeric(WEIGHT),
    Weight_kg = ifelse(Weight_kg>90, NA, Weight_kg),
    BCS_tot = sum(c(BMS, BVS, BES), na.rm = T),
    Coma = ifelse(BCS_tot<=2,1, 0),
    `Respiratory distress` = ifelse(DEEPBR==1,T,F),
    Hb = as.numeric(HB),
    `Respiratory Rate_breaths_min` = as.numeric(RESPRATE),
    Temperature_C = as.numeric(TEMP),
    `Base Excess_mmol_L` = as.numeric(BE),
    Lactate = as.numeric(LACTATE),
    Glucose_mmol_L = as.numeric(GLUCOSE),
    HCT = as.numeric(HCT),
    pH_ = PH,
    `Partial Pressure Carbon Dioxide_mmHg` = PCO2
  ) %>% select(-GLUCOSE, - DEEPBR, -HB, -WEIGHT, -BE) %>%
  mutate(Age_category = recode(Age_category,
                               "(0,2]" = "≤2",
                               "(2,5]" = "(2,5]",
                               "(5,15]" = "(5,15]",
                               "(15,100]" = ">15"),
         Glucose_mmol_L = ifelse(Glucose_mmol_L>20, NA, Glucose_mmol_L)
         )
# ind_col = which(colnames(smac) %in% colnames(dat_all))
# colnames(smac)[ind_col]
# smac = smac[,ind_col]
write_csv(smac, file = 'Data/smac.csv')


# ### NOT YET CURATED ####
# kemri_dat = readr::read_csv('~/Dropbox/Datasets/KEMRI Severe Malaria/1995_2020_severe_malaria_24092024_JW.csv')
# plot(kemri_dat$ageyr, kemri_dat$weight)
# kemri_dat$weight[which(kemri_dat$ageyr<6 & kemri_dat$weight>30)]=c(6.7, 17)
# mod_outlier = lm(weight~ageyr, data = kemri_dat)
# mod_outlier$residuals
# kemri_dat$USUBJID = 10^5+as.numeric(kemri_dat$pid)
# kemri_dat$doa = (as.POSIXct(as_date(kemri_dat$doa, format='%d%b%Y')))

# kemri_dat = kemri_dat %>% group_by(pid) %>%
#   mutate(
#     STUDYID='Kilifi',
#     RFSTDTC = paste(year(doa),month(doa),sep = '-'),
#     Age = agemths,
#     SEX = ifelse(sex=='female','F','M'),
#     Died = as.numeric(outcome=='Dead'),
#     Malaria_Positive=T,
#     Coma_Final = ifelse(bcs_total<=2 | cerebral_malaria==1,1, 0),
#     `Respiratory distress` = ifelse(respiratory_distress==9,NA, respiratory_distress),
#     Anemia = severe_malaria_anaemia,
#     `Heart Rate_beats/min` = pul_hrate,
#     Temperature_C = tempaxil,
#     `Base Excess_mmol/L` = baseexc,
#     `Oxygen Saturation_%` = oxysat,
#     Height_cm = height,
#     Weight_kg = weight,
#     `Glucose_mmol/L` = glucose,
#     `Creatinine_umol/L` = creat,
#     `Leukocytes_10^9/L` = wbc,
#     `Hemoglobin_g/L` = hb*10,
#     `Hematocrit_%` = ifelse(hct<1, hct*100, hct),
#     `Systolic Blood Pressure_mmHg` = bps,
#     `Diastolic Blood Pressure_mmHg` = bpd,
#     `Mid-Upper Arm Circumference_cm` = muac,
#     COUNTRY='KEN',
#     SITEID='Kilifi'
#   )
#
#
# ind_col = which(colnames(kemri_dat) %in% colnames(dat_all))
# colnames(kemri_dat)[ind_col]
# kemri_dat = kemri_dat[,ind_col]


# make the combined dataset

dat_all_final$ID = 1:nrow(dat_all_final)
smac$ID = nrow(dat_all_final)+(1:nrow(smac))
cols_merge = intersect(colnames(smac), colnames(dat_all_final))
combined_dataset = merge(smac, dat_all_final, by = c('ID', cols_merge), all = T)


cols = c("SEX" ,"HCT", "Hb" ,"STUDY" ,"Age" ,"Died within 28 days",
         "Malaria_Positive"    ,
         "SITEID","COUNTRY"  ,"Weight_kg", "BCS_tot"  ,"Respiratory distress" ,
         "para_ul" ,"Temperature_C",
         "Coma", "Respiratory Rate_breaths_min"  , "Lactate",
         "Time_to_death","Capillary Refill Time_sec"   ,
         "Head Circumference_cm","Glucose_mmol_L",
         "Heart Rate_beats_min","Height_cm",
         "Mid-Upper Arm Circumference_cm",
         "Systolic Blood Pressure_mmHg",
         "Oxygen Saturation_%"  ,
         "Diastolic Blood Pressure_mmHg",
         "Blood in urine"  ,"Epilepsy","Fever","Jaundice"  ,"Seizure",
         "Anuria"    ,   
         "Cyanosis","Edema"   , 
         "Shock","Prostration",         
         "Sepsis","Bleeding", "Hypoglycaemia" ,
         "Dehydration" , "Anion Gap_mmol_L","Base Excess_mmol_L",
         "Bicarbonate_mEq_L"  ,
         "Carbon Dioxide_mmol_L","Chloride_mEq_L","Creatinine_umol_L"     ,    
         "Leukocytes_10_9_L","Partial Pressure Carbon Dioxide_mmHg" ,
         "Platelets_10_9_L",                  
         "Potassium_mmol_L",            
         "Sodium_mmol_L"   ,
         "pH_" , "Bilirubin_umol_L","Direct Bilirubin_umol_L",
         "Eosinophils_Leukocytes_%", "Indirect Bilirubin_",
         "Lymphocytes_Leukocytes_%","Monocytes_Leukocytes_%",
         "Neutrophils_Leukocytes_%",
         "Reticulocytes_Erythrocytes_%",
         "Chloride_mmol_L", "Anion Gap_" , 
         "C Reactive Protein_mg_L","Continent","BUN", "Age_category"           ,
         "year_admission", "PfHRP2_ng_ml")
#combined_dataset = combined_dataset[which(combined_dataset$Malaria_Positive), cols]
combined_dataset = combined_dataset[ , cols]

combined_dataset = combined_dataset%>% arrange(STUDY) %>% 
  filter(!is.na(`Died within 28 days`)) %>%
  mutate(Died_28D = ifelse(`Died within 28 days`=='Yes',1,0))

combined_dataset <- combined_dataset %>% select(where(~ !all(is.na(.))))


write_csv(x = combined_dataset, file = 'Data/Combined_dataset.csv')

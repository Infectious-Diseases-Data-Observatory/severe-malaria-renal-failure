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


## Get lab data
lb_SM = read_csv(f_path('LB'),na = "\\N")
lb_SM = lb_SM %>% filter(LBTEST == 'Lactic Acid')
remove_cols = which(apply(lb_SM, 2, function(x) mean(is.na(x)))==1)
lb_SM = lb_SM[, -remove_cols]
table(lb_SM$STUDYID, lb_SM$EPOCH, useNA = 'ifany')
table(lb_SM$VISIT, lb_SM$STUDYID, useNA = 'ifany')

lb_SM = lb_SM %>% mutate(
  Time_hours = case_when(VISIT %in% c("Admission","Enrolment (Hour 0)",
                                            "Day 0 - Hour 0","Day 0/Hour 0","0 Hour",
                                            "Admission visit","Baseline","Enrolment") ~ 0,
                               VISIT %in% c("Hour 8","8hr") ~ 8,
                               VISIT %in% c("Hour 24","24hr") ~ 24,
                               VISIT=="Hour 4" ~ 4,
                               VISIT=="Hour 12" ~ 12,
                               VISIT=="Hour 48" ~48,
                               VISIT=="Hour 72" ~ 72,
                               VISIT=="Hour 168" ~ 168,
                               VISIT=="Day 1" & STUDYID=='GEZHR'~ 0,
                               VISIT=="Day 14" ~14*24))
View(lb_SM %>% filter(!is.na(LBEVINTX)))

ind = lb_SM$LBEVINTX=='AT ANY TIME DURING HOSPITALISATION' & is.na(lb_SM$Time_hours) & is.na(lb_SM$LBCDSTDY)& is.na(lb_SM$LBCDSTHR)

# View(lb_SM[which(ind), ])
lb_SM = lb_SM[-which(ind), ]

ind = is.na(lb_SM$LBEVINTX) & is.na(lb_SM$Time_hours) & is.na(lb_SM$LBCDSTDY)& is.na(lb_SM$LBCDSTHR)
which(ind)

ind = is.na(lb_SM$Time_hours) & !is.na(lb_SM$LBCDSTHR)
which(ind)
# View(lb_SM[which(ind), ])
lb_SM$Time_hours[ind] = lb_SM$LBCDSTHR[ind]

ind = is.na(lb_SM$Time_hours) & !is.na(lb_SM$LBCDSTDY) & lb_SM$STUDYID=='GEZHR'
which(ind)
# View(lb_SM[which(ind), ])
lb_SM$Time_hours[ind] = (lb_SM$LBCDSTDY[ind]-1)*24


table(is.na(lb_SM$Time_hours))

ind = which(lb_SM$STUDYID=='HRLGX' & lb_SM$LBTEST=='Lactic Acid' & is.na(lb_SM$LBSTRESN))
lb_SM$LBSTRESN[ind]=lb_SM$LBORRES[ind]

studies_longitudinal = lb_SM %>% group_by(USUBJID) %>%
  mutate(n_lac_unique_measurements = length(unique(Time_hours))) %>%
  distinct(USUBJID, .keep_all = T) %>% 
  group_by(STUDYID) %>%
  mutate(max_unique_measurements = max(n_lac_unique_measurements)) %>%
  distinct(STUDYID, .keep_all = T) %>% 
  filter(max_unique_measurements>1)%>%
  select(STUDYID) 
studies_longitudinal 
lb_SM$Lactate = as.numeric(lb_SM$LBSTRESN)
unique(lb_SM$LBSTRESU)

lb_SM = lb_SM %>% filter(STUDYID %in% c("AWPDN" ,"CCFRW" ,"GEZHR", "ITYCK", "ZQEVB")) %>%
  select(USUBJID, Lactate, Time_hours)

lb_SM %>% filter(Time_hours<=72) %>% 
  ggplot(aes(x=Time_hours, y = Lactate,group = USUBJID))+geom_jitter()+geom_line()



dat = read_csv('Data/adam_out.csv')
dat$Lactate_baseline = dat$Lactate
dat = dat %>% select(-Lactate)

serial_lactate = merge(lb_SM, dat, by = 'USUBJID', all.x = T)
length(unique(serial_lactate$USUBJID))
serial_lactate$Time_to_death

write_csv(x = serial_lactate, file = 'Data/serial_lactates.csv')


# extract first blood transfusion data for TRACT
in_SM = read_csv(f_path('IN'),na = "\\N")
bt_dat = in_SM %>% filter(INDECOD=='TRANSFUSION OF BLOOD PRODUCT', INCAT != 'MEDICAL HISTORY',
                          STUDYID=='ZQEVB', INGRPID=='Transfusion 1')

remove_cols = which(apply(bt_dat, 2, function(x) mean(is.na(x)))==1)
bt_dat = bt_dat[, -remove_cols]
bt_dat = bt_dat %>% group_by(USUBJID) %>%
  mutate(k = 1:n()) %>% filter(k==1)


bt_dat = merge(bt_dat, dat %>% filter(STUDY=='TRACT'), by = c('STUDYID','USUBJID'), all.y = T) %>%
  arrange(ARM)

bt_dat$Randomised_volume = NA
ind_AB_20 = 1:nrow(bt_dat) %in% c(grep('A_20mg/ml', x = bt_dat$ARM, fixed = T), grep('B_20mg/ml', x = bt_dat$ARM, fixed = T))
ind_AB_30 = 1:nrow(bt_dat) %in% c(grep('A_30mg/ml', x = bt_dat$ARM, fixed = T), grep('B_30mg/ml', x = bt_dat$ARM, fixed = T))
ind_whole_blood = 1:nrow(bt_dat) %in% grep('Whole blood', x = bt_dat$INTRT, fixed = T) 
bt_dat$Died_28D = ifelse(bt_dat$`Died within 28 days`=='Yes',1,0)

bt_dat$Randomised_volume[ind_AB_20 & ind_whole_blood] = 20
bt_dat$Randomised_volume[ind_AB_20 & !ind_whole_blood] = 10
bt_dat$Randomised_volume[ind_AB_30 & ind_whole_blood] = 30
bt_dat$Randomised_volume[ind_AB_30 & !ind_whole_blood] = 15
table(bt_dat$Randomised_volume, useNA = 'ifany')
table(bt_dat$Died_28D, bt_dat$Randomised_volume, useNA = 'ifany')

View(bt_dat %>% filter(is.na(Randomised_volume)))
aggregate(Died_28D ~ Randomised_volume, data = bt_dat, FUN = mean)
mod=glm(Died_28D ~ Randomised_volume, family = 'binomial', data = bt_dat %>% filter(!is.na(Randomised_volume)))
summary(mod)

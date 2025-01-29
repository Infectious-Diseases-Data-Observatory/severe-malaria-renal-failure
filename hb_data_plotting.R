sm_data$HCT = sm_data$`Hematocrit_%`
sm_data$Hb = sm_data$Hemoglobin_g_L/10
sm_data = sm_data %>% select(-Hemoglobin_g_L, -`Hematocrit_%`)
sm_data %>% ggplot(aes(x=Hb))+geom_histogram(binwidth = .1)+
  xlab('Hemoglobin (g/dL)')+theme_minimal()
sm_data %>% ggplot(aes(x=HCT))+geom_histogram(binwidth = .1)+
  xlab('Hematocrit (%)')+theme_minimal()


sum(!is.na(sm_data$HCT) & !is.na(sm_data$Hb))
sm_data %>% slice_sample(prop = 1) %>% ggplot(aes(x=Hb, y = HCT,colour = STUDY))+geom_point(alpha=0.4)+geom_smooth(aes(group=NA),method=lm)+xlim(0,15)+ylim(0,45)+
  theme_minimal()

sm_data %>% slice_sample(prop = 1) %>% ggplot(aes(x=Hb, y = (3*Hb)-HCT,colour = STUDY))+geom_point(alpha=0.4)+geom_smooth(aes(group=NA))+xlim(0,15)+ylim(-10,10)+
  theme_minimal()

sm_data %>% ggplot(aes(x= (3*Hb)-HCT))+geom_histogram(binwidth = 0.2)+theme_minimal()+xlim(-10,10)

mod1 = MASS::rlm(HCT~Hb, data = sm_data)
mod2 = MASS::rlm(Hb~HCT, data = sm_data)
coef(mod1)[2]
1/coef(mod2)[2]

table(is.na(sm_data$HCT) | is.na(sm_data$Hb))
table(is.na(sm_data$HCT) & is.na(sm_data$Hb))
# imputation just using 3x rule
ind = which(is.na(sm_data$HCT) & !is.na(sm_data$Hb))
length(ind)
sm_data$HCT[ind] = 3 * sm_data$Hb[ind]

ind = which(!is.na(sm_data$HCT) & is.na(sm_data$Hb))
length(ind)
sm_data$Hb[ind] = sm_data$HCT[ind]/3
table(is.na(sm_data$HCT) | is.na(sm_data$Hb))

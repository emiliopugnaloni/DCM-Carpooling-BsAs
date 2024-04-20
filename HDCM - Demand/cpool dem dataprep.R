# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

library(readxl);library(readr);library(dplyr);library(stringr);library(tidyr)

NSE_CarPooling <- 
  read_excel("C:/Users/epugnalo/OneDrive - Telefonica/Documents/Investigador UCA/Carpooling BsAs/Scripts/HDCM_Carpoolind_Demand/NSE CarPooling.xlsx", 
             sheet = "epanel")

dat0 = read.delim("C:/Users/epugnalo/OneDrive - Telefonica/Documents/Investigador UCA/Carpooling BsAs/Scripts/HDCM_Carpoolind_Demand/cpDem_long_epanel.txt")

dat <- dat0 %>%
  select(-NSE) %>% 
  left_join(NSE_CarPooling %>% rename(idLS = id)) %>% 
  mutate(alt_chr = case_when(alt == 1 ~ 'ca',
                             alt == 2 ~ 'cp',
                             alt == 3 ~ 'ch',
                             alt == 4 ~ 'tr')) %>% 

  mutate(modecar = as.numeric(RP03=='Auto/Camioneta <U+0096> manejando'),
         modech = as.numeric(RP03=='Charter (combi)'),
         modebus = as.numeric(RP03=='Colectivo'),
         modetrain = as.numeric(RP03=='Tren'),
        drivelicense = case_when(AU1 == 'S<ed>' ~ 1,
                                  AU1 == 'No' ~ 0,
                                  TRUE ~ 1),
        AU3 = case_when(AU3 == '1' ~ 1,
                        AU3 == '2' ~ 2,
                        AU3 == '3' ~ 3,
                        AU3 == '4' ~ 4,
                        AU3 == 'M<e1>s' ~ 5,
                        TRUE ~ 0),
        ncar = as.numeric(AU3),
        caravail = drivelicense * ncar,
        sec = case_when(CD4 == 'Pienso que es riesgoso' ~ 1,
                        CD4 == 'Pienso que puede ser inseguro' ~ 2,
                        CD4 == 'No s<e9>' ~ 3,
                        CD4 == 'Pienso que ser<e1> bastante seguro' ~ 4,
                        CD4 == 'Pienso que ser<e1> totalmente seguro' ~ 5)
        ) %>%
  # Supply: CS4 en vez de CD4
    #AU1   # N/A, No, S<ed>
  #AU2.0   # No, S<ed>
  #AU3   # 1, 2, 3, M<e1>s  (es un 1)
  # if(AU1!='No',1,0) * if(AU2.0!='S<ed>,AU3,0)

  mutate(cE1_1 = E1.SQ001., 
         cE1_2 = -(E1.SQ002.),  # inverted
         cE1_3 = E1.SQ003.,  
         cE1_6 = -(E1.SQ006.),  # inverted
         cE1_5 = E1.SQ005.,

         # cE2_preftime = case_when(
         #   E2.SQ001. == 'Que llegue r<e1>pido' ~ 5,
         #   E2.SQ002. == 'Que llegue r<e1>pido' ~ 4,
         #   E2.SQ003. == 'Que llegue r<e1>pido' ~ 3,
         #   E2.SQ004. == 'Que llegue r<e1>pido' ~ 0,
         #   TRUE ~ 1.5),
         # cE2_prefcost = case_when(
         #   E2.SQ001. == 'Que tenga bajo costo' ~ 5,
         #   E2.SQ002. == 'Que tenga bajo costo' ~ 4,
         #   E2.SQ003. == 'Que tenga bajo costo' ~ 3,
         #   E2.SQ004. == 'Que tenga bajo costo' ~ 0,
         #   TRUE ~ 1.5),
         # cE2_prefconf = case_when(
         #   E2.SQ001. == 'Que sea confortable' ~ 5,
         #   E2.SQ002. == 'Que sea confortable' ~ 4,
         #   E2.SQ003. == 'Que sea confortable' ~ 3,
         #   E2.SQ004. == 'Que sea confortable' ~ 0,
         #   TRUE ~ 1.5),
         # cE2_prefsec = case_when(
         #   E2.SQ001. == 'Que tenga bajo riesgo de accidente' ~ 5,
         #   E2.SQ002. == 'Que tenga bajo riesgo de accidente' ~ 4,
         #   E2.SQ003. == 'Que tenga bajo riesgo de accidente' ~ 3,
         #   E2.SQ004. == 'Que tenga bajo riesgo de accidente' ~ 0,
         #   TRUE ~ 1.5),
         # cE2_prefexp = case_when(
         #   E2.SQ001. == 'Que pueda viajar relajado' ~ 5,
         #   E2.SQ002. == 'Que pueda viajar relajado' ~ 4,
         #   E2.SQ003. == 'Que pueda viajar relajado' ~ 3,
         #   E2.SQ004. == 'Que pueda viajar relajado' ~ 0,
         #   TRUE ~ 1.5),
         # cE2_prefrel = case_when(
         #   E2.SQ001. == 'Que tenga horarios confiables' ~ 5,
         #   E2.SQ002. == 'Que tenga horarios confiables' ~ 4,
         #   E2.SQ003. == 'Que tenga horarios confiables' ~ 3,
         #   E2.SQ004. == 'Que tenga horarios confiables' ~ 0,
         #   TRUE ~ 1.5),

         cE2_preftime = case_when(
           E2.SQ001. == 'Que llegue r<e1>pido' ~ 1,
           E2.SQ002. == 'Que llegue r<e1>pido' ~ 1,
           TRUE ~ 0),
         cE2_prefcost = case_when(
           E2.SQ001. == 'Que tenga bajo costo' ~ 1,
           E2.SQ002. == 'Que tenga bajo costo' ~ 1,
           TRUE ~ 0),
         cE2_prefconf = case_when(
           E2.SQ001. == 'Que sea confortable' ~ 1,
           E2.SQ002. == 'Que sea confortable' ~ 1,
           TRUE ~ 0),
         cE2_prefsec = case_when(
           E2.SQ001. == 'Que tenga bajo riesgo de accidente' ~ 1,
           E2.SQ002. == 'Que tenga bajo riesgo de accidente' ~ 1,
           TRUE ~ 0),
         cE2_prefexp = case_when(
           E2.SQ001. == 'Que pueda viajar relajado' ~ 1,
           E2.SQ002. == 'Que pueda viajar relajado' ~ 1,
           TRUE ~ 0),
         cE2_prefrel = case_when(
           E2.SQ001. == 'Que tenga horarios confiables' ~ 1,
           E2.SQ002. == 'Que tenga horarios confiables' ~ 1,
           TRUE ~ 0),
                  
         cE3_1 = case_when(
           E3.SQ001. == 'Para nada' ~ 1,
           E3.SQ001. == 'No muy bien' ~ 2,
           E3.SQ001. == 'Algo' ~ 3,
           E3.SQ001. == 'Bien' ~ 4,
           E3.SQ001. == 'Me describe muy bien' ~ 5),
         cE3_2 = case_when(
           E3.SQ002. == 'Para nada' ~ 1,
           E3.SQ002. == 'No muy bien' ~ 2,
           E3.SQ002. == 'Algo' ~ 3,
           E3.SQ002. == 'Bien' ~ 4,
           E3.SQ002. == 'Me describe muy bien' ~ 5),
         cE3_3 = case_when(
           E3.SQ003. == 'Para nada' ~ 1,
           E3.SQ003. == 'No muy bien' ~ 2,
           E3.SQ003. == 'Algo' ~ 3,
           E3.SQ003. == 'Bien' ~ 4,
           E3.SQ003. == 'Me describe muy bien' ~ 5),
         cE3_4 = case_when(
           E3.SQ004. == 'Para nada' ~ 1,
           E3.SQ004. == 'No muy bien' ~ 2,
           E3.SQ004. == 'Algo' ~ 3,
           E3.SQ004. == 'Bien' ~ 4,
           E3.SQ004. == 'Me describe muy bien' ~ 5),
         cE3_5 = case_when(
           E3.SQ005. == 'Para nada' ~ 1,
           E3.SQ005. == 'No muy bien' ~ 2,
           E3.SQ005. == 'Algo' ~ 3,
           E3.SQ005. == 'Bien' ~ 4,
           E3.SQ005. == 'Me describe muy bien' ~ 5),
         cE3_6 = case_when(
           E3.SQ006. == 'Para nada' ~ 1,
           E3.SQ006. == 'No muy bien' ~ 2,
           E3.SQ006. == 'Algo' ~ 3,
           E3.SQ006. == 'Bien' ~ 4,
           E3.SQ006. == 'Me describe muy bien' ~ 5),
         cE3_7 = case_when(          #inverted
           E3.SQ007. == 'Para nada' ~ 5,
           E3.SQ007. == 'No muy bien' ~ 4,
           E3.SQ007. == 'Algo' ~ 3,
           E3.SQ007. == 'Bien' ~ 2,
           E3.SQ007. == 'Me describe muy bien' ~ 1),
         cE3_8 = case_when(
           E3.SQ008. == 'Para nada' ~ 1,
           E3.SQ008. == 'No muy bien' ~ 2,
           E3.SQ008. == 'Algo' ~ 3,
           E3.SQ008. == 'Bien' ~ 4,
           E3.SQ008. == 'Me describe muy bien' ~ 5),
         
         cE4_1 = case_when(
           E4.SQ001. == 'Totalmente en desacuerdo' ~ 1,
           E4.SQ001. == 'Parcialmente en desacuerdo' ~ 2,
           E4.SQ001. == 'Indiferente' ~ 3,
           E4.SQ001. == 'Parcialmente de acuerdo' ~ 4,
           E4.SQ001. == 'Totalmente de acuerdo' ~ 5),
         cE4_2 = case_when(
           E4.SQ002. == 'Totalmente en desacuerdo' ~ 1,
           E4.SQ002. == 'Parcialmente en desacuerdo' ~ 2,
           E4.SQ002. == 'Indiferente' ~ 3,
           E4.SQ002. == 'Parcialmente de acuerdo' ~ 4,
           E4.SQ002. == 'Totalmente de acuerdo' ~ 5),
         cE4_3 = case_when(
           E4.SQ003. == 'Totalmente en desacuerdo' ~ 1,
           E4.SQ003. == 'Parcialmente en desacuerdo' ~ 2,
           E4.SQ003. == 'Indiferente' ~ 3,
           E4.SQ003. == 'Parcialmente de acuerdo' ~ 4,
           E4.SQ003. == 'Totalmente de acuerdo' ~ 5),
         cE4_4 = case_when(
           E4.SQ004. == 'Totalmente en desacuerdo' ~ 1,
           E4.SQ004. == 'Parcialmente en desacuerdo' ~ 2,
           E4.SQ004. == 'Indiferente' ~ 3,
           E4.SQ004. == 'Parcialmente de acuerdo' ~ 4,
           E4.SQ004. == 'Totalmente de acuerdo' ~ 5),
         cE4_5 = case_when(
           E4.SQ005. == 'Totalmente en desacuerdo' ~ 1,
           E4.SQ005. == 'Parcialmente en desacuerdo' ~ 2,
           E4.SQ005. == 'Indiferente' ~ 3,
           E4.SQ005. == 'Parcialmente de acuerdo' ~ 4,
           E4.SQ005. == 'Totalmente de acuerdo' ~ 5),
         cE4_6 = case_when(
           E4.SQ006. == 'Totalmente en desacuerdo' ~ 1,
           E4.SQ006. == 'Parcialmente en desacuerdo' ~ 2,
           E4.SQ006. == 'Indiferente' ~ 3,
           E4.SQ006. == 'Parcialmente de acuerdo' ~ 4,
           E4.SQ006. == 'Totalmente de acuerdo' ~ 5)) %>% 
  
  # Centro las variables
  mutate(across(starts_with("cE"), 
                .fns =  ~ .x - mean(.x, na.rm = TRUE),
                .names = "{.col}_cen")) %>% 
  
  select(ID = idLS,
         tarea, alt, alt_chr, choice_alt, choice,
         time, cost,
         sex = SEX,
         age = AGE,
         distF = DistFast, distS = DistShort,
         modecar, modech, modebus, modetrain,
         caravail, sec,
         base_ca_t, base_cp_t, base_ch_t, base_tr_t, base_ca_c, base_cp_c, base_ch_c, base_tr_c,
         cE1_1, cE1_2, cE1_3, cE1_5, cE1_6,
         cE3_1, cE3_2, cE3_3, cE3_4, cE3_5, cE3_6, cE3_7, cE3_8,
         cE4_1, cE4_2, cE4_3, cE4_4, cE4_5, cE4_6,
         cE2_preftime, cE2_prefcost, cE2_prefconf, cE2_prefsec, cE2_prefexp, cE2_prefrel,
         cE1_1_cen, cE1_2_cen, cE1_3_cen, cE1_5_cen, cE1_6_cen,
         cE3_1_cen, cE3_2_cen, cE3_3_cen, cE3_4_cen, cE3_5_cen, cE3_6_cen, cE3_7_cen, cE3_8_cen,
         cE4_1_cen, cE4_2_cen, cE4_3_cen, cE4_4_cen, cE4_5_cen, cE4_6_cen,
         cE2_preftime_cen, cE2_prefcost_cen, cE2_prefconf_cen, cE2_prefsec_cen, cE2_prefexp_cen, cE2_prefrel_cen,
         NSE, NSE_ord) %>% 
  mutate(male = as.numeric(sex=="Hombre"),
         male_cen = male-mean(male, na.rm=T),
         female = as.numeric(sex=="Mujer"),
         age = as.numeric(2017 - age)/10,   #[decades]
         age_cen = age - mean(age, na.rm=T),
         age2 = age_cen^2, age2_cen = age2 - mean(age2),
         age3 = age_cen^3, age3_cen = age3 - mean(age3), 
         ageyng = as.numeric(age<=3.0), ageyng_cen = ageyng-mean(ageyng, na.rm=T),
         ageold = as.numeric(age>5.0), ageold_cen = ageold-mean(ageold, na.rm=T),
         income = 6 - NSE_ord,
         income_cen = income - mean(income, na.rm=T),
         modecar_cen = modecar - mean(modecar),
         modech_cen = modech - mean(modech),
         modebus_cen = modebus - mean(modebus),
         modetrain_cen = modetrain - mean(modetrain),
         dist = distF/100,   #[10km]
         dist_cen = dist - mean(dist, na.rm=T),
         dist2 = dist_cen^2,
         caravail_cen = caravail - mean(caravail),
         sec_cen = sec - mean(sec)    ) %>% 

  arrange(ID, tarea, alt) %>% 
  pivot_wider(id_cols =
                c(ID, tarea, sex, age, distF, distS,
                  male, male_cen, female,
                  age_cen, age2_cen, age3_cen,
                  ageyng, ageyng_cen, ageold, ageold_cen,
                  NSE_ord, income, income_cen,
                  modecar, modech, modebus, modetrain,
                  modecar_cen, modech_cen, modebus_cen, modetrain_cen,
                  dist, dist_cen, dist2,
                  caravail, caravail_cen,
                  sec, sec_cen,
                  cE1_1, cE1_2, cE1_3, cE1_5, cE1_6,
                  cE3_1, cE3_2, cE3_3, cE3_4, cE3_5, cE3_6, cE3_7, cE3_8,
                  cE4_1, cE4_2, cE4_3, cE4_4, cE4_5, cE4_6,
                  cE2_preftime, cE2_prefcost, cE2_prefconf, cE2_prefsec, cE2_prefexp, cE2_prefrel,
                  cE1_1_cen, cE1_2_cen, cE1_3_cen, cE1_5_cen, cE1_6_cen,
                  cE3_1_cen, cE3_2_cen, cE3_3_cen, cE3_4_cen, cE3_5_cen, cE3_6_cen, cE3_7_cen, cE3_8_cen,
                  cE4_1_cen, cE4_2_cen, cE4_3_cen, cE4_4_cen, cE4_5_cen, cE4_6_cen,
                  cE2_preftime_cen, cE2_prefcost_cen, cE2_prefconf_cen, cE2_prefsec_cen, cE2_prefexp_cen, cE2_prefrel_cen),
              names_from = alt_chr, 
              values_from = c(time, cost, choice)) %>% 
  mutate(choice = 1*choice_ca+2*choice_cp+3*choice_ch+4*choice_tr) %>% 
  select(-choice_ca, -choice_cp, -choice_ch, -choice_tr) 

database <- as.data.frame(dat)
write.table(database,"cpDem_wide_epanel.txt",sep='\t',dec='.')
rm(dat, dat0, NSE_CarPooling)






#+++++++ Data verification

# mean(database$time_ca)
# mean(database$time_cp)
# mean(database$time_ch)
# mean(database$time_tr)
# mean(database$cost_ca)
# mean(database$cost_cp)
# mean(database$cost_ch)
# mean(database$cost_tr)

# mean(database$male_cen)
# mean(database$age)
# mean(database$age2)
# mean(database$age3)
# mean(database$income_cen)
# mean(database$dist)
                         
# head(database$age,100)
# class(database$age)

#XL = subset(database,select=c(ID,age,age2,age3,male,dist,dist2,income))
#det(cor(XL))
#XW <- XL[!duplicated(XL[,c('ID')]),]
#det(cor(XW))

#XL = subset(database,select=c(ID,cE1_1,cE1_2,cE1_3,cE1_5,cE1_6))
#table(XL[,6])


#AU1   # N/A, No, S<ed>
#AU2.0   # No, S<ed>
#AU3   # 1, 2, 3, M<e1>s  (es un 1)
# if(AU1!='No',1,0) * if(AU2.0!='S<ed>,AU3,0)
#CD4
#cE2_conf
#cE2_bajoriesgo
#cE2_relax
#cE2_horconf

# length(dat$drivelicense[is.na(dat$drivelicense)])
# length(dat$ncar[is.na(dat$ncar)])
# length(dat$caravail[is.na(dat$caravail)])

#X = subset(dat,select=c(ID,AU1,AU2.0.,AU3,drivelicense,ncar,caravail))
#XI <- X[!duplicated(X[,c('ID')]),]
#table(XI$ncar,XI$caravail)

# datdemog = subset(database,select=c(caravail,sec,cE2_prefconf,cE2_prefsec,cE2_prefrel,cE2_prefexp))
# R = cor(datdemog)
# det(R)

#hist(dat$cE2_prefconf)
#hist(dat$cE2_prefsec)
#hist(dat$cE2_prefrel)
#hist(dat$cE2_prefexp)

#table(dat0$E2.SQ001.)/60

# costca = dat0$cost[dat0$alt==1]
# costcp = dat0$cost[dat0$alt==2]
# costpk = dat0$ParkCost[dat0$alt==1]
# bcostca = dat0$base_ca_c[dat0$alt==1]
# bcostveh = bcostca-costpk/2   #vechicle cost
# hist(costca/bcostca)
# hist(bcostveh)
# hist(round(costcp/bcostveh,1),breaks=(0:20)/20)





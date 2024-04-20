library(dplyr)

setwd("C:/Users/emiba/OneDrive - UCA/Documentos/Investigador UCA/Carpooling BsAs/Scripts/PreparacionDatosOferta_epanel")

dat = read.table("cpSup_wide_epanel.txt", header=T, sep="",dec=".")
NSE<-read.table(file="NSE.txt",sep="\t",header=T)

####Unir NSE con dat####
########################

dat<- dat%>%
  left_join(NSE, by="idLS")


####Crear Variables###
######################

dat$Mujer[dat$SEX=="Hombre"]<-0;dat$Mujer[dat$SEX=="Mujer"]<-1
dat$EDAD <- 2017- dat$AGE
                       

dat$PasionAuto <- as.numeric(dat$E1.SQ001.)- dat$E1.SQ002.+ dat$E1.SQ003.-dat$E1.SQ005.+ dat$E1.SQ006.

dat$soc1[dat$E3.SQ001. =="Me describe muy bien"] <- 5
dat$soc1[dat$E3.SQ001. =="Bien"] <- 4
dat$soc1[dat$E3.SQ001. =="Algo"] <- 3
dat$soc1[dat$E3.SQ001. =="No muy bien"] <- 2
dat$soc1[dat$E3.SQ001. =="Para nada"] <- 1
dat$soc2[dat$E3.SQ002. =="Me describe muy bien"] <- 5
dat$soc2[dat$E3.SQ002. =="Bien"] <- 4
dat$soc2[dat$E3.SQ002. =="Algo"] <- 3
dat$soc2[dat$E3.SQ002. =="No muy bien"] <- 2
dat$soc2[dat$E3.SQ002. =="Para nada"] <- 1
dat$soc3[dat$E3.SQ003. =="Me describe muy bien"] <- 5
dat$soc3[dat$E3.SQ003. =="Bien"] <- 4
dat$soc3[dat$E3.SQ003. =="Algo"] <- 3
dat$soc3[dat$E3.SQ003. =="No muy bien"] <- 2
dat$soc3[dat$E3.SQ003. =="Para nada"] <- 1
dat$soc4[dat$E3.SQ004. =="Me describe muy bien"] <- 5
dat$soc4[dat$E3.SQ004. =="Bien"] <- 4
dat$soc4[dat$E3.SQ004. =="Algo"] <- 3
dat$soc4[dat$E3.SQ004. =="No muy bien"] <- 2
dat$soc4[dat$E3.SQ004. =="Para nada"] <- 1
dat$soc5[dat$E3.SQ005. =="Me describe muy bien"] <- 5
dat$soc5[dat$E3.SQ005. =="Bien"] <- 4
dat$soc5[dat$E3.SQ005. =="Algo"] <- 3
dat$soc5[dat$E3.SQ005. =="No muy bien"] <- 2
dat$soc5[dat$E3.SQ005. =="Para nada"] <- 1
dat$soc6[dat$E3.SQ006. =="Me describe muy bien"] <- 5
dat$soc6[dat$E3.SQ006. =="Bien"] <- 4
dat$soc6[dat$E3.SQ006. =="Algo"] <- 3
dat$soc6[dat$E3.SQ006. =="No muy bien"] <- 2
dat$soc6[dat$E3.SQ006. =="Para nada"] <- 1
dat$soc7[dat$E3.SQ007. =="Me describe muy bien"] <- 5
dat$soc7[dat$E3.SQ007. =="Bien"] <- 4
dat$soc7[dat$E3.SQ007. =="Algo"] <- 3
dat$soc7[dat$E3.SQ007. =="No muy bien"] <- 2
dat$soc7[dat$E3.SQ007. =="Para nada"] <- 1
dat$soc8[dat$E3.SQ008. =="Me describe muy bien"] <- 5
dat$soc8[dat$E3.SQ008. =="Bien"] <- 4
dat$soc8[dat$E3.SQ008. =="Algo"] <- 3
dat$soc8[dat$E3.SQ008. =="No muy bien"] <- 2
dat$soc8[dat$E3.SQ008. =="Para nada"] <- 1


dat$Individualidad <- dat$soc1 - dat$soc2 + dat$soc3 - dat$soc4 + dat$soc5 - dat$soc6 + dat$soc7 - dat$soc8

dat$eco1[dat$E4.SQ001. =="Totalmente de acuerdo"] <- 5
dat$eco1[dat$E4.SQ001. =="Parcialmente de acuerdo"] <- 4
dat$eco1[dat$E4.SQ001. =="Indiferente"] <- 3
dat$eco1[dat$E4.SQ001. =="Parcialmente en desacuerdo"] <- 2
dat$eco1[dat$E4.SQ001. =="Totalmente en desacuerdo"] <- 1
dat$eco2[dat$E4.SQ002. =="Totalmente de acuerdo"] <- 5
dat$eco2[dat$E4.SQ002. =="Parcialmente de acuerdo"] <- 4
dat$eco2[dat$E4.SQ002. =="Indiferente"] <- 3
dat$eco2[dat$E4.SQ002. =="Parcialmente en desacuerdo"] <- 2
dat$eco2[dat$E4.SQ002. =="Totalmente en desacuerdo"] <- 1
dat$eco3[dat$E4.SQ003. =="Totalmente de acuerdo"] <- 5
dat$eco3[dat$E4.SQ003. =="Parcialmente de acuerdo"] <- 4
dat$eco3[dat$E4.SQ003. =="Indiferente"] <- 3
dat$eco3[dat$E4.SQ003. =="Parcialmente en desacuerdo"] <- 2
dat$eco3[dat$E4.SQ003. =="Totalmente en desacuerdo"] <- 1
dat$eco4[dat$E4.SQ004. =="Totalmente de acuerdo"] <- 5
dat$eco4[dat$E4.SQ004. =="Parcialmente de acuerdo"] <- 4
dat$eco4[dat$E4.SQ004. =="Indiferente"] <- 3
dat$eco4[dat$E4.SQ004. =="Parcialmente en desacuerdo"] <- 2
dat$eco4[dat$E4.SQ004. =="Totalmente en desacuerdo"] <- 1
dat$eco5[dat$E4.SQ005. =="Totalmente de acuerdo"] <- 5
dat$eco5[dat$E4.SQ005. =="Parcialmente de acuerdo"] <- 4
dat$eco5[dat$E4.SQ005. =="Indiferente"] <- 3
dat$eco5[dat$E4.SQ005. =="Parcialmente en desacuerdo"] <- 2
dat$eco5[dat$E4.SQ005. =="Totalmente en desacuerdo"] <- 1
dat$eco6[dat$E4.SQ006. =="Totalmente de acuerdo"] <- 5
dat$eco6[dat$E4.SQ006. =="Parcialmente de acuerdo"] <- 4
dat$eco6[dat$E4.SQ006. =="Indiferente"] <- 3
dat$eco6[dat$E4.SQ006. =="Parcialmente en desacuerdo"] <- 2
dat$eco6[dat$E4.SQ006. =="Totalmente en desacuerdo"] <- 1


dat$ecologia <-  -dat$eco1 + dat$eco2 - dat$eco3 + dat$eco4 - dat$eco5 + dat$eco6 


#Variable de frec uso
dat$DiasxMes_uso_auto[dat$AU7=="Todos los d<ed>as"]<-1
dat$DiasxMes_uso_auto[dat$AU7=="Todos los d<ed>as h<e1>biles"]<-20/28
dat$DiasxMes_uso_auto[dat$AU7=="2 o 3 d<ed>as por semana"]<-2.5*4/28
dat$DiasxMes_uso_auto[dat$AU7=="1 d<ed>a por semana"]<-4/28
dat$DiasxMes_uso_auto[dat$AU7=="1 d<ed>a cada 2 semanas"]<-2/28
dat$DiasxMes_uso_auto[dat$AU7=="1 d<ed>a por mes"]<-1/28
dat$DiasxMes_uso_auto[dat$AU7=="Menos frecuentemente"]<-0
dat$DiasxMes_uso_auto[is.na(dat$DiasxMes_uso_auto)] <- 1


dat$Viajes_por_Semana = 0;
dat$Viajes_por_Semana[ dat$RP01 =="1 vez por semana"] = 1/7
dat$Viajes_por_Semana[ dat$RP01 =="2 o 3 veces por semana"] = 2.5/7
dat$Viajes_por_Semana[ dat$RP01 =="4 veces por semana"] = 4/7
dat$Viajes_por_Semana[ dat$RP01 =="5 veces por semana"] = 5/7
dat$Viajes_por_Semana[ dat$RP01 =="6 o 7 veces por semana"] = 6.5/7

dat$DistFast=dat$DistFast/10

#########Eliminar Columnas####
#############################   


dat<- dat %>%
  select(idLS: base_cp_comp,
         RP03, RP04,
         E2.SQ001.,SEL3, 
         time_au:PasionAuto,
         Individualidad,ecologia,DiasxMes_uso_auto, Viajes_por_Semana, DistFast )   %>%
  
  rename(  Medio_Favorito= RP03,
           Motivo_Viaje = RP04,
           Pref_ppal = E2.SQ001.,
           EduacDelSosten =SEL3,
           NSE = NSE.y,
           Edad = EDAD
           )

write.table(dat,file = "OfertaEpanel.txt",sep="\t",dec=".")



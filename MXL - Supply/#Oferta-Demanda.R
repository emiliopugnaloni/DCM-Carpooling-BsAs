library(dplyr)
setwd("C:/Users/emiba/OneDrive - UCA/Documentos/Investigador UCA/Carpooling BsAs/DataNew")

#### Oferta ####
dat<- read.table("cpSup_wide_epanel.txt",dec=".")

SupplyPorc <- dat%>%
  select("idLS","RP03") %>%
  rename("Modo_de_Viaje"="RP03") %>%
  distinct() %>%
  group_by(Modo_de_Viaje) %>%
  summarise(Cantidad = n())

#### Demanda ####
dat<- read.table("cpDem_wide_epanel.txt",dec=".")

DemPorc <- dat%>%
  select("idLS","RP03") %>%
  rename("Modo_de_Viaje"="RP03") %>%
  distinct() %>%
  group_by(Modo_de_Viaje) %>%
  summarise(Cantidad = n())
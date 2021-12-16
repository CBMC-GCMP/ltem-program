library(readxl)
library(tidyverse)

#IDReef check
base_inv <- read_xlsx("data/ltem_2021_update_inv_data.xlsx", sheet="Captura de datos ")

list_reef <- read.csv("data/server/ltem_monitoring_reefs.csv")


#Checar Que los IDReef de la base correspondan con el nombre de la lista
merge <- merge(base_inv, list_reef[, c("IDReef", "Reef" )], by="IDReef", all.x=T) %>% 
  rename(Correct= Reef.y,
         Reef= Reef.x) %>% 
  mutate(Flag= ifelse(Reef==Correct, "Correct", "Wrong" ))

#Filtrar los arrefices que no correspondan

#Corrobar el error y corregir

#Exportar la base corregida


library(googlesheets4)
library(tidyverse)
library(readxl)
library(plyr)


db <- read_xlsx("ltem_database_07122021_original.xlsx", sheet= 1)


transects <- db %>% 
  group_by(Region) %>% 
  select(Label, Year, Month, Day, IDReef, Reef, Depth, Transect, observador) %>% 
  arrange(Year, Month,Day, IDReef, Reef, Depth,  Transect) %>% 
  unique()


### ----

CSL_fish <- transects %>%
  filter(Region=="Los Cabos") %>% 
  filter(Label=="PEC")

CSL_inv <- transects %>% 
  filter(Region=="Los Cabos") %>% 
  filter(Label=="INV")  


### ---

CP_fish <- transects %>%
  filter(Region=="Cabo Pulmo") %>% 
  filter(Label=="PEC")

CP_inv <- transects %>% 
  filter(Region=="Cabo Pulmo") %>% 
  filter(Label=="INV")  

### ----


LRT_fish <- transects %>%
  filter(Region=="Loreto") %>% 
  filter(Label=="PEC")

LRT_inv <- transects %>% 
  filter(Region=="Loreto") %>% 
  filter(Label=="INV")  

### ----




CSL_merge <- merge(CSL_inv, CSL_fish, by=c("Region", "Year", "Month", "Day", "IDReef" ,"Reef", "Depth", "Transect"), all=T )

CSL_filter <- CSL_merge %>% 
  # mutate(Flag= ifelse(Label.x == "INV", "correct", "wrong"))
  mutate(Flag= ifelse(is.na(Label.x), "wrong_INV", "correcto"), Flag= ifelse(is.na(Label.y), "wrong_PEC", Flag))
  
  

### ---


CP_merge <- merge(CP_inv, CP_fish, by=c("Region", "Year", "Month", "Day", "IDReef" ,"Reef", "Depth", "Transect"), all=T )

CP_filter <- CP_merge %>% 
  # mutate(Flag= ifelse(Label.x == "INV", "correct", "wrong"))
  mutate(Flag= ifelse(is.na(Label.x), "wrong_INV", "correcto"), Flag= ifelse(is.na(Label.y), "wrong_PEC", Flag))

### ---

LRT_merge <- merge(LRT_inv, LRT_fish, by=c("Region", "Year", "Month", "Day", "IDReef" ,"Reef", "Depth", "Transect"), all=T )

LRT_filter <- CP_merge %>% 
  # mutate(Flag= ifelse(Label.x == "INV", "correct", "wrong"))
  mutate(Flag= ifelse(is.na(Label.x), "wrong_INV", "correcto"), Flag= ifelse(is.na(Label.y), "wrong_PEC", Flag))


### ---

regiones_rbind = rbind(CSL_filter, CP_filter, LRT_filter)

### ---

Trns_INV_Wrong <- regiones_rbind %>% 
  filter(Flag== "wrong_INV")

Trans_PEC_Wrong <- regiones_rbind %>% 
filter(Flag== "wrong_PEC")


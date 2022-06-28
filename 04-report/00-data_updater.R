library(googlesheets4)
library(tidyverse)

source("00-database_connect/00-dbconnect-source.R")

#Corrected db
# db <- read_sheet("https://docs.google.com/spreadsheets/d/1vVHFQEhvtvmhh5u6GghbUBXvYYR-q-Jc7gnQTF7Zias/edit#gid=2040316618")
db <- readxl::read_xlsx("data/raw/ltem_database_07122021_original.xlsx")

dbListTables(ltem_db) #Generates a list of tables

#Select Species and Reefs List from server

spp <- tbl(ltem_db,
           "ltem_monitoring_species")
reefs <- tbl(ltem_db,
             "ltem_monitoring_reefs")

# Merge with LTEM db

db <- merge(db, spp, by = c("IDSpecies", "Species", "Label"))

db <- merge(db, reefs, by = c("Region", "IDReef", "Reef"))

#Add biomass and standarize columns

db <- db %>% 
  mutate(A_ord = as.numeric(A_ord), 
         B_pen = as.numeric(B_pen), 
         Biomass = (Quantity * A_ord * (Size^B_pen))/(Area * 100)) %>% 
  mutate(TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                          "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region, 
                         levels = c("Loreto", "Cabo Pulmo", "Los Cabos")),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)
  ) %>% 
  mutate(Depth2= case_when( Depth <= 10 ~ "Shallow",
                            Depth >= 15 ~ "Deep"),
         Degree= round(Latitude,0),
         Genus=Species) %>%
  separate(Genus, c("Genus", NA), sep=" ", fill="left")%>% 
  
  select( -observador) %>% 
  arrange(Label, Year, Region, Reef, Transect, Depth, Species)


#Save current LTEM db
saveRDS(db, "04-report/outputs/ltem_output_base.RDS")



# Update Historical db ----------------------------------------------------

#Load LTEM historical db

histor <- tbl(ltem_db,
                       "ltem_monitoring_database") %>% 
  collect()

#Correct Historical Reefs

source("00-functions/01-format_check.R")
source("00-functions/00-flags.R")

reefs <-  read.csv("data/lists/ltem_monitoring_reefs.csv")

histor <- reefsid(histor, reefs)
histor <- reefsname(histor,reefs)%>% 
  select(-c(correct_id, IDBefore, ReefBefore,correct_reef, Status)) %>% 
  mutate(Longitude= case_when(Reef=="ESPIRITU_SANTO_MORRITOS"~ -110.3134, 
                              Reef=="BAJO_SECO"~-101.640128,
                              TRUE~Longitude),
         Latitude=case_when(Reef=="ESPIRITU_SANTO_MORRITOS"~ 24.42108, 
                            Reef=="BAJO_SECO"~17.6435891,
                            TRUE~Latitude))



#Add Reefs metadata
histor<- merge(histor, reefs, by = c("Island" ,  
                                      "Protection_status", "Region", 
                                      "IDReef", "Reef"), all.x = T) %>% 
  select(-main_id) %>% 
  mutate(Latitude= ifelse(Latitude.x==Latitude.y, Latitude.x, Latitude.y),
         Longitude= ifelse(Longitude.x==Longitude.y, Longitude.x, Longitude.y)) %>% 
  select(-c(Latitude.x,Longitude.x,Latitude.y , Longitude.y ))



# Add new entries
updated <- rbind(histor, db) 


#Save updated LTEM db
saveRDS(updated, "04-report/outputs/LTEM_historic_updated.RDS")

write.csv(updated, "04-report/outputs/LTEM_historic_updated.csv")
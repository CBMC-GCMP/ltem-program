library(googlesheets4)
library(tidyverse)

source("00-database_connect/00-dbconnect-source.R")

#Corrected db
# db <- read_sheet("https://docs.google.com/spreadsheets/d/1vVHFQEhvtvmhh5u6GghbUBXvYYR-q-Jc7gnQTF7Zias/edit#gid=2040316618")

db <- readxl::read_xlsx("data/clean/2022/ltem_SB_29072022.xlsx")

dbListTables(ltem_db) #Generates a list of tables

#Select Species and Reefs List from server
spp <-  file.info(list.files("data/lists/updates/", full.names = T))

spp <- readxl::read_xlsx(rownames(spp)[which.max(spp$mtime)]) %>% 
  select(-valid_AphiaID)


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
         Region = factor(Region),
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
  
  select( -Observer) %>% 
  arrange(Label, Year, Region, Reef, Transect, Depth, Species)


#Save current LTEM db
saveRDS(db, "04-report/outputs/ltem_SB_29062022.RDS")



# Update Historical db ----------------------------------------------------

#Load historical db

source("04-report/00-historical_data_updater.R")



# Add new entries
updated <- rbind(ltem_historical_updated, db) 


#Save updated LTEM db
saveRDS(updated, "04-report/outputs/LTEM_historic_updated_29062022.RDS")


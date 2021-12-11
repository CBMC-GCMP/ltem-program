library(googlesheets4)
library(tidyverse)

source("database_connect/00-dbconnect-source.R")

db <- read_sheet("https://docs.google.com/spreadsheets/d/1vVHFQEhvtvmhh5u6GghbUBXvYYR-q-Jc7gnQTF7Zias/edit#gid=2040316618")


dbListTables(ltem_db) #Generates a list of tables

spp <- tbl(ltem_db,
           "ltem_monitoring_species")
reefs <- tbl(ltem_db,
             "ltem_monitoring_reefs")
glimpse(db)

db <- merge(db, spp, by = c("IDSpecies", "Species", "Label"))

db <- merge(db, reefs, by = c("Region", "IDReef", "Reef"))

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
  select(Label, Year, Month, Day, Region, Island, IDReef, Latitude, Longitude, Protection_level, Protection_status, 
         MPA, Location, Fishery, Type, Reef, Depth, Transect, Phylum:Functional_groups, TrophicLevelF, IDSpecies, Species, 
         Area, Size, Quantity, Biomass, observador) %>% 
  arrange(Label, Year, Region, Reef, Transect, Depth, Species)


saveRDS(db, "report/outputs/ltem_output_base.RDS")


## Historical 

histor <- tbl(ltem_db,
                       "ltem_monitoring_database") %>% 
  filter(Region %in% c("Loreto", "Los Cabos", "Cabo Pulmo")) %>% 
  collect()


histor <- merge(histor, reefs, by = c("Island" , "Latitude", "Longitude" , "Protection_status", "Region", "IDReef", "Reef"))

## Saving main regions extract
saveRDS(histor, "report/outputs/cls_lor_cp_extract.RDS")


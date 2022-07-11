library(tidyverse)


# Load data ---------------------------------------------------------------

#Load custom functions
source("R/historical_names_correction.R")
source("R/flags.R")
source("R/format_check.R")

# Connect to server database
source("00-database_connect/00-dbconnect-source.R")


#Load LTEM historical db
histor <- tbl(ltem_db,
              "ltem_monitoring_database") %>% 
  collect()

#Load Reefs list
reefs <- tbl(ltem_db,
             "ltem_monitoring_reefs")




# Reef correction ---------------------------------------------------------

#Correct Reefs IDs
histor <- reefsid(histor, reefs)

#Correct Reefs Names
histor <- reefsname(histor,reefs)%>% 
  select(-c(correct_id, IDBefore, ReefBefore,correct_reef, Status)) %>% 
  mutate(Longitude= case_when(Reef=="ESPIRITU_SANTO_MORRITOS"~ -110.3134, 
                              Reef=="BAJO_SECO"~-101.640128,
                              TRUE~Longitude),
         Latitude=case_when(Reef=="ESPIRITU_SANTO_MORRITOS"~ 24.42108, 
                            Reef=="BAJO_SECO"~17.6435891,
                            TRUE~Latitude))



# Add Reefs metadata
histor<- merge(histor, reefs, by = c("Island" ,  
                                     "Protection_status", "Region", 
                                     "IDReef", "Reef"), all.x = T) %>% 
  select(-main_id) %>% 
  mutate(Latitude= ifelse(Latitude.x==Latitude.y, Latitude.x, Latitude.y),
         Longitude= ifelse(Longitude.x==Longitude.y, Longitude.x, Longitude.y)) %>% 
  select(-c(Latitude.x,Longitude.x,Latitude.y , Longitude.y ))



# Species names correction ------------------------------------------------


spp <-  file.info(list.files("data/lists/updates/", full.names = T))
spp <- readxl::read_xlsx(rownames(spp)[which.max(spp$mtime)] )

historical <- speciesid(histor, spp)
historical <- speciesnames(historical,spp)

ltem_historical_updated <- historical %>% 
  select(-c(correct_id,IDBefore, SpeciesBefore, correct_sp, Status))

rm(reefs, histor, historical, spp)


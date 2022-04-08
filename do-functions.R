## Load custom functions

source("00-functions/to-do-functions.R")


#Load monitoring database
ltem <- read_xlsx()

#Load species list
spp <- read_xlsx()

#Load reefs list
reefs <- tbl(ltem_db,
             "ltem_monitoring_reefs")


#Merge monitoring database with species catalog

mg_db <- mg_spp (db, spp)

#merge 

merge_reef <-mg_reef (db, reefs)

#Fish Biomass

biomass_ltem <- biomass (ltem)

#Invertebrate Abundance

abundance_db <- abundance (db_sf)
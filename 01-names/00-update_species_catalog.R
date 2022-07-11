library(tidyverse)
library(readxl)



# FISH --------------------------------------------------------------------
## Load custom functions

lapply(list.files(path="R/", pattern = ".R", full.names = T), source)

#Peripheral list of species (PLoS)
species_list <- read_xlsx("data/lists/ltem_monitoring_species.xlsx")



### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Fish data

#Function used: clean_spp()

clean_md <-clean_spp (species_list, "fish")

# Scientific Names Correction 

# Connect to WoRMS and fishbase API using complentary functions
# Function resolve_names() searches for species 
# scientific names correct spelling

resolved_names <- resolve_names(clean_md, "fish")



# Names Validation 

# Validates current scientific names and automatically updates them in our PLoS

# Function used: clean_validation()

fish_validated <- clean_validation(clean_md, resolved_names, species_list, "fish")

rm(clean_md, resolved_names)


# INVERTEBRATES -----------------------------------------------------------

#Peripherical List of Species (PLoS), with fish sci-names corrected

clean_spp <- clean_spp(fish_validated, "inv")


#Check possible misspellings in PLoS
resolved_names <- resolve_names(clean_spp, "inv")

#Merge with PLoS replacing sci-names, and generating flags
clean_sp_list <- clean_validation(clean_spp, resolved_names, Label="inv")





# WoRMS validation 

#Exploratory checkup of sci-names, in case manual corrections necessary 

wormsID <- check_worms(clean_sp_list)

## If any scientific is still unaccepted, we can manually check its status

#Console: If manual input is required, always select the species entry
#with an Accepted Status



## All species that display INVALID status, require manual correction.
## You can do so by replacing the old scientific name string, with a new one:
## For example:
clean_sp_list <- clean_sp_list %>%
  mutate(Species = str_replace_all(Species, "Hyotissa solida",
                                   "Hyotissa hyotis")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Echinaster tenuispina",
                                   "Echinaster+(Othilia)+tenuispina")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Holothuria leucospilota",
                                   "Holothuria+(Mertensiothuria)+leucospilota")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Mycale ramulosa",
                                   "Mycale+(Zygomycale)+ramulosa")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Thais planospira",
                                   "Thais+(Tribulus)+planospira"))


# The replacements above may vary, always check for new invalid species

## Re execute this line, if all species are displayed in green, we can proceed

wormsID <- check_worms(clean_sp_list)



## If all species displayed a VALID status, we then retrieve updated sci-names
## from WoRMS, and replace them in our PLoS

ltem_species <- worms_format(wormsID, clean_spp, fish_validated)

rm(clean_sp_list,clean_spp,fish_validated,resolved_names, species_list, wormsID)



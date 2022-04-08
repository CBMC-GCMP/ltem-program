library(tidyverse)
library(readxl)
library(stringr)

# Load data ---------------------------------------------------------------

## Load custom functions


#Ltem from fish scientific name correction:
source("03-names/01-fish_scientific_names.R")



### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
### Filtering just Invertebrate data

#Peripherical List of Species (PLoS), with fish sci-names corrected

clean_spp <- clean_spp(fish_validated, "inv")


#Check possible misspellings in PLoS
resolved_names <- resolve_names(clean_spp, "inv")

#Merge with PLoS replacing sci-names, and generating flags
clean_sp_list <- clean_validation(clean_spp, resolved_names, inv_metadata, "inv")





# WoRMS validation --------------------------------------------------------

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

inv_validated <- worms_format(wormsID, clean_spp, fish_validated)


# LTEM database correction ------------------------------------------------

# Correction of possible errors in IDSpecies
ltem <- speciesid(ltem, inv_validated, "inv")

# Correction of species names mispellings
ltem <- speciesnames(ltem, inv_validated, "inv")


# Generate report for modified IDSpecies or Species (Optional)
# test <-   flags(ltem)





# END ---------------------------------------------------------------------







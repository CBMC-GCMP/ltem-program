library(tidyverse)
library(readxl)
library(stringr)



# Load data ---------------------------------------------------------------

## Load custom functions

source("00-functions/03-02-inv_scientific_names.R")

#Ltem from fish scientific name correction:
source("03-names/01-fish_scientific_names.R")

#Peripheral list of species (PLoS)
inv_metadata <- read.csv ("data/inv_monitoring_species.csv")


### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Invertebrate data

#PLoS
clean <- clean_inv(inv_metadata)

#Monitoring data
clean_data <- clean_data(ltem)


# Scientific Names Correction ---------------------------------------------

### First step is to correct the spelling

##We load WoRMS as our source

sources <- c(worms = 9)

##Then, we apply a taxsize function: gnr_resolve, for matching correct
## spellings of Scientific Names, in three parts:

names_resolved <- names(resolved_names)

## We merge our resolved_names with our clean PLoS (clean_md)

merge <- merge_clean_resolved (clean_md, resolved_names)

# First Checkpoint --------------------------------------------------------


## If any scientific is still unaccepted, we can manually check its status

## Scientific names' status: Must be executed individually

spp_inv <- data.frame( IDSpecies= clean_md$Species, Species= stringr::str_replace(clean_md$Species," ", "+") )

wormsID <- taxize:: get_wormsid(spp_inv$Species)

#Console: If manual input is required, always select the species entry
#with an Accepted Status


## All the other species which are not valid, require manual correction.
## You can do so by replacing the old scientific name string, with a new one:

clean_md <- spp_inv %>%
mutate(Species = str_replace_all(Species, "Hyotissa solida",
                                 "Hyotissa hyotis")) %>% 
  mutate(Species = str_replace_all(Species,
                                 "Echinaster tenuispina",
                                 "Echinaster (Othilia) tenuispina")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Holothuria leucospilota",
                                   "Holothuria (Mertensiothuria) leucospilota")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Mycale ramulosa",
                                   "Mycale (Zygomycale) ramulosa")) %>% 
  mutate(Species = str_replace_all(Species,
                                 "Thais planospira",
                                 "Thais (Tribulus) planospira"))


# The replacements above may vary, always check for new invalid species

## Re execute this line, if all species are displayed in green, we can proceed

wormsID <- taxize:: get_wormsid(clean_md$Species)




# Validate scientific names with WoRMS IDs --------------------------------
library(plyr)
library(httr)
library(worms)

## For scientific names validation, we need the AphiaIDs from WoRMS servers
## This IDs will help us check if a scientific neame is currently unaccepted,
## and will give us the valid one

# First Step: We change the format of the WoRMS IDs generated in previous section
wormsID <- worms_format (wormsID)


## We clean our worms_names df, and add our IDSpecies

# Second Checkpoint -------------------------------------------------------

## We merge our valid_names with our clean PLoS

#Manual check: Wrong Spellings
#view(merge_md)

##If there are no conflicts in replacing resolved scientific names, continue:

##We can now update our clean PLoS with the correct scientific names:

validate_names <- validate_names (worms_names)


# Third Checkpoint --------------------------------------------------------

##We merge the validated scientific names in the PLoS, with our monitoring
## database
merge_valid <- mg_df (clean_df, clean_md)


##Manual Check: All "Pending" species must be verified, and should only
## contain errors in spelling or in updates of the scientific name.

#view(merge_df)


#If no issues are displayed, proceed:





# Replacing species in the monitoring database ----------------------------

## Finally, we use a match to correct scientific names with the valid ones
## from our PLoS in the complete monitoring database

ltem_inv <- inv_ltem (names_inv, ltem)


# END ---------------------------------------------------------------------



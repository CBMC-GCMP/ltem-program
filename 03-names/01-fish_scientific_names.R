library(readxl)
library(rfishbase)
library(tidyverse)



# Load data ---------------------------------------------------------------
## Load custom functions

source("00-functions/03-01-fish_scientific_names.R")

#Monitoring Data
ltem_fish<- read_xlsx ("data/drive/ltem_database_07122021.xlsx", 
                       sheet= 1) %>% 
  rowid_to_column( "ID")

#Peripheral list of species (PLoS)
metadata_fish <- read.csv ("data/lists/ltem_monitoring_species.csv")



### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Fish data

clean <-clean_fish(ltem_fish)
clean_md <-clean_metadata (metadata_fish)

# Scientific Names Correction ---------------------------------------------

##It is easier to work with the PLoS

### First step is to correct the spelling

##We load WoRMS and Fishbase as our sources
sources <- c(worms = 9, fishbase = 155)


##Then, we apply a taxsize function: gnr_resolve, for matching correct
## spellings of Scientific Names, in three parts:

#Retrieve best species matches from WoRMS and Fishbase

#Leave only the correct results, and format it as data.frame

#Filter absent values (NAs)

names_resolved <- names(resolved_names)


# First Checkpoint --------------------------------------------------------

## We merge our resolved_names with our clean PLoS (clean_md)

merge <- merge_clean_resolved(clean_md, resolved_names)


#Manual check: Wrong Spellings
#view(merge_md)

##If there are no conflicts in replacing resolved scientific names, continue:

##We can now update our clean PLoS with the correct scientific names:




# Second Checkpoint: Valid names ------------------------------------------

## To identify obsolete species names, we can validate them with:
#Manual check: Are the updated names correct?
#view(valid_names)
#If there are no issues with the validated names, once again
# we update our PLoS:

valid_names <- validate_names(clean_md)


# Third Checkpoint: Correct spellings in the monitoring database ----------

##We merge the validated scientific names in the PLoS, with our monitoring
## database

##Manual Check: All "Pending" species must be verified, and should only
## contain errors in spelling or in updates of the scientific name.
#view(merge_df)

#If no issues are displayed, proceed:


merge_valid <- mg_df (clean_df, clean_md)

# Replacing species in the monitoring database ----------------------------

## Finally, we use a match to correct scientific names with the valid ones
## from our PLoS in the complete monitoring database

ltem_fish <- fish_ltem (names_fish, ltem_fish)



# END ---------------------------------------------------------------------



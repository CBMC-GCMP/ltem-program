library(readxl)
library(tidyverse)



# Load data ---------------------------------------------------------------


## Load custom functions

source("00-functions/scientific_names.R")

#Monitoring Data
ltem<- read_xlsx ("data/drive/ltem_database_07122021.xlsx", 
                       sheet= 1) 
#Peripheral list of species (PLoS)
species_list <- read.csv ("data/lists/ltem_monitoring_species.csv")



### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Fish data

#Function used: clean_spp()

clean_md <-clean_spp (species_list, "fish")

# Scientific Names Correction ---------------------------------------------

# Connect to WoRMS and fishbase API using complentary functions
# Function resolve_names() searches for species scientific names correct spelling

names_resolved <- resolve_names(clean_md, "fish")



# Names Validation --------------------------------------------------------

# Validates current scientific names and automatically updates them in our PLoS

# Function used: clean_validation()

fish_validated <- clean_validation(clean_md, names_resolved, species_list, "fish")




# LTEM database correction ------------------------------------------------

# Correction of possible errors in IDSpecies
ltem <- speciesid(ltem, fish_validated, "fish")

# Correction of species names mispellings
ltem <- speciesnames(ltem, fish_validated, "fish")


# Generate report for modified IDSpecies or Species (Optional)
# test <-   flags(ltem)





# END ---------------------------------------------------------------------



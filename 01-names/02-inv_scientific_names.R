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



# LTEM database correction ------------------------------------------------

# Correction of possible errors in IDSpecies
ltem <- speciesid(ltem, inv_validated, "inv")

# Correction of species names mispellings
ltem <- speciesnames(ltem, inv_validated, "inv")


# Generate report for modified IDSpecies or Species (Optional)
# test <-   flags(ltem)





# END ---------------------------------------------------------------------







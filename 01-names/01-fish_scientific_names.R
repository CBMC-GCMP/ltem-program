library(readxl)
library(tidyverse)



# Load data ---------------------------------------------------------------


## Load custom functions
## Load custom functions

lapply(list.files(path="R/", pattern = ".R", full.names = T), source)
#Monitoring Data
ltem<- read_xlsx ("data/raw/2022/LTEM_SB_27062022.xlsx") 
# list <- read_xlsx("data/lists/updates/ltem_monitoring_species_2022-06-25.xlsx")


# LTEM database correction ------------------------------------------------


# Correction of possible errors in IDSpecies
test_id <- speciesid(ltem, ltem_species)

# Correction of species names mispellings
test_names <- speciesnames(test_id, ltem_species)



test <- flags(test_names)



ltem <- test_names %>% 
  select(-c(correct_id,IDBefore, SpeciesBefore, correct_sp, Status))

rm(test_id,test_names,test)




# Generate report for modified IDSpecies or Species (Optional)
# test <-   flags(ltem)





# END ---------------------------------------------------------------------



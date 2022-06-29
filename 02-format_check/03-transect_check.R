library(googlesheets4)
library(tidyverse)
library(readxl)


# Load data ---------------------------------------------------------------

source("02-format_check/02-IDReef_check.R")

# ltem <- read_sheet("https://docs.google.com/spreadsheets/d/1vVHFQEhvtvmhh5u6GghbUBXvYYR-q-Jc7gnQTF7Zias/edit#gid=2040316618")


# Transect arrangement ----------------------------------------------------

## Select unique transects per region
transects <- unique_trnsct(ltem)


## Sort transects by label (PEC & INV) into separate df's

# Unique fish transects
fish <- transects %>%
  filter(Label=="PEC")

# Unique inv transects
inv <- transects %>% 
  filter(Label=="INV")  


## Merge fish and inv transects so we can compare them easily

compared <- compare_trnsct(inv, fish)

## Apply flag if there's a missing transect
missing <- flag_trnsct(compared)


# Export file -------------------------------------------------------------

## If missing transects occur, we can export the df for further inspection

# writexl::write_xlsx(missing, "data/pending/missing_transects.xlsx")




# END ---------------------------------------------------------------------





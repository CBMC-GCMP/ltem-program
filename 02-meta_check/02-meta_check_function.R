library(tidyverse)


# Load data ---------------------------------------------------------------
source("00-functions/02-meta_check.R")
## Our confidence intervals reference metadata



## Load the LTEM databse that needs checking
ltem <- readxl::read_xlsx("data/drive/ltem_database_07122021_original.xlsx")



# Data check function -----------------------------------------------------




# First checkpoint: Output inspection -------------------------------------

## Test data_check function with size values
size <- data_check(ltem, "Size")




## Test data_check function with quantity values
quantity <- data_check(ltem, "Quantity")



# END: Manual inspection and correction -----------------------------------





# Second checkpoint: Return correct values --------------------------------

## After monitors data inspection, we return corrected values to
## original dataframe

c_size <- read_xlsx("data/auxiliar/size_check.xlsx") %>%
  filter(Status == "Delete") #"eliminar" is the suggested flag in the status column (Manuallly created)
#for all incorrect entries

c_quantity <- read_xlsx("data/auxiliar/quantity_check.xlsx") %>%
  filter(Status == "Delete")

## Merge the correct data
#Size
merge_ltem <-
  merge(ltem, c_size[, c("ID", "status")], by = "ID", all = T) %>%
  mutate(status = ifelse(is.na(status), "correct", status)) %>%
  filter(status != "eliminar") %>%
  select(-status)
#Quantity
ltem_database <-
  merge(merge_ltem, c_quantity[, c("ID", "status")], by = "ID", all = T) %>%
  mutate(status = ifelse(is.na(status), "correct", status)) %>%
  filter(status != "eliminar") %>%
  select(-status,-ID) %>%
  rename(Reef = Reefs)



# Export data -------------------------------------------------------------

## Corrected LTEM database
writexl::write_xlsx(ltem_database, "data/ltem_database_07122021.xlsx")



# END ---------------------------------------------------------------------

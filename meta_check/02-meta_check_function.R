library(tidyverse)


# Load data ---------------------------------------------------------------

## Our confidence intervals reference metada
refs_meta <- readRDS("data/auxiliar/refs_meta.RDS")


## Load the LTEM databse that needs checking
ltem <- readxl::read_xlsx("data/ltem_2021_valid_names.xlsx")



# Data check function -----------------------------------------------------

## This is our function for automatic flagging values outside given their respective
## confidence interval, per species
data_check <- function(x, type) {
  if (type == "Quantity") {
    merge(x, refs_meta[, c("IDSpecies", "Size_min", "Size_max", "Quantity_max")],
          by = "IDSpecies") %>%
      mutate(q_flag = ifelse(Quantity > Quantity_max, "FLAG_Q", "NO")) %>%
      filter(q_flag == "FLAG_Q")
    
  } else if (type == "Size") {
    merge(x, refs_meta[, c("IDSpecies", "Size_min", "Size_max", "Quantity_max")], by = "IDSpecies") %>%
      mutate(
        q_flag = ifelse(Size > Size_max, "FLAG_S", "NO"),
        q_flag = ifelse(Size < Size_min, "FLAG_S_min", q_flag)
      ) %>%
      filter(q_flag == "FLAG_S" |
               q_flag == "FLAG_S_min")
    
  } else {
    stop("Tienes que poner Quantity o Size!")
  }
  
}



# First checkpoint: Output inspection -------------------------------------

## Test data_check function with size values
size <- data_check(ltem, "Size")
## Export flagged values
writexl::write_xlsx(size, "data/auxiliar/size_check.xlsx")

## Test data_check function with quantity values
quantity <- data_check(ltem, "Quantity")
## Export flagged values
writexl::write_xlsx(quantity, "data/auxiliar/quantity_check.xlsx")


# END: Manual inspection and correction -----------------------------------





# Second checkpoint: Return correct values --------------------------------

## After monitors data inspection, we return corrected values to
## original dataframe

c_size <- read_xlsx("data/auxiliar/size_check.xlsx") %>%
  filter(status == "eliminar") #"eliminar" is the suggested flag in the status column (Manuallly created)
#for all incorrect entries

c_quantity <- read_xlsx("data/auxiliar/quantity_check.xlsx") %>%
  filter(status == "eliminar")

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

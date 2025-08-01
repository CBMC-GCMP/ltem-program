library(tidyverse)

# Load data ---------------------------------------------------------------

source("00-functions/02-meta_check.R")

## Load latest (and complete) LTEM database
ltem <- readRDS("data/auxiliar/LTEM_historic_v2.RDS")



# Confidence Intervals ----------------------------------------------------

## Define our confidence level (95%)
## Calculate statistics per species

refs_meta <- ci_95(ltem)

## Historic Max & Min values for Size and Quantity

max_min <- ranges(ltem)




## Replace all flagged confidence intervals with their
## respective Max & Min size and quantity values
# refs_meta <-
#   merge(refs_meta,
#         mm,
#         by = c("IDSpecies", "Species"),
#         all = T) %>%
#   mutate(
#     flag = ifelse(is.na(flag), "Flag", flag),
#     c_interval = ifelse(flag == "Correct", "True", "False"),
#     Size_min = ifelse(flag == "Flag", size_min, Size_min),
#     Size_max = ifelse(flag == "Flag", size_max, Size_max),
#     Quantity_max = ifelse(flag == "Flag", q_max, Quantity_max)
#   ) %>%
#   select(-c(flag, size_max, size_min, q_max))
# 
# 
# 
# # Export data -------------------------------------------------------------
# 
# ## Export reference metadata for Size and Quantity
# 
# write_rds(refs_meta, "data/auxiliar/refs_meta.RDS")


# END ---------------------------------------------------------------------

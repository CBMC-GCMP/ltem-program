library(readxl)
library(tidyverse)



# Load data ---------------------------------------------------------------

source("02-format_check/01-size_check.R")

## Load peripherical list of Reefs
reefs <-  read.csv("data/lists/ltem_monitoring_reefs.csv")


## Merge and replace

# Replace wrong IDs
ltem <- reefsid(ltem,reefs) 
# Correct wrong Names
ltem <- reefsname(ltem,reefs)

## Visualize changed reefs
changes <- flags(ltem)



# Format LTEM database ----------------------------------------------------

ltem <- ltem %>% 
  select(-c(correct_id, IDBefore, ReefBefore,correct_reef))

rm(changes,
   reefs)


# END ---------------------------------------------------------------------



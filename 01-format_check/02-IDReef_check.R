library(readxl)
library(tidyverse)



# Load data ---------------------------------------------------------------

source("01-format_check/01-size_check.R")

## Load peripherical list of Reefs
reefs <-  read.csv("data/lists/ltem_monitoring_reefs.csv")


## Merge and replace wrong Reefs
ltem <- reefsid(ltem,reefs) 

## Visualize changed reefs
changes <- flags(ltem)



# Format LTEM database ----------------------------------------------------

ltem <- ltem %>% 
  select(-c(Before,correct_reef))

rm(changes,
   reefs)


# END ---------------------------------------------------------------------



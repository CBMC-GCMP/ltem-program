library(googlesheets4)
library(tidyverse)
library(readxl)


# Load data ---------------------------------------------------------------

## Load from google sheet; just paste the link
# db <- read_sheet("https://docs.google.com/spreadsheets/d/1vVHFQEhvtvmhh5u6GghbUBXvYYR-q-Jc7gnQTF7Zias/edit#gid=2040316618")

## Or, you can load an excel file (CSV, RDS, etc.)
db <- read_xlsx("data/drive/ltem_database_07122021_original.xlsx")



# Transect arrangement ----------------------------------------------------

## Select unique transects per region
transects <- db %>% 
  group_by(Region) %>% 
  select(Label, Year, Month, Day, IDReef, Reef, Depth, Transect, observador) %>% 
  arrange(Year, Month,Day, IDReef, Reef, Depth,  Transect) %>% 
  unique()


## Sort transects by label (PEC & INV) into separate df's

# Unique fish transects
fish <- transects %>%
  filter(Label=="PEC")

# Unique inv transects
inv <- transects %>% 
  filter(Label=="INV")  


## Merge fish and inv transects so we can compare them easily
t_merge <- merge(inv, fish, by=c("Region", 
                                 "Year",
                                 "Month", 
                                 "Day", 
                                 "IDReef" ,
                                 "Reef",
                                 "Depth", 
                                 "Transect"), all=T ) %>% 
  rename(Label_inv= Label.x,
         Label_pec= Label.y)


## Apply flag if there's a missing transect
m_transects <- t_merge %>% 
  mutate(Flag= ifelse(is.na(Label_inv), "missing_INV", "CORRECT"),
         Flag= ifelse(is.na(Label_pec), "missing_PEC", Flag)
  ) %>% 
  group_by(Region) %>% 
  filter(Flag != "CORRECT")



# Export file -------------------------------------------------------------

## If missing transects occur, we can export the df for further inspection

# writexl::write_xlsx(m_transects, "data/pending/missing_transects.xlsx")




# END ---------------------------------------------------------------------





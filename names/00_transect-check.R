library(googlesheets4)
library(tidyverse)

db <- read_sheet("https://docs.google.com/spreadsheets/d/1vVHFQEhvtvmhh5u6GghbUBXvYYR-q-Jc7gnQTF7Zias/edit#gid=2040316618")


transects <- db %>% 
  group_by(Region) %>% 
  select(Label, Year, Month, Day, IDReef, Reef,  Depth, Transect, observador) %>% 
  arrange(Year, Month, Day, IDReef, Reef, Depth,  Transect) %>% 
  unique()

# Cabo Pulmo-----

cp_fish <- transects %>%
  filter(Region=="Cabo Pulmo") %>% 
  filter(Label=="PEC")

cp_inv <- transects %>% 
  filter(Region=="Cabo Pulmo") %>% 
  filter(Label=="INV")  


library(plyr)
cp_merge <- merge(cp_inv, cp_fish, by=c("Region", "Year", "Month", "Day", "IDReef" ,"Reef", "Depth", "Transect"), all=T )

writexl::write_xlsx(cp_merge, "data/auxiliar/cabo-pulmo_transects.xlsx")



# Loreto-----

lto_fish <- transects %>%
  filter(Region=="Loreto") %>% 
  filter(Label=="PEC")

lto_inv <- transects %>% 
  filter(Region=="Loreto") %>% 
  filter(Label=="INV")  

test <- merge(lto_fish, lto_inv,  by=c("Region", "Year", "Month", "Day", "IDReef" ,"Reef", "Habitat", "Depth", "Transect"), all=T )
library(plyr)
lto_merge <- join(lto_inv, lto_fish, by=c("Region", "Year", "Month", "Day", "IDReef" ,"Reef", "Habitat", "Depth", "Transect"), type="full" )

writexl::write_xlsx(test, "data/auxiliar/loreto_transects.xlsx") 


# Los Cabos-----

csl_fish <- transects %>%
  filter(Region=="Los Cabos") %>% 
  filter(Label=="PEC")

csl_inv <- transects %>% 
  filter(Region=="Los Cabos") %>% 
  filter(Label=="INV")  


library(plyr)
csl_merge <- merge(csl_inv, csl_fish, by=c("Region", "Year", "Month", "Day", "IDReef" ,"Reef",  "Depth", "Transect"), all=T )

writexl::write_xlsx(csl_merge, "data/auxiliar/csl_transects.xlsx")



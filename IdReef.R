library(tidyverse)
library(readxl)

id_to_correct <- read_xlsx("ltem_2021_update_fish_data.xlsx", sheet= 1)
list_reef <-  read.csv("ltem_monitoring_reefs.csv") %>% 
rename(Reefs= Reef, IDReefs= IDReef)

### ----
merge <- merge(id_to_correct, list_reef [, c("IDReefs", "Reefs")], by= "IDReefs", all.x= T) %>% 
  rename(correct_reefs= Reefs.y,
         Reefs= Reefs.x)%>% 
    mutate(Flag= ifelse(Reefs== correct_reefs, "correct", "Wrong"))

### ---

Flag_Wrong <- merge %>% 
  select(Flag) %>% 
  filter(Flag== "wrong")

### ---



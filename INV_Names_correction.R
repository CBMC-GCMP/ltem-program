library(rfishbase)
library(worms)
library(data.table)
library(taxize)
library(tidyverse)
library(readxl)
library(jsonlite)
library(httr)

DB_Spe <- read_xlsx ("data_CMBC (1).xlsx", sheet= 3) %>% 
rename(IDSpecies = "IDSpecies (Species)")


### Filtro Para obtener solo invertebrados ---- 
DBs_inv <-  DB_Spe %>% 
  filter (Label== "INV") %>% 
  filter(str_detect(Species, " " )) %>%
  filter(!str_detect(Species, " sp| spp" )) %>% 
  filter(!str_detect(Species, " cf| cf."))


#############

sources <- c(worms = 9)

resolved_names <- sources %>% 
  map(~ taxize::gnr_resolve(data_source_ids = .x, 
                            names = DBs_inv$Species,
                            best_match_only = T, 
                            fields = c("all"), 
                            canonical = T ))

resolved_names_df <- resolved_names %>% 
  map(~ .x %>% 
        filter(!match_value %in% c("Could only match genus")
               & str_count(matched_name2, "\\w+") >= 2) %>% 
        select(user_supplied_name, matched_name2, taxon_id)) %>% 
  reduce(full_join, by = "user_supplied_name") %>% 
  set_names(c("user_supplied_name",
              "worms_sci_name",
              "worms_id"))



resolved_names <- resolved_names_df %>% 
  mutate(resolved_scientific_name = case_when(
    !is.na(worms_sci_name) ~ worms_sci_name,
    TRUE ~ user_supplied_name
  )) %>% 
  select(user_supplied_name, resolved_scientific_name, everything()) %>% 
  rename(Species= user_supplied_name)

####

merge_names <- merge(DBs_inv, resolved_names[c("Species",
                                               "resolved_scientific_name")],
                     by= "Species", all.x= TRUE)

### quita NA

inv_resolved <- merge_names %>% 
  filter(!is.na (resolved_scientific_name))
  
  
###
inv_resolved <- inv_resolved %>% 
mutate(Species= ifelse(Species== resolved_scientific_name,
                       Species, resolved_scientific_name)) %>% 
select(-resolved_scientific_name)


DBs_inv$Species [match(inv_resolved$IDSpecies,
                       DBs_inv$IDSpecies)] <- inv_resolved$Species

###

id_inv_worms2 <- get_wormsid(DBs_inv$Species)
#--------------------------------------------------------
df <- DBs_inv %>%
mutate(Species = str_replace_all(Species, "Hyotissa solida",
                                 "Hyotissa hyotis")) %>% 
  mutate(Species = str_replace_all(Species,
                                 "Echinaster tenuispina",
                                 "Echinaster (Othilia) tenuispina")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Holothuria leucospilota",
                                   "Holothuria (Mertensiothuria) leucospilota")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Thais planospira",
                                   "Thais (Tribulus) planospira"))


id_inv_worms2 <- get_wormsid(df$Species)


# 4
Id_inv_worms <- data.frame(id_inv_worms2)

# 5
Id_inv_worms2= as.numeric(Id_inv_worms$ids)

#6
# Extraer base de datos del worms ----


pureva1 <- wormsbyid(Id_inv_worms2,
                     verbose = FALSE,
                     ids = TRUE,
                     sleep_btw_chunks_in_sec = 0.01)

#7
prueva2 <- pureva1 %>% 
  mutate(IDSpecies= ifelse(df$Species== scientificname,
                           df$IDSpecies, ""), Species= df$Species) %>% 
  select(IDSpecies, valid_name, valid_AphiaID, Species)

#---------------
merge_inv <- merge(DBs_inv, prueva2[, c("IDSpecies",
                                        "valid_name",
                                        "valid_AphiaID")], 
                   by = "IDSpecies", all.x = TRUE) %>% 
  mutate(Species= ifelse(Species== valid_name, Species, valid_name)) %>% 
  select (-valid_name)

rm(df, sources)

DB_Spe$Species [match(merge_inv$IDSpecies,
                       DB_Spe$IDSpecies)] <- merge_inv$Species


### Hoja 1 ----

DB_Spe_1 <- read_xlsx ("data_CMBC (1).xlsx", sheet= 1)






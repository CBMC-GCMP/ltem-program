library(tidyverse)
library(readxl)
library(stringr)



# Load data ---------------------------------------------------------------

#Ltem from fish scientific name correction:
source("03-names/01-fish_scientific_names.R")

#Peripheral list of species (PLoS)
inv_metadata <- read.csv ("data/inv_monitoring_species.csv")




### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Invertebrate data

#PLoS
clean_inv <- function(inv_metadata){
clean_md<-  inv_metadata %>% 
  filter (Label== "INV") %>% 
  filter(str_detect(Species, " " )) %>%
  filter(!str_detect(Species, " sp| spp" )) %>% 
  filter(!str_detect(Species, " cf| cf."))
}


#Monitoring data
clean_data <- function(ltem){
clean_df <- ltem %>% 
  filter(Label=="INV") %>% 
  filter(!str_detect(Species, " sp| spp" )) %>% 
  filter(!str_detect(Species, " cf| cf."))
}


# Scientific Names Correction ---------------------------------------------

### First step is to correct the spelling

##We load WoRMS as our source

sources <- c(worms = 9)

##Then, we apply a taxsize function: gnr_resolve, for matching correct
## spellings of Scientific Names, in three parts:

##Part I:
names <- function(resolved_names){
resolved_names <- sources %>% 
  map(~ taxize::gnr_resolve(data_source_ids = .x, 
                            names = clean_md$Species,
                            best_match_only = T, 
                            fields = c("all"), 
                            canonical = T ))

#Part II:
resolved_names_df <- resolved_names %>% 
  map(~ .x %>% 
        filter(!match_value %in% c("Could only match genus")
               & str_count(matched_name2, "\\w+") >= 2) %>% 
        select(user_supplied_name, matched_name2, taxon_id)) %>% 
  reduce(full_join, by = "user_supplied_name") %>% 
  set_names(c("user_supplied_name",
              "worms_sci_name",
              "worms_id"))

#Part III:
resolved_names <- resolved_names_df %>% 
  mutate(resolved_scientific_name = case_when(
    !is.na(worms_sci_name) ~ worms_sci_name,
    TRUE ~ user_supplied_name
  )) %>% 
  select(user_supplied_name, 
         resolved_scientific_name, 
         everything())%>% 
  rename(Species= user_supplied_name) %>% 
  select(Species, resolved_scientific_name)
}



## We merge our resolved_names with our clean PLoS (clean_md)
## And then remove possible NAs (missing values) in 
## the resolved scientific names

merge_clean_resolved <- function(clean_md, resolved_names){
  inv_resolved <- merge(clean_md, resolved_names[c("Species", "resolved_scientific_name")],
                  by= "Species", all.x= TRUE) %>% 
  filter(!is.na (resolved_scientific_name)) %>% 
  mutate(Species= ifelse(Species== resolved_scientific_name,
                         Species, resolved_scientific_name)) %>% 
  select(-resolved_scientific_name)


clean_md$Species [match(inv_resolved$IDSpecies,
                        clean_md$IDSpecies)] <- inv_resolved$Species
}


# First Checkpoint --------------------------------------------------------


## If any scientific is still unaccepted, we can manually check its status

## Scientific names' status: Must be executed individually

spp_inv <- data.frame( IDSpecies= clean_md$Species, Species= stringr::str_replace(clean_md$Species," ", "+") )

wormsID <- taxize:: get_wormsid(spp_inv$Species)

#Console: If manual input is required, always select the species entry
#with an Accepted Status


## All the other species which are not valid, require manual correction.
## You can do so by replacing the old scientific name string, with a new one:


clean_md <- spp_inv %>%
  mutate(Species = str_replace_all(Species, "Hyotissa solida",
                                   "Hyotissa hyotis")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Echinaster tenuispina",
                                   "Echinaster (Othilia) tenuispina")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Holothuria leucospilota",
                                   "Holothuria (Mertensiothuria) leucospilota")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Mycale ramulosa",
                                   "Mycale (Zygomycale) ramulosa")) %>% 
  mutate(Species = str_replace_all(Species,
                                   "Thais planospira",
                                   "Thais (Tribulus) planospira"))



# The replacements above may vary, always check for new invalid species

## Re execute this line, if all species are displayed in green, we can proceed

wormsID <- taxize:: get_wormsid(clean_md$Species)




# Validate scientific names with WoRMS IDs --------------------------------
library(plyr)
library(httr)
library(worms)

## For scientific names validation, we need the AphiaIDs from WoRMS servers
## This IDs will help us check if a scientific neame is currently unaccepted,
## and will give us the valid one

# First Step: We change the format of the WoRMS IDs generated in previous section
worms_format <- function(wormsID){
wormsID <- data.frame(wormsID)



# Second Step
wormsID = as.numeric(wormsID$ids)

# Third Step: Downloading WoRMS info

worms_names <- wormsbyid(wormsID,
                         verbose = FALSE,
                         ids = TRUE,
                         sleep_btw_chunks_in_sec = 0.01)

}

## We clean our worms_names df, and add our IDSpecies
validate_names <- function(worms_names){
valid_names<- worms_names %>% 
  mutate(IDSpecies= ifelse(clean_md$Species== scientificname,
                           clean_md$IDSpecies, "")) %>% 
  select(IDSpecies, valid_name, valid_AphiaID)



# Second Checkpoint -------------------------------------------------------

## We merge our valid_names with our clean PLoS

merge_md <- merge(clean_md, valid_names[, c("IDSpecies",
                                            "valid_name",
                                            "valid_AphiaID")],
                  by = "IDSpecies",
                  all.x = TRUE) %>%
  
  mutate(Results = ifelse(Species == valid_name,
                          "Correct",
                          "Wrong")) %>% 
  select(IDSpecies, Species, valid_name, valid_AphiaID, Results)

#Manual check: Wrong Spellings
#view(merge_md)

##If there are no conflicts in replacing resolved scientific names, continue:

##We can now update our clean PLoS with the correct scientific names:

clean_md <- merge_md %>% 
  select(-Species, -Results)

}



# Third Checkpoint --------------------------------------------------------

##We merge the validated scientific names in the PLoS, with our monitoring
## database

mg_df <- function(clean_df, clean_md){
merge_df <- merge(clean_df, clean_md[, c("IDSpecies", "valid_name")], 
                  by= "IDSpecies", all.x = TRUE) %>% 
  mutate(Status= ifelse(Species==valid_name, "Correct", "Pending"))

##Manual Check: All "Pending" species must be verified, and should only
## contain errors in spelling or in updates of the scientific name.

#view(merge_df)


#If no issues are displayed, proceed:
names_inv <- merge_df %>%
  filter(Species != "Actinostella californica") %>% 
  mutate(Species= valid_name) %>% 
  select(-c(valid_name, Status))

}

# Replacing species in the monitoring database ----------------------------

## Finally, we use a match to correct scientific names with the valid ones
## from our PLoS in the complete monitoring database

inv_ltem <- function(names_inv, ltem){
ltem$Species[match(names_inv$ID, ltem$ID)] <- names_inv$Species

writexl:: write_xlsx(ltem, "data/ltem_2021_valid_names.xlsx")

}






# END ---------------------------------------------------------------------
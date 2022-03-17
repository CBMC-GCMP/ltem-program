library(readxl)
library(rfishbase)
library(tidyverse)



# Load data ---------------------------------------------------------------

#Monitoring Data
ltem_fish<- read_xlsx ("data/drive/ltem_database_07122021.xlsx", 
                       sheet= 1) %>% 
  rowid_to_column( "ID")

#Peripheral list of species (PLoS)
metadata_fish <- read.csv ("data/lists/ltem_monitoring_species.csv")



### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Fish data

clean_fish <- function(ltem_fish){
clean_df <-  ltem_fish%>% 
  filter (IDLabel== "PEC") %>%
  filter(!str_detect(Species, " sp| spp" ))
}

clean_metadata <- function(metadata_fish){
clean_md <-  metadata_fish %>% 
  select(Label, IDSpecies, Species ) %>% 
  filter (Label== "PEC") %>%
  filter(str_detect(Species, " " )) %>% 
  filter(!str_detect(Species, " sp| spp" ))

}


# Scientific Names Correction ---------------------------------------------

##It is easier to work with the PLoS

### First step is to correct the spelling

##We load WoRMS and Fishbase as our sources

##Then, we apply a taxsize function: gnr_resolve, for matching correct
## spellings of Scientific Names, in three parts:

## Part I: 
#Retrieve best species matches from WoRMS and Fishbase

names <- function(resolved_names){
  sources <- c(worms = 9, fishbase = 155)
resolved_names <- sources %>% 
  map(~ taxize::gnr_resolve(data_source_ids = .x, 
                            sci = clean_md$Species,
                            best_match_only = T, 
                            fields = c("all"), 
                            canonical = T ))



## Part II:
#Leave only the correct results, and format it as data.frame

resolved_names_df <- resolved_names %>% 
  map(~ .x %>% 
        filter(!match_value %in% c("Could only match genus") & 
                 str_count(matched_name2, "\\w+") >= 2) %>% 
        select(user_supplied_name, matched_name2, taxon_id)) %>% 
  reduce(full_join, by = "user_supplied_name") %>% 
  set_names(c("user_supplied_name", "worms_sci_name",
              "worms_id", "fishbase_sci_name", "fishbase_id"))


## Part III:
#Filter absent values (NAs)

resolved_names <- resolved_names_df %>% 
  mutate(resolved_scientific_name = case_when(
    !is.na(worms_sci_name) ~ worms_sci_name,
    !is.na(fishbase_sci_name) ~ fishbase_sci_name,
    TRUE ~ user_supplied_name
  )) %>% 
  select(user_supplied_name, 
         resolved_scientific_name, 
         everything())%>% 
  rename(Species= user_supplied_name) %>% 
  select(Species, resolved_scientific_name)

}


# First Checkpoint --------------------------------------------------------

## We merge our resolved_names with our clean PLoS (clean_md)

merge_clean_resolved <- function(clean_md, resolved_names){
merge_md <- merge(clean_md, resolved_names[, c("Species", "resolved_scientific_name")], 
                  by= "Species", all.x = TRUE)%>% 
  mutate(Status= ifelse( Species== resolved_scientific_name, "Correct", "Modified_sp"),
         Species= ifelse( Species== resolved_scientific_name,
                          Species, 
                          resolved_scientific_name)) %>% 
  select(Label, IDSpecies, Species)

}

#Manual check: Wrong Spellings
#view(merge_md)

##If there are no conflicts in replacing resolved scientific names, continue:

##We can now update our clean PLoS with the correct scientific names:



# Second Checkpoint: Valid names ------------------------------------------

## To identify obsolete species names, we can validate them with:


validate_names <- function(clean_md){
valid_names<- clean_md %>% 
  mutate(New=  rfishbase::validate_names(Species),
         Validated = ifelse(Species== New, Species, New),
         Status= ifelse(Species== Validated, "Correct", "Modified_sp"))


#Manual check: Are the updated names correct?
#view(valid_names)


#If there are no issues with the validated names, once again
# we update our PLoS: 

clean_md <-  valid_names %>% 
  select(-c(New, Species, Results))

}


# Third Checkpoint: Correct spellings in the monitoring database ----------

##We merge the validated scientific names in the PLoS, with our monitoring
## database

mg_df <- function(clean_df, clean_md){
merge_df <- merge(clean_df, clean_md[, c("IDSpecies", "Validated")], 
                  by= "IDSpecies", all.x = TRUE) %>% 
  mutate(Status= ifelse(Species==Validated, "Correct", "Modified_sp"))


##Manual Check: All "Pending" species must be verified, and should only
## contain errors in spelling or in updates of the scientific name.

#view(merge_df)


#If no issues are displayed, proceed:

names_fish <- merge_df %>% 
  mutate(Species= Validated) %>% 
  select(-c(Validated, Status))

}

# Replacing species in the monitoring database ----------------------------

## Finally, we use a match to correct scientific names with the valid ones
## from our PLoS in the complete monitoring database

fish_ltem <- function(names_fish, ltem_fish){
ltem_fish$Species[match(names_fish$ID, ltem_fish$ID)] <- names_fish$Species

}


# END ---------------------------------------------------------------------
library(readxl)
library(rfishbase)
library(tidyverse)



# Load data ---------------------------------------------------------------

#Monitoring Data
ltem_fish<- read_xlsx ("data/drive/ltem_database_07122021.xlsx", 
                       sheet= 1) %>% 
  rowid_to_column( "ID")

#Peripheral list of species (PLoS)
metadata_fish <- read.csv ("data/server/ltem_monitoring_species.csv")



### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Fish data

clean_df <-  ltem_fish%>% 
  filter (IDLabel== "PEC") %>%
  filter(!str_detect(Species, " sp| spp" ))


clean_md <-  metadata_fish %>% 
  select(Label, IDSpecies, Species ) %>% 
  filter (Label== "PEC") %>%
  filter(str_detect(Species, " " )) %>% 
  filter(!str_detect(Species, " sp| spp" ))

rm(metadata_fish)



# Scientific Names Correction ---------------------------------------------

##It is easier to work with the PLoS

### First step is to correct the spelling

##We load WoRMS and Fishbase as our sources
sources <- c(worms = 9, fishbase = 155)


##Then, we apply a taxsize function: gnr_resolve, for matching correct
## spellings of Scientific Names, in three parts:

## Part I: 
#Retrieve best species matches from WoRMS and Fishbase
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

rm(sources, resolved_names_df)



# First Checkpoint --------------------------------------------------------

## We merge our resolved_names with our clean PLoS (clean_md)


merge_md <- merge(clean_md, resolved_names[, c("Species", "resolved_scientific_name")], 
                  by= "Species", all.x = TRUE)%>% 
  mutate(Spelling= ifelse( Species== resolved_scientific_name, "Correct", "Wrong"))


#Manual check: Wrong Spellings
#view(merge_md)

##If there are no conflicts in replacing resolved scientific names, continue:

##We can now update our clean PLoS with the correct scientific names:

clean_md <- merge_md %>% 
  mutate(Species= ifelse( Species== resolved_scientific_name,
                          Species, 
                          resolved_scientific_name)) %>% 
  select(Label, IDSpecies, Species)

rm(resolved_names, 
   merge_md)


# Second Checkpoint: Valid names ------------------------------------------

## To identify obsolete species names, we can validate them with:
valid_names<- clean_md %>% 
  mutate(New= validate_names(Species),
         Validated = ifelse(Species== New, Species, New),
         Results= ifelse(Species== Validated, "Correct", "Wrong"))

#Manual check: Are the updated names correct?
#view(valid_names)


#If there are no issues with the validated names, once again
# we update our PLoS: 
clean_md <-  valid_names %>% 
  select(-c(New, Species, Results))

rm(valid_names)





# Third Checkpoint: Correct spellings in the monitoring database ----------

##We merge the validated scientific names in the PLoS, with our monitoring
## database

merge_df <- merge(clean_df, clean_md[, c("IDSpecies", "Validated")], 
                  by= "IDSpecies", all.x = TRUE) %>% 
  mutate(Status= ifelse(Species==Validated, "Correct", "Pending"))

##Manual Check: All "Pending" species must be verified, and should only
## contain errors in spelling or in updates of the scientific name.

#view(merge_df)


#If no issues are displayed, proceed:
names_fish <- merge_df %>% 
  filter(Species != "Fistularia corneta") %>% 
  mutate(Species= Validated) %>% 
  select(-c(Validated, Status))

rm(merge_df)



# Replacing species in the monitoring database ----------------------------

## Finally, we use a match to correct scientific names with the valid ones
## from our PLoS in the complete monitoring database

ltem_fish$Species[match(names_fish$ID, ltem_fish$ID)] <- names_fish$Species


ltem <- ltem_fish %>% 
  rename(Label= IDLabel,
         Year= IDYear,
         Month= IDMonths,
         Day= IDDays,
         IDReef= IDReefs,
         Habitat= IDHabitats,
         Depth= IDDepths,
         Transect= IDTransects,
         Area= IDAreas,
         Size= "talla num",
         Quantity= Total
         ) %>% 
  select(-talla)


rm(clean_df,
   clean_md,
   names_fish,
   ltem_fish
   )



















# table <-(data.table(clean_df))



# Scientific names correction ---------------------------------------------

### Retrieve Mexican fish species list from fishbase

# species_list <- rfishbase::country(species_list = ) %>% 
#     select(Species,country) %>% 
#     filter(country== "Mexico")
# 
# species_list <- species_list$Species

### Similitary check between fishbase species list and our species
## In order for the loop to work, it requires a string of correctly spelled
## scientific names (fishbase species list) and a dataframe with a "Species" column
## containing all the names that will be checked and corrected


### CORREGIR
#Begin
# for (i in 1:nrow(table)){#range that goes for all the species rows in our table 
#   string <- table[i, Species]# String of our species' names
#   max <- 0 #Do not change these values
#   similarity <- 0
#   for(j in species_list){
#     similarity <-   length(Reduce(intersect, strsplit(c(string, j), split = "")))
#     if(similarity > max){
#       max <- similarity
#       to_replace <- j
#     }
#   }
# #   table[i,"Species"] <- to_replace
# # }
# #End
# 
# # Outout object
# # 
# ltem_fish_database <- as.data.frame(table)
# 
# data_fish$Species[match(ltem_fish_database$ID, data_fish$ID)] <- ltem_fish_database$Species
# 
# ltem_fish_database<- data_fish %>% 
#   filter(IDLabel=="PEC")
# 
# rm(clean_df,
#    table,
#    i,
#    j,
#    max,
#    similarity,
#    species_list,
#    string,
#    to_replace, 
#    data_fish)
# 









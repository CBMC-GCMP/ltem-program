
source("00-functions/00-flags.R")
### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Fish data


clean_spp <- function(species_list, Label){
  if(Label=="fish"){
  clean_md <-  species_list %>% 
  select(Label, IDSpecies, Species ) %>% 
  filter (Label== "PEC") %>%
  filter(str_detect(Species, " " )) %>% 
  filter(!str_detect(Species, " sp | spp" ))
  } else if(label=="inv"){
    clean_md<-  inv_metadata %>% 
      filter (Label== "INV") %>% 
      filter(str_detect(Species, " " )) %>%
      filter(!str_detect(Species, " sp| spp" )) %>% 
      filter(!str_detect(Species, " cf| cf."))    
  }else {
      stop("Please specify 'fish' or 'inv'")
    }
  
}


# Scientific Names Correction ---------------------------------------------

##It is easier to work with the PLoS

### First step is to correct the spelling

##We load WoRMS and Fishbase as our sources

##Then, we apply a taxsize function: gnr_resolve, for matching correct
## spellings of Scientific Names, in three parts:

## Part I: 
#Retrieve best species matches from WoRMS and Fishbase

resolve_names <- function(clean_spp){
  sources <- c(worms = 9, fishbase = 155)
resolved_names <- sources %>% 
  map(~ taxize::gnr_resolve(data_source_ids = .x, 
                            sci = clean_spp$Species,
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

clean_validation <- function(clean_md, resolved_names, species_list) {
  merge_md <-
    merge(clean_md, resolved_names[, c("Species", "resolved_scientific_name")],
          by = "Species", all.x = TRUE) %>%
    mutate(
      SpeciesBefore = Species,
      Status = ifelse(
        Species == resolved_scientific_name,
        "correct_sp",
        "Modified_sp"
      ),
      Species = ifelse(
        Species == resolved_scientific_name,
        Species,
        resolved_scientific_name
      )
    )
  

  
  valid_names <- merge_md %>%
    mutate(
      New =  rfishbase::validate_names(Species),
      correct_sp = ifelse(Species == New, Species, New),
      Status = ifelse(Species == correct_sp, Status, "Modified_sp")
    ) %>%
    select(-New)
  
  dir.create("data/flag_reports", showWarnings = F)
  
  flags <- valid_names %>%
    filter(str_detect(Status, "Modified_")) %>% 
  select(IDSpecies, Species, SpeciesBefore, correct_sp, Status) 
  writexl::write_xlsx(flags, "data/flag_reports/fish_validated_sci-names.xlsx")
  
  valid_names <- valid_names %>%
    select(IDSpecies, correct_sp) 
  
  species_list <- merge(species_list, valid_names, by="IDSpecies", all=T) %>% 
    mutate(correct_sp= ifelse(is.na(correct_sp),Species, correct_sp ),
           Species= ifelse(Species==correct_sp, Species, correct_sp)) %>% 
    select(-c(X,correct_sp))
  dir.create("data/lists/updates", showWarnings = F)
  write.csv(species_list, "data/lists/updates")
}


speciesid <- function(LTEM, clean_sp_list, Label){
  LTEM <- LTEM %>% 
    tibble::rowid_to_column("ID")
  
  if(Label=="fish"){
    LTEM <- LTEM %>% 
      filter(Label=="PEC")
    
    clean_sp_list <- clean_sp_list %>% 
      filter(Label=="PEC")
    
  merge_df <- merge(LTEM, clean_sp_list[, c("Label","IDSpecies", "Species")], 
                    by= c("Label", "Species"), all.x = TRUE) %>% 
    rename(correct_id= IDSpecies.y,
           IDSpecies= IDSpecies.x)%>%
    mutate(IDBefore= IDSpecies,
           Status= ifelse(IDSpecies==correct_id,"Correct", "Modified_IDSp"),
           IDSpecies= ifelse(IDSpecies==correct_id, IDSpecies, correct_id),
           IDSpecies= ifelse(is.na(correct_id), IDBefore, IDSpecies))
  
  } else if(Label=="inv"){
    
    LTEM <- LTEM %>% 
      filter(Label=="INV")
    clean_sp_list <- clean_sp_list %>% 
      filter(Label=="INV" )   
    
    merge_df <- merge(LTEM, clean_sp_list[, c("Label" ,"IDSpecies", "Species")], 
                      by= c("Label", "Species"), all.x = TRUE) %>% 
      rename(correct_id= IDSpecies.y,
             IDSpecies= IDSpecies.x)%>%
      mutate(IDBefore= IDSpecies,
             Status= ifelse(IDSpecies==correct_id,"Correct", "Modified_IDSp"),
             IDSpecies= ifelse(IDSpecies==correct_id, IDSpecies, correct_id),
             IDSpecies= ifelse(is.na(correct_id), IDBefore, IDSpecies)) 
    
    
  }
  else{
    stop( print("Please specify fish or inv"))
  }
  
}

speciesnames <- function(LTEM_IDs, clean_sp_list, Label){
  
  LTEM <- LTEM_IDs
  if(Label=="fish"){
  
    LTEM <- LTEM %>% 
      filter(Label=="PEC")
    
    clean_sp_list <- clean_sp_list %>% 
      filter(Label=="PEC")  
    
  merge_df <- merge(LTEM, clean_sp_list[, c("Label", "IDSpecies", "Species")], 
                    by= c("Label", "IDSpecies"), all.x = TRUE) %>% 
    rename(correct_sp= Species.y,
           Species= Species.x)%>%
    mutate(SpeciesBefore= Species,
           Status= ifelse(Species==correct_sp, Status, "Modified_sp"),
           Species= ifelse(Species==correct_sp, Species, correct_sp))
  } else if(Label=="inv"){
    
    LTEM <- LTEM %>% 
      filter(Label=="INV")
    clean_sp_list <- clean_sp_list %>% 
      filter(Label=="INV" )  
    
    merge_df <- merge(LTEM, clean_sp_list[, c("Label", "IDSpecies", "Species")], 
                      by= c("Label", "IDSpecies"), all.x = TRUE) %>% 
      rename(correct_sp= Species.y,
             Species= Species.x)%>%
      mutate(SpeciesBefore= Species,
             Status= ifelse(Species==correct_sp, Status, "Modified_sp"),
             Species= ifelse(Species==correct_sp, Species, correct_sp))    
    
  }else{
    stop( print("Please specify fish or inv"))
  }
  
}


# END ---------------------------------------------------------------------
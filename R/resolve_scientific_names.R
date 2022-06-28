##It is easier to work with a peripherical list of Species (PLoS)

### First step is to correct the spelling

##We load WoRMS and Fishbase as our sources

##Then, we apply a taxsize function: gnr_resolve, for matching correct
## spellings of Scientific Names, in three parts:


##Both INVs & FISH

resolve_names <- function(clean_spp, Label){
  
  #FISH
  if(Label== "fish"){
    
    ## Part I: 
    #Retrieve best species matches from WoRMS and Fishbase      
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
    
    
    #INVs
  }else if(Label=="inv"){
    ##Part I:
    sources <- c(worms = 9)
    resolved_names <- sources %>% 
      map(~ taxize::gnr_resolve(data_source_ids = .x, 
                                names = clean_spp$Species,
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
  } else{
    stop("Please specify 'fish' or 'inv'")
  }
  
}

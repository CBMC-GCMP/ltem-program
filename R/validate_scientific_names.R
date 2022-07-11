clean_validation <- function(clean_md, resolved_names, species_list, Label) {
  
  merge_md <-
    merge(clean_md, resolved_names[, c("Species", "resolved_scientific_name")],
          by = "Species", all.x = TRUE) %>%
    mutate(resolved_scientific_name= ifelse(is.na(resolved_scientific_name), 
                                            Species, resolved_scientific_name),
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
  
  dir.create("data/flag_reports", showWarnings = F)
  dir.create("data/lists/updates", showWarnings = F)
  
  if(Label=="fish"){
    valid_names <- merge_md %>%
      mutate(
        New =  rfishbase::validate_names(Species),
        correct_sp = ifelse(Species == New, Species, New),
        Status = ifelse(Species == correct_sp, Status, "Modified_sp")
      ) %>%
      select(-New)
    
    
    flags <- valid_names %>%
      filter(str_detect(Status, "Modified_")) %>% 
      select(IDSpecies, Species, SpeciesBefore, correct_sp, Status) 
    writexl::write_xlsx(flags, "data/flag_reports/fish_validated_sci-names.xlsx")
    
    valid_names <- valid_names %>%
      select(IDSpecies, correct_sp) 
    
    species_list <- merge(species_list, valid_names, by="IDSpecies", all=T) %>% 
      mutate(correct_sp= ifelse(is.na(correct_sp),Species, correct_sp ),
             Species= ifelse(Species==correct_sp, Species, correct_sp)) %>% 
      select(-c(correct_sp))
    
    write.csv(species_list, "data/lists/updates/fish_updated_names.csv")
    species_list <- species_list 
    
  }else if(Label=="inv"){
    flags <- merge_md  %>%
      filter(str_detect(Status, "Modified_")) %>% 
      select(IDSpecies, Species, SpeciesBefore, resolved_scientific_name, Status) 
    writexl::write_xlsx(flags, "data/flag_reports/inv_resolved_sci-names.xlsx")
    
    valid_names <- merge_md  %>%
      select(IDSpecies, Species) 
    
  }else{
    stop("Please specify 'fish' or 'inv'")
  }
  
}

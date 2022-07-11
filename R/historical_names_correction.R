speciesid <- function(LTEM, clean_sp_list){
  
  merge_df <- merge(LTEM, clean_sp_list[, c("Label","IDSpecies", "Species" 
                                            ,"A_ord", "B_pen", "TrophicLevel",
                                            "Functional_groups"
  )], 
  by= c("Label", "Species"), all.x = TRUE) %>% 
    rename(correct_id= IDSpecies.y,
           IDSpecies= IDSpecies.x)%>%
    mutate(IDBefore= IDSpecies,
           Status= ifelse(IDSpecies==correct_id,"Correct", "Modified_IDSp"),
           IDSpecies=ifelse(is.na(IDSpecies), correct_id, IDSpecies),
           IDSpecies= ifelse(IDSpecies==correct_id, IDSpecies, correct_id),
           IDSpecies= ifelse(is.na(correct_id), IDBefore, IDSpecies))
  
}



speciesnames <- function(LTEM_IDs, clean_sp_list){
  
  
  LTEM <- LTEM_IDs
  
  
  merge_df <- merge(LTEM, clean_sp_list[, c("Label", "IDSpecies", "Species" 
                                            ,"A_ord", "B_pen",
                                            "TrophicLevel",
                                            "Functional_groups"
  )], 
  by= c("Label", "IDSpecies"
        # ,"A_ord", "B_pen", "TrophicLevel",
        # "Functional_groups"
  ), all.x = TRUE) %>% 
    rename(correct_sp= Species.y,
           Species= Species.x)%>%
    mutate(SpeciesBefore= Species,
           # correct_sp= ifelse(is.na(correct_sp), Species, correct_sp),
           Status= ifelse(Species==correct_sp, Status, "Modified_sp"),
           Species= ifelse(Species==correct_sp, Species, correct_sp))
  
  
  
}
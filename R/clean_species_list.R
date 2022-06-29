### Apply filters for removing all species entries that contain "sp" or "spp" 
### Including Species with just genus or families
##Filtering just Fish data

##Both INVs & FISH

clean_spp <- function(species_list, Label){
  if(Label=="fish"){
    clean_md <-  species_list %>% 
      select(Label, IDSpecies, Species ) %>% 
      filter (Label== "PEC") %>%
      filter(str_detect(Species, " " )) %>% 
      filter(!str_detect(Species, " sp | spp" ))
  } else if(Label=="inv"){
    clean_md<-  species_list %>% 
      filter (Label== "INV") %>% 
      filter(str_detect(Species, " " )) %>%
      filter(!str_detect(Species, " sp| spp" )) %>% 
      filter(!str_detect(Species, " cf| cf."))    
  }else {
    stop("Please specify 'fish' or 'inv'")
  }
  
}

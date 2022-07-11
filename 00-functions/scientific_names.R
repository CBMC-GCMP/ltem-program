
# source("00-functions/00-flags.R")






# Scientific Names Correction ---------------------------------------------




## We merge our resolved_names with our clean PLoS, in the case of FISH data
## Scientific names are also validated for updates







## Now the corrected and validated PLoS can be used for correcting
## the spelling in our LTEM database

# #IDSpecies check
# speciesid <- function(LTEM, clean_sp_list, Label){
#   LTEM <- LTEM %>% 
#     tibble::rowid_to_column("ID")
#   
#   if(Label=="fish"){
#     LTEM <- LTEM %>% 
#       filter(Label=="PEC")
#     
#     clean_sp_list <- clean_sp_list %>% 
#       filter(Label=="PEC")
#     
#   merge_df <- merge(LTEM, clean_sp_list[, c("Label","IDSpecies", "Species")], 
#                     by= c("Label", "Species"), all.x = TRUE) %>% 
#     rename(correct_id= IDSpecies.y,
#            IDSpecies= IDSpecies.x)%>%
#     mutate(IDBefore= IDSpecies,
#            Status= ifelse(IDSpecies==correct_id,"Correct", "Modified_IDSp"),
#            IDSpecies= ifelse(IDSpecies==correct_id, IDSpecies, correct_id),
#            IDSpecies= ifelse(is.na(correct_id), IDBefore, IDSpecies))
#   
#   } else if(Label=="inv"){
#     
#     LTEM <- LTEM %>% 
#       filter(Label=="INV")
#     clean_sp_list <- clean_sp_list %>% 
#       filter(Label=="INV" )   
#     
#     merge_df <- merge(LTEM, clean_sp_list[, c("Label" ,"IDSpecies", "Species")], 
#                       by= c("Label", "Species"), all.x = TRUE) %>% 
#       rename(correct_id= IDSpecies.y,
#              IDSpecies= IDSpecies.x)%>%
#       mutate(IDBefore= IDSpecies,
#              Status= ifelse(IDSpecies==correct_id,"Correct", "Modified_IDSp"),
#              IDSpecies= ifelse(IDSpecies==correct_id, IDSpecies, correct_id),
#              IDSpecies= ifelse(is.na(correct_id), IDBefore, IDSpecies)) 
#     
#     
#   }
#   else{
#     stop( print("Please specify fish or inv"))
#   }
#   
# }
# 
# #Species names correction
# speciesnames <- function(LTEM_IDs, clean_sp_list, Label){
#   
#   LTEM <- LTEM_IDs
#   if(Label=="fish"){
#   
#     LTEM <- LTEM %>% 
#       filter(Label=="PEC")
#     
#     clean_sp_list <- clean_sp_list %>% 
#       filter(Label=="PEC")  
#     
#   merge_df <- merge(LTEM, clean_sp_list[, c("Label", "IDSpecies", "Species")], 
#                     by= c("Label", "IDSpecies"), all.x = TRUE) %>% 
#     rename(correct_sp= Species.y,
#            Species= Species.x)%>%
#     mutate(SpeciesBefore= Species,
#            Status= ifelse(Species==correct_sp, Status, "Modified_sp"),
#            Species= ifelse(Species==correct_sp, Species, correct_sp))
#   } else if(Label=="inv"){
#     
#     LTEM <- LTEM %>% 
#       filter(Label=="INV")
#     clean_sp_list <- clean_sp_list %>% 
#       filter(Label=="INV" )  
#     
#     merge_df <- merge(LTEM, clean_sp_list[, c("Label", "IDSpecies", "Species")], 
#                       by= c("Label", "IDSpecies"), all.x = TRUE) %>% 
#       rename(correct_sp= Species.y,
#              Species= Species.x)%>%
#       mutate(SpeciesBefore= Species,
#              Status= ifelse(Species==correct_sp, Status, "Modified_sp"),
#              Species= ifelse(Species==correct_sp, Species, correct_sp))    
#     
#   }else{
#     stop( print("Please specify fish or inv"))
#   }
#   
# }


# END ---------------------------------------------------------------------
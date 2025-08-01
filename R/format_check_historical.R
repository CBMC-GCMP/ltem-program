
sizeid <- function(LTEM,IDSize){
  ltem <- merge(LTEM, IDSize, by= c("Label","IDSize"), all.x= T) %>% 
    rename(correct_size= Size.y,
           Size= Size.x)%>% 
    mutate(Before= Size,
           Status= ifelse(Size==correct_size, "Correct", "Modified"),
           Size= ifelse(Size== correct_size, Size, correct_size))
}


reefsid <- function(LTEM,IDReef){
  ltem <- merge(LTEM, IDReef [, c("IDReef", "Reef", "Habitat")], by= c("Reef", "Habitat"), all.x= T) %>% 
    rename(correct_id= IDReef.y,
           IDReef= IDReef.x)%>% 
    mutate(IDBefore= IDReef,
           Status= ifelse(IDReef==correct_id, "Correct", "Modified_ID"),
           IDReef= ifelse(IDReef== correct_id, IDReef, correct_id),
           IDReef= ifelse( is.na(correct_id), IDBefore, IDReef))
}

reefsname <- function(LTEM,IDReef){
  ltem <- merge(LTEM, IDReef [, c("IDReef", "Reef")], by= "IDReef", all.x= T) %>% 
    rename(correct_reef= Reef.y,
           Reef= Reef.x)%>% 
    mutate(ReefBefore= Reef,
           Status= ifelse(Reef==correct_reef, Status, "Modified_Reef"),
           Reef= ifelse(Reef== correct_reef, Reef, correct_reef))
}



flags <- function(LTEM_corrected){
  IDs <- c("IDReef", "IDSize")
  correct <- c("correct_size", "correct_reef", "correct_id")
  Before <- c("IDBefore", "ReefBefore")
  flags <- LTEM_corrected %>%
    filter(str_detect(Status, "Modified_" )) %>% 
    select(Label, Year, Month, Day, Region, Depth, Transect, any_of(IDs), any_of(Before), any_of(correct), Status) 
}


unique_trnsct <- function(LTEM){
  transects <- LTEM %>% 
  group_by(Year,Region) %>% 
  select(Label, Year, Month, Day, IDReef, Reef, Depth, Transect, Habitat) %>% 
  arrange(Year, Month,Day, IDReef, Reef, Depth,  Transect) %>% 
  unique()

}

compare_trnsct <- function(INV, FISH) {
  t_merge <- merge(
    INV,
    FISH,
    by = c(
      "Region",
      "Year",
      "Month",
      "Day",
      "IDReef" ,
      "Reef",
      "Habitat",
      "Depth",
      "Transect"
    ),
    all = T
  ) %>%
    rename(Label_inv = Label.x,
           Label_pec = Label.y)
  
}



flag_trnsct <- function(COMPARED)  {
  m_transects <- COMPARED %>% 
  mutate(Flag= ifelse(is.na(Label_inv), "missing_INV", "CORRECT"),
         Flag= ifelse(is.na(Label_pec), "missing_PEC", Flag)
  ) %>% 
  group_by(Region) %>% 
  filter(Flag != "CORRECT")
  write_rds(m_transects,"data/missing_transects.RDS") 
}

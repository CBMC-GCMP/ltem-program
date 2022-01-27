
sizeid <- function(LTEM,IDSize){
  ltem <- merge(LTEM, IDSize, by= c("Label","IDSize"), all.x= T) %>% 
    rename(correct_size= Size.y,
           Size= Size.x)%>% 
    mutate(Before= Size,
           Status= ifelse(Size==correct_size, "Correct", "Modified"),
           Size= ifelse(Size== correct_size, Size, correct_size))
}


reefsid <- function(LTEM,IDReef){
  ltem <- merge(LTEM, IDReef [, c("IDReef", "Reef")], by= "IDReef", all.x= T) %>% 
    rename(correct_reef= Reef.y,
           Reef= Reef.x)%>% 
    mutate(Before= Reef,
           Status= ifelse(Reef==correct_reef, "Correct", "Modified"),
           Reef= ifelse(Reef== correct_reef, Reef, correct_reef))
}

flags <- function(LTEM_corrected){
  IDs <- c("IDReef", "IDSize")
  correct <- c("correct_size", "correct_reef")
  flags <- LTEM_corrected %>%
    filter(Status== "Modified") %>% 
    select(any_of(IDs), Before, any_of(correct), Status) 
}


unique_trnsct <- function(LTEM){
  transects <- LTEM %>% 
  group_by(Region) %>% 
  select(Label, Year, Month, Day, IDReef, Reef, Depth, Transect, observador) %>% 
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
      "Depth",
      "Transect"
    ),
    all = T
  ) %>%
    rename(Label_inv = Label.x,
           Label_pec = Label.y)
  
}



flag_trnsct <- function(COMPARED)
  m_transects <- COMPARED %>% 
  mutate(Flag= ifelse(is.na(Label_inv), "missing_INV", "CORRECT"),
         Flag= ifelse(is.na(Label_pec), "missing_PEC", Flag)
  ) %>% 
  group_by(Region) %>% 
  filter(Flag != "CORRECT")
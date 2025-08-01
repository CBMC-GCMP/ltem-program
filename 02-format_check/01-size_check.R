library(googlesheets4)
library(tidyverse)
library(readxl)


# Load data ---------------------------------------------------------------

## Load custom functions
lapply(list.files(path="R/", pattern = ".R", full.names = T), source)

## Load from google sheet; just paste the link
# ltem <- read_sheet("https://docs.google.com/spreadsheets/d/1vVHFQEhvtvmhh5u6GghbUBXvYYR-q-Jc7gnQTF7Zias/edit#gid=2040316618")

## Or, you can load an excel file (CSV, RDS, etc.)
 ltem <- read_xlsx("data/auxiliar/summit/Templates.xlsx", sheet=2)

## Load peripherical list of Sizes
sizes <-  read.csv("data/lists/ltem_size_list.csv")

## Merge and replace wrong Sizes
ltem <- sizeid(ltem,sizes)

## Visualize changed sizes
changes <- flags(ltem)



# Format LTEM database ----------------------------------------------------

ltem <- ltem %>% 
  select(-c(IDSize,Before,correct_size, Status))

rm(changes,
   sizes)


# END ---------------------------------------------------------------------


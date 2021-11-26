
# Loading libraries -------------------------------------------------------

## Install necessary packages, then execute:

library(dplyr)
library(RMySQL)
library(odbc)
library(tidyverse)


# Establish connection to server db ---------------------------------------

## Connection details

# Important: Do not modify
host= "goc-rds-production.cbujotn7bqi9.us-east-1.rds.amazonaws.com"
port = 3306
dbname = "ecological_monitoring"
.user = Sys.getenv("userid")
.password = Sys.getenv("pwd")


# This connects to the database (creates an image, does not actually download anything)
ltem_db = dbConnect(MySQL(),
                  dbname=dbname,
                  host=host,
                  port=port,
                  user= .user,
                  password= .password)


# Explore data ------------------------------------------------------------

## To explore all tables available in this database, use:
dbListTables(ltem_db) #Generates a list of tables


## For available fields (columns) inside each table, use:

dbListFields(ltem_db, #This is my database 
             "ltem_monitoring_reefs" #Name of the table from which I require the fields
             ) 




# Downloading tables ------------------------------------------------------


### Monitoring Data table ###

## First, we get the table using tbl function:

monitoring <- tbl(ltem_db,
                       "ltem_monitoring_database")

#This creates an object of class "tbl_MySQLConnection", not yet a data.frame



## To transform the object into a df, use collect function:
ltem_monitoring <- collect(monitoring) #WARNING: May take a while

#This is a slow process, depending on internet connection


## We can export the df as a csv or excel file if we want

# write.csv(ltem_monitoring, "Data/ltem_monitoring_database.csv")



# Monitoring Metadata -----------------------------------------------------



### Monitoring Reefs table ###

# Repeat the same steps as before

## First

reefs <- tbl(ltem_db, "ltem_monitoring_reefs")

## Second

ltem_reefs <-  collect(reefs)

## Third (Optional)

# write.csv(ltem_reefs, "Data/ltem_monitoring_reefs.csv")





### Monitoring Species table ###

species <- tbl(ltem_db, "ltem_monitoring_species")

ltem_species <- collect(species)

# write.csv(ltem_species, "Data/ltem_monitoring_reefs.csv")









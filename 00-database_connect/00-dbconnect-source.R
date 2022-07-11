
# Loading libraries -------------------------------------------------------

## Install necessary packages, then execute:

library(dplyr)
library(RMySQL)
library(odbc)
library(tidyverse)


# Establish connection to server db ---------------------------------------

## Connection details

# Important: Do not modify
# Requires user and password manual input
host= "goc-rds-production.cbujotn7bqi9.us-east-1.rds.amazonaws.com"
port = 3306
dbname = "ecological_monitoring"
.user = rstudioapi::askForPassword("User")
.password= rstudioapi::askForPassword("Database password")


# This connects to the database (creates an image, does not actually download anything)
ltem_db = dbConnect(MySQL(),
                    dbname=dbname,
                    host=host,
                    port=port,
                    user= .user,
                    password= .password)

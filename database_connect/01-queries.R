
# Loading libraries and objects -------------------------------------------

library(dplyr)
library(RMySQL)
library(odbc)
library(tidyverse)

source("database_connect/00-database-connect.R")

# To work with specific chunks of our data, we can use QUERIES

# Queries -----------------------------------------------------------------

# The cool thing about this, is that we let the server do the work, not our computer, 
# however, we need to know how to "ask" the server, this is done by creating a SQL query,
# SQL is another type of language, now it is possible to do this with dplyr, but 
# it is cool to know a little bit of SQL...

# this is how we create a query: 

count_query <- "SELECT spp_id, COUNT(*)
                FROM temporal
                GROUP BY spp_id"

#' The query above, selects the column species id (spp_id) of each species fished in the
#' conapesca database, and then counts how many times it appears using "COUNT", 
#' the (*) symbol means all rows. Then we need to tell him which table we want to search, 
#' that would be the temporal table, and then that we want to group the count by
#' each species id.
#' 
#' To run the query now we have different options, a first one is with the db package
#' the function below takes the connection object and the query string

dbGetQuery(my_db, count_query)


#' so that above counts how many times the species id is mentioned! cool!!
#' Now we can also use tbl to create tables and collect it!! so we don't have to 
#' download all the dataset


count_data <- tbl(my_db, sql(count_query)) %>% 
        collect()


head(count_data)


# Now let's do another one with species names and make a graph!

count_query <- "SELECT sp_gral_name, COUNT(*)
                FROM temporal
                GROUP BY sp_gral_name"



count_data <- tbl(my_db, sql(count_query)) %>% 
        collect()


head(count_data)


# I want to change the name of the column count, and then plot the species!

count_data %>% 
        rename(count = `COUNT(*)`) %>% 
        ggplot(aes(x=sp_gral_name, y=count)) +
        geom_col() +
        coord_flip()

library(readxl)
library(data.table)

##LOAD MONITORING DATABASE

antes <- read_xlsx("Data/bad_species.xlsx", sheet = 1)

unique(antes$Species)




##CORRECT SPECIES' NAMES
#first we load a file containing all species' names spelled correctly
species <- read.csv("Data/species_list.csv")

correct <- c(species$Specie)


#then we format our dataframe as a table
table <- data.table(antes)

#### Beginning ####
for (i in 1:nrow(table)){#range that goes for all the species rows in our table 
        string <- table[i, Species]# String of our species' names
        max <- 0 #Do not change these values
        similarity <- 0
        
#This will substitute the values in our Species column
#with the values from "correct" that have the highest 
#number of common characters
        for(j in correct){ 
                similarity <-   length(Reduce(intersect, strsplit(c(string, j), split = "")))
                if(similarity > max){
                        max <- similarity
                        to_replace <- j
                }
        }
        table[i,"Species"] <- to_replace
}
#### End ####


#If no errors displayed, we will then proceed
#with our dataframe
despues <- as.data.frame(table)

unique(inv_2021$Species)



#Pueden checar también la página
# https://cran.r-project.org/web/packages/fuzzyjoin/readme/README.html
# Es una función similar a la que se presenta aquí: Se cuenta con un diccionario de palabras
# Luego una columna con nombres mal escritos, se comparan con el diccionario
# Y se reemplazan, en caso de haber errores

install.packages("RSQLite")
library(RSQLite)

setwd("F:/Anol")

filename <- "compas.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

dbListTables(db)

compas <- dbReadTable(db,"compas")
people <- dbReadTable(db,"people")
charge <- dbReadTable(db,"charge")
prisonhistory <- dbReadTable(db,"prisonhistory")
jailhistory <- dbReadTable(db,"jailhistory")
casearrest <- dbReadTable(db,"casearrest")
summary <- dbReadTable(db,"summary")

#EDA

install.packages("DataExplorer") 
library(DataExplorer)

str(compas)
str(people)
people$sex <- as.factor(people$sex)
people$sex <- relevel(x = people$sex,ref = "Male")

save.image(file = "Compasdb.Rdata")

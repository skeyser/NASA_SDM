#Package Loading 
library("pacman")
pacman::p_load("DBI", "odbc", "tidyverse")

#Old RODBC 
#Check out the tables in the database
#con <- RODBC::odbcConnect("mammalDB")
#sqlTables(con, tableType = "TABLE")$TABLE_NAME
#Specify a query of interest 
#qry <- "SELECT DISTINCT(species) 
#        FROM furbearer_harvest_counts"
#Implement the query using the sqlQuery function
#Species <- sqlQuery(con, qry)
#Pull out Harvest database, commented code is 
#old using RODBC formatting DBI and OBDC 
#use dplyr in the background to work around SQL
#fur <- sqlFetch(con, "furbearer_harvest_counts")
#sqlColumns(con, "furbearer_harvest_counts")$COLUMN_NAME
#Specify a query of interest 
# qry <- "SELECT DISTINCT(species) 
#         FROM furbearer_harvest_counts"
#Implement the query using the sqlQuery function
#Species <- sqlQuery(con, qry)


#Establish a connection with the local version of the mammal database
con <- dbConnect(odbc(), "mammalDB")

#Pull out the Harvevst Data from the DB and name an object for it 
fur <- tbl(con, "furbearer_harvest_counts")
fur <- fur %>% 
  collect()

#Pull out sp, season, state info
fur %>% select(season, species, state)

#Remove whitspace from names
fur$species <- trimws(fur$species, which = "both")

#Pull out unique species
sp <- fur %>% select(species) %>%
  distinct(species)

#Pull out unique species by states
sp_st <- fur %>% select(state, species) %>%
  group_by(state) %>% distinct(species) 

sr <- fur %>% 
  group_by(state) %>%
  summarise(SR = n_distinct(species))

#show_query(fur2)

#Harvest States
states <- fur %>% select(state) %>% distinct(state)

#Pull in the regulations DB
regs <- tbl(con, "season_start_and_end_dates")
regs <- regs %>% select(state) %>% collect() %>% distinct(state)

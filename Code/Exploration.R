#Hi Git
library("pacman")
pacman::p_load("tidyverse", "data.table")

getwd()
setwd("C:/Users/skeyser/Box Sync/UW-Madison PhD/NASA Project/Mammal_Data")
fur <- read.csv("furbearer_harvest_counts.csv", header = F)

#Take a look at what is in this file
names(fur) <- c("ID", "state", "season", "count", "species", "geo_ref", 
                "harvest_type", "harvest_est_source", "geo_id")

fur$state <- trimws(fur$state, which = "both")
states <- as.character(unique(fur$state))
states <- states[order(states)]
print(states)

fur$species <- trimws(fur$species, which = "both")
species <- as.character(unique(fur$species))
species <- species[order(species)]
print(species)

setDT(fur)
fur[, .(count = .N), by = species]

geo <- fur[, c("state", "geo_ref")]
geo <- geo[!duplicated(geo),]

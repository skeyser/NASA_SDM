#######################################
#~~~~~~~GBIF Mammal SDM Script~~~~~~~~#
#~~~~~~~~By: Spencer R Keyser~~~~~~~~~#
#######################################
#Script Purpose: First pass for producing
#SDMs using GBIF data. This script will 
#act as a base script for producing mammal
#SDMs in the US using solely GBIF data. 
#In future this script will be modified to add
#fur bearer records and expert range maps.

#Package install
#install.packages('rgbif')

#Set up rgbif account creds
#usethis::edit_r_environ()

#Package Loading
library(rgbif)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)


#Start with a species of interest
#Bobcat to start
#This is the structure for remote data request via GBIF server
name_backbone("Lynx rufus")$usageKey #find the identifier for the species
name_backbone("Martes americana")$usageKey #find the identifier for the species
name_backbone("Martes pennanti")$usageKey #find the identifier for the species
name_backbone("Lontra canadensis")$usageKey #find the identifier for the species


#Request the download
bobcat_gbif <- occ_download(
  type = "and",
  pred("taxonKey", 2435246), #species
  pred("hasGeospatialIssue", FALSE), #remove geospatial issues
  pred("hasCoordinate", TRUE), #Only take occurrence records with coordinates
  pred("occurrenceStatus", "PRESENT"), #Only take where confirmed presence
  pred_gte("year", "1900"), #Only for years greater than 1900
  pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))), #Remove fossils and living specimens
  pred("country", "US"), #Take only for the US
  pred_or(pred_lt("coordinateUncertaintyInMeters",10000),pred_isnull("coordinateUncertaintyInMeters")), #Keep numbers only for points with at least 10km spatial accuracy
  format = "SIMPLE_CSV" #Return a CSV
)

marten_gbif <- occ_download(
  type = "and",
  pred("taxonKey", 5218864), #species
  pred("hasGeospatialIssue", FALSE), #remove geospatial issues
  pred("hasCoordinate", TRUE), #Only take occurrence records with coordinates
  pred("occurrenceStatus", "PRESENT"), #Only take where confirmed presence
  pred_gte("year", "1900"), #Only for years greater than 1900
  pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))), #Remove fossils and living specimens
  pred("country", "US"), #Take only for the US
  pred_or(pred_lt("coordinateUncertaintyInMeters",10000),pred_isnull("coordinateUncertaintyInMeters")), #Keep numbers only for points with at least 10km spatial accuracy
  format = "SIMPLE_CSV" #Return a CSV
)

fisher_gbif <- occ_download(
  type = "and",
  pred("taxonKey", 5218855), #species
  pred("hasGeospatialIssue", FALSE), #remove geospatial issues
  pred("hasCoordinate", TRUE), #Only take occurrence records with coordinates
  pred("occurrenceStatus", "PRESENT"), #Only take where confirmed presence
  pred_gte("year", "1900"), #Only for years greater than 1900
  pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))), #Remove fossils and living specimens
  pred("country", "US"), #Take only for the US
  pred_or(pred_lt("coordinateUncertaintyInMeters",10000),pred_isnull("coordinateUncertaintyInMeters")), #Keep numbers only for points with at least 10km spatial accuracy
  format = "SIMPLE_CSV" #Return a CSV
)

otter_gbif <- occ_download(
  type = "and",
  pred("taxonKey", 2433727), #species
  pred("hasGeospatialIssue", FALSE), #remove geospatial issues
  pred("hasCoordinate", TRUE), #Only take occurrence records with coordinates
  pred("occurrenceStatus", "PRESENT"), #Only take where confirmed presence
  pred_gte("year", "1900"), #Only for years greater than 1900
  pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))), #Remove fossils and living specimens
  pred("country", "US"), #Take only for the US
  pred_or(pred_lt("coordinateUncertaintyInMeters",10000),pred_isnull("coordinateUncertaintyInMeters")), #Keep numbers only for points with at least 10km spatial accuracy
  format = "SIMPLE_CSV" #Return a CSV
)


#check the order wait time
occ_download_wait(marten_gbif)

#Load in the data
bobcat <- occ_download_get(bobcat_gbif) %>%
  occ_download_import()

marten <- occ_download_get(marten_gbif) %>%
  occ_download_import()

fisher <- occ_download_get(fisher_gbif) %>%
  occ_download_import()

otter <- occ_download_get(otter_gbif) %>%
  occ_download_import()


#Once the file has been downloaded we can read it into R via the file key
#saved locally in the project.
bobcat <- occ_download_import(key = "0242490-220831081235567")
marten <- occ_download_import(key = "0249247-220831081235567")
fisher <- occ_download_import(key = "0249196-220831081235567")
otter <- occ_download_import(key = "0249198-220831081235567")

names(bobcat)

#fur
fur <- rbind(bobcat, marten, fisher, otter)


#Make bobcat a spatial DF
fur.sf <- fur %>%
  rename("Lat" = "decimalLatitude") %>%
  rename("Long" = "decimalLongitude") %>%
  filter(!is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters <= 3000) %>%
  filter(year >= 2003) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) 

table(fur.sf$species)


#Visualize the occurrence points
ggplot(fur.sf) +
  geom_sf(aes(color = species))

#Next we need to bring in the expert range maps
mammal.erm <- st_read("F:/IUCN/MAMMS_ALLSPP.shp")
str(mammal.erm)

#Select a species of interest
bobcat.erm <- mammal.erm %>% filter(Species == "Lynx rufus")
marten.erm <- mammal.erm %>% filter(Species == "Martes americana")
fisher.erm <- mammal.erm %>% filter(Species == "Martes pennanti")
otter.erm <- st_read("F:/IUCN/IUCN_MissingSpp/Lontra_canadensis/data_0.shp")

#Visualize the expert range map
fur.sf %>%
  filter(species == "Lynx rufus") %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data = bobcat.erm, fill = NA, color = "red") +
  theme_bw()
  
fur.sf %>%
  filter(species == "Martes americana") %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data = marten.erm, fill = NA, color = "blue") + 
  theme_bw()

fur.sf %>%
  filter(species == "Pekania pennanti") %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data = fisher.erm, fill = NA, color = "green") + 
  theme_bw()

fur.sf %>%
  filter(species == "Lontra canadensis") %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data = otter.erm, fill = NA, color = "orange") + 
  theme_bw()


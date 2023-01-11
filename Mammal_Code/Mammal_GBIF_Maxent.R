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
install.packages('rgbif')

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

#check the order wait time
occ_download_wait(bobcat_gbif)

#Load in the data
d <- occ_download_get(bobcat_gbif) %>%
  occ_download_import()

names(d)

#Make bobcat a spatial DF
d.sf <- d %>%
  rename("Lat" = "decimalLatitude") %>%
  rename("Long" = "decimalLongitude") %>%
  filter(!is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters <= 3000) %>%
  filter(year >= 2003) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) 

#Visualize the occurrence points
ggplot(d.sf) +
  geom_sf()

#Next we need to load in the environmental data

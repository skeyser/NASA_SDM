##########################################################################
######################Snow Track Sruveys Minnesota########################
###########Thinking about an occupancy model for these data###############
##########################################################################

pacman::p_load(tidyverse,
               sf,
               lwgeom)

#Bring in the snow track survey data 
track <- read.csv("P:/Spencer Keyser/NASA/Harvest Data/HarvestData/Minnesota/MN_SnowTrack.csv")
routes <- read.csv("P:/Spencer Keyser/NASA/Harvest Data/HarvestData/Minnesota/routeslookup.csv")
glimpse(track)


#Bring in Wisconsin Snow Track Data 
track.wi <- read.csv("P:/Spencer Keyser/NASA/Harvest Data/Raw_Data/Wisconsin/Wisconsin Snow Track/Transects.csv")

#Bring in the spatial info on the routes
route.sf <- st_read("P:/Spencer Keyser/NASA/Harvest Data/HarvestData/Minnesota/snow_track_routes_3_25_16.shp")
plot(route.sf)
test <- route.sf %>%
  mutate(YEAR_END = if_else(YEAR_END == "current", "2017", YEAR_END)) %>%
  filter(is.na(Sub_Route))
  
# mutate(BrokenRoute = if_else(YEAR_END != 2017, 1, 0)) %>%
  # group_by(Route) %>%
  # filter(any(BrokenRoute == 1))
  
  mutate(total_length = sum(MILES))

#lwgeom split
splt <- nngeo::st_segments(test)
plot(splt$result, col = rainbow(length(splt)))

#Summarise Marten/Hare Observations
mn.t <- track %>%
  dplyr::select(-date) %>%
  group_by(Year, county) %>%
  dplyr::select(starts_with("marten")) %>%
  dplyr::select(matches("\\d$")) %>%
  ungroup() %>%
  mutate(across(starts_with("marten"), ~ifelse(.x > 1, 1, .x))) %>%
  dplyr::select(starts_with("marten")) %>%
  summarise(sum(.))

mn.h <- track %>%
  dplyr::select(-date) %>%
  group_by(Year, county) %>%
  dplyr::select(starts_with("hare")) %>%
  dplyr::select(matches("\\d$")) %>%
  ungroup() %>%
  mutate(across(starts_with("hare"), ~ifelse(.x > 1, 1, .x))) %>%
  dplyr::select(starts_with("hare")) %>%
  summarise(sum(.))

#Summarise Marten Tracks in Wisconsin Snow Track Survey 
unique(track.wi$YR)

track.wi %>%
  dplyr::select(MARTEN) %>%
  mutate(across(MARTEN, ~ifelse(.x > 1, 1, .x))) %>%
  summarise(Tot_Mart = sum(., na.rm = T))

track.wi %>%
  dplyr::select(HARESP) %>%
  mutate(across(HARESP, ~ifelse(.x > 1, 1, .x))) %>%
  summarise(Tot_Hare = sum(., na.rm = T))

#Michigan
mi.mart <- fur %>% 
  filter(state == "Michigan") %>%
  distinct(species)
                

#Michigan SnowTrack
mi.track <- read.csv("P:/Spencer Keyser/NASA/Harvest Data/Raw_Data/Michigan/Snowtrack/MI_Raw_SnowTrack.csv")
mi.track02 <- read.csv("P:/Spencer Keyser/NASA/Harvest Data/Raw_Data/Michigan/Snowtrack/2002_SnowTrack.csv")

mi.snowshoe <- mi.track %>% 
  filter(Species == "Hare") %>%
  select(Species, Rte.No, Mileage) %>%
  group_by(Rte.No, Mileage) %>%
  count()

mi.02 <- mi.track02 %>%
  filter(Species == 1)

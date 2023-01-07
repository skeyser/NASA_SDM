########################################################################
#####Script Written for DB Cleaning for species and geography###########
#####################Written By: Spencer R Keyser#######################
########################Last edits: 7/1/2020############################
########################################################################

#Package Loading 
library("pacman")
pacman::p_load("DBI", "odbc", "tidyverse", "here", "janitor")

pauli_path <- ("P:/Spencer Keyser/NASA/Spatial Data/Spatial Data/")

'%notin%' <- Negate('%in%')

fruit <- c("apple", "grape", "berry")
fruit2 <- c("apple" , "grape")

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
#con <- dbConnect(odbc(), "furdb")
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)}; Dbq=E:/My Drive/PhD/NASA Project/Mammal_Data/Database/SDG_Mammal_Database.accdb;")

#Table Querying
tbls <- odbc::dbListTables(con) #Take a look at the database tables
tbls

#Look at the fields within a sheet in the DB
odbc::dbListFields(con, "season_bag_limits_statewide")
odbc::dbListFields(con, "license_types")
odbc::dbListFields(con, "furbearer_license_sales")
odbc::dbListFields(con, "season_start_and_end_dates")
odbc::dbListFields(con, "state_subdivision_season_bag_limits")
odbc::dbListFields(con, "state_subdivision_season_bag_limits_geography")

#Pull each table 
bag_lim <- tbl(con, "season_bag_limits_statewide")
bag_lim <- bag_lim %>%
  collect()

mn.lim <- bag_lim %>%
  filter(state == "Minnesota" & species %in% c("marten", "fisher"))

#License type
lic <- tbl(con, "license_types")
lic <- lic %>%
  collect()

#Sales
sales <- tbl(con, "furbearer_license_sales")
sales <- sales %>%
  collect()

mn.sales <- sales %>%
  mutate(state = trimws(state)) %>%
  filter(state == "Minnesota")

#Dates
stend <- tbl(con, "season_start_and_end_dates")
stend <- stend %>%
  collect() 

#States
states <- tbl(con, "states")
states <- states %>%
  collect()

#Bag Limits Subdivision
bag_sub <- tbl(con, "state_subdivision_season_bag_limits")
bag_sub <- bag_sub %>% 
  collect()

#Bag Sub Geo
sub_geo <- tbl(con, "state_subdivision_season_bag_limits_geography")
sub_geo <- sub_geo %>%
  collect()

#Combine Sub-lim and sub-geo using the unique ID that Jake created for relating these 
sub_merge <- sub_geo %>%
  full_join(bag_sub, by = c("season_limit_fk_id" = "ID"))

#Pull out relevant Data from the DB and name an object for it 
#Harvest counts
odbc::dbListFields(con, "furbearer_harvest_counts")
fur <- tbl(con, "furbearer_harvest_counts")
fur <- fur %>% 
  collect()

fur <- fur %>% 
  mutate_at(c("state", "geographic_location"), function(x) trimws(x, which = "both")) %>%
  mutate_at(c("geographic_location"), function(x) tolower(x)) %>%
  select(-Field9, - Field10)


####Hare subsampling#####
hare <- fur %>%
  filter(grepl("hare", species, ignore.case = T))
  
#write.csv(fur, here::here("Generated_DFs/HarvestData.csv"))

#Pull in the Geographic Info to get XY coordinates for records
geo <- tbl(con, "XY_Coordinates")
odbc::dbListFields(con, "XY_Coordinates")
geo.full <- geo %>%
  collect()

fur.fix <- geo %>%
  collect() %>%
  rename(geographic_location = NAME, 
         geographic_reference_type = Geographical_Type,
         lon = X,
         lat = Y) %>%
  rename_at(c("State"), .funs = tolower) %>%
  dplyr::select(c("state", "geographic_location", "lon", "lat")) %>%
  mutate_at(c("state", "geographic_location"), function(x) trimws(x, which = "both")) %>%
  mutate_at(c("geographic_location"), function(x) tolower(x)) %>%
  mutate_at(c("geographic_location"), function(x) str_replace(x, "wmu-d[0-9]", "d")) %>%
  mutate_at(c("geographic_location"), function(x) str_replace(x, "wmu-e[0-9]", "e")) %>%
  mutate_at(c("geographic_location"), function(x) str_replace(x, "wmu-f[0-9]", "f")) %>%
  mutate_at(c("geographic_location"), function(x) str_replace(x, "wmu-j[0-9]", "j")) %>%
  mutate_at(c("geographic_location"), function(x) str_replace(x, "wmu-", "")) %>%
  #anti_join(fur, by = c("state", "geographic_location"))
  right_join(fur, by = c("state", "geographic_location")) %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  mutate(start_year = as.integer(substr(season, start = 1, stop = 4))) %>%
  filter(start_year >= 1980) %>%
  distinct()

ny <- fur.fix %>%
  filter(state == "New York")


######################################################################################
###########################Cleaning Time##############################################
######################################################################################

#Cleaning Maine
me <- fur %>% 
  filter(state == "Maine") %>%
  distinct(geographic_location)

#Bring Maine Wildlife Units into the mix
me.wmd <- sf::st_read("E:/My Drive/PhD/NASA Project/Mammal_Data/Spatial Data/Maine_Wildlife_Management_Districts/Maine_Wildlife_Management_Districts.shp")
plot(me.wmd)

#summarize by IDENTIFIER
me.wmd <- me.wmd %>%
  group_by(IDENTIFIER) %>%
  summarise(Area_sq = sum(AREASQMI),
            Area = sum(Shape__Are)) %>%
  filter(!IDENTIFIER %in% c("27", "29", "0"))

me.fur <- fur.fixed %>% 
  filter(state == "Maine") %>%# & species == "marten") %>% 
  #filter(state == "Maine", species == "marten" | species == "bobcat" | species == "coyote" | species == "raccoon") %>%
  dplyr::select(-ID) %>%
  #pivot_wider(names_from = species, values_from = count, values_fill = 0)
  group_by(species, geographic_location) %>%
  summarise(count = sum(count)) %>%
  filter(!is.na(species))


me.fur.shp <- me.wmd %>%
  mutate(ID = as.character(IDENTIFIER)) %>%
  left_join(me.fur, by = c("ID" = "geographic_location")) 
  

#Maine License Sales
me.sales <- sales %>%
  filter(state == "Maine") %>%
  rename(season = `license season or year`,
         sales = `furbearer licenses sold`,
         ID_fur = ID)

me.fur.shp <- me.fur.shp %>%
  left_join(me.sales, by = "season")

me.fur.shp %>%
  filter(!IDENTIFIER == 27 | !IDENTIFIER == 29 | !IDENTIFIER == 0) %>%
  group_by(species, ID) %>%
  ggplot() + 
  geom_sf(data = me.wmd, fill = "gray") +
  geom_sf(aes(fill = log(count))) + 
  facet_wrap(~species, nrow = 2) + 
  theme_bw() + 
  scale_fill_viridis_c(option = "G") +
  theme(strip.text.x = element_text(size = 14),
        axis.text = element_blank())

ggsave("C:/Users/skeyser/Documents/MainePlot.jpg",
       height = 10, width = 10, units = "in", dpi = 800)

#Load in raster for ssl 
ssl.files <- list.files("G:/WHI/SSL/", pattern = "*.tif$", full.names = T)
ssl.files <- ssl.files[2:16]
ssl.st <- raster::stack(ssl.files)

scv.files <- list.files("G:/WHI/SCV/", pattern = "*.tif$", full.names = T)
scv.files <- scv.files[2:16]
scv.st <- raster::stack(scv.files)

fwos.files <- list.files("G:/WHI/FWOS/", pattern = "*tif$", full.names = T)
fwos.files <- fwos.files[2:16]
fwos.st <- raster::stack(fwos.files)

crop.ssl <- raster::crop(ssl.st, me.wmd)
raster::plot(crop.ssl)

crop.scv <- raster::crop(scv.st, me.wmd)
crop.scv <- raster::reclassify(crop.scv, cbind(100, Inf, NA))
raster::plot(crop.scv)

crop.fwos <- raster::crop(fwos.st, me.wmd)
crop.fwos <- raster::reclassify(crop.fwos, cbind(100, Inf, NA))
raster::plot(crop.fwos)

snow.ex <- exactextractr::exact_extract(crop.ssl, me.wmd, 'mean', stack_apply = T, force_df = T)
scv.ex <- exactextractr::exact_extract(crop.scv, me.wmd, 'mean', stack_apply = T, force_df = T)
fwos.ex <- exactextractr::exact_extract(crop.fwos, me.wmd, 'mean', stack_apply = T, force_df = T)

snow.ex <- snow.ex %>%
  rownames_to_column(var = "ID") %>%
  pivot_longer(cols = starts_with("mean."),
               names_to = "season",
               values_to = "ssl") %>%
  mutate(season = gsub("mean.SSL_MOD_Daily_cUSA_", "", season)) %>%
  mutate(season = gsub("_WGS84_14d", "", season)) %>%
  mutate(season = sub("(.{4})(.*)", "\\1-\\2", season)) 

scv.ex <- scv.ex %>%
  rownames_to_column(var = "ID") %>%
  pivot_longer(cols = starts_with("mean."),
               names_to = "season",
               values_to = "scv") %>%
  mutate(season = gsub("mean.SCV_MODMYD_8day_cUSA_Winter_", "", season)) %>%
  mutate(season = gsub("_WGS84_14d", "", season)) %>%
  mutate(season = sub("(.{4})(.*)", "\\1-\\2", season))

fwos.ex <- fwos.ex %>%
  rownames_to_column(var = "ID") %>%
  pivot_longer(cols = starts_with("mean."),
               names_to = "season",
               values_to = "fwos") %>%
  mutate(season = gsub("mean.pFWOS_MODMYD_8day_cUSA_Winter_", "", season)) %>%
  mutate(season = gsub("_WGS84_14d", "", season)) %>%
  mutate(season = sub("(.{4})(.*)", "\\1-\\2", season))

snow.ex <- snow.ex %>%
  full_join(scv.ex) %>%
  full_join(fwos.ex)

me.mod.df <- me.fur.shp %>%
  mutate(season.lag = case_when(season == "2005-2006" ~ "2004-2005",
                                season == "2006-2007" ~ "2005-2006",
                                season == "2007-2008" ~ "2006-2007",
                                season == "2008-2009" ~ "2007-2008",
                                season == "2009-2010" ~ "2008-2009",
                                season == "2010-2011" ~ "2009-2010",
                                season == "2011-2012" ~ "2010-2011",
                                season == "2012-2013" ~ "2011-2012",
                                season == "2013-2014" ~ "2012-2013",
                                season == "2014-2015" ~ "2013-2014",
                                season == "2015-2016" ~ "2014-2015",
                                season == "2016-2017" ~ "2015-2016",
                                season == "2017-2018" ~ "2016-2017",
                                season == "2018-2019" ~ "2017-2018")) %>%
  left_join(snow.ex, by = c("ID", "season")) %>%
  sf::st_set_geometry(NULL) %>%
  filter(!is.na(ssl))

snow.ex2 <- snow.ex %>%
  mutate(ssl2 = ssl) %>%
  dplyr::select(ID, season, ssl2)

scv.ex2 <- scv.ex %>%
  mutate(scv2 = scv) %>%
  dplyr::select(ID, season, scv2)

fwos.ex2 <- fwos.ex %>%
  mutate(fwos2 = fwos) %>%
  dplyr::select(ID, season, fwos2)


me.mod.df <- me.mod.df %>%
  left_join(snow.ex2, by = c("ID", "season.lag" = "season")) %>%
  left_join(scv.ex2, by = c("ID", "season.lag" = "season")) %>%
  left_join(fwos.ex2, by = c("ID", "season.lag" = "season"))

marten.me.glmer <- glm(count ~ scale(ssl2) + scale(scv) + scale(sales), family = poisson, data = me.mod.df)
summary(marten.me.glmer)



#Cleaning Minnesota
mn <- fur %>%
  filter(state == "Minnesota") %>%
  distinct(geographic_location)


mn.cent <- read.csv("P:/Spencer Keyser/NASA/Spatial Data/Spatial Data/Minnesota_Township/TR_Centroids.csv")
mn.cent <- as_tibble(mn.cent)
mn.cent <- mn.cent %>%
  mutate_at("TWP_LABEL", as.character) %>%
  mutate(name = str_remove(TWP_LABEL, "T")) %>%
  mutate(name = str_remove(name, "R")) %>%
  mutate(name = str_replace(name, " ", "n")) %>%
  mutate(name = str_trim(name, side = "both")) %>%
  mutate(name = str_to_lower(name)) %>%
  select(name, X, Y, AREA, PERIMETER)

mn.geo <- mn.cent %>% select(name)
mn.fur <- mn %>% select(name = geographic_location) %>% full_join(mn.geo)

#Put it into the fixed centroid folder
#write.csv(mn.cent, paste0(pauli_path, "Fixed_Centroid_CSVs", "/", "Minnesota_TwnshpRng.csv"))

#Cleaning Nevada
nv <- fur %>% 
  filter(state == "Nevada") #%>%
  # distinct(geographic_location)

nv$geographic_location[nv$geographic_location == "mn"] <- "mineral"
nv$geographic_location[nv$geographic_location == "cc"] <- "carson city"
nv$geographic_location[nv$geographic_location == "wp"] <- "white pine"
nv$geographic_location[nv$geographic_location == "claek"] <- "clark"

nv.geo <- geo.full %>%
  filter(State == "Nevada") %>%
  distinct(NAME)

nv.cent <- read.csv(paste0(pauli_path, "Nevada_County/Nevada_County_Centroid.csv"))
nv.cent <- nv.cent %>%
  mutate(NAME = str_to_lower(NAME)) %>%
  select(name = STATE_NAME, NAME, X, Y, PERIMETER, AREA)

test <- nv.cent %>% select(name = NAME) 
test1 <- nv %>% select(name = geographic_location) %>% anti_join(test)

nv.off <- nv %>%
  filter(geographic_location %in% test1$name)

nv.fix <- nv %>%
  filter(geographic_location == "cc" |
         geographic_location == "mn" |
         geographic_location == "wp" |
         geographic_location == "claek")

#Cleaning Ohio
oh <- fur %>%
  filter(state == "Ohio") %>%
  distinct(geographic_location) %>%
  mutate(abbrev = str_sub(geographic_location, start = 1, end = 3))

oh.geo <- geo.full %>%
  filter(State == "Ohio") %>%
  distinct(NAME)

#Cleaning Illinois
il <- fur %>%
  filter(state == "Illinois") %>%
  mutate(geographic_location = str_trim(geographic_location, side = "both")) %>%
  group_by(geographic_location) %>%
  add_count() %>%
  select(name = geographic_location, n) %>%
  distinct()
il.geo <- geo.full %>%
  filter(State == "Illinois") %>%
  distinct(NAME)

#Cleaning Rhode Island
ri <- fur %>%
  filter(state == "Rhode Island") %>%
  mutate(geographic_location = str_trim(geographic_location, side = "both")) %>%
  distinct(geographic_location) %>%
  select(name = geographic_location)

ri.geo <- geo.full %>%
  filter(State == "Rhode Island") %>%
  distinct(NAME) %>%
  select(name = NAME)

#Cleaning Alaska 
ak <- fur %>%
  filter(state == "Alaska") %>%
  filter(str_detect(geographic_location, "27")) %>%
  filter(geographic_location == "27z")
  # filter(state == "Alaska")  %>%
  # filter(str_detect(geographic_location, "19"))

ak.gmu <- fur %>% 
  filter(state == "Alaska") %>%
  #filter(str_detect(geographic_location, "z")) %>%
  distinct(geographic_location) %>%
  select(name = geographic_location)

ak.geo <- geo.full %>%
  filter(State == "Alaska") %>%
  #filter(str_detect(NAME, "z")) %>%
  distinct(NAME) %>%
  select(name = NAME) 

'%notin%' <- Negate('%in%')

ak.miss <- ak$name[!ak$name %in% ak.geo$name]

#Cleaning Wyoming
wy <- fur %>%
  filter(state == "Wyoming") %>%
  group_by(geographic_location) %>%
  add_count() %>%
  select(geographic_location, n) %>%
  distinct(geographic_location, n)
  #distinct(geographic_location) %>%
  #select(name = geographic_location)

wy.geo <- geo.full %>%
  filter(State == "Wyoming") %>%
  distinct(NAME) %>%
  select(name = NAME)

#Look at sites that have missing geographic information
fur_miss <- fur %>% filter(is.na(lon), is.na(lat))

#What are the problem states and how much are we missing
bad_locs <- fur_miss %>% count(state) %>% arrange(desc(n))

################################################################
#############Bring in NY Harvest Data Raw#######################
################################################################

ny.tab <- read.csv("E:/My Drive/PhD/NASA Project/Mammal_Data/Harvest Data/HarvestData/New York/NY_FUR_RAW.csv")
ny.tab2 <- read.csv("E:/My Drive/PhD/NASA Project/Mammal_Data/Harvest Data/HarvestData/New York/NY_FUR.csv")

ny.tab$Sex[which(ny.tab$Sex == 1)] = "Male"
ny.tab$Sex[which(ny.tab$Sex == 2)] = "Female"
ny.tab$Sex[which(ny.tab$Sex == 3)] = "Unknown"
ny.tab$Species[which(ny.tab$Species == 41)] = "Beaver"
ny.tab$Species[which(ny.tab$Species == 42)] = "Fisher"
ny.tab$Species[which(ny.tab$Species == 43)] = "Otter"
ny.tab$Species[which(ny.tab$Species == 44)] = "Bobcat"
ny.tab$Species[which(ny.tab$Species == 45)] = "Coyote"
ny.tab$Species[which(ny.tab$Species == 46)] = "Marten"

#Bring in the full NY Geo
ny.geo <- read.csv("P:/Spencer Keyser/NASA/Spatial Data/Spatial Data/New_York_Municipality/NewYork_Municipality_XY.csv")

#Pull out the geographic data and format to match the furbearer daataset
ny.geo <- ny.geo %>%
  select(Town = NAME, County = COUNTY, Longitude = X, Latitude = Y) %>%
  mutate(Town = toupper(Town), County = toupper(County)) %>%
  mutate(Town = substr(Town, start = 1, stop = 7)) %>%
  mutate(County = substr(County, start = 1, stop = 4)) %>%
  filter(!str_detect(Town, "\\d"))

#Merge the furbearer data and the geographic data together to see if we can get a county + municipality match
ny.test <- ny.tab %>% 
  mutate_if(is.factor, as.character) %>%
  select(County, Town, SY, Species, Take) %>%
  mutate(County = toupper(County), Town = toupper(Town)) %>%
  group_by(County, Town, SY, Species) %>%
  summarise(Take = sum(Take)) %>%
  left_join(ny.geo, by = c("Town", "County")) %>%
  distinct() %>%
  filter(!is.na(Species), !is.na(Latitude)) %>%
  ungroup() %>%
  mutate(SY = as.character(SY))

ny <- fur %>%
  filter(state == "New York")

ny.fix <- ny %>%
  select(season) %>%
  separate(season, into = c("SY", "EY"), sep = "-") %>%
  mutate(SY = as.character(SY), EY = as.character(EY), 
         season = paste0(SY, "-", EY)) %>%
  distinct() %>%
  select(season, SY) %>%
  left_join(ny.test, by = "SY") %>%
  mutate(state = "New York", 
         geographic_reference_type = "Municipality",
         harvest_type = NA,
         harvest_estimate_source = "pelt_sealed",
         geographic_location = paste0(County, "_", Town),
         ID = NA) %>%
  select(state, season, count = Take, species = Species, geographic_reference_type, ID,
         harvest_type, harvest_estimate_source, geographic_location, lon = Longitude, lat = Latitude)

#write.csv(ny.fix, here::here("Generated_DFs/NY_Fur_Data_Fixed.csv"))  

fur.fixed <- fur.fix %>%
  filter(state != "New York") %>%
  select(state, season, count, species, geographic_reference_type, ID,
         harvest_type, harvest_estimate_source, geographic_location, lon, lat) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
  bind_rows(ny.fix)

#write.csv(fur.fixed, here::here("Generated_DFs/NASA_Fur_GIS.csv"))

#Visualize species in geographic space
fur.sf <- fur.fixed %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) 

coyote.sf <- fur.sf %>%
  mutate(species = tolower(trimws(species, which = "right"))) %>%
  filter(species == "bobcat") 

ggplot(fur.sf) +
  geom_sf()

############################################################################
###Subset the data just to pull out Wisconsin to start workin on the SDMs###
############################################################################

fur.wi <- fur.fixed %>%
  filter(state == "Wisconsin") %>%
  mutate(species = trimws(species, which = "both"))

#write.csv(fur.wi, here::here("Generated_DFs/WI_Furbearer.csv"))


################################################################

# #Take a look at the problem states
# ny <- fur %>% 
#   mutate_all(~trimws(., which = "both")) %>% 
#   filter(state == "New York") %>%
#   mutate(start_year = as.integer(substr(season, start = 1, stop = 4))) %>%
#   filter(start_year >= 1994) #%>%
#   #select(geographic_location) %>%
#   #distinct(geographic_location) %>%
#   #mutate(db = "Harvest") %>%
#   #distinct(geographic_location, geographic_reference_type)
# 
# ny.geo <- geo.full %>% filter(State == "New York") %>%
#   rename(geographic_location = NAME) %>%
#   mutate(dups = duplicated(geographic_location)) %>%
#   filter(dups == T) %>%
#   mutate(geographic_location = substr(geographic_location, start = 1, stop = 7)) %>%
#   filter(!str_detect(geographic_location, "\\d")) %>%
#   select(geographic_location, X, Y)
#   #mutate(db = "GEO") #%>%
#   #distinct(geographic_location, .keep_all = T) #%>%
#   #bind_rows(ny)
# 
# ny.fix <- ny %>%
#   left_join(ny.geo, by = "geographic_location")
# 
# ny.fix <- ny.fix %>%
#   filter(!is.na(X) & !is.na(Y))
#   
# 
# ny.names <- as.character(as.vector(ny.geo$geographic_location))
# 
# jaro <- sapply(ny$geographic_location, FUN = function(x) (jarowinkler(ny.names, x, r = 0.5)))  
# rownames(jaro) <- ny.names
# jaro.df <- t(as.data.frame(jaro))
# jaro.df <- jaro.df[, jaro > 0.8]
# 
# ref <- ny.names
# words <- as.character(as.vector(ny$geographic_location))
# 
# wordlist <- expand.grid(words = words, ref = ref, stringsAsFactors = FALSE)
# word.df <- wordlist %>% group_by(words) %>% mutate(match_score = jarowinkler(words, ref)) %>%
#   summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])
# 
# #write.csv(word.df, here::here("Mammal_Data/NY_matches.csv"))
# #write.csv(ref, here::here("Mammal_Data/NY_Geo.csv"))
# 
# ny.geo.names <- ny.geo$geographic_location
# ny.muni <- unique(ny$geographic_location)
# ny.same <- as.data.frame(ny.muni[ny.muni %in% ny.geo.names])
# ny.diff <- as.data.frame(ny.muni[ny.muni %notin% ny.geo.names])


correct.ct <- ct.geo[duplicated(ct.geo$geographic_location),]
incorrect.ct <- ct.geo[!duplicated(ct.geo$geographic_location),]

#New Hampshire 
nh <- fur %>%
  filter(state == "New Hampshire") %>%
  distinct(geographic_location)

nh.geo <- geo.full %>%
  filter(State == "New Hampshire") %>%
  distinct(NAME)

#Vermont needs some witch craft to fix
#Bring in and clean the Vermont OG CSV
vt.test <- fur %>%
  filter(state == "Vermont") %>%
  mutate(geographic_location = str_remove(geographic_location, "wmu-")) %>%
  #mutate(wsmu = str_detect(geographic_location, "wsmu")) %>%
  #mutate(wsmu = ifelse(wsmu == T, geographic_location, wmsu)) %>%
  mutate(geographic_location = str_remove(geographic_location, "wsmu-")) %>%
  #mutate(wsmu = str_remove(wsmu, "wsmu-")) %>%
  separate(season, into = c("Start", "End"), sep = "-", remove = F) %>%
  mutate(geographic_location = tolower(geographic_location)) %>%
  mutate(unique_id = paste0(geographic_location, "_", Start, "_", species))
  
  #distinct(geographic_location)

vt.geo <- geo.full %>%
  filter(State == "Vermont") %>%
  distinct(NAME)

vt.bad <- vt %>%
  filter(!geographic_location %in% vt.geo$NAME)

vt.og <- read.csv("Z:/Spencer Keyser/NASA/Harvest Data/HarvestData/Vermont/Vermont_Harvest.csv")
head(vt.og)
head(vt.test)

vt.og <- vt.og %>%
  select(Season, geographic_location = WMU, wsmu = WSMU, Days, Traps, Caught, Species) %>%
  mutate(geographic_location = tolower(geographic_location)) %>%
  mutate(string_detection = str_detect(geographic_location, ".*[0-9].*")) %>%
  mutate(geographic_location = ifelse(string_detection == T, wsmu, geographic_location)) %>%
  mutate(wsmu = tolower(wsmu)) %>%
  mutate(Species = tolower(Species)) %>%
  mutate(unique_id = paste0(geographic_location, "_", Season, "_", Species)) %>%
  left_join(vt.test, by = "unique_id") %>%
  select(ID, state, season, count = Caught, species, geographic_reference_type,
         harvest_type, harvest_estimate_source, 
         geographic_location = geographic_location.x) %>%
  filter(!is.na(ID))
  
#write.csv(vt.og, file = "Z:/Spencer Keyser/NASA/Harvest Data/HarvestData/Vermont/Vermont_Harvest_Fixed.csv")

#Colorado

#Pull out sp, season, state info
fur %>% select(season, species, state)

#Remove whitspace from names
fur$species <- trimws(fur$species, which = "both")

#Pull out unique species
sp <- fur %>% select(species) %>%
  distinct(species)

#Pull out unique species by states
sp_st <- fur %>% 
  select(state, species) %>%
  mutate(species = str_trim(species, side = "both"),
         species = str_to_lower(species)) %>%
  group_by(state) %>%
  distinct(species) 

weasel <- fur %>%
  mutate(species = str_to_lower(species),
         species = str_trim(species, side = "both")) %>%
  #filter(species == "weasel")
  filter(str_detect(species, "weasel") & state == "California")

#Remove weird duplcate words
sp_st$species <- sapply(sp_st$species, function(x) paste(unique(unlist(str_split(as.character(x), " "))), collapse = " "))
sp_st <- sp_st %>%
  mutate(species = str_to_lower(species))
#write.csv(sp_st, here::here("Mammal_Data/UniqueSppList.csv"))

sr <- fur %>% 
  group_by(state) %>%
  summarise(SR = n_distinct(species))

#show_query(fur2)

#Harvest States
states <- fur %>% select(state) %>% distinct(state)

#Pull in the regulations DB
regs <- tbl(con, "season_start_and_end_dates")
regs <- regs %>% select(state) %>% collect() %>% distinct(state)

#Taxonomy Issues
weasel <- fur.fix %>%
  filter(species == "weasel") %>%
  filter(count > 0) %>%
  distinct()

fox <- fur.fix %>%
  filter(species == "fox") %>%
  filter(count > 0) %>%
  distinct()

skunk <- fur.fix %>%
  filter(species == "skunk") %>%
  filter(count > 0) %>%
  distinct()

all_tax <- fur.fix %>%
  filter(species == "weasel" | species == "fox" | species == "skunk") %>%
  filter(count > 0) %>%
  distinct()

unique(fur.fix$state)


write.csv(weasel, here::here("Mammal_Data/Weasel_Tax.csv"))
write.csv(skunk, here::here("Mammal_Data/Skunk_Tax.csv"))
write.csv(fox, here::here("Mammal_Data/Fox_Tax.csv"))
write.csv(all_tax, here::here("Mammal_Data/Unresolved_Tax.csv"))

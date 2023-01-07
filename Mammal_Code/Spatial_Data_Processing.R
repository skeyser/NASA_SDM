########################################################
######Geoprocessing Script for State Mammal Data########
########################################################
################Date: 10/23/2020########################
########################################################
################By: Spencer R Keyser####################
########################################################

#This script is desinged to do some basic geoprocessing on the state shapefiles used for the 
#winter mammal database

#Goals: 1) Extract centroid, perimeter, and areas of GPU 2) Join this data with a working version of the mammal data 3)extract relevant raster data

#Goal 1: Extract centorid, perimeter, and areas of GPU

#Package Loading 
if(!require("pacman")){
  install.packages("pacman")
} else {
  print("Pacman already installed!")
}

pacman::p_load("sf", 
               "sp", 
               "rgdal", 
               "rgeos", 
               "tidyverse",
               "gdalUtils",
               "DBI",
               "odbc")

#Function defining
select <- dplyr::select

#Path specifications
pauli.path <- "Z:/Spencer Keyser/NASA/"
env.path <- "Z:/Spencer Keyser/NASA/Environmental_Data/"
spat.path <- "Z:/Spencer Keyser/NASA/Spatial Data/Spatial Data/State_Shp/"

#Proj4 string 
alb.prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Bring in the fixed furbearer dataset
fur <- data.table::fread(here::here("Generated_DFs/NASA_Fur_GIS.csv"))

#Clean the data
#Nevada has ~1000 records that span 2 years. 
#Mass has ~700 that have a season within the same  year. 
#Utah has a couple records with 2 years
fur <- fur %>%
  mutate(across(where(is.character), trimws)) %>%
  separate(., season, into = c("Start", "End"), "-") %>%
  mutate(End = if_else(nchar(End) < 4, paste0("20", End), End)) %>%
  #mutate(Yr.Dif = as.numeric(End) - as.numeric(Start)) %>%
  #filter(Yr.Dif != 1)
  mutate(Season = paste0(Start, "-", End)) %>%
  mutate(species = tolower(species)) %>%
  mutate(species = gsub("[[:punct:]]", "", species)) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  mutate(species = recode(species, river_otter = "otter",
         weasel_long_and_short_tail = "weasel",
         striped_striped_skunk = "striped_skunk",
         grey_fox = "gray_fox",
         gray_wolf = "wolf")) 

#Taking a look at the # of records per species x state
sp.rec <- fur %>%
  select(species, state) %>%
  group_by(species, state) %>%
  add_count(species) %>%
  distinct(species, .keep_all = T) %>%
  arrange(desc(n))
         


#######################################################################################################################
###################Connect with the other files in the db to crreate a complete DF for records#########################
########This should include: license sales for effort, seasons, bag limits for quotas and background sampling,#########
##################################and subdivision bag limts/geography for trickier states.#############################
#######################################################################################################################

#Bring in data for these files
#Connect to the DB 
con <- dbConnect(odbc(), "furdb")

#List DB tables
tbls <- odbc::dbListTables(con) #Take a look at the database tables
tbls

#Look at the fields within a sheet in the DB
odbc::dbListFields(con, "season_bag_limits_statewide")
odbc::dbListFields(con, "license_types")
odbc::dbListFields(con, "furbearer_license_sales")
odbc::dbListFields(con, "season_start_and_end_dates")
odbc::dbListFields(con, "state_subdivision_season_bag_limits")
odbc::dbListFields(con, "state_subdivision_season_bag_limits_geography")

#In order to merge we need to make sure that we have consistency between the geography and the temporal aspects of the DB tables
#Pull each table 
#Bag limits are states specific. They have seasons that can encompass one season or multiple seaosons (multiyear).
#Need to split the seasons. 
bag_lim <- tbl(con, "season_bag_limits_statewide")
bag_lim <- bag_lim %>%
  collect()

glimpse(bag_lim)
distinct(bag_lim, season, species) %>% arrange(desc(season)) %>% as_tibble() %>% print(n = 200)
distinct(bag_lim, species) %>% as_tibble() %>% print(n = 200)

#Create consisten species names 
bag_lim <- bag_lim %>%
  separate(col = season, into = c("Start", "End"), sep = "-", remove = F) %>%
  mutate(species = gsub("[[:punct:]]", " ", species)) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  mutate(species = recode(species,
                          long_tailed_weasel = "weasel",
                          short_tailed_weasel = "weasel",
                          gray_wolf = "wolf",
                          river_otter = "otter",
                          grey_fox = "gray_fox")) %>%
  rename_with(., ~gsub(" ", "_", .x, fixed = T)) %>%
  mutate(season = gsub("-", "_", season)) %>%
  mutate(season_limit_fk_id = NA,
         geographic_location = NA,
         geographic_reference_type1 = "State") %>%
  select(ID, season_limit_fk_id, geographic_location,
         geographic_reference_type1, state, season,
         Start, End, season_limit, species)
  

mn.lim <- bag_lim %>%
  filter(state == "Minnesota" & species %in% c("marten", "fisher"))

#License type
#5x2 matrix of description on type of trapping
lic <- tbl(con, "license_types")
lic <- lic %>%
  collect()
glimpse(lic)

lic <- lic %>%
  rename("license_type" = ID)

#Sales
sales <- tbl(con, "furbearer_license_sales")
sales <- sales %>%
  collect() 
glimpse(sales)

sales <- sales %>%
  mutate(across(.cols = everything(), trimws)) %>%
  rename_with(., ~ gsub(" ", "_", .x, fixed = TRUE)) %>%
  separate(col = license_season_or_year, into = c("Start", "End"), sep = "-", remove = F) %>%
  mutate(license_season_or_year = gsub("-", "_", license_season_or_year)) 

#Take Minnesota to get started
mn.sales <- sales %>% 
  filter(state == "Minnesota")

#Dates
#Season start and end dates and the corresponding geography. 
stend <- tbl(con, "season_start_and_end_dates")
stend <- stend %>%
  collect() 

stend1 <- stend %>% 
  dplyr::select(-contains("field")) %>%
  filter(state != "Rhode Island") %>%
  mutate(season_open_or_closed = gsub(" ", "_", season_open_or_closed)) %>%
  mutate(season_open_or_closed = tolower(season_open_or_closed)) %>%
  mutate(season_open_or_closed = recode(season_open_or_closed, cosed = 'closed_season',
                                        closed = 'closed_season',
                                        no_closed_season = 'open_season')) %>%
  mutate(across(where(is.character), ~gsub("-", "_", .))) %>%
  mutate(season = gsub("/", "_", season),
         season_start = gsub("/", "_", season_start),
         season_end = gsub("/", "_", season_end)) %>%
  mutate(season_start = recode(season_start, Dec_26 = '26_Dec')) %>%
  separate(col = season, into = c("Start_Yr", "End_Yr"), sep = "_", remove = F) %>%
  separate(col = season_start, into = c("Day", "Month"), sep = "_", remove = F) %>%
  separate(col = season_end, into = c("Day", "Month"), sep = "_", remove = F) %>%
  separate(col = season_start, into = c("StDay", "StMon"), sep = "_", remove = F) %>%
  separate(col = season_end, into = c("EDay", "EMon"), sep = "_", remove = F) %>%
  mutate(StMon = recode(StMon, 
                        "Jan" = '1',
                        "Feb" = '2',
                        "Mar" = '3',
                        "Apr" = '4',
                        "May" = '5',
                        "Jun" = '6',
                        "Jul" = '7',
                        "Aug" = '8',
                        "Sep" = '9',
                        "Oct" = '10',
                        "Nov" = '11',
                        "Dec" = '12')) %>%
  mutate(EMon = recode(EMon,
                       "Jan" = '1',
                       "Feb" = '2',
                       "Mar" = '3',
                       "Apr" = '4',
                       "May" = '5',
                       "Jun" = '6',
                       "Jul" = '7',
                       "Aug" = '8',
                       "Sep" = '9',
                       "Oct" = '10',
                       "Nov" = '11',
                       "Dec" = '12')) %>%
  mutate(StartDate = paste(StMon, StDay, Start_Yr, sep = "_"),
         EndDate = paste(EMon, EDay, End_Yr, sep = "_")) %>%
  dplyr::select(-EMon, -EDay, -End_Yr, -Start_Yr, -StMon, -StDay, -Day, -Month) %>%
  #mutate(StartDate = na_if(StartDate, starts_with("NA_"))) %>%
  naniar::replace_with_na_at(.vars = c("StartDate","EndDate"),
                     condition = ~str_detect(.x, "NA_"))


#Rhode Island has wonky date structure. Remove it and fix separately. 
ri <- stend %>%
  filter(state == "Rhode Island") %>%
  dplyr::select(-contains("field")) %>%
  mutate(across(where(is.character), ~gsub("-", "_", .))) %>%
  mutate(season_open_or_closed = gsub(" ", "_", season_open_or_closed)) %>%
  mutate(season_open_or_closed = tolower(season_open_or_closed)) %>%
  mutate(season_open_or_closed = recode(season_open_or_closed, cosed = 'closed_season',
                                        closed = 'closed_season',
                                        no_closed_season = 'open_season')) %>%
  mutate(season = gsub("/", "_", season),
         season_start = gsub("/", "_", season_start),
         season_end = gsub("/", "_", season_end)) %>%
  separate(col = season, into = c("Start_Yr", "End_Yr"), sep = "_", remove = F) %>%
  mutate(StartDate = season_start,
         EndDate = season_end) %>%
  dplyr::select(-Start_Yr, -End_Yr)
  #separate(col = season_start, into = c("Month", "Day", "Year"), sep = "_", remove = F) %>%
  #separate(col = season_end, into = c("Month", "Day", "Year"), sep = "_", remove = F) %>%

stend <- stend1 %>%
  bind_rows(ri)

mn.stend <- stend %>%
  filter(state == "Minnesota")

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
  full_join(bag_sub, by = c("season_limit_fk_id" = "ID")) %>%
  separate(col = "season", into = c("Start", "End"), sep = "-", remove = F) %>%
  mutate(season = gsub("-", "_", season))

#Bring together the subdivision bag limits with the full state limits
#States with a sub limit: WI, OR, MI, SD, WA, NY, IL, PA, NH, OH, MT, AK
#OH - Sublimit on otter only
#WI - sublimt on Bobcat, fisher, otter 1990-2018
#MI - County limits for select counties
#SD - Only limits county for bobcat
#WA - Limits per county for otter and bobcat
#NY - WMU limits. Messy.
#IL - Zone limits on bagder only.
#PA - Beaver, bobcat, fisher WMU limits
#NH - Fisher wmu limits
#MT - WMU limits for most species
#AK - Wolverine, red fox, beaver, fisher limits WMU
#34329 records that need geographic sub limits

bag_lim_f <- bag_lim %>%
  bind_rows(sub_merge) %>%
  filter(geographic_reference_type1 == "State")

#Begin finding ways to merge all of the above files 
#Start by merging effort and trapping regs. Then move into merging with harvest data
glimpse(stend)

glimpse(sales)

#Merge sales and stend. Sales can be *either* season or yearly sales.
season.sales <- sales %>%
  filter(sale_period_tracked == "season")
season.states <- unique(season.sales$state)

wi.sales <- sales %>%
  filter(state == "Wisconsin") %>%
  mutate(species = str_extract(extra_information, pattern = "bobcat|otter|fisher"))

#Wisconsin is the only state that sells licenses by the species. Keep that in mind and make the merge different for WI.
wisc.merged <- stend %>%
  filter(state == "Wisconsin") %>%
  full_join(wi.sales, by = c("season" = "license_season_or_year", "species", "state"))

#Full merge for the seasonal trapping sales. This is the bulk of states.  
season.merged <- stend %>%
  filter(state %in% season.states) %>%
  filter(state != "Wisconsin") %>%
  full_join(season.sales, by = c("state", "season" = "license_season_or_year")) %>%
  bind_rows(wisc.merged)

#Yearly license sales. 
year.sales <- sales %>%
  filter(sale_period_tracked == "year") %>%
  filter(state != "Michigan" & state != "New York") %>%
  mutate(End = as.numeric(Start) + 1) %>%
  mutate(license_season_or_year = paste(Start, End, sep = "_"))

year.states <- unique(year.sales$state)

year.merged <- stend %>%
  filter(state %in% year.states) %>%
  full_join(year.sales, by = c("state", "season" = "license_season_or_year")) %>%
  mutate(End = as.character(End))
  
  

miny.sales <- sales %>%
  filter(state %in% c("Michigan", "New York")) %>%
  mutate(End = if_else(state == "Michigan", as.numeric(Start) + 1, as.numeric(End))) %>%
  mutate(license_season_or_year = paste(as.character(Start), as.character(End), sep = "_"))
  
miny.merge <- stend %>%
  filter(state %in% c("Michigan", "New York")) %>%
  full_join(miny.sales, by = c("state", "season" = "license_season_or_year")) %>%
  mutate(across(.cols = c("Start", "End"), as.character))

sales.dates <- season.merged %>%
  bind_rows(year.merged) %>%
  bind_rows(miny.merge) %>%
  mutate(geographic_reference_type = recode(geographic_reference_type, 
                                            wildlife_managment_unit = "wildlife_management_unit"))

#Completed Sales and Season Start End Date Merging 

glimpse(bag_lim)

sd.lim <- sales.dates %>%
  full_join(bag_lim_f, by = c("state", "species", "season"))


glimpse(sub_merge)

#Work with Minnesota to start since the data is complete and readily merged
#Subset fur and merge
mn.fur <- fur %>%
  filter(state == "Minnesota")

gl.marten <- fur %>% 
  filter(state %in% c("Minnesota", "Michigan", "Wisconsin"))%>%
  filter(species == "marten")

mn.marten <- gl.marten %>%
  filter(state == "Minnesota") %>%
  summarise(MartenTot = sum(count))

table(gl.marten$state)

#Merge with other info
mn.df <- mn.fur %>%
  left_join(mn.lim, by = c("state", "Start", "End", "species")) %>%
  left_join(mn.sales, by = c("state", "Start", "End")) %>%
  left_join(mn.stend, by = c("state", "license_season_or_year" = "season", "species")) %>%
  dplyr::select(!ends_with(".y")) %>%
  dplyr::select(!contains("ID")) %>%
  dplyr::select(-V1)
  
#Minnesota complete. Write a CSV for this state
#data.table::fwrite(mn.df, file = here::here("Generated_DFs/Minnesota_Complete.csv"))

###########################################################################################
#End product will be merged df that includes harvets records and associated collection
#info

############################################################################################
#############################Move onto the spatial processing############################### 
############################################################################################

#Load in the state shapefiles
#List files
state.shp <- list.files(spat.path, pattern = ".shp$")
#Add path name to the file names
state.shp <- paste0(spat.path, state.shp)
#Read in all the files
state.list <- lapply(state.shp, st_read)
#Rename shape files in the list
state.names <-  state.shp %>%
  gsub(spat.path, "", .) %>%
  gsub(".shp", "", .)

names(state.list) <- state.names

#remove Alaska for now because we dont have snow data 
state.list <- state.list[-1]

#Project all shapefiles on the same projection
#check out the data types
st_crs(state.list$CA_County_Albers_Equal_Area)[1]

#iteratively transform CRS
#List the different data sources
state.alb <- list()
states <- names(state.list)

for (i in 1:length(state.list)){
  state.tmp <- state.list[i]
  name.tmp <- states[i]
  if(st_crs(state.tmp[[1]])[1] == "USA_Contiguous_Albers_Equal_Area_Conic"){
    print(paste0(name.tmp, " ", "is in Albers EA"))
    state.alb[i] <- state.tmp
    } else {
    print(paste0(name.tmp, " ", "needs to be transformed!"))
    state.trans <- state.tmp %>%
      map(~st_transform(.x, crs = st_crs(state.alb[[1]])))
    state.alb[i] <- state.trans
    }
} #i

names(state.alb) <- states

#Calculate the metrics of interest per shapefile
for(i in 1:length(state.alb)){
  state.alb[[i]]$area <- st_area(state.alb[[i]])
  state.alb[[i]] <- select(state.alb[[i]], matches("name|coun|wmu|town|muni|area|unit|ident|township|rang|tr|jurisdic", ignore.case = T))
}#i


#State by state extraction of relevant information. Too many different names with no unified pattern will make this too difficult to automate. 
#California
ca.shp <- state.alb[[1]]
ca.shp <- ca.shp %>% select(geographic_location = NAME, area, geometry) %>%
  mutate(state = "California", geographic_reference_type = "county")

#Colorado
co.shp <- state.alb[[2]]
co.shp <- co.shp %>% select(geographic_location = COUNTY, area, geometry) %>%
  mutate(state = "Colorado", geographic_reference_type = "county")

#Connecticut
ct.shp <- state.alb[[3]]
ct.shp <- ct.shp %>% select(geographic_location = town, area, geometry) %>%
  mutate(state = "Connecticut", geographic_reference_type = "municipality")

#Idaho
id.shp <- state.alb[[4]]
id.shp <- id.shp %>% select(geographic_location = County_Nam, area, geometry) %>%
  mutate(state = "Idaho", geographic_reference_type = "county")

#Illinois
il.shp <- state.alb[[5]]
il.shp <- il.shp %>% select(geographic_location = WMU, area, geometry) %>%
  mutate(state = "Illinois", geographic_reference_type = "wildlife management unit")

#Iowa 
ia.shp <- state.alb[[6]]
ia.shp <- ia.shp %>% select(geographic_location = COUNTY, area, geometry) %>%
  mutate(state = "Iowa", geographic_reference_type = "county")

#Maine 
me.shp <- state.alb[[7]]
me.shp <- me.shp %>% select(geographic_location = IDENTIFIER, area, geometry) %>%
  mutate(state = "Maine", geographic_reference_type = "wildlife management unit")

#Massachusetts
ma.shp <- state.alb[[8]]
ma.shp <- ma.shp %>% select(geographic_location = TOWN, area, geometry) %>%
  mutate(state = "Massachusetts", geographic_reference_type = "municipality")

#Michigan
mi.shp <- state.alb[[9]]
mi.shp <- mi.shp %>% select(geographic_location = NAME, area, geometry) %>%
  mutate(state = "Michigan", geographic_reference_type = "county")

#Minnesota
mn.shp <- state.alb[[10]]
mn.shp <- mn.shp %>% 
  mutate(township_range = paste0(TOWN, "n", RANG, "w")) %>% 
  select(geographic_location = township_range, area, geometry) %>%
  mutate(state = "Minnesota", geographic_reference_type = "township_range")

#Montana County
mtco.shp <- state.alb[[11]]
mtco.shp <- mtco.shp %>% select(geographic_location = NAMELABEL, area, geometry) %>%
  mutate(state = "Montana", geographic_reference_type = "county")

#Montana Trapping Districts
mttrap.shp <- state.alb[[12]]
mttrap.shp <- mttrap.shp %>% select(geographic_location = NAME, area, geometry) %>%
  mutate(state = "Montana", geographic_reference_type = "wildlife management unit")

#New Hampshire WMU
nhwmu.shp <- state.alb[[13]]
nhwmu.shp <- nhwmu.shp %>% select(geographic_location = WMU_Fur, area, geometry) %>%
  mutate(state = "New Hampshire", geographic_reference_type = "wildlife management unit")

#New Jersey 
nj.shp <- state.alb[[14]]
nj.shp <- nj.shp %>% select(geographic_location = COUNTY, area, geometry) %>%
  mutate(state = "New Jersey", geographic_reference_type = "county")

#New York Muni
ny.shp <- state.alb[[15]]
ny.shp <- ny.shp %>% 
  mutate(COUNTY = substr(COUNTY, start = 1, stop = 4), NAME = substr(NAME, start = 1, stop = 7)) %>%
  mutate(geographic_location = paste0(COUNTY, "_", NAME)) %>%
  select(geographic_location, area, geometry) %>%
  mutate(state = "New York", geographic_reference_type = "municipality")

#New York WMU
nywmu.shp <- state.alb[[16]]
nywmu.shp <- nywmu.shp %>% select(geographic_location = UNIT, area, geometry) %>%
  mutate(state = "New York", geographic_reference_type = "wildlife management unit")

#Nevada
nv.shp <- state.alb[[17]]
nv.shp <- nv.shp %>% select(geographic_location = NAME, area, geometry) %>%
  mutate(state = "Nevada", geographic_reference_type = "county")

#Ohio
oh.shp <- state.alb[[18]]
oh.shp <- oh.shp %>% select(geographic_location = COUNTY_SEA, area, geometry) %>%
  mutate(state = "Ohio", geographic_reference_type = "county")

#Oregon
or.shp <- state.alb[[19]]
or.shp <- or.shp %>% select(geographic_location = Co_Name, area, geometry) %>%
  mutate(state = "Oregon", geographic_reference_type = "county")

#Pennsylvania
pa.shp <- state.alb[[20]]
pa.shp <- pa.shp %>% select(geographic_location = WMU_ID, area, geometry) %>%
  mutate(state = "Pennsylvania", geographic_reference_type = "wildlife management unit")

#Rhode Island
ri.shp <- state.alb[[21]]
ri.shp <- ri.shp %>% select(geographic_location = COUNTY, area, geometry) %>%
  mutate(state = "Rhode Island", geographic_reference_type = "county")

#South Dakota
sd.shp <- state.alb[[22]]
sd.shp <- sd.shp %>% select(geographic_location = NAME, area, geometry) %>%
  mutate(state = "South Dakota", geographic_reference_type = "county")

#Vermont 
vt.shp <- state.alb[[23]]
vt.shp <- vt.shp %>% select(geographic_location = UNIT, area, geometry) %>%
  mutate(state = "Vermont", geographic_reference_type = "county")

#Washington
wa.shp <- state.alb[[24]]
wa.shp <- wa.shp %>% select(geographic_location = JURISDIC_2, area, geometry) %>%
  mutate(state = "Washington", geographic_reference_type = "county")

#Wisconsin 
wi.shp <- state.alb[[25]]
wi.shp <- wi.shp %>% select(geographic_location = COUNTY_NAM, area, geometry) %>%
  mutate(state = "Wisconsin", geographic_reference_type = "county")

####Finish cleaning the states####

#Bind states together
shp.list <- mget(ls(pattern = "*.shp"), envir = .GlobalEnv)
shp.list <- shp.list[names(shp.list) != "state.shp"]
state.merge <- do.call(rbind, shp.list)

#Now we can plot the states and get ready for extraction
#plot(state.merge)

#Ready to load in some raster data and extract from the shapefiles

#Bring in some temperature data using prism similar to ebird


#Marten work
martes <- fur %>%
  filter(species == "marten" & state == "Minnesota") 


#Start with the US Road Density Map 
roads <- raster::raster(paste0(env.path, "USA_Road_Density.tif"))
plot(roads)

#LCC proj
lcc <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +a=6378137 +b=6356752.314706705 +units=m +no_defs"

#Reproject Raster
gdalwarp(srcfile = paste0(env.path, "USA_Road_Density.tif"),
         dstfile = paste0(env.path, "USA_Road_Density_lcc.tif"),
         s_srs = alb.prj,
         t_srs = lcc,
         tr = c(1000, 1000),
         r = "bilinear",
         output_Raster = T,
         overwrite = TRUE, verbose = TRUE)

#road.proj <- raster::projectRaster(roads, crs = alb.prj, method = "ngb", res = 1000)

#road.proj <- raster::raster(paste0(env.path, "USA_Road_Density_lcc.tif"))
#plot(road.proj)

ssl.path <- "G:/WHI/SSL/"
ssl.mean <- raster::raster(list.files(ssl.path, pattern = "mean.+.tif$", full.names = T))

#Reproject Raster
gdalwarp(srcfile = list.files(ssl.path, pattern = "mean.+.tif$", full.names = T),
         dstfile = paste0(ssl.path, "SSL_MOD_Daily_cUSA_mean20032020_WGS84_14d_lcc.tif"),
         s_srs = raster::crs(ssl.mean),
         t_srs = lcc,
         tr = c(500, 500),
         r = "bilinear",
         output_Raster = T,
         overwrite = TRUE, verbose = TRUE)

ssl.mean <- raster::raster(list.files(ssl.path, pattern = "lcc.tif$", full.names = T))
plot(ssl.mean)

scv.path <- "G:/WHI/SCV/"
scv.mean <- raster::raster(list.files(scv.path, pattern = "mean.+.tif$", full.names = T))

#Reproject Raster
gdalwarp(srcfile = list.files(scv.path, pattern = "mean.+.tif$", full.names = T),
         dstfile = paste0(scv.path, "SCV_MODMYD_8day_cUSA_Winter_mean20032020_WGS84_14d_lcc.tif"),
         s_srs = raster::crs(scv.mean),
         t_srs = lcc,
         tr = c(500, 500),
         r = "bilinear",
         output_Raster = T,
         overwrite = TRUE, verbose = TRUE)

scv.mean <- raster::raster(list.files(scv.path, pattern = "lcc.tif$", full.names = T))
plot(scv.mean)


fwos.path <- "G:/WHI/FWOS/"
# fwos.mean <- raster::raster(list.files(fwos.path, pattern = "mean.+.tif$", full.names = T))
# 
# #Reproject Raster
# gdalwarp(srcfile = list.files(fwos.path, pattern = "mean.+.tif$", full.names = T),
#          dstfile = paste0(fwos.path, "pFWOS_MODMYD_8day_cUSA_Winter_mean20032020_WGS84_14d_lcc.tif"),
#          s_srs = raster::crs(fwos.mean),
#          t_srs = lcc,
#          tr = c(500, 500),
#          r = "bilinear",
#          output_Raster = T,
#          overwrite = TRUE, verbose = TRUE)
# 
fwos.mean <- raster::raster(list.files(fwos.path, pattern = "lcc.tif$", full.names = T))
plot(fwos.mean)

whi.stack <- raster::stack(ssl.mean, scv.mean, fwos.mean)
plot(whi.stack)
#Let's start with data extraction for Wisconsin/Minnesota
#Extract only Wisconsin and Minnesota from the records
wimn <- state.merge %>%
  filter(state %in% c("Minnesota")) %>%
  st_transform(., crs = lcc)

snow.crop <- raster::crop(whi.stack, wimn)
snow.mask <- raster::mask(snow.crop, wimn)

# recordsSpatial <- SpatialPointsDataFrame(
#   coords=cbind(martes$lon, martes$lat), 
#   data=martes,
#   proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs')
# )
# recordsSpatial <- spTransform(recordsSpatial, lcc)
# plot(raster::mask(raster::crop(raster::raster("G:/WHI/SSL/SSL_MOD_Daily_cUSA_mean20032020_WGS84_14d_lcc.tif"), wimn), wimn))
# points(recordsSpatial@coords, color = "count")
# 
# plot(raster::mask(raster::crop(raster::raster("G:/WHI/SCV/SCV_MODMYD_8day_cUSA_Winter_mean20032020_WGS84_14d_lcc.tif"), wimn), wimn))
# points(recordsSpatial@coords)
# 
# plot(raster::mask(raster::crop(raster::raster("G:/WHI/FWOS/pFWOS_MODMYD_8day_cUSA_Winter_mean20032020_WGS84_14d_lcc.tif"), wimn), wimn))
# points(recordsSpatial@coords)

mn.whi <- exactextractr::exact_extract(snow.mask, wimn, )


#Clip the raster for the states of interest
road.crop <- raster::crop(roads, state.merge)
road.crop <- raster::mask(roads, state.merge)
plot(road.crop)
plot(state.merge, col = "black", add = T)

#Extract only Wisconsin and Minnesota from the records
wimn <- state.merge %>%
  filter(state %in% c("Minnesota")) %>%
  st_transform(., crs = raster::crs(road.crop))

#Extract value per polygon and average the value within the polygon
road.ex <- road.crop %>%
  exactextractr::exact_extract(., wimn, c('mean', 
                                          'stdev', 
                                          'coefficient_of_variation'),
                               force_df = T) %>%
  rename(Roads_mean = mean,
         Roads_sd = stdev,
         Roads_CV = coefficient_of_variation) %>%
  bind_cols(., wimn) %>%
  exactextractr::exact_extract(ssl.mean, wimn, c('mean', 
                                          'stdev', 
                                          'coefficient_of_variation'),
                               force_df = T) 
  
  
#Look at the Wisconsin/Minnesoat Furbearer Data
martes.pts <- fur %>%
  filter(state == "Minnesota") %>%
  filter(species == "marten") %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(., crs = lcc)
  #st_transform(crs = raster::crs(road.crop)) 
  
ggplot() +
  geom_sf(data = wimn) +
  geom_sf(data = martes.pts, aes(color = count)) +
  facet_wrap(vars(season),
             nrow = 3) 

ggplot(wimn) + 
  geom_sf() + 
  geom_point(martes.pts, aes(color = count, geometry = geometry)) 
  


#road.ex <- raster::extract(road.crop, state.merge, fun = mean, na.rm = T, df = T)
state.merge <- state.merge %>%
  mutate(ID = 1:nrow(state.merge)) %>%
  left_join(., road.ex, by = "ID")

#Load in clim.data
unique(fur$season)

#Bring in some world clim data to attach to the data
clim <- raster::getData('worldclim', var = 'bio', res = 2.5)
plot(clim[[1:4]])

tmax <- raster::getData('worldclim', var = 'tmax', res = 2.5)

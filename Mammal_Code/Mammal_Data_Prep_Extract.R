######################################################
####Winter Mammal Data Processing and Preparation#####
######################################################

#Load Packages for script
pacman::p_load("here", "tidyverse", "stars", "sf", "sp", "raster", "dismo", "reshape2", "rgdal", "maptools")

#Install non-CRAN packages
#devtools::install_github("michaeldorman/geobgu")
pacman::p_load("geobgu")

#Set pathways for data management 
pauli_path <- "Z:/Spencer Keyser/NASA/"
enviro_path <- "Z:/Spencer Keyser/NASA/Environmental_Data/" 
occ_path <- "Z:/Spencer Keyser/NASA/Generated_DFs/"
spatial_path <- "Z:/Spencer Keyser/NASA/Spatial Data/Spatial Data/"
dsi_path <- "C:/Users/skeyser/Box Sync/UW-Madison PhD/NASA Project/DSI"
#Bring in the data of interest

####################################################
####Species occurence record data manipulation######
####################################################

#Starting with the Wisconsin State Data
fur <- read.csv(paste0(occ_path, "WI_Furbearer.csv"))

#Narrow the data down to just one species. We will start with bobcat
bob <- fur %>%
  filter(species == "bobcat") 

####################################################
############Spatial enviromental Data###############
####################################################

#Bring in the environmental data of interest as a raster

#Snow season length. Start with the first year (2003-2004)
#use new 'stars' package to read in raster data
ssl.03 <- read_stars(paste0(dsi_path, "/SSL/SSL_TERRA_Daily_USA_20032004_WGS84_Final.tif"))
class(ssl.03)
#Visualize raster
plot(ssl.03)

#Read in the vectorized spatial data of interest. For this script this will be WI county data.
wi.co <- st_read(paste0(spatial_path, "Wisconsin_County/Wisconsin_County_AlbersEqual.shp"))

#Transform the polygon data to have equal to the raster
wi.co <- wi.co %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Plot maps to take a look and make sure the shapefile is appropriate
plot(wi.co[1])

#If working on a smaller spatial extent, crop the raster to the polygon of interest
ssl.03.crop <- st_crop(ssl.03, wi.co)

#Visualize cropped raster
plot(ssl.03.crop)

#Extracting raster info for each polygon (i.e. county)
#Extract/Summarize mean, median, standard dev for a raster layer
# at the boundary of a polygon. raster_extract is a function from the 
# 'geobgu' package that was taken from github ("michaeldorman/geobgu")
# it is a wrapper function for raster::extract using the stars package

wi.ext <- wi.co %>%
  mutate(ssl.mean = raster_extract(ssl.03.crop, wi.co, fun = mean, na.rm = T),
         ssl.sd = raster_extract(ssl.03.crop, wi.co, fun = sd, na.rm = T),
         ssl.med = raster_extract(ssl.03.crop, wi.co, fun = median, na.rm = T)
         )

#Create an 'sf' object for species occurrences
bob.sf <- bob %>%
  filter(season == "2004-2005") %>%
  st_as_sf(coords = c("lon", "lat"))
str(bob.sf)

#What is the crs of the sf object
st_crs(bob.sf)

#Assign the object loations a crs
st_crs(bob.sf) <- 4326

plot(st_geometry(bob.sf))

#Take a look at the data offered by DISMO 
fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''),pattern='grd',full.names=TRUE )

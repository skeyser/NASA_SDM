#Load packages
pacman::p_load(tidyverse, 
               raster, 
               sf, 
               sp, 
               rgdal,
               spdep)

#Path names
#Path specifications
pauli.path <- "Z:/Spencer Keyser/NASA/"
env.path <- "Z:/Spencer Keyser/NASA/Environmental_Data/"
spat.path <- "Z:/Spencer Keyser/NASA/Spatial Data/Spatial Data/State_Shp/"

#Proj4 string 
alb.prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


#Load Minnesota data
mn.df <- data.table::fread(here::here("Generated_DFs/Minnesota_Complete.csv"))
glimpse(mn.df)

#Load in the Minnesota township range data 
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
  state.alb[[i]] <- dplyr::select(state.alb[[i]], matches("name|coun|wmu|town|muni|area|unit|ident|township|rang|tr|jurisdic", ignore.case = T))
}#i

#Minnesota
mn.shp <- state.alb[[10]]
mn.shp <- mn.shp %>% 
  mutate(township_range = paste0(TOWN, "n", RANG, "w")) %>% 
  dplyr::select(geographic_location = township_range, area, geometry) %>%
  mutate(state = "Minnesota", geographic_reference_type = "township_range") %>%
  st_transform(., crs = 4326)
plot(mn.shp)

#Furbearer Management Zones
north <- st_read("Z:/Spencer Keyser/NASA/Environmental_Data/shp_trans_roads_mndot_tis/NorthI94.shp")
north <- north %>% 
  mutate(township_range = paste0(TOWN, "n", RANG, "w")) %>%
  dplyr::select(geographic_location = township_range, area = AREA, geometry) %>%
  mutate(state = "Minnesota", geographic_reference_type = "township_range") %>%
  st_transform(., crs = 4326)

south <- st_read("Z:/Spencer Keyser/NASA/Environmental_Data/shp_trans_roads_mndot_tis/SouthI94.shp")
south <- south %>% 
  mutate(township_range = paste0(TOWN, "n", RANG, "w")) %>% 
  dplyr::select(geographic_location = township_range, area = AREA, geometry) %>%
  mutate(state = "Minnesota", geographic_reference_type = "township_range") %>%
  st_transform(., crs = 4326)

plot(north)
plot(south)

#Bring in Rasters and clip them to Minnesota
ssl.files <- list.files(path = "E:/WHI/SSL/", pattern = "*tif$", full.names = T)
scv.files <- list.files(path = "E:/WHI/SCV/", pattern = "*.tif$", full.names = T)
fwos.files <- list.files(path = "E:/WHI/FWOS/", pattern = "*.tif$", full.names = T)

#Remove the mean file
ssl.files <- ssl.files[-length(ssl.files)]
scv.files <- scv.files[-length(scv.files)]
fwos.files <- fwos.files[-length(fwos.files)]

#Stack the ssl data
ssl.s <- raster::stack(ssl.files)


#Stack SCv
scv.s <- raster::stack(scv.files)
plot(scv.s)

#Reclassify where the NAs are 128
scv.s <- raster::reclassify(scv.s, c(-Inf, -1, NA, 101, Inf, NA), include.lowest = T)
plot(scv.s)

#Reclassify where the NAs are 128
#Stack Fwos
fwos.s <- raster::stack(fwos.files)
fwos.s <- raster::reclassify(fwos.s, c(-Inf, -1, NA, 101, Inf, NA), include.lowest = T)
plot(fwos.s)

#Stack all the WHIs
whi.s <- raster::stack(ssl.s, scv.s, fwos.s)

#Crop and mask to Northern MN
mn.crop <- raster::crop(whi.s, mn.shp)
mn.mask <- raster::mask(mn.crop, mn.shp)
mn.mask.n <- raster::mask(mn.crop, north)
mn.mask.s <- raster::mask(mn.crop, south)

#Put tree cover (30 m), road density (500 m) and NTL into the mix
tree.cov <- raster::raster("Z:/Spencer Keyser/NASA/Environmental_Data/NLCD_2016_MN_Tree_Cover.tif")
roads <- raster::raster("Z:/Spencer Keyser/NASA/Environmental_Data/Road_Density_MN.tif")

#Need to resample Tree Cover and Road Density to the level of MODIS (500 meter)
tree.cov <- resample(tree.cov, mn.mask.n[[1]], method = 'ngb')
road.mask <- resample(roads, mn.mask.n[[1]], method = 'ngb')

#Stack the resampled rasters with the WHIs
other.covs <- raster::stack(tree.cov, road.mask)
other.covs <- raster::mask(other.covs, north)

#Extract 2017-2018 from the Rasters 
whi.18 <- mn.mask.n[[grep("20172018", names(whi.s))]]

#Stack and create cov sf object
cov <- raster::stack(whi.18, other.covs)
cov <- raster::aggregate(cov, fact = 20, fun = mean, na.rm = T)
plot(cov)
#writeRaster(cov, "Z:/Spencer Keyser/NASA/Environmental_Data/HarvestModelCovNorthMN_10km.tif", 
#            format = "GTiff", overwrite = T)
# gdalUtils::gdalwarp("Z:/Spencer Keyser/NASA/Environmental_Data/HarvestModelCovNorthMN_10km.tif",
#                              dstfile= "Z:/Spencer Keyser/NASA/Environmental_Data/HarvestModelCovNorthMN_10km_AEA.tif",
#                              t_srs= "+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
#                              #tr = res(ssl.mean)/16.66667,
#                              r = "near",
#                              output_Raster=TRUE,
#                              overwrite=TRUE,verbose=TRUE)

cov <- cov %>% stars::st_as_stars() %>% st_as_sf()

#Extract the vars to each of the township ranges in the North 
whi.ex18 <- exactextractr::exact_extract(whi.18, north, c("mean"),
                                         force_df = T, stack_apply = T)

colnames(whi.ex18) <- c("SSL_Mean", "SCV_Mean", "FWOS_Mean")

cov.ex18 <- exactextractr::exact_extract(other.covs, north, c("mean"),
                                         force_df = T, stack_apply = T)
colnames(cov.ex18) <- c("Tree_Cover", "Road_Density")

north <- north %>%
  bind_cols(whi.ex18) %>%
  bind_cols(cov.ex18)

#Deal with the spatial data for the occurrence records
#Complete the test dataset for Martens
#Dateset contains fisher counts from the northern portion of the state for 2018
fisher <- mn.df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(species, count, 
                season, furbearer_licenses_sold, 
                geographic_location = geographic_location.x,
                geometry) %>%
  filter(species == "fisher" & season == "2017_2018") %>%
  drop_na() 
  
fisher <- st_join(north, fisher, join = st_intersects, left = FALSE) %>%
  dplyr::select(geographic_location = geographic_location.x, 
                species, count, season, SSL_Mean, SCV_Mean, FWOS_Mean, Tree_Cover,
                Road_Density, geometry)



#Next we need to get all of the values for the blocks that don't have fisher and set those to 0
no.fisher <- north %>%
  st_difference(st_union(fisher)) %>%
  mutate(season = unique(fisher$season),
         furbearer_licenses_sold = unique(fisher$furbearer_licenses_sold),
         count = NA, species = "fisher") %>%
  dplyr::select(geographic_location, 
                species, count, season, SSL_Mean, SCV_Mean, FWOS_Mean, Tree_Cover,
                Road_Density, geometry)

#Fisher bind
fisher.f <- fisher %>% bind_rows(no.fisher) %>%
  mutate(across(.cols = c(SSL_Mean, SCV_Mean, FWOS_Mean, Tree_Cover, Road_Density), scale)) 


#Get fisher harvest counts from each TR and merge it with the township range data 
north.tr <- st_read("Z:/Spencer Keyser/NASA/Environmental_Data/NorthI94_SmallRemoved.shp")
missing.tr <- north.tr %>%
  filter(TWP_LABEL == "T48 R15W" | TWP_LABEL == "T36 R20W") %>%
  mutate(species = unique(fisher.f$species),
         count = NA,
         geographic_location = paste0(TOWN, "n", RANG, "w")) %>%
  dplyr::select(geographic_location, species, count,
                trIndex, geometry)

final.harv <- fisher.f %>%
  st_transform(crs(north.tr)) %>% 
  st_centroid() %>%
  st_join(north.tr, join = st_within, left = F) %>%
  distinct(geometry, .keep_all = T) %>%
  dplyr::select(geographic_location, species, count,
                trIndex, geometry) %>%
  bind_rows(missing.tr)

#Bring in the spatial grid for data colation
covs <- st_read(here::here("Generated_DFs/final_covariates_5_poly_10km.shp"))
# covs$ssl <- scale(covs$ssl)
# covs$scv <- scale(covs$scv)
# covs$fwos <- scale(covs$fwos)
# covs$tree <- scale(covs$tree)
# covs$road <- scale(covs$road)

#bring in the high low df
hl <- st_read(here:::here("Generated_DFs/TR_lowhigh_5grid_10km.shp"))

#Create a neighbor adjacency matrix
neighs <- spdep::poly2nb(covs)
nbInfo <- spdep::nb2WB(neighs)


data_harvest_only <- base::list(
  harvest = final.harv$count,
  num = nbInfo$num,
  adj = nbInfo$adj,
  weights = nbInfo$weights,
  ssl = covs$ssl,
  scv = covs$scv,
  fwos = covs$fwos, 
  tree = covs$tree,
  roads = covs$road 
)

constants_harvest_only <- base::list(
  #ncell = 13040, # number of cells in the predictive grid
  ncell = 2159, #Number of cell in the predictive grid 10 km 
  ntr = 1568, # number of township ranges used for th northern portion of the state
  low = hl$LOW,
  high = hl$HIGH,
  #neigh = 102066
  neigh = 16524 #10 km gride neighbor
)

#save.image(file = here::here("Generated_DFs/Fisher_2018_HarvestModel.Rdata"))

rm(list = setdiff(ls(), c("data_harvest_only", "constants_harvest_only")))

#10 km
save.image(file = here::here("Generated_DFs/Fisher_2018_HarvestModel_10km.Rdata"))

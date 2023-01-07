#Working with SDMs for Minnesota to start

#Load packages
pacman::p_load(tidyverse, 
               raster, 
               sf, 
               sp, 
               rgdal,
               maxnet,
               ENMeval,
               maptools,
               dismo)

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

#Pull out Marten to start
marten <- mn.df %>%
  filter(species %in% c("fisher", "marten", "bobcat")) %>%
  filter(Start >= 2004)

#Plot the marten data in space
marten.sf <- marten %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) #%>%
  # ggplot() + 
  # geom_sf()

#Generate some background points
#First run through, I will use only the northern portion of the state 
#where marten can be legally be trapped.

#Bring in the shapefile for the Northern half 
north <- st_read("Z:/Spencer Keyser/NASA/Environmental_Data/shp_trans_roads_mndot_tis/NorthI94.shp")
north <- north %>% 
  mutate(township_range = paste0(TOWN, "n", RANG, "w")) %>%
  dplyr::select(geographic_location = township_range, area = AREA, geometry) %>%
  mutate(state = "Minnesota", geographic_reference_type = "township_range") %>%
  st_transform(., crs = 4326)

#Visualize the shapefile
tribal <- st_read("Z:/Spencer Keyser/NASA/Spatial Data/Spatial Data/Minnesota_Township/shp_bdry_tribal_government/Tribal_Government__in_Minnesota.shp")
tribal <- tribal %>% st_transform(crs = 4326) 
ggplot(north) +
  geom_sf(color = "black", fill = NA) +
  geom_sf(data = marten.sf, aes(color = count)) + 
  geom_sf(data = tribal, fill = NA, color = "blue")

#Bring in spatial data to clip to North MN for background point creation
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
#plot(scv.s)

#Stack FWOS
fwos.s <- raster::stack(fwos.files)

#Dissolve the Northern TR shapefile into one Northern Shapefile
north.mask <- north %>%
  st_jo
  st_union() %>%
  st_cast(., to = "POLYGON") %>%
  as_Spatial(.)

plot(north.mask)
north.ex <- extent(north)

#Crop snow data to mask
ssl.north <- raster::crop(ssl.s, north.ex)
ssl.north <- raster::mask(ssl.north, north.mask)
plot(ssl.north[[1]])

scv.north <- raster::crop(scv.s, north.ex)
scv.north <- raster::mask(scv.north, north.mask)
scv.north <- raster::reclassify(scv.north, c(-Inf, -1, NA, 101, Inf, NA), include.lowest = T)
plot(scv.north[[1]])

fwos.north <- raster::crop(fwos.s, north.ex)
fwos.north <- raster::mask(fwos.north, north.mask)
fwos.north <- raster::reclassify(fwos.north, c(-Inf, -1, NA, 101, Inf, NA), include.lowest = T)
plot(fwos.north[[1]])

#Put tree cover (30 m), road density (500 m) and NTL into the mix
tree.cov <- raster::raster("Z:/Spencer Keyser/NASA/Environmental_Data/NLCD_2016_MN_Tree_Cover.tif")
roads <- raster::raster("Z:/Spencer Keyser/NASA/Environmental_Data/Road_Density_MN.tif")

#Bring in the SNODAS Data 
depth.stack <- raster::stack() #Empty stack for the loop to add to 
years <- 2004:2018 #Set the year range

for(i in 1:length(years)){
  #Pull out the range of dates. This is tailored to the marten trapping season at the moment
  day.list1 <- sort(paste0(years[i], 11, sprintf("%02d", 15:30)))
  day.list <- sort(c(day.list1, paste0(years[i], 12, sprintf("%02d", 1:15))))
  #Subset the files for daily SNODAS rasters
  snodas.files <- list.files(c(paste0("G:/SNODAS/", years[i], "_12_Dec/"), paste0("G:/SNODAS/", years[i], "_11_Nov/")), pattern = "*tif", full.names = T)
  snodas.files <- snodas.files[grep(paste(day.list, collapse = "|"), snodas.files)]
  #Grab the depth variables 
  depth.files <- snodas.files[grep("11036", snodas.files)]
  #Stack the depth files
  depth.s <- raster::stack(depth.files)
  depth.s <- crop(depth.s, north.ex)
  depth.s <- mask(depth.s, north.mask)
  #Calculate the mean snow depth per pixel. Divide by the scaling factor per SNODAS
  depth.mn <- calc(depth.s, mean) / 1000
  
  #Correct for the small spatial shift in extents post 2013
  if(years[i] >= 2013){
    depth.mn <- resample(depth.mn, depth.stack)
  }
  
  depth.stack <- raster::stack(depth.stack, depth.mn)
  
  # swe.files <- snodas.files[grep("11034", snodas.files)]
  # swe.s <- raster::stack(swe.files)
  # swe.s <- crop(swe.s, north.ex)
  # swe.s <- mask(swe.s, north.mask)
  # swe.mn <- calc(swe.s, max) / 1000
  
}

#Need to resample Tree Cover and Road Density to the level of MODIS (500 meter)
tree.cov <- resample(tree.cov, ssl.north[[1]], method = 'ngb')
road.resamp <- resample(roads, ssl.north[[1]], method = 'ngb')
depth.resamp <- resample(depth.stack, ssl.north[[1]], method = 'ngb')
names(depth.resamp) <- paste0("SnowDepth_", c("20042005", "20052006", "20062007", "20072008", "20082009",
                         "20092010", "20102011", "20112012", "20122013", "20132014",
                         "20142015", "20152016", "20162017", "20172018", "20182019"))
#Crop and mask tree cover and roads
tree.north <- raster::crop(tree.cov, north.ex)
tree.north <- raster::mask(tree.north, north.mask)
plot(tree.north)

roads.north <- raster::crop(road.resamp, north.ex)
roads.north <- raster::mask(roads.north, north.mask)
plot(roads.north)

#Stack the envrionmental covs
env.stack <- raster::stack(ssl.north, scv.north, fwos.north, tree.north, roads.north, depth.resamp)

#Create some background points within this distribution to sample from
bg.rando <- randomPoints(ssl.north[[1]], 1000, p = marten[, c(marten$lat, marten$lon)], excludep = T)
bg.rando <- as.data.frame(bg.rando)
colnames(bg.rando) <- c("lon", "lat")

bg.rando %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  ggplot() + 
  geom_sf() + 
  geom_sf(data = north, fill = NA, color = "black") +
  geom_sf(data = marten.sf, color = "red")

#Maybe for background points it just makes the most sense to sample from trappable regions that don't have marten
#occurrences annually. 
seasons <- sort(unique(marten.sf$season))
seasons <- seasons[-1]
species <- unique(marten.sf$species)
#seasons <- seasons[-1:-2]
marten.df <- data.frame(Tree_Cov = NA, Road_Dens = NA, SSL = NA, SCV = NA, FWOS = NA, SnowDepth = NA, Sample_type = NA, season = NA,
                        species = NA, count = NA, geographic_location = NA, geometry = NA)
bg.df <- data.frame(Tree_Cov = NA, Road_Dens = NA, SSL = NA, SCV = NA, FWOS = NA, SnowDepth = NA, Sample_type = NA, season = NA,
                    species = NA, count = NA, geographic_location = NA, geometry = NA)

for(i in 1:length(seasons)){
  for(j in 1:length(species)){

marten.temp <- marten.sf %>%
  filter(season == seasons[i] & species == species[j])

marten.env <- north %>% 
  st_join(., marten.temp, join = st_intersects) %>%
  filter(!is.na(state.y)) %>%
  dplyr::select(count, geometry, geographic_location = geographic_location.x)

marten.data <- exactextractr::exact_extract(env.stack, marten.env, fun = "mean", stack_apply = T)

season.id <- gsub("_", "", seasons[i])

marten.data.sub <- marten.data %>%
  dplyr::select(mean.NLCD_2016_MN_Tree_Cover, mean.Road_Density_MN, contains(season.id)) %>%
  mutate(Sample_type = "Harvest", season = seasons[i], species = species[j]) %>%
  rename(Road_Dens = "mean.Road_Density_MN",
         Tree_Cov = "mean.NLCD_2016_MN_Tree_Cover",
         FWOS = paste0("mean.pFWOS_MODMYD_8day_cUSA_Winter_", season.id, "_WGS84_14d"),
         SCV = paste0("mean.SCV_MODMYD_8day_cUSA_Winter_", season.id, "_WGS84_14d"),
         SSL = paste0("mean.SSL_MOD_Daily_cUSA_", season.id, "_WGS84_14d"),
         SnowDepth = paste0("mean.SnowDepth_", season.id)) %>%
  bind_cols(marten.env)

marten.df <- rbind(marten.df, marten.data.sub)

north.bg <- north %>%
  st_join(., marten.temp, join = st_intersects) %>%
  filter(is.na(state.y)) %>%
  dplyr::select(count, geometry, geographic_location = geographic_location.x)

bg.data <- exactextractr::exact_extract(env.stack, north.bg, fun = "mean", stack_apply = T)

season.id <- gsub("_", "", seasons[i])

bg.data.good <- bg.data %>%
  dplyr::select(mean.NLCD_2016_MN_Tree_Cover, mean.Road_Density_MN, contains(season.id)) %>%
  mutate(Sample_type = "Background", season = seasons[i], species = species[j]) %>%
  rename(Road_Dens = "mean.Road_Density_MN",
         Tree_Cov = "mean.NLCD_2016_MN_Tree_Cover",
         FWOS = paste0("mean.pFWOS_MODMYD_8day_cUSA_Winter_", season.id, "_WGS84_14d"),
         SCV = paste0("mean.SCV_MODMYD_8day_cUSA_Winter_", season.id, "_WGS84_14d"),
         SSL = paste0("mean.SSL_MOD_Daily_cUSA_", season.id, "_WGS84_14d"),
         SnowDepth = paste0("mean.SnowDepth_", season.id)) %>%
  bind_cols(north.bg)

bg.df <- rbind(bg.df, bg.data.good)

  }
}

marten.df <- marten.df %>% slice(-1)
bg.df <- bg.df %>% slice(-1)

final.df <- rbind(marten.df, bg.df)

final.df %>%
  filter(species == "marten") %>%
  st_sf(.) %>%
  #filter(season == "2016_2017") %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, color = ifelse(Sample_type == "Background", 'black', 'red'), fill = SnowDepth)) + 
  facet_wrap(~season)


ggplot(final.df, aes(x = Tree_Cov)) +
  geom_histogram(data = subset(final.df, Sample_type == "Background"), fill = "red", alpha = 0.2) + 
  geom_histogram(data = subset(final.df, Sample_type == "Harvest"), fill = "blue", alpha = 0.2) + 
  facet_wrap(~season) + 
  theme_bw()

#Try to look at the realtionship between snow cover and marten abundance rather than SDMs per se 
pacman::p_load(ggeffects)
marten.ct <- marten.df %>% filter(species == "fisher") %>% drop_na()
ct.mod <- glmmTMB::glmmTMB(count ~ scale(SSL) + scale(SCV) + scale(FWOS) + scale(Tree_Cov) + scale(Road_Dens) + scale(SnowDepth) + (1|season),
          data = marten.ct, family = list(family = "truncated_poisson", link = "log"))
summary(ct.mod)
pr <- ggpredict(ct.mod, "SSL")
plot(pr)
pr <- ggpredict(ct.mod, "SCV")
plot(pr)
pr <- ggpredict(ct.mod, "FWOS")
plot(pr)
pr <- ggpredict(ct.mod, "Tree_Cov")
plot(pr)
pr <- ggpredict(ct.mod, "Road_Dens")
plot(pr)
pr <- ggpredict(ct.mod, "SnowDepth")
plot(pr)



#Load SDMtune
pacman::p_load("SDMtune")

#Maxnet model for Marten N Minnesota
final.df <- final.df %>%
  mutate(PA = if_else(Sample_type == "Background", 0, 1))  %>% 
  filter(!is.na(SSL) | !is.na(SCV) | !is.na(FWOS) | !is.na(SnowDepth) | !is.na(Tree_Cov) | !is.na(Road_Dens))

pa <- final.df$PA
e.mat <- final.df %>% 
  dplyr::select(SSL, SCV, FWOS, Road_Dens, Tree_Cov, SnowDepth)

#####################################################################################
#####################################################################################
#Logistic Regression

#Create a DF from maps of spatial predictors across N MN to predict to
#Bring in the mean WHIs
whi <- raster::stack("E:/WHI/SSL_Mean_2003_2018.tif", "E:/WHI/SCV_Mean_2003_2018.tif", "E:/WHI/FWOS_Mean_2003_2018.tif")
whi <- crop(whi, north.ex)
whi <- mask(whi, north.mask)
plot(whi)

snow.depth.mn <- calc(depth.resamp, mean)
plot(snow.depth.mn)

env.pred <- raster::stack(whi, tree.north, roads.north, snow.depth.mn)
env.pred <- raster::aggregate(env.pred, fact = 10, fun = mean, na.rm = T)
names(env.pred) <- c("SSL", "SCV", "FWOS", "Tree_Cov", "Road_Dens", "SnowDepth")
#Try to model distribution using a logistic regression
#Build the logistic regression model
mod.log <- glm(pa ~ SSL + SCV + FWOS + Tree_Cov + Road_Dens + SnowDepth,
               data = final.df, family = "binomial")

#Check out the summary stats
summary(mod.log)

#Make a DF for the prediction space
env.df <- as.data.frame(env.pred, xy = T) 
env.df <- env.df %>% drop_na()# %>% rename(SSL = SSL_Mean_2003_2018, SCV = SCV_Mean_2003_2018, FWOS = FWOS_Mean_2003_2018,
                                          #Tree_Cov = NLCD_2016_MN_Tree_Cover, Road_Dens = Road_Density_MN)
ggplot(env.df) + geom_raster(aes(x = x, y  = y, fill = SnowDepth))

#Predict onto the surface
env.df$preds <- predict(mod.log, env.df, type = "response")

#ggplot probability of occurrence
ggplot(env.df) + geom_raster(aes(x = x, y  = y, fill = preds)) + 
  scale_fill_viridis_c(option = "D") + theme_void()


#Maxnet model
mod1 <- maxnet(p = pa, data = e.mat)
plot(mod1, type = "link")

maxnet.cloglog.map <- predict(env.pred, mod1, type = "cloglog")
plot(maxnet.cloglog.map, axes = F, box = F)


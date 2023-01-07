#Working with SDMs for Minnesota to start

#Load packages
pacman::p_load(tidyverse, 
               raster, 
               sf, 
               sp, 
               rgdal)

#Path names
#Path specifications
pauli.path <- "P:/Spencer Keyser/NASA/"
env.path <- "P:/Spencer Keyser/NASA/Environmental_Data/"
spat.path <- "P:/Spencer Keyser/NASA/Spatial Data/Spatial Data/State_Shp/"

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
north <- st_read("P:/Spencer Keyser/NASA/Environmental_Data/shp_trans_roads_mndot_tis/NorthI94.shp")
north <- north %>% 
  mutate(township_range = paste0(TOWN, "n", RANG, "w")) %>%
  dplyr::select(geographic_location = township_range, area = AREA, geometry) %>%
  mutate(state = "Minnesota", geographic_reference_type = "township_range") %>%
  st_transform(., crs = 4326)

south <- st_read("P:/Spencer Keyser/NASA/Environmental_Data/shp_trans_roads_mndot_tis/SouthI94.shp")
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
tree.cov <- raster::raster("P:/Spencer Keyser/NASA/Environmental_Data/NLCD_2016_MN_Tree_Cover.tif")
roads <- raster::raster("P:/Spencer Keyser/NASA/Environmental_Data/Road_Density_MN.tif")

#Need to resample Tree Cover and Road Density to the level of MODIS (500 meter)
tree.cov <- resample(tree.cov, mn.mask[[1]], method = 'ngb')
road.mask <- resample(roads, mn.mask[[1]], method = 'ngb')

#Stack the resampled rasters with the WHIs
other.covs <- raster::stack(tree.cov, road.mask)

#Extract WHI data for each county
whi.ex <- exactextractr::exact_extract(mn.mask, north, c("mean", "stdev"), 
                                       force_df = T, stack_apply = T)

cov.ex <- exactextractr::exact_extract(other.covs, north, c("mean", "stdev"),
                                       force_df = T, stack_apply = T)

#Create a list of names fo the WHI data 
df.names <- paste(rep(c("SSL", "SCV", "FWOS"), c(17,17,17)), rep(2003:2019, 3), rep(2004:2020, 3), rep("Mean"), sep = "_")
df.names <- c(df.names, paste(rep(c("SSL", "SCV", "FWOS"), c(17,17,17)), rep(2003:2019, 3), rep(2004:2020, 3), rep("SD"), sep = "_"))

colnames(whi.ex) <- df.names

colnames(cov.ex) <- c("Tree_Cover", "Road_Density", "Tree_Cover_SD", "Road_Density_SD")

mn.whi <- north %>% 
  bind_cols(whi.ex) %>%
  bind_cols(cov.ex)  

#Looking at harvest records per species
marten.plot <- mn.df %>% 
  filter(species == "marten") %>% 
  group_by(lon, lat) %>% 
  summarise(total_harvest = sum(count)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  ggplot() + 
  geom_sf(aes(color  = total_harvest)) + 
  geom_sf(data = north, fill = NA, color = "black") +
  scale_colour_viridis_c(option = "G") + 
  theme_void() + 
  labs(color = "Total Harvest") #+
  #facet_wrap(~season)

fisher.plot <- mn.df %>% 
  filter(species == "fisher") %>% 
  group_by(lon, lat) %>% 
  summarise(total_harvest = sum(count)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  ggplot() + 
  geom_sf(aes(color  = total_harvest)) + 
  geom_sf(data = north, fill = NA, color = "black") +
  #geom_sf(data = south, fill = NA, color = "black") +
  scale_colour_viridis_c(option = "D") + 
  theme_void() + 
  labs(color = "Total Harvest") #+ 
  #facet_wrap(~season)

otter.plot <- mn.df %>% 
  filter(species == "otter") %>% 
  group_by(lon, lat) %>% 
  summarise(total_harvest = sum(count)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  ggplot() + 
  geom_sf(aes(color  = total_harvest)) + 
  geom_sf(data = north, fill = NA, color = "black") +
  geom_sf(data = south, fill = NA, color = "black") +
  scale_colour_viridis_c(option = "C") + 
  theme_void() + 
  labs(color = "Total Harvest") +
  theme(legend.position = "left")
#facet_wrap(~season)

bobcat.plot <- mn.df %>% 
  filter(species == "bobcat") %>% 
  group_by(lon, lat) %>% 
  summarise(total_harvest = sum(count)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  ggplot() + 
  geom_sf(aes(color  = total_harvest)) + 
  geom_sf(data = north, fill = NA, color = "black") +
  #geom_sf(data = south, fill = NA, color = "black") +
  scale_colour_viridis_c(option = "B") + 
  theme_void() + 
  labs(color = "Total Harvest") +
  theme(legend.position = "left")
#facet_wrap(~season)

#cowplot::plot_grid(marten.plot, fisher.plot, nrow = 1)

layout <- "
########
AAAACCCC
AAAACCCC
AAAACCCC
BBBBDDDD
BBBBDDDD
BBBBDDDD
"


otter.plot + bobcat.plot + marten.plot + fisher.plot + plot_layout(design = layout)

ggsave("F:/My Drive/PhD/NASA Project/R_projects/Mammal_SDMs/Figures_SDM/FurbearerMap.jpg", height = 8, width = 8, dpi = 800)
ggsave(marten.plot, filename = "E:/BobcatPlot.jpg", height = 6, width = 8, dpi = 500, device = "jpeg")

#Hunter plot scaled by bag limits 
mn.df %>%
  filter(species == "fisher") %>%
  mutate(Trapper_no = count / season_limit) %>%
  group_by(lon, lat, season) %>%
  summarise(AvgTrapper = mean(Trapper_no)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  ggplot() + 
  geom_sf(aes(color = AvgTrapper)) + 
  geom_sf(data = north, fill = NA, color = "black") + 
  scale_colour_viridis_c(option = "A") + 
  theme_void() + 
  labs(color = "Average Trapper #") + 
  facet_wrap(~season)

mn.df %>%
  filter(species == "marten") %>%
  mutate(Trapper_no = count / season_limit) %>%
  group_by(lon, lat, season) %>%
  summarise(AvgTrapper = mean(Trapper_no)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  ggplot() + 
  geom_sf(aes(color = AvgTrapper)) + 
  geom_sf(data = north, fill = NA, color = "black") + 
  scale_colour_viridis_c(option = "A") + 
  theme_void() + 
  labs(color = "Average Trapper #") + 
  facet_wrap(~season)

mn.df %>%
  filter(species == "marten") %>%
  mutate(Trapper_no = count / season_limit) %>%
  group_by(lon, lat) %>%
  summarise(TotTrapper = mean(Trapper_no),
            Take = mean(count)) %>%
  #st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  ggplot(aes(x = TotTrapper, y = Take)) + 
  geom_point() + 
  geom_smooth(method = "lm") #+ 
  #labs(color = "Average Trapper #") + 
  #facet_wrap(~season)


mn.trends <- mn.df %>% 
  filter(species == "marten") %>%
  group_by(Season, species) %>% 
  summarise(cpu = sum(count)/((mean(furbearer_licenses_sold)*0.8)*0.25)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Season, y = cpu, color = species, group = species)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_color_viridis_d() + 
  labs(color = "Species") + 
  ylab("Catch per unit effort") 


#Scale values were based on a 2012-2013 trapper survey for the state of Minnesota 
#80% of respondents trapped. 35% of trappers targeted fisher and 21% targeted marten
#26% went for bobcat statewide and 35% went for otter. 
mn.df %>% 
  #filter(species == "fisher" | species == "marten") %>%
  mutate(cpu.scale = case_when(species == "fisher" ~ 0.35, 
                               species == "marten" ~ 0.21,
                               species == "otter" ~ 0.35,
                               species == "bobcat" ~ 0.26)) %>%
  group_by(Season, species) %>% 
  summarise(cpu = mean(count * 0.8 * cpu.scale)) %>%
  ungroup() %>% 
  ggplot(aes(x = Season, y = cpu, color = species, group = species)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_color_viridis_d() + 
  labs(color = "Species") + 
  ylab("Catch per unit effort") 

ggsave(plot = mn.trends, filename = "E:/mn_cpu_trends.jpg",
       units = "in", height = 6, width = 8, dpi = 500, device = "jpeg")

mn.df %>% 
  group_by(Season) %>% 
  summarize(mean = mean(furbearer_licenses_sold)) %>% 
  ggplot() + 
  geom_point(aes(x = Season, y = mean)) 

whi.ex %>% 
  pivot_longer(cols = everything()) %>% 
  separate(col = "name", into = c("whi", "start", "end", "metric"), sep = "_") %>%
  mutate(season = paste0(start, "-", end)) %>% 
  filter(metric == "Mean") %>% 
  group_by(whi, season) %>% 
  summarise(mean = mean(value, na.rm = T)) %>% 
  ggplot(aes(season, mean, color = whi, group = whi)) + 
  geom_smooth(se = F) + 
  facet_wrap(~whi, scales = "free")

#Extract SSI information
#Mn.fur WHI merge
mn.fur.sf <- st_as_sf(mn.df, coords = c("lon", "lat"), crs = 4326)

test <- mn.whi %>% 
  st_join(mn.fur.sf, join = st_intersects, left = TRUE) #%>%
  pivot_longer(cols = starts_with(c("SSL", "SCV", "FWOS"))) %>%
  filter(!is.na(value)) %>%
  filter(str_detect(name, "SSL|SCV|FWOS")) %>%
  filter(str_detect(name, "Mean"))

ssi.ssl <- test %>% 
  separate(col = "name", into = c("whi", "whi_start", "whi_end", "metric")) %>% 
  filter(!is.na(species)) %>% 
  group_by(species, whi) %>% 
  summarise(ssi = mean(value), ssi_sd = sd(value))

#Try the exraction at the season level
test <- test %>% 
  filter(Start >= 2003) %>% 
  mutate(ID = 1:nrow(.))

df.holder <- data.frame(ID = NA, SSL_Mean = NA, SSL_SD = NA, 
                        SCV_Mean = NA, SCV_SD = NA, 
                        FWOS_Mean = NA, FWOS_SD = NA)

season <- sort(unique(mn.df$Season))
season <- season[-1:-2]
#for(i in 1:length(unique(mn.df$Season))){
for(i in 1:length(season)){
  season.tmp <- season[i]
  df.tmp <- test %>% filter(Season == season.tmp) %>% st_set_geometry(NULL)
  df.tmp <- df.tmp %>% mutate(Season = gsub("-", "_", Season)) %>%
    mutate(SSL_Mean = NA, SSL_SD = NA, SCV_Mean = NA, SCV_SD = NA, FWOS_Mean = NA, FWOS_SD = NA)
  df.fix <- df.tmp %>% dplyr::select(ID, starts_with("SSL") | starts_with("SCV") | starts_with("FWOS")) %>%
    dplyr::select(ID, contains(unique(df.tmp$Season))) %>%
    coalesce()
  colnames(df.fix) <- c("ID", "SSL_Mean", "SSL_SD", "SCV_Mean", "SCV_SD", "FWOS_Mean", "FWOS_SD")
  df.holder <- df.holder %>% bind_rows(df.fix)
  
}

df.holder <- df.holder %>% slice(-1)

test <- test %>% left_join(df.holder, by = "ID") %>% dplyr::select(-5:-106)
names(test)

#Calculate the SSI from the new DF
test %>% pivot_longer(cols = c("SSL_Mean", "SCV_Mean", "FWOS_Mean"), 
                       names_to = "whi") %>%
  group_by(species, whi) %>% summarise(ssi = mean(value, na.rm = T),
                                       ssi.sd = sd(value, na.rm = T)) %>%
  ggplot(aes(x = species, y = ssi, fill = whi)) + 
  geom_col() + 
  #geom_errorbar(aes(ymin = ssi - ssi.sd, ymax = ssi + ssi.sd), width = 0.2, position = position_dodge(0.9)) + 
  facet_wrap(~whi, scales = "free") + 
  ylab("SSI Value") + 
  xlab("Species") +
  theme_bw() + 
  scale_fill_grey()

test %>%
  dplyr::select("SSL_Mean", "SCV_Mean", "FWOS_Mean") %>%
  drop_na() %>%
  st_set_geometry(NULL) %>%
  cor(.) %>%
  corrplot::corrplot(method = c("number"))


#Complete the test dataset for Martens
#Dateset contains fisher counts from the northern portion of the state for 2018
fisher <- test %>%
  dplyr::select(species, count, 
                Tree_Cover, Road_Density, 
                SSL_Mean, SCV_Mean, FWOS_Mean, 
                season, furbearer_licenses_sold, 
                geographic_location = geographic_location.x,
                geometry) %>%
  filter(species == "fisher" & season == "2017_2018") %>%
  drop_na()

#Next we need to get all of the values for the blocks that don't have fisher and set those to 0
no.fisher <- north %>%
  dplyr::select(geographic_location, geometry) %>%
  st_difference(st_union(fisher)) %>%
  mutate(season = unique(fisher$season),
         furbearer_licenses_sold = unique(fisher$furbearer_licenses_sold),
         count = 0)

#Trying to model fisher in the northern half of the state for one year. 



pacman::p_load("glmmTMB", "effects")

marten <- test %>% filter(species == "marten")

m0 <- glmmTMB(count ~ 1 + (1|season), data = test, family = "poisson")
m1 <- glmmTMB(count ~ scale(SSL_Mean) + scale(FWOS_Mean) + scale(Tree_Cover) + scale(Road_Density) + scale(furbearer_licenses_sold) + (1|season), data = test, family = "poisson")
summary(m1)
m.aic <- MuMIn::dredge(m1)

plot(allEffects(m1))








#Old shit, probably going to delete soon here.

#WHI stack
ssl.mean <- raster::raster("E:/WHI/SSL_Mean_2003_2018.tif")
scv.mean <- raster::raster("E:/WHI/SCV_Mean_2003_2018.tif")
fwos.mean <- raster::raster("E:/WHI/FWOS_Mean_2003_2018.tif")
roads.path <- "Z:/Spencer Keyser/NASA/Environmental_Data/USA_Road_Density.tif"

# gdalwarp(roads.path,
#          dstfile= "Z:/Spencer Keyser/NASA/Environmental_Data/USA_Road_Density_WGS84.tif",
#          t_srs= crs(ssl.mean),
#          tr = res(ssl.mean)*2,
#          r = "near",
#          output_Raster=TRUE,
#          overwrite=TRUE,verbose=TRUE)

roads <- raster::raster("Z:/Spencer Keyser/NASA/Environmental_Data/USa_Road_Density_WGS84.tif")

whi.s <- raster::stack(ssl.mean, scv.mean, fwos.mean)
# nlcd <- raster::raster("Z:/Spencer Keyser/NASA/Environmental_Data/mn_nlcd06.tif")
# gdalwarp("Z:/Spencer Keyser/NASA/Environmental_Data/mn_nlcd06.tif",
#          dstfile= "Z:/Spencer Keyser/NASA/Environmental_Data/NLCD_06_WGS84.tif",
#          t_srs= crs(ssl.mean),
#          tr = res(ssl.mean)/16.66667,
#          r = "near",
#          output_Raster=TRUE,
#          overwrite=TRUE,verbose=TRUE)
# 
# nlcd <- raster::raster("Z:/Spencer Keyser/NASA/Environmental_Data/NLCD_06_WGS84.tif")
# plot(nlcd)


# gdalwarp("Z:/Spencer Keyser/NASA/Environmental_Data/nlcd_2016_treecanopy_2019_08_31/nlcd_2016_treecanopy_2019_08_31.img",
#          dstfile= "Z:/Spencer Keyser/NASA/Environmental_Data/nlcd_2016_treecanopy_2019_08_31/NLCD_TreeCov_WGS84.tif",
#          t_srs= crs(ssl.mean),
#          #tr = res(ssl.mean)/16.66667,
#          r = "near",
#          output_Raster=TRUE,
#          overwrite=TRUE,verbose=TRUE)

tree.cov <- raster::raster("Z:/Spencer Keyser/NASA/Environmental_Data/nlcd_2016_treecanopy_2019_08_31/NLCD_TreeCov_WGS84.tif")
plot(tree.cov)


mean.crop <- crop(whi.s, mn.shp)
mean.mask <- mask(mean.crop, mn.shp)
plot(mean.mask)

roads.crop <- crop(roads, mn.shp)
road.mask <- mask(roads.crop, mn.shp)
plot(road.mask)
#writeRaster(road.mask, "Z:/Spencer Keyser/NASA/Environmental_Data/Road_Density_MN.tif", overwrite = T)

tree.crop <- crop(tree.cov, mn.shp)
tree.mask <- mask(tree.crop, mn.shp)
plot(tree.mask)
# writeRaster(tree.mask, "Z:/Spencer Keyser/NASA/Environmental_Data/NLCD_2016_MN_Tree_Cover.tif",
#             format = "GTiff", overwrite = T)

nlcd.crop <- crop(nlcd, mn.shp)
nlcd.mask <- mask(nlcd.crop, mn.shp)
plot(nlcd.mask)

nlcd.mask <- resample(nlcd.mask, mean.mask, method = 'ngb')
road.mask <- resample(road.mask, mean.mask, method = 'ngb')

env.s <- raster::stack(mean.mask, nlcd.mask, road.mask)
plot(env.s)
















#Doing Laura's run for class for now
#Generate the background points for minnesota 
randomBgSites <- dismo::randomPoints(mean.mask, 1000)
## can also do spatial thinning of background points
# extract environment at sites
randomBgEnv <- raster::extract(env.s, randomBgSites)
randomBgEnv <- as.data.frame(randomBgEnv)

isNa <- is.na (rowSums(randomBgEnv))
?any
any(isNa) ###in this case there isn't any TRUE statements for is.na
if (any(isNa)) { ##if there were TRUE statements, then remove these records from analysis
  randomBgSites <- randomBgSites[-which(isNa), ]
  randomBgEnv <- randomBgEnv[-which(isNa), ]
}

#
mn.max <- mn.df %>% filter(Start >= "2003" & species == "fisher") %>%
  dplyr::select("lon", "lat")

recordsSpatial <- SpatialPointsDataFrame(
  coords=cbind(mn.max$lon, mn.max$lat), 
  data=mn.max,
  proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs')
)


# combine with coordinates and rename coordinate fields
randomBg <- cbind(randomBgSites, randomBgEnv)
colnames(randomBg)[1:2] <- c('longitude', 'latitude')
head(randomBg)

plot(env.s[[1]]) # plot first raster in climate stack
points(randomBgSites, pch='.')

records <- as.data.frame(recordsSpatial)

enmeval_results <- ENMeval::ENMevaluate(records[,c('lon','lat')], 
                               env.s, 
                               bg.coords=randomBgSites[1:1000,],
                               method="randomkfold", #validation method
                               kfold=4, #number of folds
                               RMvalues = seq(0.5, 4, 0.5), #regularization
                               fc=c('L','Q','LQ'), ##features
                               algorithm='maxnet',rasterPreds=T)

## differences in AUC due to differences in regularization and features
eval.plot(enmeval_results@results, 'avg.test.AUC', var='var.test.AUC')
## differences in AICc due to differences in regularization and features
eval.plot(enmeval_results@results)

par(mfrow=c(1,2), mar=c(2,2,1,0))

aic.mod <- enmeval_results@models[[which(enmeval_results@results$delta.AICc==0)]]
var.importance(aic.mod)

#Let’s see how model complexity changes the predictions in our example. 
#We’ll compare the model predictions of the model with only linear feature classes and with the highest regularization multiplier value we used
plot(enmeval_results@predictions[['L_4']],legend=T, main='L_4 prediction')
# to the model with all three feature classe and  the lowest regularization multiplier value we used
plot(enmeval_results@predictions[['LQ_0.5']], legend=T, main='LQ_0.5 prediction')
## remember that this are raw outputs, so it refers to relative occurrence rate (ROR)

### Niche overlap statistics between model predictions 
overlap <- calc.niche.overlap(enmeval_results@predictions, stat='D')
overlap['L_4','LQ_0.5'] ##similarity between the two predictions

minAICc<-min(enmeval_results@results$AICc)
which(enmeval_results@results$AICc==minAICc)
maxAUC<-max(enmeval_results@results$avg.test.AUC)
which(enmeval_results@results$avg.test.AUC==maxAUC)

plot(enmeval_results@predictions[[which(enmeval_results@results$AICc==minAICc)]],legend=T, main='minAICc')
# to the model with all three feature classe and  the lowest regularization multiplier value we used
plot(enmeval_results@predictions[[which(enmeval_results@results$avg.test.AUC==maxAUC)]], legend=T, main='maxAUC')
## remember that this are raw outputs, so it refers to relative occurrence rate (ROR)

bestmod = which(enmeval_results@results$delta.AICc==0)
var.importance(bestmod)
par(mfrow=c(1,1))
# Now let’s access the RasterStack of the model predictions. 
# Note that these predictions are in the ‘raw’ output format.
# "Raw output is useful for comparing different models for the same species using the same background samples"
plot(enmeval_results@predictions[[bestmod]], main="Relative occurrence rate")
# Now add the occurrence and background points, colored by evaluation bins:
points(enmeval_results@bg.pts, pch=3, col='grey', cex=0.5)
points(enmeval_results@occ.pts, pch=21, bg=enmeval_results@occ.grp)

# make prediction - logistic output
ENMmodel<-enmeval_results@models[[bestmod]]
ENMmap = predict(env.s,ENMmodel, type = 'logistic')

par(mfrow=c(1,1))
plot(ENMmap)
points(records)

plot(ENMmodel, vars = names(ENMmodel$samplemeans), common.scale = T,
                      type = "logistic")


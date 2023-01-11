#########################################################
#~~~~~Environmental Data Preparation for Mammal SDM~~~~~#
#########################################################

#Load Packages
library(sf)
library(terra)
library(dplyr)
library(ggplot2)

#List of the relevant environmental parameters
#1. Snow cover (WHIs)
#2. Average Winter Minimum Temperature (Daymet or PRISM)
#3. Elevation (GMTED)
#4. TPI (GMTED)
#5. Forest Cover (Global Forest Cover)
#6. Urbanized land (VIIRS)
#7. Road Density (TIGER)
#8. Stream Network Density
#9. Distance to water (Water Mask)

#Keep all of these at a 3km scale for modelling

#WHIs mean aggregation to ~3km scale
ssl <- rast("G:/WHI/Backfilled/SSL/SSL_BF_20032020.tif")
plot(ssl)
ssl.rs <- terra::aggregate(ssl, fact = 6, fun = function(x) mean(x, na.rm = T))
plot(ssl.rs)

scv <- rast("G:/WHI/Backfilled/SCv/SCV_BF_20032020.tif")
plot(scv)
scv.rs <- terra::aggregate(scv, fact = 6, fun = function(x) mean(x, na.rm = T))
plot(scv.rs)

fwos <- rast("G:/WHI/Backfilled/FWOS/FWOS_BF_20032020.tif")
plot(fwos)
fwos.rs <- terra::aggregate(fwos, fact = 6, fun = function(x) mean(x, na.rm = T))
plot(fwos.rs)

#Winter Average Minimum Temperature
tmin <- rast("E:/My Drive/GEE_Output/TempWinterMinDMT.tif")
plot(tmin)
tmin.rs <- terra::resample(tmin, ssl.rs, method = "bilinear")
plot(tmin.rs)

#Urbanization
nlt <- rast("E:/My Drive/GEE_Output/Average_Monthly_Ntl_VIIRS.tif")
plot(nlt)

#Percent Forest Cover
fc <- rast("G:/CanopyCover/nlcd_2016_treecanopy_2019_08_31.img")
plot(fc)
activeCat(fc) <- "value"
fc <- aggregate(fc, fact = 100, method = "mode")
fc
fc <- project(fc, crs(ssl.rs))

fc.rs <- resample(fc, ssl.rs, method = "mode")
plot(fc.rs)
fc.rs

#Road network density

#Stream density

#Distance to water



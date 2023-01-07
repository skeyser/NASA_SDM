pacman::p_load(sf,
               stars, 
               tidyverse)

north <- st_read("Z:/Spencer Keyser/NASA/Environmental_Data/shp_trans_roads_mndot_tis/North194_Fix.shp")
grid <- read_stars("Z:/Spencer Keyser/NASA/Environmental_Data/HarvestModelCovNorthMN_10km_AEA.tif")

# vectorize raster grid
grid <- st_as_sf(grid[1], as_points = FALSE, merge = FALSE)
grid <- grid %>% drop_na()
ggplot(grid) + geom_sf()
#grid <- st_transform(grid, crs = st_crs(north))

# get grid only within Counties
t <- sapply(st_intersects(grid, north), function(z) if (length(z)==0) NA_integer_ else z[1])
grid <- grid[!is.na(t),] #; rm(t)
r <- sapply(st_intersects(north, grid), function(z) if (length(z)==0) NA_integer_ else z[1])
north <- north[!is.na(r),] #; rm(t)

#Plot the TR with the grid 
ggplot(north) + geom_sf(data = grid, fill = "grey", color = "black") + geom_sf(fill = NA, color = "red")

# assign index
north$trIndex <- 1:nrow(north)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

## per grid cell, identify ID covering largest area
lst <- lapply(1:nrow(grid), function(i) {
  
  # create polygons subset based on current grid cell
  spy_poly_crp <- st_crop(north, grid[i, ])
  
  # case 1: no polygon intersects with current cell
  if (is.null(spy_poly_crp)) {
    print('\n\nGrid cell',i, 'did not intersect with a county\n\n')
    # case 2: one polygon intersects with current cell  
  } else if (nrow(spy_poly_crp) == 1)  {
    return(spy_poly_crp$trIndex) 
    # case 3: multiple polygons intersect with current cell
    # -> choose sub-polygon with largest area
  } else {
    areas <- st_area(spy_poly_crp)
    index <- which.max(areas)
    return(spy_poly_crp[index, ]$trIndex)
  }
})

# add info to grid
grid$trIndex <- unlist(lst)
range(grid$trIndex)
seq2 <- min(grid$trIndex):max(grid$trIndex)
trIndex.drop <- seq2[!seq2 %in% grid$trIndex]
grid.drop <- north[north$trIndex %in% trIndex.drop, ]
ggplot(grid.drop) + geom_sf()
ggplot(grid) + geom_sf()
#Group grid by indices to generate the high and low for each 

#Some township ranges are really small. We need to remove those because they new actually get 
#assigned a cell
north.new <- north[!north$trIndex %in% trIndex.drop, ]
ggplot(north.new) + geom_sf()
north.new$trIndex <- 1:nrow(north.new)

## per grid cell, identify ID covering largest area
lst <- lapply(1:nrow(grid), function(i) {
  
  # create polygons subset based on current grid cell
  spy_poly_crp <- st_crop(north.new, grid[i, ])
  
  # case 1: no polygon intersects with current cell
  if (is.null(spy_poly_crp)) {
    print('\n\nGrid cell',i, 'did not intersect with a county\n\n')
    # case 2: one polygon intersects with current cell  
  } else if (nrow(spy_poly_crp) == 1)  {
    return(spy_poly_crp$trIndex) 
    # case 3: multiple polygons intersect with current cell
    # -> choose sub-polygon with largest area
  } else {
    areas <- st_area(spy_poly_crp)
    index <- which.max(areas)
    return(spy_poly_crp[index, ]$trIndex)
  }
})

grid$trIndex <- unlist(lst)
range(grid$trIndex)
seq2 <- min(grid$trIndex):max(grid$trIndex)
trIndex.drop <- seq2[!seq2 %in% grid$trIndex]

# now get low and high
low <- c()
high <- c()
bad <- c()
for(i in 1:length(unique(grid$trIndex))){
  holdInt <- grid[grid$trIndex == i,]
  if(nrow(holdInt) == 0){
    next
  #bad <- c(bad, i)  
  }else{
    if(i == 1){
      holdInt$index <- 1:nrow(holdInt)
      prevRow <- nrow(holdInt)
      low[i] <- 1
      high[i] <- prevRow
    }else{
      holdInt$index <- (nrow(goodstuff)+1):(nrow(holdInt)+prevRow)
      prevRow <- sum(nrow(holdInt), nrow(goodstuff))
      low[i] <- (nrow(goodstuff)+1)
      high[i] <- prevRow
    }
  }
  
  if(i == 1){
    goodstuff <- rbind(holdInt)
  }else{
    goodstuff <- rbind(goodstuff, holdInt)
  }
}

grid <- goodstuff
# rm(holdInt, goodstuff, i, prevRow, lst)

# proof of purchase
plot(grid[1]) # covs still look good
plot(grid[6]) # county index

tr_low_high <- north.new %>% 
  tibble::add_column(LOW = low, 
                    HIGH = high)

final_covariates_again <- goodstuff %>% 
  rename(ssl = HarvestModelCovNorthMN_10km_AEA.tif.V1,
         scv = HarvestModelCovNorthMN_10km_AEA.tif.V2,
         fwos = HarvestModelCovNorthMN_10km_AEA.tif.V3, 
         tree = HarvestModelCovNorthMN_10km_AEA.tif.V4, 
         road = HarvestModelCovNorthMN_10km_AEA.tif.V5) 
  

st_write(tr_low_high, here::here("Generated_DFs/TR_lowhigh_5grid_10km.shp"))
st_write(final_covariates_again, here::here("Generated_DFs/final_covariates_5_poly_10km.shp"))
st_write(north.new, "Z:/Spencer Keyser/NASA/Environmental_Data/NorthI94_SmallRemoved_10km.shp")

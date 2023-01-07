getwd()
setwd("C:/Users/skeyser/Box Sync/UW-Madison PhD/NASA Project/Mammal_Data/Spatial Data")
cdir <- "C:/Users/skeyser/Box Sync/UW-Madison PhD/NASA Project/Mammal_Data/Spatial Data/"
files.names <- list.files(path = cdir, pattern = "*.zip", full.names = T)

for (i in 1:length(files.names)){
  name <- files.names[i]
  newdir.tmp <- sub(cdir, "", name)
  newdir <- sub(".zip", "", newdir.tmp)
  unzip(files.names[i], exdir = newdir)
}

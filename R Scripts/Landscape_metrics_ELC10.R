
### Load packages
library(terra)
library(sf)
library(exactextractr)

#### DATA ----------------------------------------------------------------------------------
### Buffers
buffers.list=list.files("Input Data/Buffer/", recursive = T, full.names = T, pattern = "\\.shp$")
buffers.names=list.files("Input Data/Buffer/", recursive = T,  pattern = "\\.shp$", include.dirs = F)
### Land-cover map
# The map is splitted for the cities, we need to load two different rasters and merge them
# Info on the landcover map and the values is in https://zenodo.org/records/4407051 :
# 0 (not mapped #000000), 
# 1 (Artificial land, #CC0303), 
# 2 (Cropland, #CDB400), 
# 3 (Woodland, #235123), 
# 4 (Shrubland, #B76124), 
# 5 (Grassland, #92AF1F), 
# 6 (Bare land, #F7E174), 
# 7 (Water/permanent snow/ice, #2019A4), 
# 8 (Wetland, #AEC3D6).


## Geneva and Zurich
ECL10_1=terra::rast("Input Data/ECL10/ELC10_2018_10m-0000279552-0000159744.tif")
## Lugano
ECL10_2=terra::rast("Input Data/ECL10/ELC10_2018_10m-0000279552-0000199680.tif")
# We merge the rasters, it takes a bit of time
s <- sprc(ECL10_1, ECL10_2)
ECL10_merged <- merge(s)
terra::writeRaster(ECL10_merged, "Input Data/ECL10/ECL10_merged.tif") ## And we export it so we don't have to redo that
## Load the merged raster
ECL10_merged=terra::rast("Input Data/ECL10/ECL10_merged.tif")

#### Calculation ----------------------------------------------------------------------------------
zonal.statistic.city=list()
for (b in 1:length(buffers.list)) {
  print(paste("Using buffer:", buffers.names[b]))
  # We first select one of the buffers for one of the cities 
  selected.buffer.path=buffers.list[[b]] ## the path
  selected.buffer=terra::vect(paste(selected.buffer.path)) # we load the bvector file
  selected.buffer=terra::project(selected.buffer, 'EPSG:3035') # change the projection to match the ELC10
  data.trans=list()
  for(s in 1:60){ ## loop to for each site within a city
    # Select one site
    buff=selected.buffer[s,] 
    # Crop the ELC according to the site
    extracted.values=crop(x =  ECL10_merged, y= buff,mask=T) ## zonal statistics
    # convert to dataframe
    extracted.values.df=as.data.frame(extracted.values)
    extracted.values.df[,1]=as.factor(extracted.values.df[,1])
    # transpose
    df.temp=as.data.frame.matrix(t(table(extracted.values.df)))
    # Format the dataframe
    df.temp$ID=buff$ID # add site ID
    df.temp$city=paste(substr(x = buffers.names[b], start = 1, stop=2)) # add site citx
    df.temp$buffer=paste(substr(x = buffers.names[b], start = 4, stop=(nchar(buffers.names[b])-4))) # add buffer ID
    # Store as list
    data.trans[[s]]=df.temp
  }
  # Make sure that all dataframes have the same number of columns. Each number is a landcover type
  col <-unique(unlist(sapply(data.trans, names)))
  df.lst <- lapply(data.trans, function(df) {
    df[, setdiff(col, names(df))] <- NA
    df})
  ## Unlist the the individual site calculations and bind them
  df.complete=do.call(rbind, df.lst)
  # Remove NAs
  df.complete[is.na(df.complete)] <- 0
  # Store again as a list (each item will be the collection of all the sites in a city for a given buffer)
  zonal.statistic.city[[b]]=df.complete
}
### The final file
# Again, we need to make sure all dataframes have the same dimensions before binding them
col2 <- c("1", "2",'3','4','5','6','7','8', "ID", "city", "buffer")
df.lst2 <- lapply(zonal.statistic.city, function(df) {
  df[, setdiff(col2, names(df))] <- NA
  df})
df.zonal.statistic.all=do.call(rbind, df.lst2)
df.zonal.statistic.all[is.na(df.zonal.statistic.all)] <- 0 ## Tschüss NA


write.csv(x = df.zonal.statistic.all, file = "Output Data/df.zonal.statistic.all.csv")





                  
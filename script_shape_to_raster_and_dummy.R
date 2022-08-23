## Load libraries
library(raster)
library(rgdal)

# Set working directory and load shapefile and raster
setwd('C:/Users/neliq/Documents/NIF/Rothamsted')
shp <- shapefile('shapefiles/soil_layer.shp')
img <- stack("satellites_images/Sentinel/2016-07-19.tif")

# Convert character to numeric
shp@data$Soil_Code <- as.numeric(shp@data$Soil_Code)

# Shapefile to raster
shp_to_raster <- rasterize(shp, img, 'Soil_Code')
plot(shp_to_raster)

# Check extent and export raster
extent(shp_to_raster) == extent(img)  
writeRaster(shp_to_raster, 'rasters/soil_raster.tif')

# Convert raster to dummy
dummyRaster <- function(rast){
  rast <- as.factor(rast)
  result <- list()
  for(i in 1:length(levels(rast)[[1]][[1]])){
    result[[i]] <- rast == levels(rast)[[1]][[1]][i]
    names(result[[i]]) <- paste0(names(rast),
                                 levels(rast)[[1]][[1]][i])
  }
  return(stack(result))
}


soil_dummy <- dummyRaster(shp_to_raster)
names(soil_dummy) <- c("Dg_Chb", 'Fa', 'Hk', 'Hw')
plot(soil_dummy)


# Stack image and new soil dummy raster
covs_dummy <- stack(img, soil_dummy)
plot(covs_dummy)

writeRaster(covs_dummy, 'rasters/covs_s2a_soil_dummy.tif')

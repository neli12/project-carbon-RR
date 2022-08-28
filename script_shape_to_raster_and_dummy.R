## Load libraries
library(raster)))
library(rgdal)

# Set working directory and load shapefile and raster
setwd('C:/Users/neliq/Documents/NIF/Rothamsted')
shp_soil <- shapefile('shapefiles/soil_layer.shp')
shp_field <- shapefile('shapefiles/field.shp')
img <- stack("satellites_images/Landsat/2011-04-21.tif")

# Convert character to numeric
shp_soil@data$Soil_Code <- as.numeric(shp_soil@data$Soil_Code)
shp_field@data$code_field <- as.numeric(shp_field@data$code_field)

# Shapefile to raster
soil_to_raster <- rasterize(shp_soil, img, 'Soil_Code')
field_to_raster <- rasterize(shp_field, img, 'code_field')
plot(soil_to_raster)
plot(field_to_raster)

# Check extent and export raster
extent(soil_to_raster) == extent(img)  
extent(field_to_raster) == extent(img)
writeRaster(soil_to_raster, 'rasters/fieldL5_raster.tif', overwrite=TRUE)
writeRaster(field_to_raster, 'rasters/soilL5_raster.tif', overwrite=TRUE)

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


# Apply dummy function and rename
soil_dummy <- dummyRaster(soil_to_raster)
field_dummy <- dummyRaster(field_to_raster)
names(soil_dummy) <- c("Dg_Chb", 'Fa', 'Hk', 'Hw')

names_field <- levels(sort(factor(shp_field@data$Field_Name)))
names(field_dummy) <- names_field
plot(field_dummy)


# Join image and new dummys and export

covs <- stack(img, soil_dummy, field_dummy)
names(covs )

writeRaster(soil_dummy, 'rasters/soilS2A_dummy.tif', overwrite=TRUE)
writeRaster(field_dummy, 'rasters/fieldS2A_dummy.tif', overwrite=TRUE)
writeRaster(covs, 'rasters/covsS2A_soil_field.tif', overwrite=TRUE)


writeRaster(soil_dummy, 'rasters/soilL5_dummy.tif' , overwrite=TRUE)
writeRaster(field_dummy, 'rasters/fieldL5_dummy.tif', overwrite=TRUE)
writeRaster(covs, 'rasters/covsL5_soil_field_1.tif', overwrite=TRUE)

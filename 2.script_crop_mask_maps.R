# Load libraries
library(raster)
library(RColorBrewer)

# Load files
setwd("~/NIF/Rothamsted/8.predicted_files")
obs2012 <- read.csv("obs_geo_pred2012.csv")
obs2016 <- read.csv("obs_geo_pred2016.csv")
coordinates(obs2012) <- ~Easting+Northing
coordinates(obs2016) <- ~Easting+Northing
plot(obs2012)
plot(obs2016)

# Load shapefile
setwd("~/NIF/Rothamsted/2.shapefiles")
area_field <- shapefile("area_field.shp")
plot(area_field, add = TRUE)

# Load geo maps
setwd("~/NIF/Rothamsted/4.geostatistics")
GEOmap2012 <- raster("Ctotal_2012_geo27700.tif")
GEOmap <- raster("Ctotal_2016_geo27700.tif")

# Load GBM maps
setwd("~/NIF/Rothamsted/6.results")
GBMmap2012 <- raster("CTotalL8_2012.tif")
GBMmap <- raster("CTotal2016_gbm25.tif")

# Crop and mask
GEOmap.crop <- crop(GEOmap2012, area_field)
GEOmap.mask <- mask(GEOmap.crop, area_field)

GBMmap.crop <- crop(GBMmap2012, area_field)
GBMmap.mask <- mask(GBMmap.crop, area_field)


# Plot and save
jpeg("example2.jpeg", width = 3500, height = 2000, res = 300)
par(mfrow = c(1,2))
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
cols <- brewer.pal(5, 'RdYlGn')
plot(GBMmap.mask, col = cols, main = 'Modeling by GB', breaks = c(3, 3.5, 4.5, 5, 6.8), legend=FALSE)
plot(area_field, add = TRUE)
plot(GEOmap.mask, col = cols, main = 'Geostatistics', breaks = c(3, 3.5, 4.5, 5, 6.8))
plot(area_field, add = TRUE)
dev.off()

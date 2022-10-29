# Load libraries
library(raster)

# Set working directory
setwd("~/NIF/Rothamsted/5.satellites_images/")
list.files()

# Load DEM and image
DEM <- raster("DEM_terrain/DEM_5m.tif")
terrain <- stack("DEM_terrain/terrain_crop_5m.tif")
S2A_2018 <- stack("Sentinel/30m/S2A_2018.tif")
S2A_2019 <- stack("Sentinel/30m/S2A_2019.tif")
S2A_2020 <- stack("Sentinel/30m/S2A_2020.tif")
S2A_2021 <- stack("Sentinel/30m/S2A_2021.tif")

# Resample S2A 
S2A_res <- resample(S2A_2021, DEM, method = 'ngb')
plot(S2A_res)

## Stack S2A, DEM and terrain and export
covs_stack <- stack(S2A_res, DEM, terrain)
names(covs_stack) <- c("B2", "B3", "B4", "B5", "B6", "B7", 'B8',
                       '8A', "B11", "B12", "NDVI", "NDRE", "GNDVI", "NDMI",
                       "EVI", "BSI","DEM", "ASP", "CI", "LS", "PC",
                       "RSP", "SLP", "TWI")
plot(covs_stack)
writeRaster(covs_stack, 'S2ADEMTerrain2021.tif')



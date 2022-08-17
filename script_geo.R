## Load libraries
library(gstat)
library(sp)
library(raster)
library(hydroGOF)
library(leaflet)
library(sf)
library("dplyr")
library(ggplot2)
library(patchwork)
library(viridis)

# Set working directory and load files
setwd("C:/Users/neliq/Documents/NIF/Rothamsted/datasets_RR")
list.files()

field_2016 <- read.csv("field_2016.csv", sep = ';')
colnames(field_2016) <- c('Experiment',"SP_ID", "sample_distance_m", "Field", "Sample_date", 
                          "Sample_time", 'X', 'Y', "pH", "BD_gcm3", "OM%", "N_total%", "C_total",
                          "conductivity_uScm")

hist(field_2016$C_total)


# Set dataset as spatial object
coordinates(field_2016) <- ~X+Y
plot(field_2016)

spplot(field_2016["C_total"], main = "C%")

## Fit universal kriging 
g <- gstat(id="C_total%", formula = C_total~X+Y, data = field_2016)
variog <- gstat::variogram(g)
plot(variog, pch=16, cex=1)


fit_exp <- fit.variogram(variog, vgm(1, "Exp", 200, 0.4))   #vgm(psill, model, range, nugget)
plot(variog, fit_exp, pch=16, cex=1)
fit_exp

fit_shp <- fit.variogram(variog, vgm(1, "Sph", 200, 0.4))   #vgm(psill, model, range, nugget)
plot(variog, fit_shp, pch=16, cex=1)
fit_shp


## Cross-validation
# Exponential
crossval_exp <- krige.cv(C_total~X+Y, locations = field_2016, model = fit_exp, nfold=10)
plot(crossval_exp$var1.pred ~ field_2016$C_total, cex = 1.2, lwd = 2, xlim = c(2,10), ylim = c(2,10))
abline(0, 1, col = "red", lwd = 2)
lm_exp <- lm(crossval_exp$var1.pred ~ field_2016$C_total)
abline(lm_exp, col = "green", lwd = 2)
r2_exp <- summary(lm_exp)$r.squared
rmse_exp <- hydroGOF::rmse(crossval_exp$var1.pred, field_2016$C_total)
print(paste('The R-squared of the exponential model is: ', round(r2_exp, 2)))
print(paste('The RMSE of the exponential model is: ', round(rmse_exp, 2)))

# Spherical
crossval_shp <- krige.cv(C_total~X+Y, locations = field_2016, model = fit_shp, nfold = 10)
plot(crossval_shp$var1.pred ~ field_2016$C_total, cex = 1.2, lwd = 2, xlim = c(2,10), ylim = c(2,10))
abline(0, 1, col = "red", lwd = 2)
lm_shp <- lm(crossval_shp$var1.pred ~ field_2016$C_total)
abline(lm_shp, col = "green", lwd = 2)
r2_shp <- summary(lm_shp)$r.squared
rmse_shp <- hydroGOF::rmse(crossval_shp$var1.pred, field_2016$C_total)
print(paste('The R-squared of the Spherical model is: ', round(r2_shp, 2)))
print(paste('The RMSE of the Spherical model is: ', round(rmse_shp, 2)))

# Plot residuals
bubble(crossval_exp , "residual", main = "C% 5-fold CV residuals - Exponential")
bubble(crossval_shp , "residual", main = "C% 5-fold CV residuals - Spherical")

# Kriging
# Convert to sf file
points_sf <- st_as_sf(field_2016, coords = c("X", "Y"), crs = 32630) %>% 
  cbind(st_coordinates(.))

# Create a grid of 5x5m
grd_5m_sf <- points_sf %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_make_grid(cellsize = c(5, 5), what = "centers") %>%
  st_as_sf() %>%
  cbind(., st_coordinates(.))

# Convert to sp object
grd_5m_sp <- as(grd_5m_sf, "Spatial")
gridded(grd_5m_sp) <- TRUE           
grid <- as(grd_5m_sp , "SpatialPixels")
colnames(grid@coords) <- c('X', 'Y')

# Exponential kriging
predicted_exp <- krige(formula = C_total~X+Y, locations = field_2016, newdata = grid, model = fit_exp)
plot(predicted_exp)

# Spherical kriging
predicted_shp <- krige(formula = C_total~X+Y, locations = field_2016, newdata = grid, model = fit_shp)
plot(predicted_shp)

# Plot maps
par(mfrow=c(1,2))
predicted_raster_exp <- raster(predicted_exp)
plot(predicted_raster_exp, main = 'Kriging - Exponential')

predicted_raster_shp <- raster(predicted_shp)
plot(predicted_raster_shp, main = 'Kriging - Spherical')

## Export raster
writeRaster(predicted_raster_shp, 'predicted_2016.tif')


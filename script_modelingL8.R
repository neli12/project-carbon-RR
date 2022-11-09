## Example: http://uc-r.github.io/gbm_regression
## 1. Load libraries
library(gbm)          # basic implementation
library(caret)        # an aggregator package for performing many machine learning models
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization
library(dplyr)
library(raster)


## 2. Set working directory and load data
setwd("C:/Users/neliq/Documents/NIF/Rothamsted/")
dat1 <- read.csv('11.models/Landsat/datLandsat2013.csv', sep=',')
head(dat1)

## 3. Create data partitions
set.seed(125)
trainIndex <- createDataPartition(dat1$TC_perc, p = .75, 
                                  list = FALSE, 
                                  times = 1)
dat_train = dat1[ trainIndex,]
dat_test = dat1[-trainIndex,]

X_train <- dat_train[,-1]
Y_train <- dat_train[,1]
X_test <- dat_test[,-1]
Y_test <- dat_test[,1]

## 4. Train model and save
gbm.fitGNSVI <- gbm(formula = TC_perc ~ .-GNDVI, 
               data = dat_train, 
               n.trees = 1000, 
               interaction.depth = 1,
               shrinkage = 0.01,
               cv.folds = 10,
               n.cores = 4, # will use all cores by default
               verbose = FALSE)

saveRDS(gbm.fit, "finalGBM_modelL82013.rds")

par(mar = c(5, 8, 1, 1))
summary(gbm.fit, 
        cBars = 14,
        method = relative.influence,
        las = 2)


# 4.1. Check RMSE and plot
print(paste("RMSE: ", round(sqrt(min(gbm.fit$cv.error)),2), "%"))
gbm.perf(gbm.fit, method = "cv")

# 4.2. predict values for raining and test data
predTrainDefault <- predict(gbm.fit, n.trees = gbm.fit$n.trees, X_train)
predDefault <- predict(gbm.fit, n.trees = gbm.fit$n.trees, X_test)


# 4.3. Print metrics and plot
RSQUARE <- function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
print(paste("RMSE training data: ", 
            round(caret::RMSE(predTrainDefault, Y_train),2), "%"))
print(paste("RMSE test data: ", 
            round(caret::RMSE(predDefault, Y_test),2), "%"))
print(paste("Rsquare training data: ", 
            round(RSQUARE(Y_train, predTrainDefault),2)))
print(paste("Rsquare training data: ", 
            round(RSQUARE(Y_test, predDefault),2)))

par(mfrow=c(1,2))
plot(Y_train, PRED, ylim = c(0,10), xlim = c(0,10))
abline(0,1)
plot(Y_test, PREDS, ylim = c(0,10), xlim = c(0,10))
abline(0,1)

## 5. Load raster and predict
# 2013
list.files()
covs <- stack("5.satellites_images/Landsat/5m/median/L8MedianDEMTerrain2013.tif")
names(covs) <- c("B2", "B3", "B4", "B5", "B6", "B7", 'B10',
                 "NDVI", "GNDVI", "NDMI", "EVI", "BSI","DEM", 
                 "ASP", "CI", "LS", "PC", "RSP", "SLP", "TWI")
plot(covs)
covs.df <- as.data.frame(covs, xy = TRUE)
predMapOpt <- predict(gbm.fit, newdata = covs.df)
preds <- cbind(covs.df[,1:2], predMapOpt)
colnames(preds) <- c('X', 'Y', 'CTotal')

preds_map <- rasterFromXYZ(preds)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(preds_map,col = pal(100))
area_field <- shapefile("C:/Users/neliq/Documents/NIF/Rothamsted/2.shapefiles/area_field.shp")
plot(area_field, add = TRUE)
crs(preds_map) <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
writeRaster(preds_map, '6.results/CTotalL8_2013.tif')

## Predict to raster of other years
# 2016
terrainStack2016 <- stack("5.satellites_images/Landsat/5m/L8DEMTerrain2016.tif")
names(terrainStack2016) <- c("B2", "B3", "B4", "B5", "B6", "B7", 'B10',
                             "NDVI", "GNDVI", "NDMI", "EVI", "BSI","DEM", 
                             "ASP", "CI", "LS", "PC", "RSP", "SLP", "TWI")
covs2016.df <- as.data.frame(terrainStack2016, xy = TRUE)
predMapOpt2016 <- predict(gbm.fit, newdata = covs2016.df)
preds2016 <- cbind(covs.df[,1:2], predMapOpt2016)
colnames(preds2016) <- c('X', 'Y', 'CTotal')

mapCT2016 <- rasterFromXYZ(preds2016)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2016,col = pal(100))
plot(area_field, add = TRUE)
crs(mapCT2016) <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
writeRaster(mapCT2016, '6.results/CTotalL8_2016.tif', overwrite = TRUE)

# 2018
terrainStack2018 <- stack("5.satellites_images/Landsat/5m/L8DEMTerrain2018.tif")
names(terrainStack2018) <- c("B2", "B3", "B4", "B5", "B6", "B7", 'B10',
                             "NDVI", "GNDVI", "NDMI", "EVI", "BSI","DEM", 
                             "ASP", "CI", "LS", "PC", "RSP", "SLP", "TWI")
covs2018.df <- as.data.frame(terrainStack2018, xy = TRUE)
predMapOpt2018 <- predict(gbm.fit, newdata = covs2018.df)
preds2018 <- cbind(covs2018.df[,1:2], predMapOpt2018)
colnames(preds2018) <- c('X', 'Y', 'CTotal')

mapCT2018 <- rasterFromXYZ(preds2018)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2018,col = pal(100))
plot(area_field, add = TRUE)
crs(mapCT2018) <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
writeRaster(mapCT2018, '6.results/CTotalL8_2018.tif', overwrite = TRUE)

# 2019
terrainStack2019 <- stack("5.satellites_images/Landsat/5m/L8DEMTerrain2019.tif")
names(terrainStack2019) <- c("B2", "B3", "B4", "B5", "B6", "B7", 'B10',
                             "NDVI", "GNDVI", "NDMI", "EVI", "BSI","DEM", 
                             "ASP", "CI", "LS", "PC", "RSP", "SLP", "TWI")
covs2019.df <- as.data.frame(terrainStack2019, xy = TRUE)
predMapOpt2019 <- predict(gbm.fit, newdata = covs2019.df)
preds2019 <- cbind(covs2019.df[,1:2], predMapOpt2019)
colnames(preds2019) <- c('X', 'Y', 'CTotal')

mapCT2019 <- rasterFromXYZ(preds2019)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2019, col = pal(100))
plot(area_field, add = TRUE)
crs(mapCT2019) <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
writeRaster(mapCT2019, '6.results/CTotalL8_2019.tif', overwrite = TRUE)

# 2020
terrainStack2020 <- stack("5.satellites_images/Landsat/5m/L8DEMTerrain2020.tif")
names(terrainStack2020) <- c("B2", "B3", "B4", "B5", "B6", "B7", 'B10',
                             "NDVI", "GNDVI", "NDMI", "EVI", "BSI","DEM", 
                             "ASP", "CI", "LS", "PC", "RSP", "SLP", "TWI")
covs2020.df <- as.data.frame(terrainStack2020, xy = TRUE)
predMapOpt2020 <- predict(gbm.fit, newdata = covs2020.df)
preds2020 <- cbind(covs2020.df[,1:2], predMapOpt2020)
colnames(preds2020) <- c('X', 'Y', 'CTotal')

mapCT2020 <- rasterFromXYZ(preds2020)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2020, col = pal(100))
plot(area_field, add = TRUE)
crs(mapCT2020) <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
writeRaster(mapCT2020, '6.results/CTotalL8_2020.tif', overwrite = TRUE)

# 2021
terrainStack2021 <- stack("5.satellites_images/Landsat/5m/L8DEMTerrain2021.tif")
names(terrainStack2021) <- c("B2", "B3", "B4", "B5", "B6", "B7", 'B10',
                             "NDVI", "GNDVI", "NDMI", "EVI", "BSI","DEM", 
                             "ASP", "CI", "LS", "PC", "RSP", "SLP", "TWI")
covs2021.df <- as.data.frame(terrainStack2021, xy = TRUE)
predMapOpt2021 <- predict(gbm.fit, newdata = covs2021.df)
preds2021 <- cbind(covs2021.df[,1:2], predMapOpt2021)
colnames(preds2021) <- c('X', 'Y', 'CTotal')

mapCT2021 <- rasterFromXYZ(preds2021)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2021, col = pal(100))
plot(area_field, add = TRUE)
crs(mapCT2021) <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
writeRaster(mapCT2021, '6.results/CTotalL8_2021.tif', overwrite = TRUE)

## Take mean by fields
all_maps <- stack(preds_map, mapCT2016, mapCT2018, mapCT2019, mapCT2020, mapCT2021)
names(all_maps) <- c("Pred2012", "Pred2016", "Pred2018", "Pred2019", "Pred2020", "Pred2021")
plot(all_maps)
mapCTFields <- cbind(area_field$Field_Name, 
                     raster::extract(all_maps, area_field, fun = mean, df = TRUE))

write.csv(mapCTFields, "6.results/PredsbyFieldLandsat/preds_by_fieldL8.csv")
{## Hyperparameter tuning ##############################################
  # 6.1. create hyperparameter grid
  hyper_grid <- expand.grid(shrinkage = c(.01, .05, .1),
                            interaction.depth = c(1, 3, 5),
                            n_trees = c(100, 500, 700, 100),
                            n.minobsinnode = c(5, 10, 15),
                            bag.fraction = c(.65, .8, 1), 
                            optimal_trees = 0,               # a place to dump results
                            min_RMSE = 0)                     # a place to dump results
  
  
  nrow(hyper_grid)
  
  # 6.2. Grid search 
  for(i in 1:nrow(hyper_grid)) {
    
    # reproducibility
    set.seed(123)
    
    # train model
    gbm.tune <- gbm(formula = TC_perc ~ ., 
                    data = dat_train,
                    distribution = "gaussian",
                    n.trees = hyper_grid$n_trees[i],
                    interaction.depth = hyper_grid$interaction.depth[i],
                    shrinkage = hyper_grid$shrinkage[i],
                    n.minobsinnode = hyper_grid$n.minobsinnode[i],
                    bag.fraction = hyper_grid$bag.fraction[i],
                    train.fraction = .75,
                    cv.folds = 5,
                    n.cores = 3,
                    verbose = TRUE)
    # add min training error and trees to grid
    hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
    hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
  }
  
  hyper_grid %>% 
    dplyr::arrange(min_RMSE) %>%
    head(10)
  
  
  ## 7. Train tunned GBM model
  gbm.fit.final <- gbm(formula = TC_perc ~ ., 
                       data = dat_train,
                       distribution = "gaussian",
                       n.trees = 700,
                       interaction.depth = 2,
                       shrinkage = 0.01,
                       n.minobsinnode = 20,
                       bag.fraction = .65, 
                       train.fraction = 1,
                       n.cores = 2,
                       verbose = FALSE)  
  saveRDS(gbm.fit.final, "finalGBM_model.rds")
  
  
  par(mar = c(5, 8, 1, 1))
  summary(gbm.fit, 
          cBars = 18,
          method = relative.influence,
          las = 2)
  
  gbm.fit %>%
    partial(pred.var = "EVI", n.trees = gbm.fit$n.trees, grid.resolution = 100) %>%
    autoplot(rug = TRUE, train = dat_train)
  
  
  # 7.1. predict values for training and test data
  predTrainOpt <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, X_train)
  predOpt <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, X_test)
  
  
  # 7.3. Print metrics and plot
  print(paste("RMSE training data: ", 
              round(caret::RMSE(predTrainOpt, Y_train),2), "%"))
  print(paste("RMSE test data: ", 
              round(caret::RMSE(predOpt, Y_test),2), "%"))
  print(paste("Rsquare training data: ", 
              round(RSQUARE(Y_train, predTrainOpt),2)))
  print(paste("Rsquare training data: ", 
              round(RSQUARE(Y_test, predOpt),2)))
  
  par(mfrow=c(1,2))
  plot(Y_train, predTrainOpt, ylim = c(0,10), xlim = c(0,10))
  abline(0,1)
  plot(Y_test, predOpt, ylim = c(0,10), xlim = c(0,10))
  abline(0,1)
}
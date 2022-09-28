## Example: http://uc-r.github.io/gbm_regression
## 1. Load libraries
library(gbm)          # basic implementation
library(caret)        # an aggregator package for performing many machine learning models
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization
library(dplyr)
library(raster)

RSQUARE <- function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

## 2. Load data
dat <- read.csv('dataL8Terrain5m.csv', sep=',')
head(dat)
dat1 <- dat[,c(20, 22:41)]
colnames(dat1)

cor(dat1)

## 3. Create data partitions
set.seed(125)
trainIndex <- createDataPartition(dat1$C_total_.w, p = .75, 
                                  list = FALSE, 
                                  times = 1)
dat_train = dat1[ trainIndex,]
dat_test = dat1[-trainIndex,]

X_train <- dat_train[,-1]
Y_train <- dat_train[,1]
X_test <- dat_test[,-1]
Y_test <- dat_test[,1]

## 4. Train default model
gbm.fit <- gbm(formula = C_total_.w ~ ., 
               data = dat_train[,-9:-10], 
               n.trees = 20000, 
               interaction.depth = 1,
               shrinkage = 0.001,
               cv.folds = 10,
               n.cores = NULL, # will use all cores by default
               verbose = FALSE)

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
print(paste("RMSE training data: ", 
            round(caret::RMSE(predTrainDefault, Y_train),2), "%"))
print(paste("RMSE test data: ", 
            round(caret::RMSE(predDefault, Y_test),2), "%"))
print(paste("Rsquare training data: ", 
            round(RSQUARE(Y_train, predTrainDefault),2)))
print(paste("Rsquare training data: ", 
            round(RSQUARE(Y_test, predDefault),2)))

par(mfrow=c(1,2))
plot(Y_train, predTrainDefault, ylim = c(0,10), xlim = c(0,10))
abline(0,1)
plot(Y_test, predDefault, ylim = c(0,10), xlim = c(0,10))
abline(0,1)


## 5. Fit second model with less trees and larger learning rate
gbm.fit2 <- gbm(formula = C_total_.w ~ B5+B6+B10+DEM+GNDVI+BSI+DEM+TWI,
                data = dat_train,
                n.trees = 300,
                interaction.depth = 5,
                shrinkage = 0.1,
                cv.folds = 10,
                n.cores = NULL, # will use all cores by default
                verbose = FALSE)  

par(mar = c(5, 8, 1, 1))
summary(gbm.fit2, 
        cBars = 14,
        method = relative.influence,
        las = 2)

# 5.1. Check RMSE and plot
print(paste("RMSE: ", round(sqrt(min(gbm.fit2$cv.error)),2), "%"))
gbm.perf(gbm.fit2, method = "cv")


# 5.2. predict values for raining and test data
predTrain1 <- predict(gbm.fit2, n.trees = gbm.fit2$n.trees, X_train)
pred1 <- predict(gbm.fit2, n.trees = gbm.fit2$n.trees, X_test)


# 5.3. Print metrics and plot
print(paste("RMSE training data: ", 
            round(caret::RMSE(predTrain1, Y_train),2), "%"))
print(paste("RMSE test data: ", 
            round(caret::RMSE(pred1, Y_test),2), "%"))
print(paste("Rsquare training data: ", 
            round(RSQUARE(Y_train, predTrain1),2)))
print(paste("Rsquare training data: ", 
            round(RSQUARE(Y_test, pred1),2)))

par(mfrow=c(1,2))
plot(Y_train, predTrain1, ylim = c(0,10), xlim = c(0,10))
abline(0,1)
plot(Y_test, pred, ylim = c(0,10), xlim = c(0,10))
abline(0,1)


## 6. Hyperparameter tuning ##############################################
# 6.1. create hyperparameter grid
hyper_grid <- expand.grid(shrinkage = c(.01, .05, .1),
                          interaction.depth = c(3, 5, 7),
                          n.minobsinnode = c(5,8, 10),
                          bag.fraction = c(.65, .8, 1), 
                          optimal_trees = 0,               # a place to dump results
                          min_RMSE = 0)                     # a place to dump results

                          
nrow(hyper_grid)

# 6.2. Grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(formula = C_total_.w ~ B5+B6+DEM+GNDVI+BSI+DEM+TWI+NDMI,
                  distribution = "gaussian",
                  data = dat_train,
                  n.trees = 50,
                  interaction.depth = hyper_grid$interaction.depth[i],
                  shrinkage = hyper_grid$shrinkage[i],
                  n.minobsinnode = hyper_grid$n.minobsinnode[i],
                  bag.fraction = hyper_grid$bag.fraction[i],
                  train.fraction = .75,
                  cv.folds = 5,
                  n.cores = NULL, # will use all cores by default
                  verbose = TRUE)
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


## 7. Train tunned GBM model
gbm.fit.final <- gbm(formula = C_total_.w ~ B5+B6+DEM+GNDVI+BSI+DEM+TWI+NDMI+SLP,
                     distribution = "gaussian",
                     data = dat_train,
                     n.trees = 25,
                     interaction.depth = 5,
                     shrinkage = 0.1,
                     n.minobsinnode = 5,
                     bag.fraction = .65, 
                     train.fraction = 1,
                     n.cores = NULL,
                     verbose = FALSE)  
saveRDS(gbm.fit.final, "finalGBM_model.rds")


par(mar = c(5, 8, 1, 1))
summary(gbm.fit.final, 
        cBars = 18,
        method = relative.influence,
        las = 2)

gbm.fit.final %>%
  partial(pred.var = "B6", n.trees = gbm.fit.final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = dat_train)



ice1 <- gbm.fit.final %>%
  partial(
    pred.var = "DEM", 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = dat_train, alpha = .2) +
  ggtitle("Non-centered")

ice2 <- gbm.fit.final %>%
  partial(
    pred.var = "DEM", 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = dat_train, alpha = .3, center = TRUE) +
  ggtitle("Centered")

gridExtra::grid.arrange(ice1, ice2, nrow = 1)


# 7.1. predict values for yraining and test data
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

list.files()
covs <- stack("L8Terrain5m_DEF.tif")
names(covs) <- c("B2", "B3", "B4", "B5", "B6", "B7", "B10","NDVI","GNDVI","NDMI", "EVI", "BSI", "ASP", "CI",
                 "DEM", "LS", "PC", "RSP", "SLP", "TWI")
covs.df <- as.data.frame(covs, xy = TRUE)
covs.df1 <- covs.df[,c(6:7, 11:12, 14, 17, 21:22)]
predMapOpt <- predict(gbm.fit.final, newdata = covs.df1)
preds <- cbind(covs.df[,1:2], predMapOpt)
colnames(preds) <- c('X', 'Y', 'CTotal')

preds_map <- rasterFromXYZ(preds)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(preds_map,col = pal(100))
area_field <- shapefile("C:/Users/neliq/Documents/NIF/Rothamsted/shapefiles/area_field.shp")
plot(area_field, add = TRUE)
writeRaster(preds_map, 'CTotal_gbm25.tif')



## Lime
model_type.gbm <- function(x, ...) {
  return("regression")
}

predict_model.gbm <- function(x, newdata, ...) {
  pred <- predict(x, newdata, n.trees = x$n.trees)
  return(as.data.frame(pred))
}

# get a few observations to perform local interpretation on
y <- X_test[24:25,c(4:5, 9:10, 12, 15, 19:20)]
local_obs <- y

# apply LIME
x <- X_train[24:25, c(4:5, 9:10, 12, 15, 19:20)]
explainer <- lime(x, gbm.fit.final)
explanation <- explain(local_obs, explainer, n_features = 8)
plot_features(explanation)

## Predict to raster of other years
# 2016
area_field <- shapefile("C:/Users/neliq/Documents/NIF/Rothamsted/shapefiles/area_field.shp")
covs2016.df <- as.data.frame(terrainStack2016, xy = TRUE)
covs.df1 <- covs2016.df[,c(6:7, 11:12, 14, 17, 21:22)]
predMapOpt2016 <- predict(gbm.fit.final, newdata = covs.df1)
preds2016 <- cbind(covs.df[,1:2], predMapOpt2016)
colnames(preds2016) <- c('X', 'Y', 'CTotal')

mapCT2016 <- rasterFromXYZ(preds2016)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2016,col = pal(100))
plot(area_field, add = TRUE)
writeRaster(mapCT2016, 'CTotalPred_2016.tif')

# 2018
covs2018.df <- as.data.frame(terrainStack2018, xy = TRUE)
covs.df2018 <- covs2018.df[,c(6:7, 11:12, 14, 17, 21:22)]
predMapOpt2018 <- predict(gbm.fit.final, newdata = covs.df2018)
preds2018 <- cbind(covs.df[,1:2], predMapOpt2018)
colnames(preds2018) <- c('X', 'Y', 'CTotal')

mapCT2018 <- rasterFromXYZ(preds2018)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2018,col = pal(100))
plot(area_field, add = TRUE)
writeRaster(mapCT2018, 'CTotalPred_2018_2.tif')

# 2019
covs2019.df <- as.data.frame(terrainStack2019, xy = TRUE)
covs.df2019 <- covs2019.df[,c(6:7, 11:12, 14, 17, 21:22)]
predMapOpt2019 <- predict(gbm.fit.final, newdata = covs.df2019)
preds2019 <- cbind(covs.df[,1:2], predMapOpt2019)
colnames(preds2019) <- c('X', 'Y', 'CTotal')

mapCT2019 <- rasterFromXYZ(preds2019)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2019, col = pal(100))
plot(area_field, add = TRUE)
writeRaster(mapCT2019, 'CTotalPred_2019.tif')

# 2020
covs2020.df <- as.data.frame(terrainStack2020, xy = TRUE)
covs.df2020 <- covs2020.df[,c(6:7, 11:12, 14, 17, 21:22)]
predMapOpt2020 <- predict(gbm.fit.final, newdata = covs.df2020)
preds2020 <- cbind(covs.df[,1:2], predMapOpt2020)
colnames(preds2020) <- c('X', 'Y', 'CTotal')

mapCT2020 <- rasterFromXYZ(preds2020)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2020, col = pal(100))
plot(area_field, add = TRUE)
writeRaster(mapCT2020, 'CTotalPred_2020.tif')

# 2021
covs2021.df <- as.data.frame(terrainStack2021, xy = TRUE)
covs.df2021 <- covs2021.df[,c(6:7, 11:12, 14, 17, 21:22)]
predMapOpt2021 <- predict(gbm.fit.final, newdata = covs.df2021)
preds2021 <- cbind(covs.df[,1:2], predMapOpt2021)
colnames(preds2021) <- c('X', 'Y', 'CTotal')

mapCT2021 <- rasterFromXYZ(preds2021)
pal <- colorRampPalette(c("#d7191c","#fdae61","#ffffc0","#a6d96a","#1a9641"))
plot(mapCT2021, col = pal(100))
plot(area_field, add = TRUE)
writeRaster(mapCT2021, 'CTotalPred_2021.tif')

par(mfrow=c(1,3))

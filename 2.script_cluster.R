### Script for mapping clusters using raster maps

## Load libraries
library(raster)
library(cluster)

## Set directory
setwd("C:/Users/neliq/Documents/NIF/Rothamsted/test-prediction-2012")

# Load raster stack 
L8_terrain <- stack("L8Terrain5m_DEF.tif")
plot(L8_terrain)

# Get the values of the raster stack
stack_vals <- getValues(L8_terrain)
summary(stack_vals)

# Elbow method to check the optimal number of cluster
k.max <- 20
data <- (na.omit(scale(stack_vals))) #scale and exclude NA's
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15)$tot.withinss})
wss
plot(wss)
#tot.withinss = Total within-cluster sum of squares, i.e. sum(withinss).


### Clustering
# Use the clara function from cluster package. In this code, a sample of 1000 and the euclidean distance are used.
# I used k=10, as example
clus5 <- cluster::clara(data, k=5, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T)  
clus10 <- cluster::clara(data, k=10, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T) 


# Create am index
idx <- 1:ncell(L8_terrain)
idx1 <- idx[-unique(which(is.na(stack_vals), arr.ind=TRUE)[,1])] #The which() function in R returns the position or the index of the value which satisfies the given condition. The Which() function in R gives you the position of the value in a logical vector. The position can be of anything like rows, columns and even vector as well
str(idx1)

# Create an empty raster using the first raster of your stack
clust_raster <- L8_terrain[[1]]
plot(clust_raster)
clust_raster[] <- NA

# Transfer the clustering results to your empty raster and save it
library(RColorBrewer)
clust_raster[idx1] <- clus5$clustering
clust_raster[idx1] <- clus10$clustering
cols <- brewer.pal(5, 'RdYlGn')
cols <- brewer.pal(10, 'Spectral')
plot(clust_raster, col = cols) 

writeRaster(clust_raster, "Clusters_5", format = "GTiff", datatype = "FLT4S", overwrite = T)



## Load libraries
library(raster)
library(stats)

## Take a sample
sr <- sampleRandom(L8_terrain, 1000)

## Perform PCA
pca <- prcomp(sr, scale = TRUE)
pca.df <- pca$x
print(pca)

## Plot the screeplot
screeplot(pca)

## Predict to the entire raster   -- With three PCs
pci <- predict(L8_terrain, pca, index = 1:3)
plot(pci)


## Save
writeRaster(pci, "PCA_L8terrain.tif")


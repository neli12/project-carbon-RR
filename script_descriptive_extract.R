## Import libraries
library(raster)
library(tidyverse)
library(kableExtra)
library(rstatix)
library(RColorBrewer)
library(ggpubr)
library(ggplot2)
library(nortest)
library(car)
library(ggstance)
library(broom.mixed)
library(olsrr)

## 2. Set working directory and load data
setwd("C:/Users/neliq/Documents/NIF/Rothamsted/")
list.files()


## Load data and extract raster values
dat2012 <- read.csv('3.clean_data/dat2012_withcovsL8.csv', sep=',')[, c(5:6, 8:15)]
covs_stack2013 <- stack("5.satellites_images/Landsat/5m/median/L8MedianDEMTerrain2013.tif")
head(dat2012)
coordinates(dat2012) <-~Easting+Northing
plot(dat2012)

dat_extract <- data.frame(raster::extract(covs_stack2013, dat2012))
dat_extract
summary(dat)

## Cbind data and raster values
dat <- cbind(dat2012$C_total_.w.w, dat_extract)
colnames(dat) <- c('TC_perc', "B2", "B3", "B4", "B5", "B6", "B7", 'B10',
                   "NDVI", "GNDVI", "NDMI",
                   "EVI", "BSI","DEM", "ASP", "CI", "LS", "PC",
                   "RSP", "SLP", "TWI")

dat %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

## Descriptive statistics
summary(dat)

sd(dat$TC_perc)
# Correlogram
dat_corr <- cor_mat(dat)

jpeg("10.figures/correlation_plot.jpeg", width = 3500, height = 3500, res = 300)
my.palette <- get_palette("PuOr", 200)
dat_corr %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE, insignificant = "blank", palette = my.palette)
dev.off()

# Histogram
jpeg("10.figures/histogram_plot.jpeg", width = 3500, height = 3500, res = 300)
ggplot(dat, aes(x = TC_perc)) + geom_histogram(aes(y = ..density..),
                                               colour = 'black',
                                               fill = 'skyblue') + 
  stat_function(fun = dnorm, args = list(mean = mean(dat$TC_perc), sd = sd(dat$TC_perc))) +
  theme_minimal() + xlab("TC (%)") + theme(text = element_text(size = 28), 
                                           axis.text.x = element_text(size = 28),
                                           axis.text.y = element_text(size = 28))
dev.off()

write.csv(dat, '11.models/Landsat/datLandsat2013.csv')



          
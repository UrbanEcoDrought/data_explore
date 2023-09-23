# Script to determine best fit lm using AICc
# Call Ross's script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)

unique(ndvi.all$type)

# create data set with only full years, so 2001-2022, cutting 2023
ndvi.allFullYears <- ndvi.all[!ndvi.all$year %in% 2023,]

# some orienting plots
ggplot(data=ndvi.allFullYears[!ndvi.allFullYears$year %in% c(2005,2012),]) + facet_wrap(type~.) +
  stat_smooth(aes(x=doy, y=NDVI)) +
  geom_line(data=ndvi.allFullYears[ndvi.allFullYears$year %in% c(2005, 2012),], aes(x=doy, y=NDVI, col=as.factor(year)))


# we do have duplicates in dates as the collections were taken by different satellites
# running a brief script to take the average of the dates per cover type. Note: some of the measurements appear to be identical for replicate dates and others are close. Will take the mean for now


ndvi.allAgg <- aggregate(NDVI~date + type + doy, FUN=mean, data=ndvi.all)
head(ndvi.allAgg)

#Create a new table object with NA values removed from data.
NDVIomitNA <- na.omit(ndvi.allAgg)
summary(NDVIomitNA)

library(dplyr)

#Add month column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Month = month(NDVIomitNA$date))
summary(NDVIomitNA)

#Add day column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Day = day(NDVIomitNA$date))

#Add year column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Year = year(NDVIomitNA$date))

#Create a new table object with years 2001-2022.
NDVIomitNA2022 <-NDVIomitNA[which(NDVIomitNA$Year %in% c(2001:2022)),]

# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

# create column with date in ISO format
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")

# merge ChicagolandSPI and NDVIomitNA2022 by date columns
ChicagolandSPINDVI <- merge (ChicagolandSPI, NDVIomitNA2022, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVINA <- na.omit(ChicagolandSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
NDVIdoySPI14dmodel <- lm(NDVI ~ doy + X14d.SPI, data = ChicagolandSPINDVINA)
summary(NDVIdoySPI14dmodel)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
NDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = ChicagolandSPINDVINA)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
NDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = ChicagolandSPINDVINA)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
NDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = ChicagolandSPINDVINA)

# Install and load AICcmodavg package
install.packages("AICcmodavg")
library(AICcmodavg)

# Put all models into a list
SPINDVImodels <- list(NDVIdoySPI14dmodel, NDVIdoySPI30dmodel, NDVIdoySPI60dmodel, NDVIdoySPI90dmodel)

# Specify model names
mod.names <- c('NDVIdoySPI14dmodel', 'NDVIdoySPI30dmodel', 'NDVIdoySPI60dmodel', 'NDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = SPINDVImodels, modnames = mod.names)

#Model selection based on AICc:
  
#                    K     AICc Delta_AICc AICcWt Cum.Wt      LL
# NDVIdoySPI90dmodel 4 -3288.10       0.00   0.53   0.53 1648.05
# NDVIdoySPI30dmodel 4 -3287.81       0.29   0.46   0.99 1647.91
# NDVIdoySPI60dmodel 4 -3279.22       8.88   0.01   1.00 1643.61
# NDVIdoySPI14dmodel 4 -3238.93      49.17   0.00   1.00 1623.47

# Based off this model, using the 90 day SPI is the best fitting model. 

# Create data set with only growing season (April-September)
GrowSeasonSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(4:9)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
GSNDVIdoySPI14dmodel <- lm(NDVI ~ doy + X14d.SPI, data = GrowSeasonSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
GSNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = GrowSeasonSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
GSNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = GrowSeasonSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
GSNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = GrowSeasonSPINDVI)

# Put all models into a list
GSSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel)

# Specify model names
mod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSSPINDVImodels, modnames = mod.names)

# Compare Full Year models with Grow Season models
# Put all models into a list
FYGSSPINDVImodels <- list(NDVIdoySPI14dmodel, NDVIdoySPI30dmodel, NDVIdoySPI60dmodel, NDVIdoySPI90dmodel, GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel)

# Specify model names
mod.names <- c('NDVIdoySPI14dmodel', 'NDVIdoySPI30dmodel', 'NDVIdoySPI60dmodel', 'NDVIdoySPI90dmodel', 'GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = FYGSSPINDVImodels, modnames = mod.names)

# Model selection based on AICc:

#                      K     AICc Delta_AICc AICcWt Cum.Wt      LL
# GSNDVIdoySPI60dmodel 4 -4082.10       0.00   0.78   0.78 2045.05
# GSNDVIdoySPI30dmodel 4 -4079.52       2.57   0.22   1.00 2043.76
# GSNDVIdoySPI90dmodel 4 -4068.75      13.34   0.00   1.00 2038.38
# GSNDVIdoySPI14dmodel 4 -4058.05      24.05   0.00   1.00 2033.03
# NDVIdoySPI90dmodel   4 -3288.10     794.00   0.00   1.00 1648.05
# NDVIdoySPI30dmodel   4 -3287.81     794.29   0.00   1.00 1647.91
# NDVIdoySPI60dmodel   4 -3279.22     802.87   0.00   1.00 1643.61
# NDVIdoySPI14dmodel   4 -3238.93     843.17   0.00   1.00 1623.47

# When comparing lm for full year and grow seasons, the best fit model is 60d SPI for the growing season
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
FYSPINDVImodels <- list(NDVIdoySPI14dmodel, NDVIdoySPI30dmodel, NDVIdoySPI60dmodel, NDVIdoySPI90dmodel)

# Specify model names
FYmod.names <- c('NDVIdoySPI14dmodel', 'NDVIdoySPI30dmodel', 'NDVIdoySPI60dmodel', 'NDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = FYSPINDVImodels, modnames = FYmod.names)

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
GSmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSSPINDVImodels, modnames = GSmod.names)

# Compare Full Year models with Grow Season models
# Put all models into a list
FYGSSPINDVImodels <- list(NDVIdoySPI14dmodel, NDVIdoySPI30dmodel, NDVIdoySPI60dmodel, NDVIdoySPI90dmodel, GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel)

# Specify model names
FYGSmod.names <- c('NDVIdoySPI14dmodel', 'NDVIdoySPI30dmodel', 'NDVIdoySPI60dmodel', 'NDVIdoySPI90dmodel', 'GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = FYGSSPINDVImodels, modnames = FYGSmod.names)

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

# Create data set with only April and May
AprMaySPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(4:5)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
AprMayNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = AprMaySPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
AprMayNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = AprMaySPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
AprMayNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = AprMaySPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
AprMayNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = AprMaySPINDVI)

# Put all models into a list
AprMaySPINDVImodels <- list(AprMayNDVIdoySPI14dmodel, AprMayNDVIdoySPI30dmodel, AprMayNDVIdoySPI60dmodel, AprMayNDVIdoySPI90dmodel)

# Specify model names
AprMaymod.names <- c('AprMayNDVIdoySPI14dmodel', 'AprMayNDVIdoySPI30dmodel', 'AprMayNDVIdoySPI60dmodel', 'AprMayNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = AprMaySPINDVImodels, modnames = AprMaymod.names)

# Compare Grow Season models with April/May models
# Put all models into a list
GSAprMaySPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, AprMayNDVIdoySPI14dmodel, AprMayNDVIdoySPI30dmodel, AprMayNDVIdoySPI60dmodel, AprMayNDVIdoySPI90dmodel)

# Specify model names
GSAprMaymod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'AprMayNDVIdoySPI14dmodel', 'AprMayNDVIdoySPI30dmodel', 'AprMayNDVIdoySPI60dmodel', 'AprMayNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSAprMaySPINDVImodels, modnames = GSAprMaymod.names)

# When comparing lm for April/May and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only May and June
MayJunSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(5:6)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
MayJunNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = MayJunSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
MayJunNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = MayJunSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
MayJunNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = MayJunSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
MayJunNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = MayJunSPINDVI)

# Put all models into a list
MayJunSPINDVImodels <- list(MayJunNDVIdoySPI14dmodel, MayJunNDVIdoySPI30dmodel, MayJunNDVIdoySPI60dmodel, MayJunNDVIdoySPI90dmodel)

# Specify model names
MayJunmod.names <- c('MayJunNDVIdoySPI14dmodel', 'MayJunNDVIdoySPI30dmodel', 'MayJunNDVIdoySPI60dmodel', 'MayJunNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = MayJunSPINDVImodels, modnames = MayJunmod.names)

# Compare Grow Season models with May/June models
# Put all models into a list
GSMayJunSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, MayJunNDVIdoySPI14dmodel, MayJunNDVIdoySPI30dmodel, MayJunNDVIdoySPI60dmodel, MayJunNDVIdoySPI90dmodel)

# Specify model names
GSMayJunmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'MayJunNDVIdoySPI14dmodel', 'MayJunNDVIdoySPI30dmodel', 'MayJunNDVIdoySPI60dmodel', 'MayJunNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSMayJunSPINDVImodels, modnames = GSMayJunmod.names)

# When comparing lm for May/June and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only June and July
JunJulSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(6:7)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
JunJulNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = JunJulSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
JunJulNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = JunJulSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
JunJulNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = JunJulSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
JunJulNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = JunJulSPINDVI)

# Put all models into a list
JunJulSPINDVImodels <- list(JunJulNDVIdoySPI14dmodel, JunJulNDVIdoySPI30dmodel, JunJulNDVIdoySPI60dmodel, JunJulNDVIdoySPI90dmodel)

# Specify model names
JunJulmod.names <- c('JunJulNDVIdoySPI14dmodel', 'JunJulNDVIdoySPI30dmodel', 'JunJulNDVIdoySPI60dmodel', 'JunJulNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = JunJulSPINDVImodels, modnames = JunJulmod.names)

# Compare Grow Season models with June/July models
# Put all models into a list
GSJunJulSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, JunJulNDVIdoySPI14dmodel, JunJulNDVIdoySPI30dmodel, JunJulNDVIdoySPI60dmodel, JunJulNDVIdoySPI90dmodel)

# Specify model names
GSJunJulmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'JunJulNDVIdoySPI14dmodel', 'JunJulNDVIdoySPI30dmodel', 'JunJulNDVIdoySPI60dmodel', 'JunJulNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSJunJulSPINDVImodels, modnames = GSJunJulmod.names)

# When comparing lm for June/July and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only July and August
JulAugSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(7:8)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
JulAugNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = JulAugSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
JulAugNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = JulAugSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
JulAugNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = JulAugSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
JulAugNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = JulAugSPINDVI)

# Put all models into a list
JulAugSPINDVImodels <- list(JulAugNDVIdoySPI14dmodel, JulAugNDVIdoySPI30dmodel, JulAugNDVIdoySPI60dmodel, JulAugNDVIdoySPI90dmodel)

# Specify model names
JulAugmod.names <- c('JulAugNDVIdoySPI14dmodel', 'JulAugNDVIdoySPI30dmodel', 'JulAugNDVIdoySPI60dmodel', 'JulAugNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = JulAugSPINDVImodels, modnames = JulAugmod.names)

# Compare Grow Season models with July/August models
# Put all models into a list
GSJulAugSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, JulAugNDVIdoySPI14dmodel, JulAugNDVIdoySPI30dmodel, JulAugNDVIdoySPI60dmodel, JulAugNDVIdoySPI90dmodel)

# Specify model names
GSJulAugmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'JulAugNDVIdoySPI14dmodel', 'JulAugNDVIdoySPI30dmodel', 'JulAugNDVIdoySPI60dmodel', 'JulAugNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSJulAugSPINDVImodels, modnames = GSJulAugmod.names)

# When comparing lm for July/August and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only August and September
AugSepSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(8:9)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
AugSepNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = AugSepSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
AugSepNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = AugSepSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
AugSepNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = AugSepSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
AugSepNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = AugSepSPINDVI)

# Put all models into a list
AugSepSPINDVImodels <- list(AugSepNDVIdoySPI14dmodel, AugSepNDVIdoySPI30dmodel, AugSepNDVIdoySPI60dmodel, AugSepNDVIdoySPI90dmodel)

# Specify model names
AugSepmod.names <- c('AugSepNDVIdoySPI14dmodel', 'AugSepNDVIdoySPI30dmodel', 'AugSepNDVIdoySPI60dmodel', 'AugSepNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = AugSepSPINDVImodels, modnames = AugSepmod.names)

# Compare Grow Season models with August/September models
# Put all models into a list
GSAugSepSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, AugSepNDVIdoySPI14dmodel, AugSepNDVIdoySPI30dmodel, AugSepNDVIdoySPI60dmodel, AugSepNDVIdoySPI90dmodel)

# Specify model names
GSAugSepmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'AugSepNDVIdoySPI14dmodel', 'AugSepNDVIdoySPI30dmodel', 'AugSepNDVIdoySPI60dmodel', 'AugSepNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSAugSepSPINDVImodels, modnames = GSAugSepmod.names)

# When comparing lm for August/September and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only April through June
AprMayJunSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(4:6)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
AprMayJunNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = AprMayJunSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
AprMayJunNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = AprMayJunSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
AprMayJunNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = AprMayJunSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
AprMayJunNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = AprMayJunSPINDVI)

# Put all models into a list
AprMayJunSPINDVImodels <- list(AprMayJunNDVIdoySPI14dmodel, AprMayJunNDVIdoySPI30dmodel, AprMayJunNDVIdoySPI60dmodel, AprMayJunNDVIdoySPI90dmodel)

# Specify model names
AprMayJunmod.names <- c('AprMayJunNDVIdoySPI14dmodel', 'AprMayJunNDVIdoySPI30dmodel', 'AprMayJunNDVIdoySPI60dmodel', 'AprMayJunNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = AprMayJunSPINDVImodels, modnames = AprMayJunmod.names)

# Compare Grow Season models with April/May/June models
# Put all models into a list
GSAprMayJunSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, AprMayJunNDVIdoySPI14dmodel, AprMayJunNDVIdoySPI30dmodel, AprMayJunNDVIdoySPI60dmodel, AprMayJunNDVIdoySPI90dmodel)

# Specify model names
GSAprMayJunmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'AprMayJunNDVIdoySPI14dmodel', 'AprMayJunNDVIdoySPI30dmodel', 'AprMayJunNDVIdoySPI60dmodel', 'AprMayJunNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSAprMayJunSPINDVImodels, modnames = GSAprMayJunmod.names)

# When comparing lm for April/May/June and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only May through July
MayJunJulSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(5:7)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
MayJunJulNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = MayJunJulSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
MayJunJulNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = MayJunJulSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
MayJunJulNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = MayJunJulSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
MayJunJulNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = MayJunJulSPINDVI)

# Put all models into a list
MayJunJulSPINDVImodels <- list(MayJunJulNDVIdoySPI14dmodel, MayJunJulNDVIdoySPI30dmodel, MayJunJulNDVIdoySPI60dmodel, MayJunJulNDVIdoySPI90dmodel)

# Specify model names
MayJunJulmod.names <- c('MayJunJulNDVIdoySPI14dmodel', 'MayJunJulNDVIdoySPI30dmodel', 'MayJunJulNDVIdoySPI60dmodel', 'MayJunJulNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = MayJunJulSPINDVImodels, modnames = MayJunJulmod.names)

# Compare Grow Season models with May/June/July models
# Put all models into a list
GSMayJunJulSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, MayJunJulNDVIdoySPI14dmodel, MayJunJulNDVIdoySPI30dmodel, MayJunJulNDVIdoySPI60dmodel, MayJunJulNDVIdoySPI90dmodel)

# Specify model names
GSMayJunJulmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'MayJunJulNDVIdoySPI14dmodel', 'MayJunJulNDVIdoySPI30dmodel', 'MayJunJulNDVIdoySPI60dmodel', 'MayJunJulNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSMayJunJulSPINDVImodels, modnames = GSMayJunJulmod.names)

# When comparing lm for May/June/July and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only June through August
JunJulAugSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(6:8)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
JunJulAugNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = JunJulAugSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
JunJulAugNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = JunJulAugSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
JunJulAugNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = JunJulAugSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
JunJulAugNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = JunJulAugSPINDVI)

# Put all models into a list
JunJulAugSPINDVImodels <- list(JunJulAugNDVIdoySPI14dmodel, JunJulAugNDVIdoySPI30dmodel, JunJulAugNDVIdoySPI60dmodel, JunJulAugNDVIdoySPI90dmodel)

# Specify model names
JunJulAugmod.names <- c('JunJulAugNDVIdoySPI14dmodel', 'JunJulAugNDVIdoySPI30dmodel', 'JunJulAugNDVIdoySPI60dmodel', 'JunJulAugNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = JunJulAugSPINDVImodels, modnames = JunJulAugmod.names)

# Compare Grow Season models with June/July/August models
# Put all models into a list
GSJunJulAugSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, JunJulAugNDVIdoySPI14dmodel, JunJulAugNDVIdoySPI30dmodel, JunJulAugNDVIdoySPI60dmodel, JunJulAugNDVIdoySPI90dmodel)

# Specify model names
GSJunJulAugmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'JunJulAugNDVIdoySPI14dmodel', 'JunJulAugNDVIdoySPI30dmodel', 'JunJulAugNDVIdoySPI60dmodel', 'JunJulAugNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSJunJulAugSPINDVImodels, modnames = GSJunJulAugmod.names)

# When comparing lm for June/July/Aug and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only July through September
JulAugSepSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(7:9)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
JulAugSepNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = JulAugSepSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
JulAugSepNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = JulAugSepSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
JulAugSepNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = JulAugSepSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
JulAugSepNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = JulAugSepSPINDVI)

# Put all models into a list
JulAugSepSPINDVImodels <- list(JulAugSepNDVIdoySPI14dmodel, JulAugSepNDVIdoySPI30dmodel, JulAugSepNDVIdoySPI60dmodel, JulAugSepNDVIdoySPI90dmodel)

# Specify model names
JulAugSepmod.names <- c('JulAugSepNDVIdoySPI14dmodel', 'JulAugSepNDVIdoySPI30dmodel', 'JulAugSepNDVIdoySPI60dmodel', 'JulAugSepNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = JulAugSepSPINDVImodels, modnames = JulAugSepmod.names)

# Compare Grow Season models with July/August/September models
# Put all models into a list
GSJulAugSepSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, JulAugSepNDVIdoySPI14dmodel, JulAugSepNDVIdoySPI30dmodel, JulAugSepNDVIdoySPI60dmodel, JulAugSepNDVIdoySPI90dmodel)

# Specify model names
GSJulAugSepmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'JulAugSepNDVIdoySPI14dmodel', 'JulAugSepNDVIdoySPI30dmodel', 'JulAugSepNDVIdoySPI60dmodel', 'JulAugSepNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSJulAugSepSPINDVImodels, modnames = GSJulAugSepmod.names)

# When comparing lm for July/Aug/Sep and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only April through July
AMJJSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(4:7)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
AMJJNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = AMJJSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
AMJJNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = AMJJSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
AMJJNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = AMJJSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
AMJJNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = AMJJSPINDVI)

# Put all models into a list
AMJJSPINDVImodels <- list(AMJJNDVIdoySPI14dmodel, AMJJNDVIdoySPI30dmodel, AMJJNDVIdoySPI60dmodel, AMJJNDVIdoySPI90dmodel)

# Specify model names
AMJJmod.names <- c('AMJJNDVIdoySPI14dmodel', 'AMJJNDVIdoySPI30dmodel', 'AMJJNDVIdoySPI60dmodel', 'AMJJNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = AMJJSPINDVImodels, modnames = AMJJmod.names)

# Compare Grow Season models with April/May/June/July models
# Put all models into a list
GSAMJJSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, AMJJNDVIdoySPI14dmodel, AMJJNDVIdoySPI30dmodel, AMJJNDVIdoySPI60dmodel, AMJJNDVIdoySPI90dmodel)

# Specify model names
GSAMJJmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'AMJJNDVIdoySPI14dmodel', 'AMJJNDVIdoySPI30dmodel', 'AMJJNDVIdoySPI60dmodel', 'AMJJNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSAMJJSPINDVImodels, modnames = GSAMJJmod.names)

# When comparing lm for Apr/May/Jun/Jul and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only May through August
MJJASPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(5:8)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
MJJANDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = MJJASPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
MJJANDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = MJJASPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
MJJANDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = MJJASPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
MJJANDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = MJJASPINDVI)

# Put all models into a list
MJJASPINDVImodels <- list(MJJANDVIdoySPI14dmodel, MJJANDVIdoySPI30dmodel, MJJANDVIdoySPI60dmodel, MJJANDVIdoySPI90dmodel)

# Specify model names
MJJAmod.names <- c('MJJANDVIdoySPI14dmodel', 'MJJANDVIdoySPI30dmodel', 'MJJANDVIdoySPI60dmodel', 'MJJANDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = MJJASPINDVImodels, modnames = MJJAmod.names)

# Compare Grow Season models with May/June/July/August models
# Put all models into a list
GSMJJASPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, MJJANDVIdoySPI14dmodel, MJJANDVIdoySPI30dmodel, MJJANDVIdoySPI60dmodel, MJJANDVIdoySPI90dmodel)

# Specify model names
GSMJJAmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'MJJANDVIdoySPI14dmodel', 'MJJANDVIdoySPI30dmodel', 'MJJANDVIdoySPI60dmodel', 'MJJANDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSMJJASPINDVImodels, modnames = GSMJJAmod.names)

# When comparing lm for May/Jun/Jul/Aug and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only June through September
JJASSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(6:9)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
JJASNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = JJASSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
JJASNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = JJASSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
JJASNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = JJASSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
JJASNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = JJASSPINDVI)

# Put all models into a list
JJASSPINDVImodels <- list(JJASNDVIdoySPI14dmodel, JJASNDVIdoySPI30dmodel, JJASNDVIdoySPI60dmodel, JJASNDVIdoySPI90dmodel)

# Specify model names
JJASmod.names <- c('JJASNDVIdoySPI14dmodel', 'JJASNDVIdoySPI30dmodel', 'JJASNDVIdoySPI60dmodel', 'JJASNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = JJASSPINDVImodels, modnames = JJASmod.names)

# Compare Grow Season models with June/July/August/September models
# Put all models into a list
GSJJASSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, JJASNDVIdoySPI14dmodel, JJASNDVIdoySPI30dmodel, JJASNDVIdoySPI60dmodel, JJASNDVIdoySPI90dmodel)

# Specify model names
GSJJASmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'JJASNDVIdoySPI14dmodel', 'JJASNDVIdoySPI30dmodel', 'JJASNDVIdoySPI60dmodel', 'JJASNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSJJASSPINDVImodels, modnames = GSJJASmod.names)

# When comparing lm for Jun/Jul/Aug/Sep and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only May
MAYSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(5)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
MAYNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = MAYSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
MAYNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = MAYSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
MAYNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = MAYSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
MAYNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = MAYSPINDVI)

# Put all models into a list
MAYSPINDVImodels <- list(MAYNDVIdoySPI14dmodel, MAYNDVIdoySPI30dmodel, MAYNDVIdoySPI60dmodel, MAYNDVIdoySPI90dmodel)

# Specify model names
MAYmod.names <- c('MAYNDVIdoySPI14dmodel', 'MAYNDVIdoySPI30dmodel', 'MAYNDVIdoySPI60dmodel', 'MAYNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = MAYSPINDVImodels, modnames = MAYmod.names)

# Compare Grow Season models with May models
# Put all models into a list
GSMAYSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, MAYNDVIdoySPI14dmodel, MAYNDVIdoySPI30dmodel, MAYNDVIdoySPI60dmodel, MAYNDVIdoySPI90dmodel)

# Specify model names
GSMAYmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'MAYNDVIdoySPI14dmodel', 'MAYNDVIdoySPI30dmodel', 'MAYNDVIdoySPI60dmodel', 'MAYNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSMAYSPINDVImodels, modnames = GSMAYmod.names)

# When comparing lm for May and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only June
JUNSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(6)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
JUNNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = JUNSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
JUNNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = JUNSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
JUNNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = JUNSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
JUNNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = JUNSPINDVI)

# Put all models into a list
JUNSPINDVImodels <- list(JUNNDVIdoySPI14dmodel, JUNNDVIdoySPI30dmodel, JUNNDVIdoySPI60dmodel, JUNNDVIdoySPI90dmodel)

# Specify model names
JUNmod.names <- c('JUNNDVIdoySPI14dmodel', 'JUNNDVIdoySPI30dmodel', 'JUNNDVIdoySPI60dmodel', 'JUNNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = JUNSPINDVImodels, modnames = JUNmod.names)

# Compare Grow Season models with June models
# Put all models into a list
GSJUNSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, JUNNDVIdoySPI14dmodel, JUNNDVIdoySPI30dmodel, JUNNDVIdoySPI60dmodel, JUNNDVIdoySPI90dmodel)

# Specify model names
GSJUNmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'JUNNDVIdoySPI14dmodel', 'JUNNDVIdoySPI30dmodel', 'JUNNDVIdoySPI60dmodel', 'JUNNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSJUNSPINDVImodels, modnames = GSJUNmod.names)

# When comparing lm for June and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only July
JULSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(7)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
JULNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = JULSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
JULNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = JULSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
JULNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = JULSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
JULNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = JULSPINDVI)

# Put all models into a list
JULSPINDVImodels <- list(JULNDVIdoySPI14dmodel, JULNDVIdoySPI30dmodel, JULNDVIdoySPI60dmodel, JULNDVIdoySPI90dmodel)

# Specify model names
JULmod.names <- c('JULNDVIdoySPI14dmodel', 'JULNDVIdoySPI30dmodel', 'JULNDVIdoySPI60dmodel', 'JULNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = JULSPINDVImodels, modnames = JULmod.names)

# Compare Grow Season models with July models
# Put all models into a list
GSJULSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, JULNDVIdoySPI14dmodel, JULNDVIdoySPI30dmodel, JULNDVIdoySPI60dmodel, JULNDVIdoySPI90dmodel)

# Specify model names
GSJULmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'JULNDVIdoySPI14dmodel', 'JULNDVIdoySPI30dmodel', 'JULNDVIdoySPI60dmodel', 'JULNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSJULSPINDVImodels, modnames = GSJULmod.names)

# When comparing lm for July and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only August
AUGSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(8)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
AUGNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = AUGSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
AUGNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = AUGSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
AUGNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = AUGSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
AUGNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = AUGSPINDVI)

# Put all models into a list
AUGSPINDVImodels <- list(AUGNDVIdoySPI14dmodel, AUGNDVIdoySPI30dmodel, AUGNDVIdoySPI60dmodel, AUGNDVIdoySPI90dmodel)

# Specify model names
AUGmod.names <- c('AUGNDVIdoySPI14dmodel', 'AUGNDVIdoySPI30dmodel', 'AUGNDVIdoySPI60dmodel', 'AUGNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = AUGSPINDVImodels, modnames = AUGmod.names)

# Compare Grow Season models with August models
# Put all models into a list
GSAUGSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, AUGNDVIdoySPI14dmodel, AUGNDVIdoySPI30dmodel, AUGNDVIdoySPI60dmodel, AUGNDVIdoySPI90dmodel)

# Specify model names
GSAUGmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'AUGNDVIdoySPI14dmodel', 'AUGNDVIdoySPI30dmodel', 'AUGNDVIdoySPI60dmodel', 'AUGNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSAUGSPINDVImodels, modnames = GSAUGmod.names)

# When comparing lm for August and grow seasons, the best fit model is 60d SPI for the growing season

# Create data set with only September
SEPSPINDVI <- ChicagolandSPINDVINA[which(ChicagolandSPINDVINA$Month %in% c(9)),]

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
SEPNDVIdoySPI14dmodel <- lm(formula = NDVI ~ doy + X14d.SPI, data = SEPSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 30 day as predictor variables
SEPNDVIdoySPI30dmodel <- lm(formula = NDVI ~ doy + X30d.SPI, data = SEPSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 60 day as predictor variables
SEPNDVIdoySPI60dmodel <- lm(formula = NDVI ~ doy + X60d.SPI, data = SEPSPINDVI)

# create basic linear model with NDVI as response variable and doy and SPI 90 day as predictor variables
SEPNDVIdoySPI90dmodel <- lm(formula = NDVI ~ doy + X90d.SPI, data = SEPSPINDVI)

# Put all models into a list
SEPSPINDVImodels <- list(SEPNDVIdoySPI14dmodel, SEPNDVIdoySPI30dmodel, SEPNDVIdoySPI60dmodel, SEPNDVIdoySPI90dmodel)

# Specify model names
SEPmod.names <- c('SEPNDVIdoySPI14dmodel', 'SEPNDVIdoySPI30dmodel', 'SEPNDVIdoySPI60dmodel', 'SEPNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = SEPSPINDVImodels, modnames = SEPmod.names)

# Compare Grow Season models with September models
# Put all models into a list
GSSEPSPINDVImodels <- list(GSNDVIdoySPI14dmodel, GSNDVIdoySPI30dmodel, GSNDVIdoySPI60dmodel, GSNDVIdoySPI90dmodel, SEPNDVIdoySPI14dmodel, SEPNDVIdoySPI30dmodel, SEPNDVIdoySPI60dmodel, SEPNDVIdoySPI90dmodel)

# Specify model names
GSSEPmod.names <- c('GSNDVIdoySPI14dmodel', 'GSNDVIdoySPI30dmodel', 'GSNDVIdoySPI60dmodel', 'GSNDVIdoySPI90dmodel', 'SEPNDVIdoySPI14dmodel', 'SEPNDVIdoySPI30dmodel', 'SEPNDVIdoySPI60dmodel', 'SEPNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSSEPSPINDVImodels, modnames = GSSEPmod.names)

# When comparing lm for September and grow seasons, the best fit model is 60d SPI for the growing season
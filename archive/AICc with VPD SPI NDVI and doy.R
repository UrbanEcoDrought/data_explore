# Script to run multiple linear regression
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

# reading in Trent's VPD data
ChicagolandVPD <- read.csv(file.path(google.drive, "data/data_sets/Chicagoland_Daily_VPD.csv"))

# create column with date in ISO format
ChicagolandVPD$date <- as.Date(ChicagolandVPD$Date, "%m/%d/%Y")

# merge ChicagolandVPD and ChicagolandSPINDVINA by date columns
ChicagolandSPINDVIVPD <- merge (ChicagolandSPINDVINA, ChicagolandVPD, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVIVPDNA <- na.omit(ChicagolandSPINDVIVPD)

# Simplify column label to VPD
colnames(ChicagolandSPINDVIVPDNA)[14] = 'VPD'

# Create data set with variables
VPDSPINDVIdoy <- ChicagolandSPINDVIVPDNA[ , c("X60d.SPI", "VPD", "doy", "NDVI")]

# Look for linear associations between data
pairs(VPDSPINDVIdoy, pch = 20, col = "steelblue")

#install and load the GGally library
install.packages("GGally")
library(GGally)

#generate the pairs plot
ggpairs(VPDSPINDVIdoy)

# create basic linear model with NDVI as response variable and VPD, doy and SPI 14 day as predictor variables
VPDNDVIdoySPI14dmodel <- lm(formula = NDVI ~ VPD + doy  + X14d.SPI, data = ChicagolandSPINDVIVPDNA)
summary(VPDNDVIdoySPI14dmodel)

# create basic linear model with NDVI as response variable and VPD, doy and SPI 30 day as predictor variables
VPDNDVIdoySPI30dmodel <- lm(formula = NDVI ~ VPD + doy  + X30d.SPI, data = ChicagolandSPINDVIVPDNA)

# create basic linear model with NDVI as response variable and VPD, doy and SPI 60 day as predictor variables
VPDNDVIdoySPI60dmodel <- lm(formula = NDVI ~ VPD + doy  + X60d.SPI, data = ChicagolandSPINDVIVPDNA)

# create basic linear model with NDVI as response variable and VPD, doy and SPI 90 day as predictor variables
VPDNDVIdoySPI90dmodel <- lm(formula = NDVI ~ VPD + doy  + X90d.SPI, data = ChicagolandSPINDVIVPDNA)

# Install and load AICcmodavg package
install.packages("AICcmodavg")
library(AICcmodavg)

# Put all models into a list
FYVPDSPINDVImodels <- list(VPDNDVIdoySPI14dmodel, VPDNDVIdoySPI30dmodel, VPDNDVIdoySPI60dmodel, VPDNDVIdoySPI90dmodel)

# Specify model names
FYVPDmod.names <- c('VPDNDVIdoySPI14dmodel', 'VPDNDVIdoySPI30dmodel', 'VPDNDVIdoySPI60dmodel', 'VPDNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = FYVPDSPINDVImodels, modnames = FYVPDmod.names)

# Based off this model, using the 30 day SPI is the best fitting model. 

# Create data set with only growing season (April-September)
GrowSeasonVPDSPINDVI <- ChicagolandSPINDVIVPDNA[which(ChicagolandSPINDVIVPDNA$Month %in% c(4:9)),]

# create basic linear model with NDVI as response variable and VPD, doy and SPI 14 day as predictor variables
GSVPDNDVIdoySPI14dmodel <- lm(formula = NDVI ~ VPD + doy  + X14d.SPI, data = GrowSeasonVPDSPINDVI)

# create basic linear model with NDVI as response variable and VPD, doy and SPI 30 day as predictor variables
GSVPDNDVIdoySPI30dmodel <- lm(formula = NDVI ~ VPD + doy  + X30d.SPI, data = GrowSeasonVPDSPINDVI)

# create basic linear model with NDVI as response variable and VPD, doy and SPI 60 day as predictor variables
GSVPDNDVIdoySPI60dmodel <- lm(formula = NDVI ~ VPD + doy  + X60d.SPI, data = GrowSeasonVPDSPINDVI)

# create basic linear model with NDVI as response variable and VPD, doy and SPI 90 day as predictor variables
GSVPDNDVIdoySPI90dmodel <- lm(formula = NDVI ~ VPD + doy  + X90d.SPI, data = GrowSeasonVPDSPINDVI)

# Put all models into a list
GSVPDSPINDVImodels <- list(GSVPDNDVIdoySPI14dmodel, GSVPDNDVIdoySPI30dmodel, GSVPDNDVIdoySPI60dmodel, GSVPDNDVIdoySPI90dmodel)

# Specify model names
GSVPDmod.names <- c('GSVPDNDVIdoySPI14dmodel', 'GSVPDNDVIdoySPI30dmodel', 'GSVPDNDVIdoySPI60dmodel', 'GSVPDNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = GSSPINDVImodels, modnames = GSVPDmod.names)

# Compare Full Year models with Grow Season models
# Put all models into a list
FYGSVPDSPINDVImodels <- list(VPDNDVIdoySPI14dmodel, VPDNDVIdoySPI30dmodel, VPDNDVIdoySPI60dmodel, VPDNDVIdoySPI90dmodel, GSVPDNDVIdoySPI14dmodel, GSVPDNDVIdoySPI30dmodel, GSVPDNDVIdoySPI60dmodel, GSVPDNDVIdoySPI90dmodel)

# Specify model names
FYGSVPDmod.names <- c('VPDNDVIdoySPI14dmodel', 'VPDNDVIdoySPI30dmodel', 'VPDNDVIdoySPI60dmodel', 'VPDNDVIdoySPI90dmodel', 'GSVPDNDVIdoySPI14dmodel', 'GSVPDNDVIdoySPI30dmodel', 'GSVPDNDVIdoySPI60dmodel', 'GSVPDNDVIdoySPI90dmodel')

# Calculate the AIC of each model
aictab(cand.set = FYGSVPDSPINDVImodels, modnames = FYGSVPDmod.names)

# When comparing the full year's data with the growing season's data (April-September), using the full year's 30 day SPI is the best fitting model. 
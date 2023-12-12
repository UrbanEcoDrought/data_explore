# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)
library(mgcv)
library(dplR)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
#Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)

#Subset each satellite out and see which year it encompasses
ndvi.sat5 <- ndvi.all[ndvi.all$satellite=="Landsat5",]
unique(ndvi.sat5$year)
ndvi.sat7 <- ndvi.all[ndvi.all$satellite=="Landsat7",]
unique(ndvi.sat7$year)
ndvi.sat8 <- ndvi.all[ndvi.all$satellite=="Landsat8",]
unique(ndvi.sat8$year)
ndvi.sat9 <- ndvi.all[ndvi.all$satellite=="Landsat9",]
unique(ndvi.sat9$year)

#Create a subset of satellites 5 and 7
ndvi.sats57<- subset(ndvi.all, subset = satellite %in% c("Landsat5", "Landsat7"))

#Detrend data
ndvi.sats57.gamm.step1 <- gamm(NDVI ~ type + s(doy, k=12, by=type), random=list(satellite=~1), data=ndvi.sats57, na.rm=T)
ndvi.sats57na <- ndvi.sats57[!is.na(ndvi.sats57$NDVI),]
ndvi.sats57na$ndvi.gam.pred.step1 <-predict(ndvi.sats57.gamm.step1)

ndvi.sats57na$ndvi.gam.step1.anomaly <- ndvi.sats57na$NDVI-ndvi.sats57na$ndvi.gam.pred.step1 
ndvi.gamm.step2 <- gam(ndvi.gam.step1.anomaly~s(doy, k=12, by=satellite), data=ndvi.sats57na, na.rm=T) 
ndvi.sats57na$ndvi.gam.pred.step2 <- predict(ndvi.gamm.step2)
ndvi.sats57na$ndvi.modeled <- ndvi.sats57na$ndvi.gam.pred.step1 + ndvi.sats57na$ndvi.gam.pred.step2

#Create double detrend anomalies.
ndvi.sats57na$ndvi.anomaly <- ndvi.sats57na$NDVI-ndvi.sats57na$ndvi.modeled
head(ndvi.sats57na)
ndvi.check.all <- merge(ndvi.sats57na, ndvi.all)
summary(ndvi.check.all)

#Parse down data frame and save.
summary(ndvi.check.all)
ndvi.detrend.sats57 <- ndvi.check.all[,c("year", "doy", "type", "date", "NDVI", "ndvi.modeled", "ndvi.anomaly")]
names(ndvi.detrend.sats57) <- c("year", "doy", "type", "date", "ndvi.obs", "ndvi.modeled","ndvi.anomaly")


#Fit gam for data
gam.fitted.sats57 <- gam(ndvi.obs ~ ,data = ndvi.sats57, method = 'REML')

ndvi.sats57na$predicted <- predict(gam.fitted.sats57)
ndvi.sats57na$resids <- resid(gam.fitted.sats57)
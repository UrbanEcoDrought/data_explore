# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)
library(mgcv)
library(dplR)

# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
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

# same thing as you did above; just another way of writing it
# unique(ndvi.all$year[ndvi.all$satellite=="Landsat5"])
# unique(ndvi.all[ndvi.all$satellite=="Landsat5", "year"])


# An alternate way would've been to reference the year we care about
unique(ndvi.all$satellite[ndvi.all$year==2005])


#Create a subset of satellites 5 and 7
ndvi.sats57<- subset(ndvi.all, subset = satellite %in% c("Landsat5", "Landsat7"))
summary(ndvi.sats57)

#Detrend data
# ndvi.sats57.gamm.step1 <- gamm(NDVI ~ type + s(doy, k=12, by=type), random=list(satellite=~1), data=ndvi.sats57, na.rm=T)
# summary(ndvi.sats57.gamm.step1$gam)
ndvi.sats57.gam.step1 <- gam(NDVI ~ type + s(doy, k=12, by=type), data=ndvi.sats57, na.rm=T)
summary(ndvi.sats57.gam.step1)

# ndvi.sats57na <- ndvi.sats57[!is.na(ndvi.sats57$NDVI),]
# ndvi.sats57na$ndvi.gam.pred.step1 <- predict(ndvi.sats57.gamm.step1)


# Newdata allows prediction with data not used to train the model ("known" predictors that might've been excluded from model fitting for whatever reason)
ndvi.sats57$predict1 <- predict(ndvi.sats57.gam.step1, newdata=ndvi.sats57[,])
summary(ndvi.sats57)


# We don't want double-detrended
#Create double detrend anomalies.
# ndvi.sats57na$ndvi.anomaly <- ndvi.sats57na$NDVI-ndvi.sats57na$ndvi.modeled
# head(ndvi.sats57na)
# ndvi.check.all <- merge(ndvi.sats57na, ndvi.all)
# summary(ndvi.check.all)

#Parse down data frame and save.
summary(ndvi.check.all)
ndvi.detrend.sats57 <- ndvi.check.all[,c("year", "doy", "type", "date", "NDVI", "ndvi.modeled", "ndvi.anomaly")]
names(ndvi.detrend.sats57) <- c("year", "doy", "type", "date", "ndvi.obs", "ndvi.modeled","ndvi.anomaly")


#Fit gam for data
gam.fitted.sats57 <- gam(ndvi.obs ~ ,data = ndvi.sats57, method = 'REML')

ndvi.sats57na$predicted <- predict(gam.fitted.sats57)
ndvi.sats57na$resids <- resid(gam.fitted.sats57)
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
ndvi.sats57$predict57 <- predict(ndvi.sats57.gam.step1, newdata=ndvi.sats57[,])
summary(ndvi.sats57)

#Create anomalies
ndvi.sats57$ndvi.anomaly <- ndvi.sats57$NDVI - ndvi.sats57$predict57

#plot
#Compare modeled(predicted) data with observed values
ggplot(data = ndvi.sats57) + facet_wrap(type~.) +
  geom_point(aes(x=NDVI, y=predict57), alpha=0.25)

plot.gam(ndvi.sats57.gam.step1)


plot(NDVI ~ predict57, data=ndvi.sats57); abline(a=0, b=1, col="red")

#Plot predicted ndvi by land cover type
ggplot(data = ndvi.sats57) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=predict57))

# We want to add the 2005 observed NDVI and compare it to the predicted ("normal") NDVI
ggplot(data = ndvi.sats57[ndvi.sats57$year %in% c(2005),]) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=predict57, color="Predicted (Normal) for Sats 5/7"), size=1.5) +
  geom_line(aes(x=doy, y=NDVI, color="Observed 2005 NDVI (Drought NDVI)"))  +
  scale_color_manual(values=c("Predicted (Normal) for Sats 5/7"="black", "Observed 2005 NDVI (Drought NDVI)"="red2"))
  
#Plot 2005 anomaly
ggplot(data=ndvi.sats57) + facet_wrap(type~.) +
  geom_line(data=ndvi.sats57[ndvi.sats57$year %in% c(2005),], aes(x=doy, y=ndvi.anomaly, col=as.factor(year)))

#Compare 2005 with all available data

#Detrend data
ndvi.gam.step1 <- gam(NDVI ~ type + s(doy, k=12, by=type), data=ndvi.all, na.rm=T)
summary(ndvi.gam.step1)

# Newdata allows prediction with data not used to train the model ("known" predictors that might've been excluded from model fitting for whatever reason)
ndvi.all$predict1 <- predict(ndvi.gam.step1, newdata=ndvi.all[,])
summary(ndvi.all)

# We want to add the 2005 observed NDVI and compare it to the predicted ("normal") NDVI
ggplot(data = ndvi.all[ndvi.all$year %in% c(2005),]) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=predict1, color="Predicted (Normal) All Sats"), size=1.5) +
  geom_line(aes(x=doy, y=NDVI, color="Observed 2005 NDVI (Drought NDVI)"))  +
  scale_color_manual(values=c("Predicted (Normal) All Sats"="black", "Observed 2005 NDVI (Drought NDVI)"="red2"))

ndvi.predictions <- merge(ndvi.all, ndvi.sats57, by=c("date", "type"), all.x=F, all.y=TRUE)
ndvi.predictionsna <- na.omit(ndvi.predictions)

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

# Determine satellites in operation for 2023
unique(ndvi.all$satellite[ndvi.all$year==2023])

#Create a subset of satellites 8 and 9
ndvi.sats89 <-subset(ndvi.all, subset = satellite %in% c("Landsat8", "Landsat9"))
summary(ndvi.sats89)

#Detrend data
ndvi.sats89.gam.step1 <- gam(NDVI ~ type + s(doy, k=12, by=type), data=ndvi.sats89, na.rm=T)
summary(ndvi.sats89.gam.step1)

# Newdata allows prediction with data not used to train the model ("known" predictors that might've been excluded from model fitting for whatever reason)
ndvi.sats89$predict89 <- predict(ndvi.sats89.gam.step1, newdata=ndvi.sats89[,])
summary(ndvi.sats89)

#plot
#Compare modeled(predicted) data with observed values
plot(NDVI ~ predict89, data=ndvi.sats89); abline(a=0, b=1, col="red")

#Plot predicted ndvi by land cover type
ggplot(data = ndvi.sats89) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=predict89))

# We want to add the 2023 observed NDVI and compare it to the predicted ("normal") NDVI
ggplot(data = ndvi.sats89[ndvi.sats89$year %in% c(2023),]) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=predict89, color="Predicted (Normal)"), size=1.5) +
  geom_line(aes(x=doy, y=NDVI, color="Observed 2023 NDVI (Drought NDVI)"))  +
  scale_color_manual(values=c("Predicted (Normal)"="black", "Observed 2023 NDVI (Drought NDVI)"="red2"))

#Compare 2023 with all available data

#Detrend data
ndvi.gam.step1 <- gam(NDVI ~ type + s(doy, k=12, by=type), data=ndvi.all, na.rm=T)
summary(ndvi.gam.step1)

# Newdata allows prediction with data not used to train the model ("known" predictors that might've been excluded from model fitting for whatever reason)
ndvi.all$predict1 <- predict(ndvi.gam.step1, newdata=ndvi.all[,])
summary(ndvi.all)

# We want to add the 2023 observed NDVI and compare it to the predicted ("normal") NDVI
ggplot(data = ndvi.all[ndvi.all$year %in% c(2023),]) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=predict1, color="Predicted (Normal) All Sats"), size=1.5) +
  geom_line(aes(x=doy, y=NDVI, color="Observed 2023 NDVI (Drought NDVI)"))  +
  scale_color_manual(values=c("Predicted (Normal) All Sats"="black", "Observed 2023 NDVI (Drought NDVI)"="red2"))

# merge sat89 and sat57 data sets 
ndvi.all.predictions <- merge(ndvi.predictions, ndvi.sats89, by=c("date", "type"), all.x=F, all.y=TRUE)
summary(ndvi.all.predictions)
ndvi.all.predictionsna <- na.omit(ndvi.all.predictions)
summary(ndvi.all.predictionsna)

ggplot(data = ndvi.predictionsna[ndvi.predictionsna$year.x %in% c(2023)]) + facet_wrap(type~.) +
  geom_line(aes(x=doy.x, y=predict57, color="Predicted (Normal) Sats 5/7"), size=1.5) +
  geom_line(aes(x=doy.x, y=predict89, color="Predicted (Normal) Sats 8/9"))  +
  geom_line(aes(x=doy.x, y=NDVI.x, color="Observed 2023 NDVI"))
  scale_color_manual(values=c("Predicted (Normal) Sats 5/7"="black", "Predicted (Normal) Sats 8/9"="red2", "Observed 2023 NDVI"="yellow"))

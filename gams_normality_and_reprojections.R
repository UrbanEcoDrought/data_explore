library(mgcv) #load packages
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

######################
#read in gam derivs function
######################
source("~/work/MSB_Non-Stationarity/Example_Temporal_TreeRings/scripts/helper_functions/0_Calculate_GAMM_Derivs.R")
source("../../MSB_Non-Stationarity/Example_Temporal_TreeRings/scripts/helper_functions/0_Calculate_GAMM_Derivs.R")
# dir("../..")
######################

ndvi.latest <- read.csv(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract/NDVIall_latest.csv")) #load latest NDVI data
ndvi.latest$date <- as.Date(ndvi.latest$date)
ndvi.latest$type <- as.factor(ndvi.latest$type)
ndvi.latest$mission <- as.factor(ndvi.latest$mission)
summary(ndvi.latest)

######################
#crop
######################
ndvicrop <- ndvi.latest <- ndvi.latest[ndvi.latest$type=="crop",]
ggplot(data=ndvicrop[,], aes(x=yday,y=NDVI, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="crop")

gamcrop <- gam(NDVI ~ s(yday, k=18, by=mission) + mission-1, data=ndvicrop)
summary(gamcrop)
AIC(gamcrop)

par(mfrow=c(2,2))
plot(gamcrop)
par(mfrow=c(1,1))


ndvicrop$predMean <- predict(gamcrop, newdata=ndvicrop)
ndvicrop$resid <- ndvicrop$NDVI - ndvicrop$predMean
head(ndvicrop)
tail(ndvicrop)

# Going to "reproject" the predicted mean/normal
ndvicropDupe <- ndvicrop
ndvicropDupe$mission <- "landsat 8"
head(ndvicropDupe)
tail(ndvicropDupe)

ndvicrop$predMean.reproj <- predict(gamcrop, newdata=ndvicropDupe)
ndvicrop$NDVI.reproj <- ndvicrop$resid + ndvicrop$predMean.reproj
summary(ndvicrop)

ggplot(data=ndvicrop[,], aes(x=yday,y=NDVI.reproj, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="crop reprojected") + ylab("Reprojected NDVI")

ggplot(data=ndvicrop[,], aes(x=yday,y=NDVI)) + 
  ggtitle("crop raw ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndvicrop[ndvicrop$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 


ggplot(data=ndvicrop[,], aes(x=yday,y=NDVI.reproj)) + 
  ggtitle("crop reprojected ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndvicrop[ndvicrop$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  ylab("Reprojected NDVI")

######################
#forest
######################
ndviforest <- ndvi.latest <- ndvi.latest[ndvi.latest$type=="forest",]
ggplot(data=ndviforest[,], aes(x=yday,y=NDVI, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="forest")

gamforest <- gam(NDVI ~ s(yday, k=18, by=mission) + mission-1, data=ndviforest)
summary(gamforest)
AIC(gamforest)

par(mfrow=c(2,2))
plot(gamforest)
par(mfrow=c(1,1))


ndviforest$predMean <- predict(gamforest, newdata=ndviforest)
ndviforest$resid <- ndviforest$NDVI - ndviforest$predMean
head(ndviforest)
tail(ndviforest)

# Going to "reproject" the predicted mean/normal
ndviforestDupe <- ndviforest
ndviforestDupe$mission <- "landsat 8"
head(ndviforestDupe)
tail(ndviforestDupe)

ndviforest$predMean.reproj <- predict(gamforest, newdata=ndviforestDupe)
ndviforest$NDVI.reproj <- ndviforest$resid + ndviforest$predMean.reproj
summary(ndviforest)

ggplot(data=ndviforest[,], aes(x=yday,y=NDVI.reproj, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="forest reprojected") + ylab("Reprojected NDVI")

ggplot(data=ndviforest[,], aes(x=yday,y=NDVI)) + 
  ggtitle("forest raw ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviforest[ndviforest$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 


ggplot(data=ndviforest[,], aes(x=yday,y=NDVI.reproj)) + 
  ggtitle("forest reprojected ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviforest[ndviforest$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  ylab("Reprojected NDVI")

######################
#grassland
######################
ndvigrass <- ndvi.latest <- ndvi.latest[ndvi.latest$type=="grassland",]
ggplot(data=ndvigrass[,], aes(x=yday,y=NDVI, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="grassland")

gamgrass <- gam(NDVI ~ s(yday, k=18, by=mission) + mission-1, data=ndvigrass)
summary(gamgrass)
AIC(gamgrass)

par(mfrow=c(2,2))
plot(gamgrass)
par(mfrow=c(1,1))


ndvigrass$predMean <- predict(gamgrass, newdata=ndvigrass)
ndvigrass$resid <- ndvigrass$NDVI - ndvigrass$predMean
head(ndvigrass)
tail(ndvigrass)

# Going to "reproject" the predicted mean/normal
ndvigrassDupe <- ndvigrass
ndvigrassDupe$mission <- "landsat 8"
head(ndvigrassDupe)
tail(ndvigrassDupe)

ndvigrass$predMean.reproj <- predict(gamgrass, newdata=ndvigrassDupe)
ndvigrass$NDVI.reproj <- ndvigrass$resid + ndvigrass$predMean.reproj
summary(ndvigrass)

ggplot(data=ndvigrass[,], aes(x=yday,y=NDVI.reproj, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="grassland reprojected") + ylab("Reprojected NDVI")

ggplot(data=ndvigrass[,], aes(x=yday,y=NDVI)) + 
  ggtitle("grassland raw ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndvigrass[ndvigrass$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 


ggplot(data=ndvigrass[,], aes(x=yday,y=NDVI.reproj)) + 
  ggtitle("grassland reprojected ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndvigrass[ndvigrass$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  ylab("Reprojected NDVI")


######################
#urban-medium
######################

ndviUrbMed <- ndvi.latest <- ndvi.latest[ndvi.latest$type=="urban-medium",]
ggplot(data=ndviUrbMed[,], aes(x=yday,y=NDVI, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="urban-medium")

gamUrbMed <- gam(NDVI ~ s(yday, k=18, by=mission) + mission-1, data=ndviUrbMed)
summary(gamUrbMed)
AIC(gamUrbMed)

par(mfrow=c(2,2))
plot(gamUrbMed)
par(mfrow=c(1,1))


ndviUrbMed$predMean <- predict(gamUrbMed, newdata=ndviUrbMed)
ndviUrbMed$resid <- ndviUrbMed$NDVI - ndviUrbMed$predMean
head(ndviUrbMed)
tail(ndviUrbMed)

# Going to "reproject" the predicted mean/normal
ndviUrbMedDupe <- ndviUrbMed
ndviUrbMedDupe$mission <- "landsat 8"
head(ndviUrbMedDupe)
tail(ndviUrbMedDupe)

ndviUrbMed$predMean.reproj <- predict(gamUrbMed, newdata=ndviUrbMedDupe)
ndviUrbMed$NDVI.reproj <- ndviUrbMed$resid + ndviUrbMed$predMean.reproj
summary(ndviUrbMed)

ggplot(data=ndviUrbMed[,], aes(x=yday,y=NDVI.reproj, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="urban-medium reprojected") + ylab("Reprojected NDVI")

ggplot(data=ndviUrbMed[,], aes(x=yday,y=NDVI)) + 
  ggtitle("urban-medium raw ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbMed[ndviUrbMed$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 


ggplot(data=ndviUrbMed[,], aes(x=yday,y=NDVI.reproj)) + 
  ggtitle("urban-medium reprojected ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbMed[ndviUrbMed$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  ylab("Reprojected NDVI")

######################
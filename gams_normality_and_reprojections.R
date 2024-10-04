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
#plotting satellite data after Landsat 9 is launched
ndvi.recent <- ndvi.latest[ndvi.latest$date > "2022-01-01",]

#ndvi.recent_crop <- ndvi.recent[ndvi.recent$type=="crop",]
#ndvi.recent_forest <- ndvi.recent[ndvi.recent$type=="forest",]
#ndvi.recent_grass <- ndvi.recent[ndvi.recent$type=="grassland",]
#ndvi.recent_urbhigh <- ndvi.recent[ndvi.recent$type=="urban-high",]
#ndvi.recent_urbmed <- ndvi.recent[ndvi.recent$type=="urban-medium",]
#ndvi.recent_urblow <- ndvi.recent[ndvi.recent$type=="urban-low",]
#ndvi.recent_urbop <- ndvi.recent[ndvi.recent$type=="urban-open",]


ggplot(data=ndvi.recent[,], aes(x=date, y=NDVI)) +
  ggtitle("NDVI by Mission 2022-present") +
  facet_wrap(~type) +
  geom_line(data=ndvi.recent[ndvi.recent$mission=="landsat 7", ], aes(color="landsat 7"), size=0.25) +
  geom_line(data=ndvi.recent[ndvi.recent$mission=="landsat 8", ], aes(color="landsat 8"), size=0.25) +
  geom_line(data=ndvi.recent[ndvi.recent$mission=="landsat 9", ], aes(color="landsat 9"), size=0.25) +
  stat_smooth(data=ndvi.recent[ndvi.recent$mission=="landsat 7", ], aes(color="landsat 7", fill="landsat 7"), size=1.25, alpha=0.2, method="gam") +
  stat_smooth(data=ndvi.recent[ndvi.recent$mission=="landsat 8", ], aes(color="landsat 8", fill="landsat 8"), size=1.25, alpha=0.2, method="gam") +
  stat_smooth(data=ndvi.recent[ndvi.recent$mission=="landsat 9", ], aes(color="landsat 9", fill="landsat 9"), size=1.25, alpha=0.2, method="gam") +
  #scale_x_continuous(name="Date", expand=c(0,0), breaks=day.labels$date[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)])  +
  scale_color_manual(values=c("landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  coord_cartesian(ylim=c(0,1)) +
  labs(x="Date", col="mission")  +
  guides(fill=F) +
  theme_bw()

#ggplot(data=ndvi.recent[,], aes(x=date,y=NDVI, color=mission)) +
  #facet_wrap(~type) +
  #geom_point(size=0.1, alpha=0.3) +
  #geom_smooth(method="gam") +
  #scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  #scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  #labs(title="NDVI by mission")


######################
#crop
######################
ndvicrop <- ndvi.latest[ndvi.latest$type=="crop",]
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
ndviforest <- ndvi.latest[ndvi.latest$type=="forest",]
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
ndvigrass <- ndvi.latest[ndvi.latest$type=="grassland",]
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
#urban-high
######################

ndviUrbHigh <- ndvi.latest[ndvi.latest$type=="urban-high",]
ggplot(data=ndviUrbHigh[,], aes(x=yday,y=NDVI, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="urban-high")

gamUrbHigh <- gam(NDVI ~ s(yday, k=18, by=mission) + mission-1, data=ndviUrbHigh)
summary(gamUrbHigh)
AIC(gamUrbHigh)

par(mfrow=c(2,2))
plot(gamUrbHigh)
par(mfrow=c(1,1))


ndviUrbHigh$predMean <- predict(gamUrbHigh, newdata=ndviUrbHigh)
ndviUrbHigh$resid <- ndviUrbHigh$NDVI - ndviUrbHigh$predMean
head(ndviUrbHigh)
tail(ndviUrbHigh)

# Going to "reproject" the predicted mean/normal
ndviUrbHighDupe <- ndviUrbHigh
ndviUrbHighDupe$mission <- "landsat 8"
head(ndviUrbHighDupe)
tail(ndviUrbHighDupe)

ndviUrbHigh$predMean.reproj <- predict(gamUrbHigh, newdata=ndviUrbHighDupe)
ndviUrbHigh$NDVI.reproj <- ndviUrbHigh$resid + ndviUrbHigh$predMean.reproj
summary(ndviUrbHigh)

ggplot(data=ndviUrbHigh[,], aes(x=yday,y=NDVI.reproj, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="urban-high reprojected") + ylab("Reprojected NDVI")

ggplot(data=ndviUrbHigh[,], aes(x=yday,y=NDVI)) + 
  ggtitle("urban-high raw ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbHigh[ndviUrbHigh$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 


ggplot(data=ndviUrbHigh[,], aes(x=yday,y=NDVI.reproj)) + 
  ggtitle("urban-high reprojected ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbHigh[ndviUrbHigh$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  ylab("Reprojected NDVI")



######################
#urban-medium
######################

ndviUrbMed <- ndvi.latest[ndvi.latest$type=="urban-medium",]
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
#urban-low
######################

ndviUrbLow <- ndvi.latest[ndvi.latest$type=="urban-low",]
ggplot(data=ndviUrbLow[,], aes(x=yday,y=NDVI, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="urban-low")

gamUrbLow <- gam(NDVI ~ s(yday, k=18, by=mission) + mission-1, data=ndviUrbLow)
summary(gamUrbLow)
AIC(gamUrbLow)

par(mfrow=c(2,2))
plot(gamUrbLow)
par(mfrow=c(1,1))


ndviUrbLow$predMean <- predict(gamUrbLow, newdata=ndviUrbLow)
ndviUrbLow$resid <- ndviUrbLow$NDVI - ndviUrbLow$predMean
head(ndviUrbLow)
tail(ndviUrbLow)

# Going to "reproject" the predicted mean/normal
ndviUrbLowDupe <- ndviUrbLow
ndviUrbLowDupe$mission <- "landsat 8"
head(ndviUrbLowDupe)
tail(ndviUrbLowDupe)

ndviUrbLow$predMean.reproj <- predict(gamUrbLow, newdata=ndviUrbLowDupe)
ndviUrbLow$NDVI.reproj <- ndviUrbLow$resid + ndviUrbLow$predMean.reproj
summary(ndviUrbLow)

ggplot(data=ndviUrbLow[,], aes(x=yday,y=NDVI.reproj, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="urban-low reprojected") + ylab("Reprojected NDVI")

ggplot(data=ndviUrbLow[,], aes(x=yday,y=NDVI)) + 
  ggtitle("urban-low raw ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbLow[ndviUrbLow$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 


ggplot(data=ndviUrbLow[,], aes(x=yday,y=NDVI.reproj)) + 
  ggtitle("urban-low reprojected ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbLow[ndviUrbLow$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  ylab("Reprojected NDVI")


######################
#urban-open
######################

ndviUrbOpen <- ndvi.latest[ndvi.latest$type=="urban-open",]
ggplot(data=ndviUrbOpen[,], aes(x=yday,y=NDVI, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="urban-open")

gamUrbOpen <- gam(NDVI ~ s(yday, k=18, by=mission) + mission-1, data=ndviUrbOpen)
summary(gamUrbOpen)
AIC(gamUrbOpen)

par(mfrow=c(2,2))
plot(gamUrbOpen)
par(mfrow=c(1,1))


ndviUrbOpen$predMean <- predict(gamUrbOpen, newdata=ndviUrbOpen)
ndviUrbOpen$resid <- ndviUrbOpen$NDVI - ndviUrbOpen$predMean
head(ndviUrbOpen)
tail(ndviUrbOpen)

# Going to "reproject" the predicted mean/normal
ndviUrbOpenDupe <- ndviUrbOpen
ndviUrbOpenDupe$mission <- "landsat 8"
head(ndviUrbOpenDupe)
tail(ndviUrbOpenDupe)

ndviUrbOpen$predMean.reproj <- predict(gamUrbOpen, newdata=ndviUrbOpenDupe)
ndviUrbOpen$NDVI.reproj <- ndviUrbOpen$resid + ndviUrbOpen$predMean.reproj
summary(ndviUrbOpen)

ggplot(data=ndviUrbOpen[,], aes(x=yday,y=NDVI.reproj, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  labs(title="urban-open reprojected") + ylab("Reprojected NDVI")

ggplot(data=ndviUrbOpen[,], aes(x=yday,y=NDVI)) + 
  ggtitle("urban-open raw ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbOpen[ndviUrbOpen$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 


ggplot(data=ndviUrbOpen[,], aes(x=yday,y=NDVI.reproj)) + 
  ggtitle("urban-open reprojected ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbOpen[ndviUrbOpen$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  ylab("Reprojected NDVI")

######################
library(ggplot2)
library(lubridate)
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
# Sys.setenv(google.drive = "~/Google Drive/Shared drives/Urban Ecological Drought")
# google.drive <- Sys.getenv("GOOGLE_DRIVE")
google.drive = "~/Google Drive/Shared drives/Urban Ecological Drought"

ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))
head(ndvi.all)

ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)

# merge SPI and NDVI dataframes
ChicagolandSPINDVI <- merge(ChicagolandSPI, ndvi.all, by=c("date"), all.x=F, all.y=TRUE)
ChicagolandSPINDVI$ndvi.anomaly <- as.vector(ChicagolandSPINDVI$ndvi.anomaly)
summary(ChicagolandSPINDVI)

# reading in Trent's VPD data
ChicagolandVPD <- read.csv(file.path(google.drive, "data/data_sets/Chicagoland_Daily_VPD.csv"))

# create column with date in ISO format
ChicagolandVPD$date <- as.Date(ChicagolandVPD$Date, "%m/%d/%Y")

# merge ChicagolandVPD and ChicagolandSPINDVI by date columns
ChicagolandSPINDVIVPD <- merge (ChicagolandSPINDVI, ChicagolandVPD, by=c("date"), all.x=TRUE, all.y=TRUE)

# reading in Trent's SPEI data
ChicagolandSPEI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPEI.csv"))

# create column with date in ISO format
ChicagolandSPEI$date <- as.Date(ChicagolandSPEI$Date, "%m/%d/%Y")

# merge ChicagolandVPD and ChicagolandSPINDVI by date columns
ChicagolandSPEISPINDVIVPD <- merge (ChicagolandSPINDVIVPD, ChicagolandSPEI, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPEISPINDVIVPDNA <- na.omit(ChicagolandSPEISPINDVIVPD)

# Change land cover type to numeric levels
# ChicagolandSPINDVIVPDNA$type <- as.numeric(levels(ChicagolandSPINDVIVPDNA$type))[ChicagolandSPINDVIVPDNA$type]

# Simplify column label to VPD
colnames(ChicagolandSPEISPINDVIVPDNA)[2] = 'Other.Date'
colnames(ChicagolandSPEISPINDVIVPDNA)[13] = 'Unneeded.Date'
colnames(ChicagolandSPEISPINDVIVPDNA)[14] = 'VPD'
colnames(ChicagolandSPEISPINDVIVPDNA)[16] = 'SPEI.X14d'
colnames(ChicagolandSPEISPINDVIVPDNA)[17] = 'SPEI.X30d'  
colnames(ChicagolandSPEISPINDVIVPDNA)[18] = 'SPEI.X60d'
colnames(ChicagolandSPEISPINDVIVPDNA)[19] = 'SPEI.X90d'
summary(ChicagolandSPEISPINDVIVPDNA)

# read in Trent's Temperature data
ChicagolandTemp <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_Temps.csv"))

#create column with date in ISO format
ChicagolandTemp$date <-as.Date(ChicagolandTemp$Date, "%m/%d/%Y")

# merge ChicagolandSPEISPINDVI and ChicagolandTemp by date columns
ChicagolandTempSPEISPINDVIVPDNA <- merge(ChicagolandSPEISPINDVIVPDNA, ChicagolandTemp, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe
ChicagolandTempSPEISPINDVIVPDNA <- na.omit(ChicagolandTempSPEISPINDVIVPDNA)

ChicagolandTempSPEISPINDVIVPDNA$month <- lubridate::month(ChicagolandTempSPEISPINDVIVPDNA$date)

library(nlme)
library(mgcv)

gam.fitted.basic <- gam(ndvi.obs ~ s(year), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.basic)
summary(gam.fitted.basic)

gam.fitted.SPEI.X30d <- gam(ndvi.obs ~ s(year) + s(SPEI.X30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI.X30d)
summary(gam.fitted.SPEI.X30d)

gam.fitted.SPEI30 <- gam(ndvi.obs ~ s(year) + s(SPEI.X30d) + s(VPD) + s(TMIN30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI30)
summary(gam.fitted.SPEI30)

gam.fitted.SPEI60 <- gam(ndvi.obs ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI60)
summary(gam.fitted.SPEI60)

gam.fitted.SPEI90 <- gam(ndvi.obs ~ s(year) + s(SPEI.X90d) + s(VPD) + s(TMIN30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI90)
summary(gam.fitted.SPEI90)

gam.fitted.SPEI14 <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d) + s(VPD) + s(TMIN30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI14)
summary(gam.fitted.SPEI14)

gam.fitted.SPEI30.type <- gam(ndvi.obs ~ s(year) + s(SPEI.X30d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI30)
summary(gam.fitted.SPEI30.type)

gam.fitted.SPEI60.type <- gam(ndvi.obs ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI60)
summary(gam.fitted.SPEI60.type)

gam.fitted.SPEI90.type <- gam(ndvi.obs ~ s(year) + s(SPEI.X90d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI90)
summary(gam.fitted.SPEI90.type)

gam.fitted.SPEI14.type <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI14)
summary(gam.fitted.SPEI14.type)
#gam.fitted.SPEI14 returns the largest adjusted R squared value (0.668) and the smallest restricted max likelihood (-8546.8)

#rearranging the land cover types to make forest the intercept
ChicagolandTempSPEISPINDVIVPDNA$type <- relevel(ChicagolandTempSPEISPINDVIVPDNA$type, "forest")

gam.fitted.SPEI14 <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14)

AIC(gam.fitted.SPEI14, gam.fitted.SPEI30, gam.fitted.SPEI.X30d,gam.fitted.SPEI60,gam.fitted.SPEI60.type, gam.fitted.SPEI14.type, gam.fitted.SPEI30.type, gam.fitted.SPEI90, gam.fitted.SPEI90.type)

gam.fitted.SPEI14.type.unsmoothyearspei <- gam(ndvi.obs ~ year + SPEI.X14d + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.unsmoothyearspei)
gam.fitted.SPEI14.type.unsmoothyear <- gam(ndvi.obs ~ year + s(SPEI.X14d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.unsmoothyear)
gam.fitted.SPEI14.type.unsmoothspei <- gam(ndvi.obs ~ s(year) + SPEI.X14d + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.unsmoothspei)
gam.fitted.SPEI14.type.unsmoothvpd <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d) + VPD + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.unsmoothvpd)
gam.fitted.SPEI14.type.unsmoothtemp <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d) + s(VPD) + TMIN30d + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.unsmoothtemp)
AIC(gam.fitted.SPEI14, gam.fitted.SPEI14.type, gam.fitted.SPEI14.type.interact.vpd, gam.fitted.SPEI14.type.interact.temp, gam.fitted.SPEI14.type.interact.temp.vpd)

gam.fitted.SPEI14.type.interact.vpd <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d, VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.interact.vpd)

gam.fitted.SPEI14.type.interact.temp <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d, TMIN30d) + s(VPD) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.interact.temp)

gam.fitted.SPEI14.type.interact.temp.vpd <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d) + s(TMIN30d, VPD) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.interact.temp.vpd)

gam.fitted.SPEI14.type.interact.year <- gam(ndvi.obs ~ s(year, SPEI.X14d) + s(TMIN30d) + s(VPD) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.interact.year)

gam.fitted.SPEI14.type.interact.year.temp <- gam(ndvi.obs ~ s(year, TMIN30d) + s(SPEI.X14d) + s(VPD) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.interact.year.temp)

gam.fitted.SPEI14.type.interact.year.vpd <- gam(ndvi.obs ~ s(year, VPD) + s(SPEI.X14d) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.interact.year.vpd)

gam.fitted.SPEI14.type.interact.year.temp.vpd <- gam(ndvi.obs ~ s(year, TMIN30d, VPD) + s(SPEI.X14d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.SPEI14.type.interact.year.temp.vpd)

AIC(gam.fitted.SPEI14, gam.fitted.SPEI14.type, gam.fitted.SPEI14.type.interact.vpd, gam.fitted.SPEI14.type.interact.temp, gam.fitted.SPEI14.type.interact.temp.vpd, gam.fitted.SPEI14.type.interact.year, gam.fitted.SPEI14.type.interact.year.temp, gam.fitted.SPEI14.type.interact.year.vpd, gam.fitted.SPEI14.type.interact.year.temp.vpd)

#gam.fitted.SPEI14.type.interact.year.temp.vpd has the lowest AIC but a much larger degree of freedom. The lowest AIC without a huge jump in df is gam.fitted.SPEI14.type.interact.year.temp

gam.fitted.SPEI14.TMIN30.doy.VPD <- gam(ndvi.obs ~ s(doy, TMIN30d) + s(SPEI.X14d) + s(VPD) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI14.TMIN30.doy.VPD)
summary(gam.fitted.SPEI14.TMIN30.doy.VPD)

AIC(gam.fitted.SPEI14, gam.fitted.SPEI14.type, gam.fitted.SPEI14.type.interact.vpd, gam.fitted.SPEI14.type.interact.temp, gam.fitted.SPEI14.type.interact.temp.vpd, gam.fitted.SPEI14.type.interact.year, gam.fitted.SPEI14.type.interact.year.temp, gam.fitted.SPEI14.type.interact.year.vpd, gam.fitted.SPEI14.type.interact.year.temp.vpd, gam.fitted.SPEI14.TMIN30.doy)

gam.fitted.SPEI14.TMIN30.doy.interact <- gam(ndvi.obs ~ s(doy, TMIN30d, SPEI.X14d) + s(VPD) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI14.TMIN30.doy.interact)
summary(gam.fitted.SPEI14.TMIN30.doy.interact)

AIC(gam.fitted.SPEI14, gam.fitted.SPEI14.type, gam.fitted.SPEI14.type.interact.vpd, gam.fitted.SPEI14.type.interact.temp, gam.fitted.SPEI14.type.interact.temp.vpd, gam.fitted.SPEI14.type.interact.year, gam.fitted.SPEI14.type.interact.year.temp, gam.fitted.SPEI14.type.interact.year.vpd, gam.fitted.SPEI14.type.interact.year.temp.vpd, gam.fitted.SPEI14.TMIN30.doy, gam.fitted.SPEI14.TMIN30.doy.interact)

gam.fitted.SPEI14.TMIN30.doy.VPD.interact <- gam(ndvi.obs ~ s(doy, TMIN30d) + s(SPEI.X14d, VPD) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI14.TMIN30.doy.VPD.interact)
summary(gam.fitted.SPEI14.TMIN30.doy.VPD.interact)

gam.fitted.SPEI14.TMIN30.doy <- gam(ndvi.obs ~ s(doy, TMIN30d) + s(SPEI.X14d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.SPEI14.TMIN30.doy)
summary(gam.fitted.SPEI14.TMIN30.doy)

AIC(gam.fitted.SPEI14, gam.fitted.SPEI14.type, gam.fitted.SPEI14.type.interact.vpd, gam.fitted.SPEI14.type.interact.temp, gam.fitted.SPEI14.type.interact.temp.vpd, gam.fitted.SPEI14.type.interact.year, gam.fitted.SPEI14.type.interact.year.temp, gam.fitted.SPEI14.type.interact.year.vpd, gam.fitted.SPEI14.type.interact.year.temp.vpd, gam.fitted.SPEI14.TMIN30.doy, gam.fitted.SPEI14.TMIN30.doy.interact, gam.fitted.SPEI14.TMIN30.doy.VPD)

gam.fitted.interact.SPEI14.TMIN30.doy <- gam(ndvi.obs ~ s(doy, SPEI.X14d) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.interact.SPEI14.TMIN30.doy)
summary(gam.fitted.interact.SPEI14.TMIN30.doy)

AIC(gam.fitted.SPEI14, gam.fitted.SPEI14.type, gam.fitted.SPEI14.type.interact.vpd, gam.fitted.SPEI14.type.interact.temp, gam.fitted.SPEI14.type.interact.temp.vpd, gam.fitted.SPEI14.type.interact.year, gam.fitted.SPEI14.type.interact.year.temp, gam.fitted.SPEI14.type.interact.year.vpd, gam.fitted.SPEI14.type.interact.year.temp.vpd, gam.fitted.SPEI14.TMIN30.doy, gam.fitted.SPEI14.TMIN30.doy.interact, gam.fitted.SPEI14.TMIN30.doy.VPD, gam.fitted.interact.SPEI14.TMIN30.doy)

gam.fitted.double.interact.SPEI14.TMIN30.doy <- gam(ndvi.obs ~ s(doy, SPEI.X14d) + s(doy, TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.double.interact.SPEI14.TMIN30.doy)
summary(gam.fitted.double.interact.SPEI14.TMIN30.doy)

AIC(gam.fitted.SPEI14, gam.fitted.SPEI14.type, gam.fitted.SPEI14.type.interact.vpd, gam.fitted.SPEI14.type.interact.temp, gam.fitted.SPEI14.type.interact.temp.vpd, gam.fitted.SPEI14.type.interact.year, gam.fitted.SPEI14.type.interact.year.temp, gam.fitted.SPEI14.type.interact.year.vpd, gam.fitted.SPEI14.type.interact.year.temp.vpd, gam.fitted.SPEI14.TMIN30.doy, gam.fitted.SPEI14.TMIN30.doy.interact, gam.fitted.SPEI14.TMIN30.doy.VPD, gam.fitted.interact.SPEI14.TMIN30.doy, gam.fitted.double.interact.SPEI14.TMIN30.doy)


gam.fitted.TMIN30.doy.interact.SPEI.X14d <- gam(ndvi.obs ~ s(TMIN30d, doy) + s(SPEI.X14d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d)

plot(gam.fitted.interact.SPEI14.TMIN30.doy, select = 2, shade = TRUE, shade.col = "hotpink")

ChicagolandTempSPEISPINDVIVPDNA$predicted <- predict(gam.fitted.TMIN30.doy.interact.SPEI.X14d)
ChicagolandTempSPEISPINDVIVPDNA$resids <- resid(gam.fitted.TMIN30.doy.interact.SPEI.X14d)
plot(ndvi.obs ~ predicted, data=ChicagolandTempSPEISPINDVIVPDNA); abline(a=0, b=1, col="red")
plot(resids ~ predicted, data=ChicagolandTempSPEISPINDVIVPDNA); abline(a=0, b=0, col="red")
plot(resids ~ SPEI.X14d, data=ChicagolandTempSPEISPINDVIVPDNA); abline(a=0, b=0, col="red")

ggplot(data=ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$year>2020,]) +
  facet_wrap(~type) +
  geom_line(aes(x=date, y=ndvi.obs, color="observed"), size=0.5) +
  geom_line(aes(x=date, y=predicted, color="model"), size=0.5)


# Allowing the temp relationships to vary by landcover class
gam.fitted.TMIN30.doy.interact.SPEI.X14d2 <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d2)
AIC(gam.fitted.TMIN30.doy.interact.SPEI.X14d, gam.fitted.TMIN30.doy.interact.SPEI.X14d2)

# Now allow SPEDI to vary by landcover class
gam.fitted.TMIN30.doy.interact.SPEI.X14d3 <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d3)
AIC(gam.fitted.TMIN30.doy.interact.SPEI.X14d2, gam.fitted.TMIN30.doy.interact.SPEI.X14d3)

gam.fitted.TMIN30.doy.interact.SPEI.X14d4 <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, doy, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d4)
AIC(gam.fitted.TMIN30.doy.interact.SPEI.X14d2, gam.fitted.TMIN30.doy.interact.SPEI.X14d3, gam.fitted.TMIN30.doy.interact.SPEI.X14d4)

# Comparing the AIC of allowing doy-temp to vary by Landcover type
AIC(gam.fitted.TMIN30.doy.interact.SPEI.X14d, gam.fitted.TMIN30.doy.interact.SPEI.X14d2,gam.fitted.TMIN30.doy.interact.SPEI.X14d3)

# Creating lagged NDVI values that will serve as predictor variables in GAM models
library(dplyr)
ChicagolandTempSPEISPINDVIVPDNA <- ChicagolandTempSPEISPINDVIVPDNA %>% mutate(NDVI.obs.t.minus.1d = lag(ndvi.obs, n=1, default = NA))

gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.1d, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d)

ChicagolandTempSPEISPINDVIVPDNA <- ChicagolandTempSPEISPINDVIVPDNA %>% mutate(NDVI.obs.t.minus.2d = lag(ndvi.obs, n=2, default = NA))
gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.2d, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d)

ChicagolandTempSPEISPINDVIVPDNA <- ChicagolandTempSPEISPINDVIVPDNA %>% mutate(NDVI.obs.t.minus.2d.ave = (lag(ndvi.obs, n=1, default = NA)+lag(ndvi.obs, n=2, default = NA))/2)
gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.ave <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.2d.ave, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.ave)

AIC(gam.fitted.TMIN30.doy.interact.SPEI.X14d, gam.fitted.TMIN30.doy.interact.SPEI.X14d2,gam.fitted.TMIN30.doy.interact.SPEI.X14d3, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.ave)

ChicagolandTempSPEISPINDVIVPDNA <- ChicagolandTempSPEISPINDVIVPDNA %>% mutate(NDVI.obs.t.minus.3d.ave = (lag(ndvi.obs, n=1, default = NA)+lag(ndvi.obs, n=2, default = NA) + lag(ndvi.obs, n=3, default = NA))/3)
gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.ave <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.3d.ave, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.ave)

AIC(gam.fitted.TMIN30.doy.interact.SPEI.X14d, gam.fitted.TMIN30.doy.interact.SPEI.X14d2,gam.fitted.TMIN30.doy.interact.SPEI.X14d3, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.ave, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.ave)

ChicagolandTempSPEISPINDVIVPDNA <- ChicagolandTempSPEISPINDVIVPDNA %>% mutate(NDVI.obs.t.minus.5d.ave = (lag(ndvi.obs, n=1, default = NA)+lag(ndvi.obs, n=2, default = NA) + lag(ndvi.obs, n=3, default = NA) + lag(ndvi.obs, n=4, default = NA) + lag(ndvi.obs, n=5, default = NA))/5)
gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.5d.ave <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.5d.ave, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.5d.ave)

AIC(gam.fitted.TMIN30.doy.interact.SPEI.X14d, gam.fitted.TMIN30.doy.interact.SPEI.X14d2,gam.fitted.TMIN30.doy.interact.SPEI.X14d3,gam.fitted.TMIN30.doy.interact.SPEI.X14d4, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.ave, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.ave, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.5d.ave)

#Creating dataframe with missing values removed from 3day ave for predict/resid functions to run properly (3 NA values removed at beginning of column)
NDVI.obs.t.minus.3d.aveNA <- ChicagolandTempSPEISPINDVIVPDNA[!is.na(ChicagolandTempSPEISPINDVIVPDNA$NDVI.obs.t.minus.3d.ave),]
gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.aveNA <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.3d.ave, by=type) + type, data = NDVI.obs.t.minus.3d.aveNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.aveNA)


NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave <- predict(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.aveNA)
NDVI.obs.t.minus.3d.aveNA$resids.3d.ave <- resid(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.aveNA)
plot(ndvi.obs ~ predicted.3d.ave, data=NDVI.obs.t.minus.3d.aveNA); abline(a=0, b=1, col="red")
plot(resids.3d.ave ~ predicted.3d.ave, data=NDVI.obs.t.minus.3d.aveNA); abline(a=0, b=0, col="red")
plot(resids.3d.ave ~ SPEI.X14d, data=NDVI.obs.t.minus.3d.aveNA); abline(a=0, b=0, col="red")
plot(ndvi.anomaly.3day.ave.model ~ predicted.3d.ave, data=NDVI.obs.t.minus.3d.aveNA); abline(a=0, b=0, col="red")
plot(ndvi.obs ~ predicted.3d.ave,xaxt="n", data=NDVI.obs.t.minus.3d.aveNA); abline(lm(ndvi.obs ~ predicted.3d.ave, data=NDVI.obs.t.minus.3d.aveNA), col="red"); abline(v=0); abline(v=0.15); abline(v= 0.3); abline(v= 0.45); abline(v=0.6); abline(v= 0.75); axis(1, at = seq(0, 1, by = 0.15), las=1); axis(1, (0.075),labels = "0.0752", tick="FALSE", line=-27); axis(1, (0.225),labels = "0.0470", tick="FALSE", line=-27); axis(1, (0.375),labels = "0.0555", tick="FALSE", line=-27); axis(1, (at=0.525),labels = "0.0666", tick="FALSE", line=-27); axis(1, (0.675),labels = "0.0522", tick="FALSE", line=-27); axis(1, (0.825),labels = "0.0415", tick="FALSE", line=-27); axis(1, (-0.05),labels = "RMSE", tick="FALSE", line=-27); axis(1, (0.42),labels = "Model Average RMSE = 0.0562", tick="FALSE", line=-28.5); axis(1, (0.42),labels = "Predicted NDVI Modeled with Prior 3 day Average", tick="FALSE", line=1); axis(2, (0.42),labels = "Observed NDVI", tick="FALSE", line=1)
#previous plot by land cover type
plot(ndvi.obs ~ predicted.3d.ave,xaxt="n", data=NDVI.obs.t.minus.3d.aveNA); abline(lm(ndvi.obs ~ predicted.3d.ave, data=NDVI.obs.t.minus.3d.aveNA), col="red"); abline(v=0); abline(v=0.15); abline(v= 0.3); abline(v= 0.45); abline(v=0.6); abline(v= 0.75); axis(1, at = seq(0, 1, by = 0.15), las=1); axis(1, (0.075),labels = "0.0752", tick="FALSE", line=-27); axis(1, (0.225),labels = "0.0470", tick="FALSE", line=-27); axis(1, (0.375),labels = "0.0555", tick="FALSE", line=-27); axis(1, (at=0.525),labels = "0.0666", tick="FALSE", line=-27); axis(1, (0.675),labels = "0.0522", tick="FALSE", line=-27); axis(1, (0.825),labels = "0.0415", tick="FALSE", line=-27); axis(1, (-0.05),labels = "RMSE", tick="FALSE", line=-27); axis(1, (0.42),labels = "Model Average RMSE = 0.0562", tick="FALSE", line=-28.5); axis(1, (0.42),labels = "Predicted NDVI Modeled with Prior 3 day Average", tick="FALSE", line=1); axis(2, (0.42),labels = "Observed NDVI", tick="FALSE", line=1)
plot(ndvi.anomaly ~ resids.3d.ave, data=NDVI.obs.t.minus.3d.aveNA); abline(a=0, b=1, col="red")


rmse.3d.ave <- sqrt(mean((NDVI.obs.t.minus.3d.aveNA$ndvi.obs-NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave)^2))
rmse.3d.ave.obs.0to.15 <- sqrt(mean((NDVI.obs.t.minus.3d.aveNA$ndvi.obs[NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.15]-NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave[NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.15])^2))
rmse.3d.ave.obs.15to.3 <- sqrt(mean((NDVI.obs.t.minus.3d.aveNA$ndvi.obs[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.15&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.3]-NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.15&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.3])^2))
rmse.3d.ave.obs.3to.45 <- sqrt(mean((NDVI.obs.t.minus.3d.aveNA$ndvi.obs[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.3&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.45]-NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.3&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.45])^2))
rmse.3d.ave.obs.45to.6 <- sqrt(mean((NDVI.obs.t.minus.3d.aveNA$ndvi.obs[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.45&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.6]-NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.45&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.6])^2))
rmse.3d.ave.obs.6to.75 <- sqrt(mean((NDVI.obs.t.minus.3d.aveNA$ndvi.obs[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.6&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.75]-NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.6&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.75])^2))
rmse.3d.ave.obs.75to.9 <- sqrt(mean((NDVI.obs.t.minus.3d.aveNA$ndvi.obs[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.75&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.9]-NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave[NDVI.obs.t.minus.3d.aveNA$ndvi.obs>0.75&NDVI.obs.t.minus.3d.aveNA$ndvi.obs<=0.9])^2))

#creating anomaly from 3 day average GAM model
NDVI.obs.t.minus.3d.aveNA$ndvi.anomaly.3day.ave.model <- NDVI.obs.t.minus.3d.aveNA$ndvi.obs - NDVI.obs.t.minus.3d.aveNA$predicted.3d.ave

ggplot(data=NDVI.obs.t.minus.3d.aveNA[NDVI.obs.t.minus.3d.aveNA$year>2010,]) +
  facet_wrap(~type) +
  geom_line(aes(x=date, y=ndvi.obs, color="Observed NDVI"), size=0.5) +
  geom_line(aes(x=date, y=predicted.3d.ave, color="NDVI modeled by 3 day ave NDVI"), size=0.5) 

NDVI.obs.t.minus.5d.aveNA <- ChicagolandTempSPEISPINDVIVPDNA[!is.na(ChicagolandTempSPEISPINDVIVPDNA$NDVI.obs.t.minus.5d.ave),]
NDVI.obs.t.minus.5d.aveNA$predicted.5d.ave <- predict(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.5d.ave)
rmse.5d.ave <- sqrt(mean((NDVI.obs.t.minus.5d.aveNA$ndvi.obs-NDVI.obs.t.minus.5d.aveNA$predicted.5d.ave)^2))

gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.aveNA <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.2d.ave, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.aveNA)

NDVI.obs.t.minus.2d.aveNA <- ChicagolandTempSPEISPINDVIVPDNA[!is.na(ChicagolandTempSPEISPINDVIVPDNA$NDVI.obs.t.minus.2d.ave),]
gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.aveNA <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.2d.ave, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.aveNA)


NDVI.obs.t.minus.2d.aveNA$predicted.2d.ave <- predict(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.aveNA)
rmse.2d.ave <- sqrt(mean((NDVI.obs.t.minus.2d.aveNA$ndvi.obs-NDVI.obs.t.minus.2d.aveNA$predicted.2d.ave)^2))

NDVI.obs.t.minus.2d.NA <- ChicagolandTempSPEISPINDVIVPDNA[!is.na(ChicagolandTempSPEISPINDVIVPDNA$NDVI.obs.t.minus.2d),]
gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.NA <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.2d, by=type) + type, data = NDVI.obs.t.minus.2d.NA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.NA)


NDVI.obs.t.minus.2d.NA$predicted.2d <- predict(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.NA)
rmse.2d <- sqrt(mean((NDVI.obs.t.minus.2d.NA$ndvi.obs-NDVI.obs.t.minus.2d.NA$predicted.2d)^2))

gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d.NA <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, by=type) + s(NDVI.obs.t.minus.1d, by=type) + type, data = NDVI.obs.t.minus.2d.NA, method = 'REML')
summary(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d.NA)

NDVI.obs.t.minus.2d.NA$predicted.1d <- predict(gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d.NA)
rmse.1d <- sqrt(mean((NDVI.obs.t.minus.2d.NA$ndvi.obs-NDVI.obs.t.minus.2d.NA$predicted.1d)^2))

ggplot(data=NDVI.obs.t.minus.3d.aveNA[NDVI.obs.t.minus.3d.aveNA$year>2010,])+
  facet_wrap(~type)+
geom_line(aes(x=date, y=ndvi.obs, color="Observed NDVI")) +
  geom_line(aes(x=date, y=predicted.3d.ave, color="NDVI modeled by 3 day ave NDVI")) +
  geom_point(aes(x=date, y=ndvi.anomaly.3day.ave.model, color="NDVI anomaly modeled by 3 day ave NDVI")) 

  
# 6 figures arranged in 6 rows
attach(NDVI.obs.t.minus.3d.aveNA)
par(mfrow=c(1,6))
plot(ndvi.anomaly.3day.ave.model~predicted.3d.ave, data=NDVI.obs.t.minus.3d.aveNA[NDVI.obs.t.minus.3d.aveNA$year>2010,]);abline(a=0, b=0, col="red")

plot(ndvi.anomaly.3day.ave.model~predicted.3d.ave, main="Scatterplot of anomaly vs. predicted values");abline(a=0, b=0, col="red")
plot(ndvi.anomaly.3day.ave.model~predicted.3d.ave, main="Scatterplot of anomaly vs. predicted values");abline(a=0, b=0, col="red")
plot(ndvi.anomaly.3day.ave.model~predicted.3d.ave, main="Scatterplot of anomaly vs. predicted values");abline(a=0, b=0, col="red")
plot(ndvi.anomaly.3day.ave.model~predicted.3d.ave, main="Scatterplot of anomaly vs. predicted values");abline(a=0, b=0, col="red")
plot(ndvi.anomaly.3day.ave.model~predicted.3d.ave, main="Scatterplot of anomaly vs. predicted values");abline(a=0, b=0, col="red")
plot(ndvi.anomaly.3day.ave.model~predicted.3d.ave, main="Scatterplot of anomaly vs. predicted values");abline(a=0, b=0, col="red")


#################################
#Creating Vegetation Condition Index (VCI) to determine the NDVI in comparison to doy normal
#creating minimum NDVI for each doy by lc type
ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.min <- ave(ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs, ChicagolandTempSPEISPINDVIVPDNA$doy, ChicagolandTempSPEISPINDVIVPDNA$type, FUN = min)

#creating maximum NDVI for each doy by lc type
ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.max <- ave(ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs, ChicagolandTempSPEISPINDVIVPDNA$doy, ChicagolandTempSPEISPINDVIVPDNA$type, FUN = max)

#creating VCI
ChicagolandTempSPEISPINDVIVPDNA$VCI <- ((ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs - ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.min)/(ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.max - ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.min))

#trying to plot 7x11 grid of lowest VCI (worst vegetation condition) to compare years by lc type 
Lowest_VCI_by_Year_LC_Type <- ggplot(data=ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$year>2010 & ChicagolandTempSPEISPINDVIVPDNA$VCI<0.1,]) +
  facet_wrap(~year+type, ncol=7, nrow=11) +
  geom_line(aes(x=doy, y=ndvi.obs, color="Observed NDVI"), size=0.5) +
  geom_point(aes(x=doy, y=VCI, color="VCI"), size=0.5)

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/Low.VCI.by.LC.types.png", unit="in", height = 30, width = 20, res = 300)
plot(Lowest_VCI_by_Year_LC_Type)
dev.off()

Lowest_VCI_by_LC_Type_Year <- ggplot(data=ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$year>2010 & ChicagolandTempSPEISPINDVIVPDNA$VCI<0.1,]) +
  facet_wrap(~type+year, ncol=11, nrow=7) +
  geom_line(aes(x=doy, y=ndvi.obs, color="Observed NDVI"), size=0.5) +
  geom_point(aes(x=doy, y=VCI, color="VCI"), size=0.5)

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/Low.VCI.by.LC.types.png", unit="in", height = 20, width = 30, res = 300)
plot(Lowest_VCI_by_LC_Type_Year)
dev.off()

Less_than_0.05VCI_by_LC_Type_Year <- ggplot(data=ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$year>2010 & ChicagolandTempSPEISPINDVIVPDNA$VCI<0.05,]) +
  facet_wrap(~type+year, ncol=11, nrow=7) +
  geom_line(aes(x=doy, y=ndvi.obs, color="Observed NDVI"), size=0.5) +
  geom_point(aes(x=doy, y=VCI, color="VCI"), size=0.5)

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/Less.than.0.05.VCI.by.LC.types.png", unit="in", height = 20, width = 30, res = 300)
plot(Less_than_0.05VCI_by_LC_Type_Year)
dev.off()

Less_than_0.01VCI_by_LC_Type_Year <- ggplot(data=ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$year>2010 & ChicagolandTempSPEISPINDVIVPDNA$VCI<0.01,]) +
  facet_wrap(~type+year, ncol=11, nrow=7) +
  geom_line(aes(x=doy, y=ndvi.obs, color="Observed NDVI"), size=0.5) +
  geom_point(aes(x=doy, y=VCI, color="VCI"), size=0.5)

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/Less.than.0.01.VCI.by.LC.types.png", unit="in", height = 20, width = 30, res = 300)
plot(Less_than_0.01VCI_by_LC_Type_Year)
dev.off()
###############################
#Incorporating VCI into gam model
gam.fitted.VCI <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, doy, by=type) + s(VCI, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI)

gam.fitted.VCI.doy <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, doy, by=type) + s(VCI, doy, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.doy)
AIC(gam.fitted.TMIN30.doy.interact.SPEI.X14d, gam.fitted.TMIN30.doy.interact.SPEI.X14d2,gam.fitted.TMIN30.doy.interact.SPEI.X14d3,gam.fitted.TMIN30.doy.interact.SPEI.X14d4, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.1d, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.2d.ave, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.3d.ave, gam.fitted.TMIN30.doy.interact.SPEI.X14d.NDVI.lag.t.minus.5d.ave, gam.fitted.VCI, gam.fitted.VCI.doy)

#Hopefully removing missing values for predict functionality
gam.fitted.VCI <- gam(ndvi.obs ~ s(TMIN30d, doy, by=type) + s(SPEI.X14d, doy, by=type) + s(VCI, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML', na.rm=T)
summary(gam.fitted.VCI)

#replacement has 14023 rows, data has 14033
#ChicagolandTempSPEISPINDVIVPDNA$predicted.VCI <- predict(gam.fitted.VCI)

#replacement has 14023 rows, data has 14033...will try refitting the model with na.action=na.exclude
#ChicagolandTempSPEISPINDVIVPDNA$predicted.VCI.doy <- predict(gam.fitted.VCI.doy)
#rmse.VCI.doy <- sqrt(mean((ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs - ChicagolandTempSPEISPINDVIVPDNA$ )^2))

#Removing NA values from VCI
NDVI.obs.VCINA <- ChicagolandTempSPEISPINDVIVPDNA[!is.na(ChicagolandTempSPEISPINDVIVPDNA$VCI),]

#Check rmse for VCI gam model
NDVI.obs.VCINA$predicted.VCI <- predict(gam.fitted.VCI)
NDVI.obs.VCINA$ndvi.anomaly.VCI <- resid(gam.fitted.VCI)

rmse.VCI <- sqrt(mean((NDVI.obs.VCINA$ndvi.obs- NDVI.obs.VCINA$predicted.VCI )^2))

#Check rmse for VCI.doy gam model in general and at different NDVI.obs windows
NDVI.obs.VCINA$predicted.VCI.doy <- predict(gam.fitted.VCI.doy)
rmse.VCI.doy <- sqrt(mean((NDVI.obs.VCINA$ndvi.obs- NDVI.obs.VCINA$predicted.VCI.doy )^2))
rmse.VCI.0to.15 <- sqrt(mean((NDVI.obs.VCINA$ndvi.obs[NDVI.obs.VCINA$ndvi.obs<=0.15]-NDVI.obs.VCINA$predicted.VCI[NDVI.obs.VCINA$ndvi.obs<=0.15])^2))
rmse.VCI.15to.3 <- sqrt(mean((NDVI.obs.VCINA$ndvi.obs[NDVI.obs.VCINA$ndvi.obs>0.15&NDVI.obs.VCINA$ndvi.obs<=0.3]-NDVI.obs.VCINA$predicted.VCI[NDVI.obs.VCINA$ndvi.obs>0.15&NDVI.obs.VCINA$ndvi.obs<=0.3])^2))
rmse.VCI.3to.45 <- sqrt(mean((NDVI.obs.VCINA$ndvi.obs[NDVI.obs.VCINA$ndvi.obs>0.3&NDVI.obs.VCINA$ndvi.obs<=0.45]-NDVI.obs.VCINA$predicted.VCI[NDVI.obs.VCINA$ndvi.obs>0.3&NDVI.obs.VCINA$ndvi.obs<=0.45])^2))
rmse.VCI.45to.6 <- sqrt(mean((NDVI.obs.VCINA$ndvi.obs[NDVI.obs.VCINA$ndvi.obs>0.45&NDVI.obs.VCINA$ndvi.obs<=0.6]-NDVI.obs.VCINA$predicted.VCI[NDVI.obs.VCINA$ndvi.obs>0.45&NDVI.obs.VCINA$ndvi.obs<=0.6])^2))
rmse.VCI.6to.75 <- sqrt(mean((NDVI.obs.VCINA$ndvi.obs[NDVI.obs.VCINA$ndvi.obs>0.6&NDVI.obs.VCINA$ndvi.obs<=0.75]-NDVI.obs.VCINA$predicted.VCI[NDVI.obs.VCINA$ndvi.obs>0.6&NDVI.obs.VCINA$ndvi.obs<=0.75])^2))
rmse.VCI.75to.9 <- sqrt(mean((NDVI.obs.VCINA$ndvi.obs[NDVI.obs.VCINA$ndvi.obs>0.75&NDVI.obs.VCINA$ndvi.obs<=0.9]-NDVI.obs.VCINA$predicted.VCI[NDVI.obs.VCINA$ndvi.obs>0.75&NDVI.obs.VCINA$ndvi.obs<=0.9])^2))

plot(ndvi.obs ~ predicted.VCI,xaxt="n", data=NDVI.obs.VCINA); abline(lm(ndvi.obs ~ predicted.VCI, data=NDVI.obs.VCINA), col="red"); abline(v=0); abline(v=0.15); abline(v= 0.3); abline(v= 0.45); abline(v=0.6); abline(v= 0.75); axis(1, at = seq(0, 1, by = 0.15), las=1); axis(1, (0.075),labels = "0.0621", tick="FALSE", line=-27); axis(1, (0.225),labels = "0.0439", tick="FALSE", line=-27); axis(1, (0.375),labels = "0.0481", tick="FALSE", line=-27); axis(1, (at=0.525),labels = "0.0393", tick="FALSE", line=-27); axis(1, (0.675),labels = "0.0295", tick="FALSE", line=-27); axis(1, (0.825),labels = "0.0295", tick="FALSE", line=-27); axis(1, (-0.05),labels = "RMSE", tick="FALSE", line=-27); axis(1, (0.42),labels = "Model Average RMSE = 0.0433", tick="FALSE", line=-28.5); axis(1, (0.42),labels = "Predicted NDVI Modeled Vegetation Condition Index", tick="FALSE", line=1); axis(2, (0.42),labels = "Observed NDVI", tick="FALSE", line=1)
plot(ndvi.anomaly.VCI ~ predicted.VCI, data=NDVI.obs.VCINA); abline(a=0, b=0, col="red")


ggplot(data=NDVI.obs.VCINA[NDVI.obs.VCINA$year>2010,])+
  facet_wrap(~type)+
  geom_line(aes(x=date, y=ndvi.obs, color="Observed NDVI")) +
  geom_line(aes(x=date, y=predicted.VCI, color="NDVI modeled by VCI"))
 
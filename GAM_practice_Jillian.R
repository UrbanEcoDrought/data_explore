library(ggplot2)
library(lubridate)
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
#Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

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
gam.fitted.SPEI14.type.unsmoothyear <- gam(ndvi.obs ~ year + s(SPEI.X14d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
gam.fitted.SPEI14.type.unsmoothspei <- gam(ndvi.obs ~ s(year) + SPEI.X14d + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
gam.fitted.SPEI14.type.unsmoothvpd <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d) + VPD + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
gam.fitted.SPEI14.type.unsmoothtemp <- gam(ndvi.obs ~ s(year) + s(SPEI.X14d) + s(VPD) + TMIN30d + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')

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

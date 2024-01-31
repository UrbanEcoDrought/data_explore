Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
#Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

library(mgcv)

#subsetting data frame by land cover type and running GAM at different VCI ranges

ChicagolandData <- readRDS(file.path(google.drive, "data/data_sets/ChicagolandData"))
ChicagolandGrowSeason <- subset(ChicagolandData[ChicagolandData$doy>=91 & ChicagolandData$doy<=304,])

gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium)

gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium.doy <-gam(VCI ~ s(year) + s(X14d.SPI, doy) + s(X30d.SPI, doy) + s(X60d.SPI, doy) + s(X90d.SPI, doy) + s(SPEI.X14d, doy) + s(SPEI.X30d, doy) + s(SPEI.X60d, doy) + s(SPEI.X90d, doy) + s(TMIN14d, doy) + s(TMIN30d, doy)+ s(TMIN60d, doy)+ s(TMIN90d, doy) + s(TMAX14d, doy) + s(TMAX30d, doy) + s(TMAX60d, doy)+ s(TMAX90d, doy) + s(VPD, doy), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium.doy)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium.doy <-gam(VCI ~ s(year) + s(X30d.SPI, doy) + s(X60d.SPI, doy) + s(X90d.SPI, doy) + s(SPEI.X90d, doy) + s(TMIN30d, doy) + s(TMAX30d, doy) + s(TMAX90d, doy), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium.doy)

gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy <-gam(VCI ~ s(year) + s(X90d.SPI, doy) + s(TMIN30d, doy) + s(TMAX90d, doy), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium <-gam(VCI ~ s(year) + s(X30d.SPI) + s(X60d.SPI) + s(SPEI.X90d) + s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium)

gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium <-gam(VCI ~ s(year) + s(X90d.SPI) + s(TMIN30d) + s(TMAX90d), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium)

gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact1 <-gam(VCI ~ s(year) + s(X90d.SPI, doy) + s(TMIN30d, TMAX90d) + s(TMAX90d, X90d.SPI), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact1)

gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact2 <-gam(VCI ~ s(year) + s(X90d.SPI, doy) + s(TMIN30d, TMAX90d) + s(TMAX90d, X90d.SPI, doy), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact2)

gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact3 <-gam(VCI ~ s(year) + s(X90d.SPI, doy) + s(TMIN30d, TMAX90d, doy) + s(TMAX90d, X90d.SPI), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact3)

gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact4 <-gam(VCI ~ s(year) + s(X90d.SPI) + s(TMIN30d, TMAX90d) + s(TMAX90d, X90d.SPI), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact4)

AIC(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium, gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium, gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact1, gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact2, gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact3, gam.fitted.VCI.double.penalty.low.VCI.best.variables.urban.medium.doy.interact4)

gam.fitted.VCI.double.penalty.lower.VCI.all.variables.urban.medium <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.35 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lower.VCI.all.variables.urban.medium)

gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium <-gam(VCI ~ s(year) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.35 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium)

gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium.interact <-gam(VCI ~ s(year) + s(X30d.SPI, X60d.SPI) + s(X60d.SPI) + s(SPEI.X90d, VPD) + s(TMIN90d, TMAX90d) + s(TMAX14d, X30d.SPI) + s(TMAX30d) + s(TMAX60d, TMAX90d)+ s(TMAX90d, TMAX30d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.35 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium.interact)

summary(gam(VCI ~ s(year)+s(SPEI.X60d)+s(VPD)+s(TMIN60d), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.35 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML'))

AIC(gam.fitted.VCI.double.penalty.lower.VCI.all.variables.urban.medium,gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium)

gam.fitted.VCI.double.penalty.lowest.VCI.all.variables.urban.medium <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.1 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lowest.VCI.all.variables.urban.medium)

gam.fitted.VCI.double.penalty.lowest.VCI.wo.variables.urban.medium <-gam(VCI ~ s(X30d.SPI) + s(X90d.SPI) + s(SPEI.X90d) + s(TMIN30d), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.1 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lowest.VCI.wo.variables.urban.medium)

AIC(gam.fitted.VCI.double.penalty.lowest.VCI.all.variables.urban.medium,gam.fitted.VCI.double.penalty.lowest.VCI.wo.variables.urban.medium)

gam.fitted.VCI.double.penalty.low.VCI.urban.medium.interact <-gam(VCI ~ s(year) + s(X30d.SPI, TMAX30d) + s(X60d.SPI) + s(SPEI.X90d) + s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.urban.medium.interact)

gam.fitted.VCI.double.penalty.low.VCI.urban.medium.interact.doy <-gam(VCI ~ s(year) + s(X30d.SPI, TMAX30d, doy) + s(X60d.SPI) + s(SPEI.X90d) + s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.urban.medium.interact.doy)

gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.low <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-low',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.low)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.low <-gam(VCI ~ s(year) + s(X30d.SPI) + s(X90d.SPI) + s(TMIN30d) + s(TMIN90d) + s(TMAX30d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-low',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.low)

gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.high <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-high',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.high)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.high <-gam(VCI ~ s(year) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X60d) + s(TMIN14d) + s(TMIN30d) + s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-high',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.high)

gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.open <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-open',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.open)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.open <-gam(VCI ~ s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(TMIN30d) + s(TMIN90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-open',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.open)

gam.fitted.VCI.double.penalty.low.VCI.all.variables.forest <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='forest',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.forest)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.forest <-gam(VCI ~ s(X30d.SPI) + s(SPEI.X30d) + s(TMIN30d), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='forest',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.forest)

gam.fitted.VCI.double.penalty.low.VCI.all.variables.crop <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='crop',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.crop)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.crop <-gam(VCI ~ s(year) + s(X60d.SPI) + s(SPEI.X30d) + s(SPEI.X90d) + s(TMIN30d)+ s(TMIN60d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='crop',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.crop)

gam.fitted.VCI.double.penalty.low.VCI.all.variables.grassland <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='grassland',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.grassland)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.grassland <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN30d)+ s(TMIN60d) + s(TMAX60d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='grassland',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.grassland)

#####################
#Checking autocorrelation of best predictors in urban medium
acf.VCI.urban.medium = subset(ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$type=="urban-medium",], select = c(date, VCI, X90d.SPI, TMIN30d, TMAX90d) )
urban.medium.VCI.ts <- ts(acf.VCI.urban.medium, start=c(2001, 1), end=c(2021, 12), frequency=12)
urban.medium.VCI.autocorrelation <- acf(urban.medium.VCI.ts, lag.max=NULL, type=c("correlation"), plot=TRUE, na.action=na.fail, demean=TRUE)

#Checking autocorrelation of active predictors in urban medium
acf.VCI.urban.medium = subset(ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$type=="urban-medium",], select = c(date, VCI, X30d.SPI, X60d.SPI, SPEI.X90d, TMIN90d, TMAX14d, TMAX30d, TMAX60d, TMAX90d, VPD) )
urban.medium.VCI.ts <- ts(acf.VCI.urban.medium, start=c(2001, 1), end=c(2021, 12), frequency=12)
urban.medium.VCI.autocorrelation <- acf(urban.medium.VCI.ts, lag.max=NULL, type=c("correlation"), plot=TRUE, na.action=na.fail, demean=TRUE)

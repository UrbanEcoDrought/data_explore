Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
#Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

library(nlme)
library(mgcv)

#subsetting data frame by land cover type and running GAM at different VCI ranges

ChicagolandData <- readRDS(file.path(google.drive, "data/data_sets/ChicagolandData"))
ChicagolandGrowSeason <- subset(ChicagolandData[ChicagolandData$doy>=91 & ChicagolandData$doy<=304,])

gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium <-gam(VCI ~ s(year) + s(X30d.SPI) + s(X60d.SPI) + s(SPEI.X90d) + s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium)

gam.fitted.VCI.double.penalty.lower.VCI.all.variables.urban.medium <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.35 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lower.VCI.all.variables.urban.medium)

gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium <-gam(VCI ~ s(year) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.35 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium)

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

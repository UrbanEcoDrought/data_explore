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

#Creating Vegetation Condition Index (VCI) to determine the NDVI in comparison to doy normal
#creating minimum NDVI for each doy by lc type
ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.min <- ave(ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs, ChicagolandTempSPEISPINDVIVPDNA$doy, ChicagolandTempSPEISPINDVIVPDNA$type, FUN = min)

#creating maximum NDVI for each doy by lc type
ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.max <- ave(ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs, ChicagolandTempSPEISPINDVIVPDNA$doy, ChicagolandTempSPEISPINDVIVPDNA$type, FUN = max)

#creating VCI
ChicagolandTempSPEISPINDVIVPDNA$VCI <- ((ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs - ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.min)/(ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.max - ChicagolandTempSPEISPINDVIVPDNA$ndvi.doy.min))
summary(ChicagolandTempSPEISPINDVIVPDNA)

#remove all NA values from dataframe (see if na.action in lme works)
ChicagolandTempSPEISPINDVIVPDNA <- na.omit(ChicagolandTempSPEISPINDVIVPDNA)
summary(ChicagolandTempSPEISPINDVIVPDNA)

ChicagolandTempSPEISPINDVIVPDNA$month <- lubridate::month(ChicagolandTempSPEISPINDVIVPDNA$date)
days.use <- unique(ChicagolandTempSPEISPINDVIVPDNA$doy[ChicagolandTempSPEISPINDVIVPDNA$month >=3 & ChicagolandTempSPEISPINDVIVPDNA$month <=9])
days.use 
# lc.type <- c("crop", "forest", "grassland", "urban-low", "urban-medium", "urban-high", "urban-open")
lc.type <- unique(ChicagolandTempSPEISPINDVIVPDNA$type) # this will help avoid typos

resp.vars <- c("ndvi.obs", "VCI")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI", "VPD", "SPEI.X14d", "SPEI.X30d", "SPEI.X60d", "SPEI.X90d", "TMIN14d", "TMIN30d", "TMIN60d", "TMIN90d", "TMAX14d", "TMAX30d", "TMAX60d", "TMAX90d")

mod.out <- data.frame(TYPE = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 

row.ind = 0 

for(RESP in resp.vars){
  for(PRED in pred.vars){
    for(TYPE in lc.type){
      # TYPE<- lc.type[j]
      for(i in 1:length(days.use)){
        dayNOW <- days.use[i] 
        
        dat.tmp <- ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$doy>=dayNOW-7 & ChicagolandTempSPEISPINDVIVPDNA$doy<=dayNOW+7 & ChicagolandTempSPEISPINDVIVPDNA$type==TYPE,]
        # dat.tmp$TYPE <- dat.tmp[,TYPE] # Because the data is in long format there's no column called crop, forest, etc. like there is with our responses & predictors
        dat.tmp$RESP <- dat.tmp[,RESP]
        dat.tmp$PRED <- dat.tmp[,PRED] 
        summary(dat.tmp) 
        dim(dat.tmp)
        
        # mod.var <- nlme::lme(RESP ~ PRED, subset = (type == TYPE), random=list(year=~1), data=dat.tmp[,], na.action=na.omit) # something like this *may* work, but it was giving me issues, so cleaner to just subset up top (line 59)
        mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
        mod.sum <- summary(mod.var)
        
        row.ind = row.ind+1 
        
        mod.out[row.ind, "TYPE"] <- TYPE
        mod.out[row.ind, "PRED"] <- PRED
        mod.out[row.ind, "RESP"] <- RESP
        mod.out[row.ind, "DOY"] <- dayNOW
        
        mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
        mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
        mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
        mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
        mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
        mod.out[row.ind, "AIC"] <- AIC(mod.var) 
      } # End day loop
    } # end Type loop
  } # End PRED loop
} # End RESP loop
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))
month.july.august <- data.frame(doy = c(182, 213, 244),
                                month = c("Jul", "Aug", "Sep"))

tstat.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs and VCI) and Predictors (all SPI, SPEI, VPD, and Temp) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat.NDVI.obs.VCI.all.SPI.SPEI.VPD.Temp.LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(tstat.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types)
dev.off()

r2_NDVI.obs_VCI.all.SPI_VPD_LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=r.sq.m)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "r2 of Response (NDVI.obs and VCI) and Predictors (all SPI and VPD) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/r2_NDVI.obs_VCI.all.SPI_VPD_LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(r2_NDVI.obs_VCI.all.SPI_VPD_LC.types)
dev.off()

AIC_NDVI.obs_VCI.all.SPI_VPD_LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "AIC of Response (NDVI.obs and VCI) and Predictors (all SPI and VPD) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/AIC_NDVI.obs_VCI.all.SPI_VPD_LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(AIC_NDVI.obs_VCI.all.SPI_VPD_LC.types)
dev.off()

#####################################################

library(nlme)
library(mgcv)

gam.fitted.VCI.basic <- gam(VCI ~ s(year), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.basic)
summary(gam.fitted.VCI.basic)

gam.fitted.VCI.SPEI.X30d <- gam(VCI ~ s(year) + s(SPEI.X30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI.X30d)
summary(gam.fitted.VCI.SPEI.X30d)

gam.fitted.VCI.SPEI30.VPD <- gam(VCI ~ s(year) + s(SPEI.X30d) + s(VPD), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI30.VPD)
summary(gam.fitted.VCI.SPEI30.VPD)

gam.fitted.VCI.SPEI60.TMIN30d <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI60.TMIN30d)
summary(gam.fitted.VCI.SPEI60.TMIN30d)

gam.fitted.VCI.SPEI90.TMIN30d <- gam(VCI ~ s(year) + s(SPEI.X90d) + s(VPD) + s(TMIN30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI90.TMIN30d)
summary(gam.fitted.VCI.SPEI90.TMIN30d)

gam.fitted.VCI.SPEI14 <- gam(VCI ~ s(year) + s(SPEI.X14d) + s(VPD) + s(TMIN30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI14)
summary(gam.fitted.VCI.SPEI14)

gam.fitted.VCI.SPEI30.type <- gam(VCI ~ s(year) + s(SPEI.X30d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI30.type)
summary(gam.fitted.VCI.SPEI30.type)

gam.fitted.VCI.SPEI60.type <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI60.type)
summary(gam.fitted.VCI.SPEI60.type)

gam.fitted.VCI.SPEI90.type <- gam(VCI ~ s(year) + s(SPEI.X90d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI90.type)
summary(gam.fitted.VCI.SPEI90.type)

gam.fitted.VCI.SPEI14.type <- gam(VCI ~ s(year) + s(SPEI.X14d) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
plot.gam(gam.fitted.VCI.SPEI14.type)
summary(gam.fitted.VCI.SPEI14.type)

AIC(gam.fitted.VCI.SPEI14.type, gam.fitted.VCI.SPEI30.type, gam.fitted.VCI.SPEI90.type, gam.fitted.VCI.SPEI60.type)

gam.fitted.VCI.SPEI60.TMIN14d <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN14d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN14d)

gam.fitted.VCI.SPEI60.TMIN60d <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN60d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d)

gam.fitted.VCI.SPEI60.TMIN90d <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN90d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN90d)

AIC(gam.fitted.VCI.SPEI60.TMIN90d, gam.fitted.VCI.SPEI60.TMIN30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMIN14d)

gam.fitted.VCI.SPEI60.TMAX14d <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMAX14d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN14d)

gam.fitted.VCI.SPEI60.TMAX60d <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMAX60d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMAX60d)

gam.fitted.VCI.SPEI60.TMAX90d <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMAX90d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMAX90d)

gam.fitted.VCI.SPEI60.TMAX30d <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMAX30d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMAX30d)

AIC(gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact)

gam.fitted.VCI.SPEI60.TMIN60d.interact <- gam(VCI ~ s(SPEI.X60d, doy, by=type) + s(VPD, doy, by=type) + s(TMIN60d, doy, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.interact)

gam.fitted.VCI.SPEI60.TMIN60d.interact.year <- gam(VCI ~ s(year) + s(SPEI.X60d, doy, by=type) + s(TMIN60d, doy, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

AIC(gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

gam.fitted.VCI.SPEI60.TMIN60d.noVPD <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(TMIN60d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.noVPD)

AIC(gam.fitted.VCI.SPEI60.TMIN60d.noVPD, gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d) + s(VPD) + s(TMIN60d, SPEI.X60d), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact)

AIC(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact, gam.fitted.VCI.SPEI60.TMIN60d.noVPD, gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d) + s(VPD) + s(TMIN60d, SPEI.X60d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type)

AIC(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact, gam.fitted.VCI.SPEI60.TMIN60d.noVPD, gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d, by=type) + s(VPD) + s(TMIN60d, SPEI.X60d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp)

AIC(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact, gam.fitted.VCI.SPEI60.TMIN60d.noVPD, gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp.precip <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d, by=type) + s(VPD) + s(TMIN60d, SPEI.X60d, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp.precip)

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp.precip.year <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d, by=type) + s(VPD) + s(TMIN60d, SPEI.X60d, by=year) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp.precip.year)


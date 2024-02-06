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

#creating 3 day minimum NDVI for each doy by lc type

lc.type <- unique(ChicagolandTempSPEISPINDVIVPDNA$type) 
doyrange <- 99:301
ChicagolandTempSPEISPINDVIVPDNA$VCI.3day<-NA

for(t in 1:length(lc.type)) {
  for(d in 2:(length(doy)-1)){
    Threedaywindow <- subset(ChicagolandTempSPEISPINDVIVPDNA,
                             type==lc.type[t]&doy>(doyrange[d]-2)&doy<(doyrange[d]+2))
    ndvi.3daymin<-min(Threedaywindow$ndvi.obs)
    ndvi.3daymax<-max(Threedaywindow$ndvi.obs)
    years<-unique(Threedaywindow$year[which(Threedaywindow$doy==doyrange[d])])
    
    for(y in 1:length(years)){
      
      obs<-Threedaywindow$ndvi.obs[which(Threedaywindow$doy==doyrange[d]&Threedaywindow$year==years[y])]
      VCI3day<-((obs - ndvi.3daymin)/(ndvi.3daymax - ndvi.3daymin))
      index<-which(ChicagolandTempSPEISPINDVIVPDNA$type==lc.type[t]&ChicagolandTempSPEISPINDVIVPDNA$doy==doyrange[d]&ChicagolandTempSPEISPINDVIVPDNA$year==years[y])
      ChicagolandTempSPEISPINDVIVPDNA$VCI.3day[index]<-VCI3day
    }

    
  }

  
}


#remove all NA values from dataframe (see if na.action in lme works)
ChicagolandTempSPEISPINDVIVPDNA <- na.omit(ChicagolandTempSPEISPINDVIVPDNA)
summary(ChicagolandTempSPEISPINDVIVPDNA)

saveRDS(ChicagolandTempSPEISPINDVIVPDNA, file.path(google.drive, "data/data_sets/ChicagolandData"))

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

start.end <- c(182, 244)

tstat.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August t.stat of Response (NDVI.obs and VCI) and All Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug.png", unit="in", height = 20, width = 10, res = 300)
plot(tstat.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug)
dev.off()

r2_NDVI.obs_VCI.all.SPI_VPD_LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=r.sq.m)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "r2 of Response (NDVI.obs and VCI) and All Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/r2_NDVI.obs_VCI.all.SPI_VPD_LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(r2_NDVI.obs_VCI.all.SPI_VPD_LC.types)
dev.off()

start.end <- c(182, 244)

r2.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=r.sq.m)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August r2 of Response (NDVI.obs and VCI) and Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/r2.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug.png", unit="in", height = 20, width = 10, res = 300)
plot(r2.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug)
dev.off()

AIC_NDVI.obs_VCI.all.SPI_VPD_LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "AIC of Response (NDVI.obs and VCI) and All Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/AIC_NDVI.obs_VCI.all.SPI_VPD_LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(AIC_NDVI.obs_VCI.all.SPI_VPD_LC.types)
dev.off()

start.end <- c(182, 244)

AIC.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August AIC Of Response (NDVI.obs and VCI) and All Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/AIC.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug.png", unit="in", height = 20, width = 10, res = 300)
plot(AIC.NDVI.obs.VCI.SPI.SPEI.VPD.Temp.LC.types.july.aug)
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

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d) + s(VPD) + s(TMIN60d, SPEI.X60d) + s(ndvi.obs, doy, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy)

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d) + s(VPD) + s(TMIN60d, SPEI.X60d) + s(ndvi.obs, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs)
AIC(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact, gam.fitted.VCI.SPEI60.TMIN60d.noVPD, gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

gam.fitted.VCI.SPEI60.TMIN60d.interact.year.ndvi.obs <- gam(VCI ~ s(year) + s(SPEI.X60d, doy, by=type) + s(TMIN60d, doy, by=type) + s(ndvi.obs, doy, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary.gam.fitted.VCI.SPEI60.TMIN60d.interact.year.ndvi.obs<-summary(gam.fitted.VCI.SPEI60.TMIN60d.interact.year.ndvi.obs)

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d, doy, by=type) + s(VPD) + s(TMIN60d, SPEI.X60d, doy, by=type) + s(ndvi.obs, doy, by=type) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions)

AIC(gam.fitted.VCI.SPEI60.TMIN60d.interact.year.ndvi.obs, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact, gam.fitted.VCI.SPEI60.TMIN60d.noVPD, gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

####################
#Creating autocorrelation plots
acf.ndvi = subset(ChicagolandTempSPEISPINDVIVPDNA, select = -c(ndvi.modeled, ndvi.anomaly, Unneeded.Date, Date.x, Date.y, ndvi.doy.max, ndvi.doy.min) )
ndvi.ts <- ts(acf.ndvi, start=c(2001, 1), end=c(2021, 12), frequency=12)

ndvi.autocorrelation <- acf(ndvi.ts, lag.max=NULL, type=c("correlation"), plot=TRUE, na.action=na.fail, demean=TRUE)

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/autocorrelation.png", unit="in", height = 20, width = 20, res = 300)
plot(ndvi.autocorrelation)
dev.off()

#Reducing variables in autocorrelation plots
acf.VCI = subset(ChicagolandTempSPEISPINDVIVPDNA, select = c(date, VCI) )
VCI.ts <- ts(acf.VCI, start=c(2001, 1), end=c(2021, 12), frequency=12)
VCI.autocorrelation <- acf(VCI.ts, lag.max=NULL, type=c("correlation"), plot=TRUE, na.action=na.fail, demean=TRUE)

acf(ChicagolandTempSPEISPINDVIVPDNA$VCI)

#Trying ACF grouped by type
library(tidyverse)
LCGroupedNDVI <-ChicagolandTempSPEISPINDVIVPDNA %>%
  group_by(type) %>%
  nest()%>%
  mutate(data=map(data, ~acf(., lag.max=1, type="correlation", plot=F))) %>%
  mutate(data = map(data, ~as.data.frame(rbind(.x$acf[1,,], .x$acf[2,,])))) %>%
  unnest(data)


###################
#Checking SPI

gam.fitted.VCI.x14d.SPI.type <- gam(VCI ~ s(year) + s(X14d.SPI) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x14d.SPI.type)

gam.fitted.VCI.x30d.SPI.type <- gam(VCI ~ s(year) + s(X30d.SPI) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x30d.SPI.type)

gam.fitted.VCI.x60d.SPI.type <- gam(VCI ~ s(year) + s(X60d.SPI) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x60d.SPI.type)

gam.fitted.VCI.x90d.SPI.type <- gam(VCI ~ s(year) + s(X90d.SPI) + s(VPD) + s(TMIN30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.type)

gam.fitted.VCI.x90d.SPI.type.TMIN14 <- gam(VCI ~ s(year) + s(X90d.SPI) + s(VPD) + s(TMIN14d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.type.TMIN14)

gam.fitted.VCI.x90d.SPI.type.TMIN60 <- gam(VCI ~ s(year) + s(X90d.SPI) + s(VPD) + s(TMIN60d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.type.TMIN60)

gam.fitted.VCI.x90d.SPI.type.TMIN90 <- gam(VCI ~ s(year) + s(X90d.SPI) + s(VPD) + s(TMIN90d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.type.TMIN90)

gam.fitted.VCI.x90d.SPI.type.TMAX90 <- gam(VCI ~ s(year) + s(X90d.SPI) + s(VPD) + s(TMAX90d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.type.TMAX90)

gam.fitted.VCI.x90d.SPI.type.TMAX60 <- gam(VCI ~ s(year) + s(X90d.SPI) + s(VPD) + s(TMAX60d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.type.TMAX60)

gam.fitted.VCI.x90d.SPI.type.TMAX30 <- gam(VCI ~ s(year) + s(X90d.SPI) + s(VPD) + s(TMAX30d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.type.TMAX30)

gam.fitted.VCI.x90d.SPI.type.TMAX14 <- gam(VCI ~ s(year) + s(X90d.SPI) + s(VPD) + s(TMAX14d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.type.TMAX14)

AIC(gam.fitted.VCI.SPEI60.TMIN60d.type, gam.fitted.VCI.SPEI60.type, gam.fitted.VCI.x90d.SPI.type.TMIN90, gam.fitted.VCI.x90d.SPI.type, gam.fitted.VCI.x90d.SPI.type.TMIN60, gam.fitted.VCI.x90d.SPI.type.TMIN14, gam.fitted.VCI.x90d.SPI.type.TMAX14, gam.fitted.VCI.x90d.SPI.type.TMAX90, gam.fitted.VCI.x90d.SPI.type.TMAX60, gam.fitted.VCI.x90d.SPI.type.TMAX30)

#GAM gives best results with SPIx90d and TMIN60
#retrying earlier gams with x90SPI

gam.fitted.VCI.x90d.SPI.TMIN60d.temp.precip.interact <- gam(VCI ~ s(year) + s(X90d.SPI, TMIN60d) + s(VPD) + s(TMIN60d, X90d.SPI), data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.TMIN60d.temp.precip.interact)

gam.fitted.VCI.x90d.SPI.TMIN60d.temp.precip.interact.type <- gam(VCI ~ s(year) + s(X90d.SPI, TMIN60d) + s(VPD) + s(TMIN60d, X90d.SPI) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.x90d.SPI.TMIN60d.temp.precip.interact.type)

AIC(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact, gam.fitted.VCI.SPEI60.TMIN60d.noVPD, gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp <- gam(VCI ~ s(year) + s(SPEI.X60d, TMIN60d, by=type) + s(VPD) + s(TMIN60d, SPEI.X60d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp)

gam.fitted.VCI.SPEI60.TMIN60d.type <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN60d) + type, data = ChicagolandTempSPEISPINDVIVPDNA, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.type)

AIC(gam.fitted.VCI.SPEI60.TMIN60d.type, gam.fitted.VCI.SPEI60.type, gam.fitted.VCI.x90d.SPI.type.TMIN60, gam.fitted.VCI.x90d.SPI.TMIN60d.temp.precip.interact.type, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.temp, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type, gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact, gam.fitted.VCI.SPEI60.TMIN60d.noVPD, gam.fitted.VCI.SPEI60.TMAX90d, gam.fitted.VCI.SPEI60.TMAX30d, gam.fitted.VCI.SPEI60.TMIN60d, gam.fitted.VCI.SPEI60.TMAX14d, gam.fitted.VCI.SPEI60.TMAX60d, gam.fitted.VCI.SPEI60.TMIN60d.interact, gam.fitted.VCI.SPEI60.TMIN60d.interact.year)

##################
#Check rmse for VCI gam model
ChicagolandTempSPEISPINDVIVPDNA$predicted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy <- predict(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy )
ChicagolandTempSPEISPINDVIVPDNA$ndvi.anomaly.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy <- resid(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy)

rmse.VCI.SPEI.TMIN60d.temp.precip.interact.type.ndvi.obs.doy <- sqrt(mean((ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs- ChicagolandTempSPEISPINDVIVPDNA$predicted.VCI.SPEI60.TMIN60d.temp.precip.interact.type.ndvi.obs.doy )^2))

ChicagolandTempSPEISPINDVIVPDNA$predicted.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions <- predict(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions )
ChicagolandTempSPEISPINDVIVPDNA$ndvi.anomaly.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions <- resid(gam.fitted.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions)

rmse.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions <- sqrt(mean((ChicagolandTempSPEISPINDVIVPDNA$ndvi.obs- ChicagolandTempSPEISPINDVIVPDNA$predicted.VCI.SPEI60.TMIN60d.temp.precip.ndvi.obs.doy.interactions )^2))

############
#Rerunning GAM during grow season
ChicagolandGrowSeason <- subset(ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$doy>=91 & ChicagolandTempSPEISPINDVIVPDNA$doy<=304,])

gam.fitted.VCI.SPEI60.TMIN60d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN60d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.type.grow.season)

gam.fitted.VCI.SPEI60.TMAX60d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMAX60d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMAX60d.type.grow.season)

gam.fitted.VCI.SPEI60.TMIN60d.interact.TMAX60d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN60d, TMAX60d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.interact.TMAX60d.type.grow.season)

gam.fitted.VCI.SPEI60.TMIN60d.interact.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN60d, TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.interact.TMAX14d.type.grow.season)

gam.fitted.VCI.SPEI60.TMIN60d.interact.TMAX30d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN60d, TMAX30d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN60d.interact.TMAX30d.type.grow.season)

gam.fitted.VCI.SPEI60.TMAX60d.interact.TMIN30d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMAX60d, TMIN30d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMAX60d.interact.TMIN30d.type.grow.season)

gam.fitted.VCI.SPEI60.TMIN30d.interact.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN30d, TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN30d.interact.TMAX14d.type.grow.season)

gam.fitted.VCI.SPEI60.TMIN14d.interact.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X60d) + s(VPD) + s(TMIN14d, TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI60.TMIN14d.interact.TMAX14d.type.grow.season)

gam.fitted.VCI.SPEI30.TMIN60d.interact.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X30d) + s(VPD) + s(TMIN60d, TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI30.TMIN60d.interact.TMAX14d.type.grow.season)

gam.fitted.VCI.SPEI14.TMIN60d.interact.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X14d) + s(VPD) + s(TMIN60d, TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI14.TMIN60d.interact.TMAX14d.type.grow.season)

gam.fitted.VCI.SPEI90.TMIN60d.interact.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X90d) + s(VPD) + s(TMIN60d, TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI90.TMIN60d.interact.TMAX14d.type.grow.season)

gam.fitted.VCI.SPEI30.TMAX60d.interact.TMIN14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X30d) + s(VPD) + s(TMAX60d, TMIN14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI30.TMAX60d.interact.TMIN14d.type.grow.season)

gam.fitted.VCI.SPEI30.TMIN60d.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X30d) + s(VPD) + s(TMIN60d) + s(TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI30.TMIN60d.TMAX14d.type.grow.season)

gam.fitted.VCI.SPEI30.interact.TMIN60d.interact.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X30d, TMAX14d) + s(VPD) + s(TMIN60d, TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI30.interact.TMIN60d.interact.TMAX14d.type.grow.season)

gam.fitted.VCI.SPEI30.interact.TMIN60d.interact2.TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X30d, TMIN60d) + s(VPD) + s(TMIN60d, TMAX14d) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI30.interact.TMIN60d.interact2.TMAX14d.type.grow.season)

#Creating diagnostic plots for best fit VCI model

ChicagolandGrowSeason$predicted.VCI.interact2 <- predict(gam.fitted.VCI.SPEI30.interact.TMIN60d.interact2.TMAX14d.type.grow.season)
ChicagolandGrowSeason$resids.VCI.interact2 <- resid(gam.fitted.VCI.SPEI30.interact.TMIN60d.interact2.TMAX14d.type.grow.season)
modeled.obs.VCI.interact2 <- plot(predicted.VCI.interact2 ~ VCI, data=ChicagolandGrowSeason); abline(a=0, b=1, col="red")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/GAM_Summaries/modeled.obs.VCI.interact2", unit="in", height = 10, width = 20, res = 300)
plot(modeled.obs.VCI.interact2)
dev.off()

plot(resids.VCI.interact2 ~ predicted.VCI.interact2, data=ChicagolandGrowSeason); abline(a=0, b=0, col="red")

rmse.VCI.interact2 <- sqrt(mean((ChicagolandGrowSeason$VCI-ChicagolandGrowSeason$predicted.VCI.interact2)^2))
rmse.VCI.interact2.obs.0to.15 <- sqrt(mean((ChicagolandGrowSeason$VCI[ChicagolandGrowSeason$VCI<=0.15]-ChicagolandGrowSeason$predicted.VCI.interact2[ChicagolandGrowSeason$VCI<=0.15])^2))
rmse.VCI.interact2.obs.15to.3 <- sqrt(mean((ChicagolandGrowSeason$VCI[ChicagolandGrowSeason$VCI>0.15&ChicagolandGrowSeason$VCI<=0.3]-ChicagolandGrowSeason$predicted.VCI.interact2[ChicagolandGrowSeason$VCI>0.15&ChicagolandGrowSeason$VCI<=0.3])^2))
rmse.VCI.interact2.obs.3to.45 <- sqrt(mean((ChicagolandGrowSeason$VCI[ChicagolandGrowSeason$VCI>0.3&ChicagolandGrowSeason$VCI<=0.45]-ChicagolandGrowSeason$predicted.VCI.interact2[ChicagolandGrowSeason$VCI>0.3&ChicagolandGrowSeason$VCI<=0.45])^2))
rmse.VCI.interact2.obs.45to.6 <- sqrt(mean((ChicagolandGrowSeason$VCI[ChicagolandGrowSeason$VCI>0.45&ChicagolandGrowSeason$VCI<=0.6]-ChicagolandGrowSeason$predicted.VCI.interact2[ChicagolandGrowSeason$VCI>0.45&ChicagolandGrowSeason$VCI<=0.6])^2))
rmse.VCI.interact2.obs.6to.75 <- sqrt(mean((ChicagolandGrowSeason$VCI[ChicagolandGrowSeason$VCI>0.6&ChicagolandGrowSeason$VCI<=0.75]-ChicagolandGrowSeason$predicted.VCI.interact2[ChicagolandGrowSeason$VCI>0.6&ChicagolandGrowSeason$VCI<=0.75])^2))
rmse.VCI.interact2.obs.75to.9 <- sqrt(mean((ChicagolandGrowSeason$VCI[ChicagolandGrowSeason$VCI>0.75&ChicagolandGrowSeason$VCI<=0.9]-ChicagolandGrowSeason$predicted.VCI.interact2[ChicagolandGrowSeason$VCI>0.75&ChicagolandGrowSeason$VCI<=0.9])^2))
rmse.VCI.interact2.obs.9to1 <- sqrt(mean((ChicagolandGrowSeason$VCI[ChicagolandGrowSeason$VCI>0.9&ChicagolandGrowSeason$VCI<=1]-ChicagolandGrowSeason$predicted.VCI.interact2[ChicagolandGrowSeason$VCI>0.9&ChicagolandGrowSeason$VCI<=1])^2))

plot(predicted.VCI.interact2~VCI,xaxt="n", data=ChicagolandGrowSeason); abline(lm(VCI ~ predicted.VCI.interact2, data=ChicagolandGrowSeason), col="red"); abline(v=0); abline(v=0.15); abline(v= 0.3); abline(v= 0.45); abline(v=0.6); abline(v= 0.75); axis(1, at = seq(0, 1, by = 0.15), las=1); axis(1, (0.075),labels = "0.0752", tick="FALSE", line=-27); axis(1, (0.225),labels = "0.0470", tick="FALSE", line=-27); axis(1, (0.375),labels = "0.0555", tick="FALSE", line=-27); axis(1, (at=0.525),labels = "0.0666", tick="FALSE", line=-27); axis(1, (0.675),labels = "0.0522", tick="FALSE", line=-27); axis(1, (0.825),labels = "0.0415", tick="FALSE", line=-27); axis(1, (-0.05),labels = "RMSE", tick="FALSE", line=-27); axis(1, (0.42),labels = "Model Average RMSE = 0.0562", tick="FALSE", line=-28.5); axis(1, (0.42),labels = "Predicted NDVI Modeled with Prior 3 day Average", tick="FALSE", line=1); axis(2, (0.42),labels = "Observed NDVI", tick="FALSE", line=1)

predicted.obs.VCI.interact2 <- plot(predicted.VCI.interact2 ~ VCI,xaxt="n", data=ChicagolandGrowSeason); abline(lm(VCI ~ predicted.VCI.interact2, data=ChicagolandGrowSeason), col="red"); abline(v=0); abline(v=0.15); abline(v= 0.3); abline(v= 0.45); abline(v=0.6); abline(v= 0.75); abline(v=0.90); abline(v=1.0); axis(1, at = seq(0, 1.0, by = 0.15), las=1.15); axis(1, (0.075),labels = "0.409", tick="FALSE", line=-27); axis(1, (0.225),labels = "0.270", tick="FALSE", line=-27); axis(1, (0.375),labels = "0.185", tick="FALSE", line=-27); axis(1, (at=0.525),labels = "0.158", tick="FALSE", line=-27); axis(1, (0.675),labels = "0.204", tick="FALSE", line=-27); axis(1, (0.825),labels = "0.283", tick="FALSE", line=-27); axis(1, (0.95),labels = "0.376", tick="FALSE", line=-27); axis(1, (-0.05),labels = "RMSE", tick="FALSE", line=-27); axis(1, (0.42),labels = "Model Average RMSE = 0.313", tick="FALSE", line=-28.5); axis(1, (0.42),labels = "Observed VCI", tick="FALSE", line=1); axis(2, (0.42),labels = "Predicted VCI Modeled with Interact2", tick="FALSE", line=1)

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/GAM_Summaries/predicted.obs.VCI.interact2e", unit="in", height = 10, width = 20, res = 300)
plot(predicted.obs.VCI.interact2)
dev.off()

plot(ndvi.anomaly ~ resids.3d.ave, data=NDVI.obs.t.minus.3d.aveNA); abline(a=0, b=1, col="red")

ggplot(data=ChicagolandGrowSeason[ChicagolandGrowSeason$year>2010,])+
  facet_wrap(~type)+
  geom_line(aes(x=date, y=VCI, color="VCI")) +
  geom_line(aes(x=date, y=predicted.VCI.interact2, color="Predicted VCI"))

####################
gam.fitted.VCI.SPEI30.interactions.TMIN60d..TMAX14d.type.grow.season <- gam(VCI ~ s(year) + s(SPEI.X30d, TMIN60d, doy, by=type) + s(VPD) + s(TMIN60d, TMAX14d, doy, by=type) + type, data = ChicagolandGrowSeason, method = 'REML')
summary(gam.fitted.VCI.SPEI30.interactions.TMIN60d.TMAX14d.type.grow.season)

gam.fitted.VCI.double.penalty <- gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d)+ s(TMAX60d)+ s(TMAX90d) + s(VPD) + type, data=ChicagolandGrowSeason, select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty)

gam.fitted.VCI.double.penalty.wo.TMAX30 <- gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX60d)+ s(TMAX90d) + s(VPD) + type, data=ChicagolandGrowSeason, select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.wo.TMAX30)

#running double penalty approach on GAM for data during grow season with lower VCI to perform automatic term selection
gam.fitted.VCI.double.penalty.low.VCI <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD) + type, data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5,], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables <-gam(VCI ~ s(year) + s(X30d.SPI) + s(X60d.SPI) + s(SPEI.X30d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX60d)+ s(TMAX90d) + s(VPD) + type, data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5,], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables)

gam.fitted.VCI.double.penalty.low.VCI.best.variables <-gam(VCI ~ s(year) + s(X30d.SPI) + s(TMIN30d) + s(TMIN90d) + s(TMAX60d)+ s(TMAX90d) + s(VPD) + type, data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5,], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.best.variables)

#subsetting data frame by land cover type and running GAM at different VCI ranges

ChicagolandData <- readRDS(file.path(google.drive, "data/data_sets/ChicagolandData"))
ChicagolandGrowSeason <- subset(ChicagolandData[ChicagolandData$doy>=91 & ChicagolandData$doy<=304,])

gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium)

gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium1 <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.all.variables.urban.medium1)

gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium <-gam(VCI ~ s(year) + s(X30d.SPI) + s(X60d.SPI) + s(SPEI.X90d) + s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.5 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.low.VCI.wo.variables.urban.medium)

gam.fitted.VCI.double.penalty.lower.VCI.all.variables.urban.medium <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.35 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lower.VCI.all.variables.urban.medium)

gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium <-gam(VCI ~ s(year) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.35 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lower.VCI.wo.variables.urban.medium)

gam.fitted.VCI.double.penalty.lowest.VCI.all.variables.urban.medium <-gam(VCI ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d) + s(TMAX60d)+ s(TMAX90d) + s(VPD), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.1 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lowest.VCI.all.variables.urban.medium)

gam.fitted.VCI.double.penalty.lowest.VCI.wo.variables.urban.medium <-gam(VCI ~ s(X30d.SPI) + s(X90d.SPI) + s(SPEI.X90d) + s(TMIN30d), data=ChicagolandGrowSeason[ChicagolandGrowSeason$VCI<=0.1 & ChicagolandGrowSeason$type=='urban-medium',], select=TRUE, method = 'REML')
summary(gam.fitted.VCI.double.penalty.lowest.VCI.wo.variables.urban.medium)

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

########################
#Create plot of frequency of ndvi readings by doy for one land cover type

doy.count.of.points.grow.season <- hist(ChicagolandGrowSeasonCrop$doy, breaks=seq(90, 305, 1), xlim=c(90,305), ylim=c(0, 10), xlab='Day of Year', ylab="Frequency", main='Count of Points for Each Day of the Year During the Grow Season')

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/doy_count_of_points.png", unit="in", height = 20, width = 40, res = 100)
plot(doy.count.of.points.grow.season)
dev.off()

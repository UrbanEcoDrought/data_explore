library(ggplot2)
library(lubridate)
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
#Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

#3dVCI data saved on Google Drive
#ChicagolandTempSPEISPINDVIVPDNA <- readRDS(file.path(google.drive, "data/data_sets/Chicagoland3dVCI.RDS"))

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

saveRDS(ChicagolandTempSPEISPINDVIVPDNA, file.path(google.drive, "data/data_sets/ChicagolandData"))

#creating 3 day minimum NDVI for each doy by lc type

lc.type <- unique(ChicagolandTempSPEISPINDVIVPDNA$type) 
doyrange <- 10:301
ChicagolandTempSPEISPINDVIVPDNA$VCI.3day<-NA
ChicagolandTempSPEISPINDVIVPDNA$VCI3.14d.Lag<-NA
#ChicagolandTempSPEISPINDVIVPDNA$VCI3.30d.Lag<-NA
#ChicagolandTempSPEISPINDVIVPDNA$VCI3.45d.Lag<-NA

for(t in 1:length(lc.type)) {
  for(d in 2:(length(doyrange)-1)){
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
      
      for(D in 100:300){
        
        x<-which(ChicagolandTempSPEISPINDVIVPDNA$type==lc.type[t]&ChicagolandTempSPEISPINDVIVPDNA$doy==(D-14)&ChicagolandTempSPEISPINDVIVPDNA$year==years[y])
        if(length(x)>0){
          
          index2<-which(ChicagolandTempSPEISPINDVIVPDNA$type==lc.type[t]&ChicagolandTempSPEISPINDVIVPDNA$doy==D&ChicagolandTempSPEISPINDVIVPDNA$year==years[y])
          ChicagolandTempSPEISPINDVIVPDNA$VCI3.14d.Lag[index2] <-ChicagolandTempSPEISPINDVIVPDNA$VCI.3day[x]
          
        }
        #        xy<-which(ChicagolandTempSPEISPINDVIVPDNA$type==lc.type[t]&ChicagolandTempSPEISPINDVIVPDNA$doy==(D-30)&ChicagolandTempSPEISPINDVIVPDNA$year==years[y])
        #        if(length(xy)>0){
        #          
        #          ChicagolandTempSPEISPINDVIVPDNA$VCI3.30d.Lag[index2] <-ChicagolandTempSPEISPINDVIVPDNA$VCI.3day[xy]
        #        }
        #        xyz<-which(ChicagolandTempSPEISPINDVIVPDNA$type==lc.type[t]&ChicagolandTempSPEISPINDVIVPDNA$doy==(D-45)&ChicagolandTempSPEISPINDVIVPDNA$year==years[y])
        #        if(length(xyz)>0){
        #          
        #          ChicagolandTempSPEISPINDVIVPDNA$VCI3.45d.Lag[index2] <-ChicagolandTempSPEISPINDVIVPDNA$VCI.3day[xyz]
        #        }
      }
    }
    paste("type= ",t,", doy= ", d, ", year= ", y," all done",sep="")
    
  }
  
  
}

saveRDS(ChicagolandTempSPEISPINDVIVPDNA, file.path(google.drive, "data/data_sets/Chicagoland3dVCI.RDS"))

#remove all NA values from dataframe (see if na.action in lme works)
ChicagolandTempSPEISPINDVIVPDNA <- na.omit(ChicagolandTempSPEISPINDVIVPDNA)
summary(ChicagolandTempSPEISPINDVIVPDNA)

##########creating for LME loop 

ChicagolandTempSPEISPINDVIVPDNA$month <- lubridate::month(ChicagolandTempSPEISPINDVIVPDNA$date)
days.use <- unique(ChicagolandTempSPEISPINDVIVPDNA$doy[ChicagolandTempSPEISPINDVIVPDNA$month >=3 & ChicagolandTempSPEISPINDVIVPDNA$month <=9])
days.use 

lc.type <- unique(ChicagolandTempSPEISPINDVIVPDNA$type) # this will help avoid typos

resp.vars <- c("ndvi.obs", "VCI.3day")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI", "VPD", "SPEI.X14d", "SPEI.X30d", "SPEI.X60d", "SPEI.X90d", "TMIN14d", "TMIN30d", "TMIN60d", "TMIN90d", "TMAX14d", "TMAX30d", "TMAX60d", "TMAX90d")

mod.out <- data.frame(TYPE = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 

row.ind = 0 

for(RESP in resp.vars){
  for(PRED in pred.vars){
    for(TYPE in lc.type){
      for(i in 1:length(days.use)){
        dayNOW <- days.use[i] 
        
        dat.tmp <- ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$doy>=dayNOW-7 & ChicagolandTempSPEISPINDVIVPDNA$doy<=dayNOW+7 & ChicagolandTempSPEISPINDVIVPDNA$type==TYPE,]
        dat.tmp$RESP <- dat.tmp[,RESP]
        dat.tmp$PRED <- dat.tmp[,PRED] 
        summary(dat.tmp) 
        dim(dat.tmp)
        
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
      
      paste("RESP= ",RESP,", PRED= ", PRED, ", TYPE= ", TYPE," all done",sep="")
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

tstat.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs and VCI(3d)) and Predictors (all SPI, SPEI, VPD, and Temp) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat.NDVI.obs.3dVCI.all.SPI.SPEI.VPD.Temp.LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(tstat.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types)
dev.off()

start.end <- c(182, 244)

tstat.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August t.stat of Response (NDVI.obs and VCI(3d)) and All Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug.png", unit="in", height = 20, width = 10, res = 300)
plot(tstat.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug)
dev.off()

r2_NDVI.obs_3dVCI.all.SPI_VPD_LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=r.sq.m)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "r2 of Response (NDVI.obs and VCI(3d)) and All Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/r2_NDVI.obs_3dVCI.all.SPI_VPD_LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(r2_NDVI.obs_3dVCI.all.SPI_VPD_LC.types)
dev.off()

start.end <- c(182, 244)

r2.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=r.sq.m)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August r2 of Response (NDVI.obs and VCI(3d)) and Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/r2.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug.png", unit="in", height = 20, width = 10, res = 300)
plot(r2.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug)
dev.off()

AIC_NDVI.obs_3dVCI.all.SPI_VPD_LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "AIC of Response (NDVI.obs and VCI(3d)) and All Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/AIC_NDVI.obs_3dVCI.all.SPI_VPD_LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(AIC_NDVI.obs_3dVCI.all.SPI_VPD_LC.types)
dev.off()

start.end <- c(182, 244)

AIC.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August AIC Of Response (NDVI.obs and VCI(3d)) and All Predictors for All LC Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/AIC.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug.png", unit="in", height = 20, width = 10, res = 300)
plot(AIC.NDVI.obs.3dVCI.SPI.SPEI.VPD.Temp.LC.types.july.aug)
dev.off()


#####################
#Creating GAM models
library(mgcv)

#Creating grow season data frame
Chicagoland3dVCIGrowSeason <- subset(ChicagolandTempSPEISPINDVIVPDNA[ChicagolandTempSPEISPINDVIVPDNA$doy>=91 & ChicagolandTempSPEISPINDVIVPDNA$doy<=304,])

summary(Chicagoland3dVCIGrowSeason)

gam3dVCI.basic <- gam(VCI.3day ~ s(year) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.basic)

gam3dVCI.tmin30.spei30 <-gam(VCI.3day ~ s(year) + s(SPEI.X30d) + s(TMIN30d) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.tmin30.spei30)

gam3dVCI.tmin30.spei30.tmax60 <-gam(VCI.3day ~ s(year) + s(SPEI.X30d, TMAX60d) + s(TMIN30d) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.tmin30.spei30.tmax60)

gam3dVCI.double.penalty <- gam(VCI.3day ~ s(year) + s(X14d.SPI) + s(X30d.SPI) + s(X60d.SPI) + s(X90d.SPI) + s(SPEI.X14d) + s(SPEI.X30d) + s(SPEI.X60d) + s(SPEI.X90d) + s(TMIN14d) + s(TMIN30d)+ s(TMIN60d)+ s(TMIN90d) + s(TMAX14d) + s(TMAX30d)+ s(TMAX60d)+ s(TMAX90d) + s(VPD) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.double.penalty)

gam3dVCI.double.vars <- gam(VCI.3day ~ s(year) + s(X14d.SPI) + s(TMIN90d) + s(TMAX90d) + s(VPD) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.double.vars)

gam3dVCI.double.vars <- gam(VCI.3day ~ s(year) + s(X14d.SPI) + s(TMIN90d) + s(TMAX90d) + s(VPD) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.double.vars)

gam3dVCI.tmin30.spei30.tmax60.VCIlag <-gam(VCI.3day ~ s(year) + s(SPEI.X30d, TMAX60d) + s(TMIN30d) + s(VCI3.14d.Lag) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.tmin30.spei30.tmax60.VCIlag)

gam3dVCI.tmin30.spei30.tmax60.VCIlag.VPD.interact <-gam(VCI.3day ~ s(year) + s(SPEI.X30d, TMAX60d) + s(TMIN30d, doy, by=type) + s(VCI3.14d.Lag) + s(VPD) + type, data=Chicagoland3dVCIGrowSeasonNA, select=TRUE, method = 'REML')
summary(gam3dVCI.tmin30.spei30.tmax60.VCIlag.VPD.interact)

gam3dVCI.tmin30.spei30.tmax60.VCIlag.VPD.interact2 <-gam(VCI.3day ~ s(year) + s(SPEI.X30d) + s(TMIN30d, doy, by=type) + s(TMAX60d) + s(VCI3.14d.Lag) + s(VPD) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.tmin30.spei30.tmax60.VCIlag.VPD.interact2)

gam3dVCI.tmin30.spei30.tmax60.VCIlag.VPD.interact3 <-gam(VCI.3day ~ s(year) + s(SPEI.X30d, TMAX60d, doy, by=type) + s(TMIN30d, doy, by=type) + s(VCI3.14d.Lag) + s(VPD, doy, by=type) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.tmin30.spei30.tmax60.VCIlag.VPD.interact3)

gam3dVCI.tmin30.spei30.tmax60.VCIlag30d <-gam(VCI.3day ~ s(year) + s(SPEI.X30d, TMAX60d) + s(TMIN30d) + s(VCI3.45d.Lag) + type, data=Chicagoland3dVCIGrowSeason, select=TRUE, method = 'REML')
summary(gam3dVCI.tmin30.spei30.tmax60.VCIlag30d)

#Creating diagnostic plots for best fit VCI model
Chicagoland3dVCIGrowSeason$predicted.3dVCI.interact<-NA
Chicagoland3dVCIGrowSeason$resids.3dVCI.interact <-NA
Chicagoland3dVCIGrowSeason<-na.omit(Chicagoland3dVCIGrowSeason)

Chicagoland3dVCIGrowSeasonNA$predicted.3dVCI.interact <- predict(gam3dVCI.tmin30.spei30.tmax60.VCIlag.VPD.interact)
Chicagoland3dVCIGrowSeasonNA$resids.3dVCI.interact <- resid(gam3dVCI.tmin30.spei30.tmax60.VCIlag.VPD.interact)

#Turning variables into factors so they can be properly plotted and saved
is.factor(Chicagoland3dVCIGrowSeasonNA$predicted.3dVCI.interact)
Chicagoland3dVCIGrowSeasonNA$pred.factor.3dVCI <- factor(Chicagoland3dVCIGrowSeasonNA$predicted.3dVCI.interact)
is.factor(Chicagoland3dVCIGrowSeasonNA$pred.factor.3dVCI)

is.factor(Chicagoland3dVCIGrowSeasonNA$VCI.3day)
Chicagoland3dVCIGrowSeasonNA$factor.3dVCI <- factor(Chicagoland3dVCIGrowSeasonNA$VCI.3day)
is.factor(Chicagoland3dVCIGrowSeasonNA$factor.3dVCI)

modeled.obs.3dVCI.interact <- plot(pred.factor.3dVCI ~ factor.3dVCI, data=Chicagoland3dVCIGrowSeasonNA); abline(a=0, b=1, col="red")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/GAM_Summaries/modeled.obs.3dVCI.interact", unit="in", height = 10, width = 20, res = 300)
plot(modeled.obs.3dVCI.interact)
dev.off()

plot(resids.3dVCI.interact ~ predicted.3dVCI.interact, data=Chicagoland3dVCIGrowSeasonNA); abline(a=0, b=0, col="red", na.rm=TRUE)

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

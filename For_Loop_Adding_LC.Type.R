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
days.use <- unique(ChicagolandTempSPEISPINDVIVPDNA$doy[ChicagolandTempSPEISPINDVIVPDNA$month >=3 & ChicagolandTempSPEISPINDVIVPDNA$month <=9])
days.use 
# lc.type <- c("crop", "forest", "grassland", "urban-low", "urban-medium", "urban-high", "urban-open")
lc.type <- unique(ChicagolandTempSPEISPINDVIVPDNA$type) # this will help avoid typos

resp.vars <- c("ndvi.obs", "ndvi.anomaly")
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
        
        # mod.var <- nlme::lme(RESP ~ PRED, subset = (type == TYPE), random=list(year=~1), data=dat.tmp[,], na.action=na.omit) # something like this *may* work, but it was givving me issues, so cleaner to just subset up top (line 59)
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

tstat.NDVI.obs.anomaly.all.SPI.SPEI.VPD.Temp.LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
   geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI, SPEI, VPD, and Temp) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat.NDVI.obs.anomaly.all.SPI.SPEI.VPD.Temp.LC.types.png", unit="in", height = 20, width = 10, res = 300)
plot(tstat.NDVI.obs.anomaly.all.SPI.SPEI.VPD.Temp.LC.types)
dev.off()

start.end <- c(182, 244)

tstat_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August t.stat of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI and VPD) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug)
dev.off()

r2_NDVI.obs_anomaly.all.SPI_VPD_LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=r.sq.m)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "r2 of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI and VPD) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/r2_NDVI.obs_anomaly.all.SPI_VPD_LC.types.png", unit="in", height = 5, width = 10, res = 300)
plot(r2_NDVI.obs_anomaly.all.SPI_VPD_LC.types)
dev.off()

start.end <- c(182, 244)

r2_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=r.sq.m)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August r2 of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI and VPD) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/r2_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug.png", unit="in", height = 5, width = 10, res = 300)
plot(r2_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug)
dev.off()

AIC_NDVI.obs_anomaly.all.SPI_VPD_LC.types <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "AIC of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI and VPD) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/AIC_NDVI.obs_anomaly.all.SPI_VPD_LC.types.png", unit="in", height = 5, width = 10, res = 300)
plot(AIC_NDVI.obs_anomaly.all.SPI_VPD_LC.types)
dev.off()

start.end <- c(182, 244)

AIC_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug <- ggplot(mod.out[mod.out$p.val<0.05,]) +
  facet_grid(TYPE~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August AIC of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI and VPD) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/AIC_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug.png", unit="in", height = 5, width = 10, res = 300)
plot(AIC_NDVI.obs_anomaly.all.SPI_VPD_LC.types.for.july.aug)
dev.off()

##############################

#Created new mod.out data frame with column showing land cover and predictor variable.
mod.out2 <- transform(mod.out, TYPE_PRED=paste(TYPE, PRED))
summary(mod.out2)

tstat_NDVI.obs_anomaly.all.SPI_VPD_by_LC.types <- ggplot(mod.out2[mod.out2$p.val<0.05,]) +      
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y= TYPE_PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI and VPD) by Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_anomaly.all.SPI_VPD_by_LC.types.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_anomaly.all.SPI_VPD_by_LC.types)
dev.off()

tstat_NDVI.obs_anomaly.all.SPI_VPD_by_LC.types.for.july.aug <- ggplot(mod.out2[mod.out2$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=TYPE_PRED, fill=t.stat)) +
  scale_x_continuous(limits=start.end, breaks=month.july.august$doy, labels=month.july.august$month)+
  labs(title = "July/August t.stat of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI and VPD) by Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_anomaly.all.SPI_VPD_by_LC.types.for.july.aug.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_anomaly.all.SPI_VPD_by_LC.types.for.july.aug)
dev.off()

#Created new mod.out data frame with columbt showing predictor variable and land cover.
mod.out3 <- transform(mod.out2, PRED_TYPE=paste(PRED, TYPE))

tstat_NDVI.obs_anomaly.lc.types_by_all.SPI_VPD <- ggplot(mod.out3[mod.out3$p.val<0.05,]) +      
  facet_grid(TYPE~RESP) +
     geom_tile(aes(x=DOY, y= PRED_TYPE, fill=t.stat)) +
     scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
     labs(title = "t.stat of Response (NDVI.obs and NDVI.anomaly) and LC Type by Predictors (all SPI and VPD) when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_anomaly.lc.types_by_all.SPI_VPD.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_anomaly.lc.types_by_all.SPI_VPD)
dev.off()

tstat_NDVI.obs_anomaly.lc.types_by_all.SPI_VPD <- ggplot(mod.out3[mod.out3$p.val<0.05,]) +      
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y= PRED_TYPE, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs and NDVI.anomaly) and LC Type by Predictors (all SPI and VPD) when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_anomaly.lc.types_by_all.SPI_VPD.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_anomaly.lc.types_by_all.SPI_VPD)
dev.off()

plot(x=mod.out$t.stat, y=mod.out$p.val )

#Examining lowest p-values (<0.0002)
ggplot(mod.out3[mod.out3$p.val<0.0002,]) +      
       facet_grid(.~RESP) +
       geom_tile(aes(x=DOY, y= PRED_TYPE, fill=t.stat)) +
       scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
       labs(title = "t.stat of Response (NDVI.obs and NDVI.anomaly) and LC Type by Predictors (all SPI and VPD) when p-value <0.0002")

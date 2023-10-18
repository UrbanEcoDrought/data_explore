library(ggplot2)
library(lubridate)
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))
head(ndvi.all)


LCtype <- "urban-medium"
ndvi.test <- ndvi.all[ndvi.all$type==LCtype,]
summary(ndvi.test)

ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)


ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

RESP <- resp.vars[1] #(ndvi.obs)

mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 


row.ind = 0 
for(PRED in pred.vars){
  for(i in 1:length(days.use)){
    dayNOW <- days.use[i] 
    
    dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,]
    dat.tmp$RESP <- dat.tmp[,RESP]
    dat.tmp$PRED <- dat.tmp[,PRED] 
    summary(dat.tmp) 
    dim(dat.tmp)
    
    mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
    mod.sum <- summary(mod.var)

    row.ind = row.ind+1 
    
    mod.out[row.ind, "LandCover"] <- LCtype
    mod.out[row.ind, "PRED"] <- PRED
    mod.out[row.ind, "RESP"] <- RESP
    mod.out[row.ind, "DOY"] <- dayNOW
    
    mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
    mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
    mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
    mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
    mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    mod.out[row.ind, "AIC"] <- AIC(mod.var) 
    
  }
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))

tstat_NDVI.obs_all.SPI_Urban.Medium <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs) and Predictors (all SPI) for Urban Medium when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_all.SPI_Urban.Medium.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_all.SPI_Urban.Medium)
dev.off()

###################################
#######################################
#LME Model for Urban High

LCtype <- "urban-high"
ndvi.test <- ndvi.all[ndvi.all$type==LCtype,]
summary(ndvi.test)

ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)


ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

RESP <- resp.vars[1] #(ndvi.obs)

mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 


row.ind = 0 
for(PRED in pred.vars){
  for(i in 1:length(days.use)){
    dayNOW <- days.use[i] 
    
    dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,]
    dat.tmp$RESP <- dat.tmp[,RESP]
    dat.tmp$PRED <- dat.tmp[,PRED] 
    summary(dat.tmp) 
    dim(dat.tmp)
    
    mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
    mod.sum <- summary(mod.var)
    
    row.ind = row.ind+1 
    
    mod.out[row.ind, "LandCover"] <- LCtype
    mod.out[row.ind, "PRED"] <- PRED
    mod.out[row.ind, "RESP"] <- RESP
    mod.out[row.ind, "DOY"] <- dayNOW
    
    mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
    mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
    mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
    mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
    mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    mod.out[row.ind, "AIC"] <- AIC(mod.var) 
    
  }
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))

tstat_NDVI.obs_all.SPI_Urban.High <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs) and Predictors (all SPI) for Urban High when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_all.SPI_Urban.High.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_all.SPI_Urban.High)
dev.off()

#######################################
#LME Model for Urban Low

LCtype <- "urban-low"
ndvi.test <- ndvi.all[ndvi.all$type==LCtype,]
summary(ndvi.test)

ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)


ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

RESP <- resp.vars[1] #(ndvi.obs)

mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 


row.ind = 0 
for(PRED in pred.vars){
  for(i in 1:length(days.use)){
    dayNOW <- days.use[i] 
    
    dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,]
    dat.tmp$RESP <- dat.tmp[,RESP]
    dat.tmp$PRED <- dat.tmp[,PRED] 
    summary(dat.tmp) 
    dim(dat.tmp)
    
    mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
    mod.sum <- summary(mod.var)
    
    row.ind = row.ind+1 
    
    mod.out[row.ind, "LandCover"] <- LCtype
    mod.out[row.ind, "PRED"] <- PRED
    mod.out[row.ind, "RESP"] <- RESP
    mod.out[row.ind, "DOY"] <- dayNOW
    
    mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
    mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
    mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
    mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
    mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    mod.out[row.ind, "AIC"] <- AIC(mod.var) 
    
  }
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))

tstat_NDVI.obs_all.SPI_Urban.Low <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs) and Predictors (all SPI) for Urban Low when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_all.SPI_Urban.Low.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_all.SPI_Urban.Low)
dev.off()

################################
#LME Model for Urban Open

LCtype <- "urban-open"
ndvi.test <- ndvi.all[ndvi.all$type==LCtype,]
summary(ndvi.test)

ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)


ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

RESP <- resp.vars[1] #(ndvi.obs)

mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 


row.ind = 0 
for(PRED in pred.vars){
  for(i in 1:length(days.use)){
    dayNOW <- days.use[i] 
    
    dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,]
    dat.tmp$RESP <- dat.tmp[,RESP]
    dat.tmp$PRED <- dat.tmp[,PRED] 
    summary(dat.tmp) 
    dim(dat.tmp)
    
    mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
    mod.sum <- summary(mod.var)
    
    row.ind = row.ind+1 
    
    mod.out[row.ind, "LandCover"] <- LCtype
    mod.out[row.ind, "PRED"] <- PRED
    mod.out[row.ind, "RESP"] <- RESP
    mod.out[row.ind, "DOY"] <- dayNOW
    
    mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
    mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
    mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
    mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
    mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    mod.out[row.ind, "AIC"] <- AIC(mod.var) 
    
  }
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))

tstat_NDVI.obs_all.SPI_Urban.Open <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs) and Predictors (all SPI) for Urban Open when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_all.SPI_Urban.Open.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_all.SPI_Urban.Open)
dev.off()

################################
#LME Model for Forest

LCtype <- "forest"
ndvi.test <- ndvi.all[ndvi.all$type==LCtype,]
summary(ndvi.test)

ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)


ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

RESP <- resp.vars[1] #(ndvi.obs)

mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 


row.ind = 0 
for(PRED in pred.vars){
  for(i in 1:length(days.use)){
    dayNOW <- days.use[i] 
    
    dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,]
    dat.tmp$RESP <- dat.tmp[,RESP]
    dat.tmp$PRED <- dat.tmp[,PRED] 
    summary(dat.tmp) 
    dim(dat.tmp)
    
    mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
    mod.sum <- summary(mod.var)
    
    row.ind = row.ind+1 
    
    mod.out[row.ind, "LandCover"] <- LCtype
    mod.out[row.ind, "PRED"] <- PRED
    mod.out[row.ind, "RESP"] <- RESP
    mod.out[row.ind, "DOY"] <- dayNOW
    
    mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
    mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
    mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
    mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
    mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    mod.out[row.ind, "AIC"] <- AIC(mod.var) 
    
  }
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))

tstat_NDVI.obs_all.SPI_Forest <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs) and Predictors (all SPI) for Forest when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_all.SPI_Forest.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_all.SPI_Forest)
dev.off()


################################
#LME Model for Crop

LCtype <- "crop"
ndvi.test <- ndvi.all[ndvi.all$type==LCtype,]
summary(ndvi.test)

ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)


ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

RESP <- resp.vars[1] #(ndvi.obs)

mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 


row.ind = 0 
for(PRED in pred.vars){
  for(i in 1:length(days.use)){
    dayNOW <- days.use[i] 
    
    dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,]
    dat.tmp$RESP <- dat.tmp[,RESP]
    dat.tmp$PRED <- dat.tmp[,PRED] 
    summary(dat.tmp) 
    dim(dat.tmp)
    
    mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
    mod.sum <- summary(mod.var)
    
    row.ind = row.ind+1 
    
    mod.out[row.ind, "LandCover"] <- LCtype
    mod.out[row.ind, "PRED"] <- PRED
    mod.out[row.ind, "RESP"] <- RESP
    mod.out[row.ind, "DOY"] <- dayNOW
    
    mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
    mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
    mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
    mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
    mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    mod.out[row.ind, "AIC"] <- AIC(mod.var) 
    
  }
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))

tstat_NDVI.obs_all.SPI_Crop <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs) and Predictors (all SPI) for Crop when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_all.SPI_Crop.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_all.SPI_Crop)
dev.off()


################################
#LME Model for Grassland

LCtype <- "grassland"
ndvi.test <- ndvi.all[ndvi.all$type==LCtype,]
summary(ndvi.test)

ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)


ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

RESP <- resp.vars[1] #(ndvi.obs)

mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 


row.ind = 0 
for(PRED in pred.vars){
  for(i in 1:length(days.use)){
    dayNOW <- days.use[i] 
    
    dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,]
    dat.tmp$RESP <- dat.tmp[,RESP]
    dat.tmp$PRED <- dat.tmp[,PRED] 
    summary(dat.tmp) 
    dim(dat.tmp)
    
    mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
    mod.sum <- summary(mod.var)
    
    row.ind = row.ind+1 
    
    mod.out[row.ind, "LandCover"] <- LCtype
    mod.out[row.ind, "PRED"] <- PRED
    mod.out[row.ind, "RESP"] <- RESP
    mod.out[row.ind, "DOY"] <- dayNOW
    
    mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
    mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
    mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
    mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
    mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    mod.out[row.ind, "AIC"] <- AIC(mod.var) 
    
  }
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))

tstat_NDVI.obs_all.SPI_Grassland <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs) and Predictors (all SPI) for Grassland when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_all.SPI_Grassland.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_all.SPI_Grassland)
dev.off()

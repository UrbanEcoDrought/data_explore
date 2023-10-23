library(ggplot2)
library(lubridate)
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
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

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVIVPDNA <- na.omit(ChicagolandSPINDVIVPD)

# Change land cover type to numeric values
levels(ChicagolandSPINDVIVPDNA$type)[levels(ChicagolandSPINDVIVPDNA$type)=='crop'] <-'1'
levels(ChicagolandSPINDVIVPDNA$type)[levels(ChicagolandSPINDVIVPDNA$type)=='forest'] <-'2'
levels(ChicagolandSPINDVIVPDNA$type)[levels(ChicagolandSPINDVIVPDNA$type)=='grassland'] <-'3'
levels(ChicagolandSPINDVIVPDNA$type)[levels(ChicagolandSPINDVIVPDNA$type)=='urban-high'] <-'4'
levels(ChicagolandSPINDVIVPDNA$type)[levels(ChicagolandSPINDVIVPDNA$type)=='urban-low'] <-'5'
levels(ChicagolandSPINDVIVPDNA$type)[levels(ChicagolandSPINDVIVPDNA$type)=='urban-medium'] <-'6'
levels(ChicagolandSPINDVIVPDNA$type)[levels(ChicagolandSPINDVIVPDNA$type)=='urban-open'] <-'7'
summary(ChicagolandSPINDVIVPDNA)

# Change land cover type to numeric levels
ChicagolandSPINDVIVPDNA$type <- as.numeric(levels(ChicagolandSPINDVIVPDNA$type))[ChicagolandSPINDVIVPDNA$type]

# Simplify column label to VPD
colnames(ChicagolandSPINDVIVPDNA)[14] = 'VPD'
summary(ChicagolandSPINDVIVPDNA)

ChicagolandSPINDVIVPDNA$month <- lubridate::month(ChicagolandSPINDVIVPDNA$date)
days.use <- unique(ChicagolandSPINDVIVPDNA$doy[ChicagolandSPINDVIVPDNA$month >=3 & ChicagolandSPINDVIVPDNA$month <=9])
days.use 
lc.type <- c("1", "2", "3", "4", "5", "6", "7")
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI", "VPD")

mod.out <- data.frame(TYPE = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 

row.ind = 0 

for(RESP in resp.vars){
  for(PRED in pred.vars){
    for(TYPE in lc.type){
        for(i in 1:length(days.use)){
      dayNOW <- days.use[i] 
      
      dat.tmp <- ChicagolandSPINDVIVPDNA[ChicagolandSPINDVIVPDNA$doy>=dayNOW-7 & ChicagolandSPINDVIVPDNA$doy<=dayNOW+7 ,]
      dat.tmp$TYPE <- dat.tmp[,TYPE]
      dat.tmp$RESP <- dat.tmp[,RESP]
      dat.tmp$PRED <- dat.tmp[,PRED] 
      summary(dat.tmp) 
      dim(dat.tmp)
      
      mod.var <- nlme::lme(RESP ~ PRED + TYPE, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
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
    }
  }
  }
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))


tstat_NDVI.obs_anomaly.all.SPI_VPD_LC.types <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs and NDVI.anomaly) and Predictors (all SPI and VPD) for All Lc Types when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/tstat_NDVI.obs_anomaly.all.SPI_VPD_LC.types.png", unit="in", height = 5, width = 10, res = 300)
plot(tstat_NDVI.obs_anomaly.all.SPI_VPD_LC.types)
dev.off()
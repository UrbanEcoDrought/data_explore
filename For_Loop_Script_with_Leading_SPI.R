library(ggplot2)
library(lubridate)
library(dplyr)
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

# Create SPI variables with 5, 10, 15, and 30 days lead time so NDVI is lagged behind
ChicagolandSPI <- ChicagolandSPI %>% mutate(X14d.SPI_lead.5days.past = lead(X14d.SPI, n=5, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X30d.SPI_lead.5days.past = lead(X30d.SPI, n=5, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X60d.SPI_lead.5days.past = lead(X60d.SPI, n=5, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X90d.SPI_lead.5days.past = lead(X90d.SPI, n=5, default = NA))

ChicagolandSPI <- ChicagolandSPI %>% mutate(X14d.SPI_lead.10days.past = lead(X14d.SPI, n=10, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X30d.SPI_lead.10days.past = lead(X30d.SPI, n=10, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X60d.SPI_lead.10days.past = lead(X60d.SPI, n=10, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X90d.SPI_lead.10days.past = lead(X90d.SPI, n=10, default = NA))

ChicagolandSPI <- ChicagolandSPI %>% mutate(X14d.SPI_lead.15days.past = lead(X14d.SPI, n=15, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X30d.SPI_lead.15days.past = lead(X30d.SPI, n=15, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X60d.SPI_lead.15days.past = lead(X60d.SPI, n=15, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X90d.SPI_lead.15days.past = lead(X90d.SPI, n=15, default = NA))

ChicagolandSPI <- ChicagolandSPI %>% mutate(X14d.SPI_lead.30days.past = lead(X14d.SPI, n=30, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X30d.SPI_lead.30days.past = lead(X30d.SPI, n=30, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X60d.SPI_lead.30days.past = lead(X60d.SPI, n=30, default = NA))
ChicagolandSPI <- ChicagolandSPI %>% mutate(X90d.SPI_lead.30days.past = lead(X90d.SPI, n=30, default = NA))

ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
ChicagolandSPINDVI$ndvi.anomaly <- as.vector(ChicagolandSPINDVI$ndvi.anomaly)
summary(ChicagolandSPINDVI)

ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <- c("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI","X14d.SPI_lead.5days.past", "X30d.SPI_lead.5days.past", "X60d.SPI_lead.5days.past", "X90d.SPI_lead.5days.past", "X14d.SPI_lead.10days.past", "X30d.SPI_lead.10days.past", "X60d.SPI_lead.10days.past", "X90d.SPI_lead.10days.past", "X14d.SPI_lead.15days.past", "X30d.SPI_lead.15days.past", "X60d.SPI_lead.15days.past", "X90d.SPI_lead.15days.past","X14d.SPI_lead.30days.past", "X30d.SPI_lead.30days.past", "X60d.SPI_lead.30days.past", "X90d.SPI_lead.30days.past")

mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 


row.ind = 0 
for(RESP in resp.vars){
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
}
summary(mod.out)
head(mod.out)

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))

t.stat_NDVI.obs.anomaly_all.SPI_with.leads_Urban.Medium <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "t.stat of Response (NDVI.obs/anomaly) and Predictors (all SPI and leads) for Urban Medium when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/t.stat_NDVI.obs.anomaly_all.SPI_with.leads_Urban.Medium.png", unit="in", height = 5, width = 10, res = 300)
plot(t.stat_NDVI.obs.anomaly_all.SPI_with.leads_Urban.Medium)
dev.off()

AIC_NDVI.obs.anomaly_all.SPI_with.leads_Urban.Medium <- ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=AIC)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)+
  labs(title = "AIC of Response (NDVI.obs/anomaly) and Predictors (all SPI and leads) for Urban Medium when p-value is significant")

png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/LME tstats/AIC_NDVI.obs.anomaly_all.SPI_with.leads_Urban.Medium.png", unit="in", height = 5, width = 10, res = 300)
plot(AIC_NDVI.obs.anomaly_all.SPI_with.leads_Urban.Medium)
dev.off()

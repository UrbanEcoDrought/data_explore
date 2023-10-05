# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)

unique(ndvi.all$type)

# for now let's just look at full years, so cutting 2023
ndvi.all <- ndvi.all[!ndvi.all$year %in% 2023,]

# some orienting plots
ggplot(data=ndvi.all[!ndvi.all$year %in% c(2005,2012),]) + facet_wrap(type~.) +
  stat_smooth(aes(x=doy, y=NDVI)) +
  geom_line(data=ndvi.all[ndvi.all$year %in% c(2005, 2012),], aes(x=doy, y=NDVI, col=as.factor(year)))


# we do have duplicates in dates as the collections were taken by different satellites
# running a brief script to take the average of the dates per cover type. Note: some of the measurements appear to be identical for replicate dates and others are close. Will take the mean for now


ndvi.all2 <- aggregate(NDVI~date + type, FUN=mean, data=ndvi.all)
head(ndvi.all2)

# separating ndvi object into individual land-cover objects
ndvi.crop <- ndvi.all2[ndvi.all2$type=="crop",]
ndvi.forest <- ndvi.all2[ndvi.all2$type=="forest",]
ndvi.grass <- ndvi.all2[ndvi.all2$type=="grassland",]
ndvi.hi <- ndvi.all2[ndvi.all2$type=="urban-high",]
ndvi.lo <- ndvi.all2[ndvi.all2$type=="urban-low",]
ndvi.med <- ndvi.all2[ndvi.all2$type=="urban-medium",]
ndvi.open <- ndvi.all2[ndvi.all2$type=="urban-open",]

summary(ndvi.open)


## Want to think about the above plot WRT creating an NDVI-anaomalies dataset
# missing days may prove to be an issue

# NDVI correlations----
# creating a correlation matrix for NDVI
# Crop and Forest
ndvi.cor <- merge(ndvi.crop[,c("date", "NDVI")], ndvi.forest[,c("date", "NDVI")], by="date", all=T)
summary(ndvi.cor)
names(ndvi.cor) <- c("date", "crop", "forest")

# adding grassland
ndvi.cor2 <- merge(ndvi.cor, ndvi.grass[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor2) <- paste(c(names(ndvi.cor),"grassland"))
head(ndvi.cor2)

# Adding medium urban
ndvi.cor3 <- merge(ndvi.cor2, ndvi.med[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor3) <- paste(c(names(ndvi.cor2),"urban.med"))
head(ndvi.cor3)

# adding med urban
ndvi.cor4 <- merge(ndvi.cor3, ndvi.lo[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor4) <- paste(c(names(ndvi.cor3),"urban.low"))
head(ndvi.cor4)

# Adding open urban
ndvi.cor5 <- merge(ndvi.cor4, ndvi.open[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor5) <- paste(c(names(ndvi.cor4),"urban.open"))
head(ndvi.cor5)

# Adding high urban
ndvi.cor6 <- merge(ndvi.cor5, ndvi.hi[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor6) <- paste(c(names(ndvi.cor5),"urban.hi"))
head(ndvi.cor6)


length(unique(ndvi.cor6$date))
dim(ndvi.cor6)
head(ndvi.cor6)
tail(ndvi.cor6)

ndvi.cor.mat <- as.matrix(ndvi.cor6[,!names(ndvi.cor6) %in% "date"])
row.names(ndvi.cor.mat) <- paste(ndvi.cor6$date)

cor.ndvi <- cor(ndvi.cor.mat, use="pairwise.complete.obs")
# heatmap(cor.ndvi)

cov.ndvi <- cov(ndvi.cor.mat, use="pairwise.complete.obs")
# heatmap(cov.ndvi)

# trying this presentation of correlation out for nowdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABwAAAAcCAYAAAByDd+UAAAC10lEQVR42u2W20vTYRjHva/+jQkSEzrQRdB9VwXdVEvYxAJnUSDzwlDotBZqTrYfLdq0aefVDMNp22JbOxVGB6OMrY3pclP7CSUTB7/s7f1OFmv+jtpV9MLD7/x8fu/zfp/neWtq/g86ahsd22p11gMqndVI7b5Kx/ioeVQ6ixP38AzvbBpUp2e2UodNO1rsYb0tkLjqmWIHItmVWy/nV/ujuR+93vRyh+s9q+n1J+qb7WG8u2Ew/nr7CVuobTCaevR6gXO/Zcndia9kID5ProfzhAnliBUWzJG+QI5cHs9wmr5Auq7JFsK3CkPIaPYZhuK3Y9nCk8lFcmdigdgopOfZLDE9zZKLY1lyYWymdDTSa5M3S654v5Su21yfCrtPO+PwIQumarTs39vqjD18lS8+frdI7NE50u1fc3beMyNo+AEYztuHk8Vdp/pj8CUKUzd0bUEYneHMEmAIHWZUdgQLJr6JgsvWeu/DEsILHQjPTsscN9yMphBGR2yuFKpqRxhyoUfMwRR8Ckp/Z4s94qYCwZr1+Gf/mFklUC60cyTDqZtvRHiVq9JaDuptwQTUeO15XnDNKocc6KEubwK+ecRivdTt+ciWZkfVKOSgekhBTzrfsCgO64G0gtCkLiLPIBS5QClouztZhG8eIONDBYEyxVJAaAhBz41Or8I33wxHHbRcoYLwiUUKGEp+Vwpkhsy+9DLKldIZCsHEQ0pF0+GaZFEbjQrWUAwmLhoqXY3Zn0Ah5kt4PqAUTDQtkJz1NElN49McCrHQOiqBiSZ+ubQd7Qumyt1ACCgHJlnayg0XBdfwYKog5EQuDG1KsniXG++eM4Mvzg5/LspxzKtM+i18yG7EKq21AU0Uf6kU9rsB6yzHFG8xEJLD5kC6cyTNSQskzeHdDW0xKpWLjZGabpAgb+QUTeQVWj1+wnCOe3im3uwmah2Y5lLVNtG3dk7v0Wd/BfRPjF/sOXqT33GGYwAAAABJRU5ErkJggg==
# found here https://r-coder.com/correlation-plot-r/
library(PerformanceAnalytics)

chart.Correlation(ndvi.cor.mat, histogram = TRUE, method = "pearson")

# Daily correlations----
# wanting to look at a 'daily' correlation.
# Will need to rewrite some code to generate a daily correlation

# dont' want to have any NA's will get to complete cases here

ndvi.all.cc <- ndvi.cor6
ndvi.all.cc$doy <- yday(ndvi.all.cc$date)
summary(ndvi.all.cc)

ndvi.all.cc$year <- year(ndvi.all.cc$date)
ndvi.all.cc$month <- month(ndvi.all.cc$date)

summary(ndvi.all.cc)

# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

# create column with date in ISO format
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")

# merge ChicagolandSPI and NDVIomitNA2022 by date columns
ChicagolandSPINDVI.all.cc <- merge (ChicagolandSPI, ndvi.all.cc, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVI.all.ccNA <- na.omit(ChicagolandSPINDVI.all.cc)

# reading in Trent's VPD data
ChicagolandVPD <- read.csv(file.path(google.drive, "data/data_sets/Chicagoland_Daily_VPD.csv"))

# create column with date in ISO format
ChicagolandVPD$date <- as.Date(ChicagolandVPD$Date, "%m/%d/%Y")

# merge ChicagolandVPD and ChicagolandSPINDVINA by date columns
ChicagolandSPINDVIVPD.all.cc <- merge (ChicagolandSPINDVI.all.ccNA <- na.omit(ChicagolandSPINDVI.all.cc), ChicagolandVPD, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVIVPD.all.ccNA <- na.omit(ChicagolandSPINDVIVPD.all.cc)
head(ChicagolandSPINDVIVPD.all.ccNA)

# Simplify column label to VPD
colnames(ChicagolandSPINDVIVPD.all.ccNA)[18] = 'VPD'
head(ChicagolandSPINDVIVPD.all.ccNA)

# Remove unneeded columns
ChicagolandSPINDVIVPD.all.ccNA <- ChicagolandSPINDVIVPD.all.ccNA[,-c(2,17)]
head(ChicagolandSPINDVIVPD.all.ccNA)

# limiting days use to March - september
days.use <- unique(ChicagolandSPINDVIVPD.all.ccNA$doy[ndvi.all.cc$month >=3 & ndvi.all.cc$month <=9])
vars.resp <- names(ChicagolandSPINDVIVPD.all.ccNA)[!names(ChicagolandSPINDVIVPD.all.ccNA) %in% c("date", "doy", "year", "month")] # set response variable
vars.pred <- names(ChicagolandSPINDVIVPD.all.ccNA)[!names(ChicagolandSPINDVIVPD.all.ccNA) %in% c("date", "doy", "year", "month")] # set predictor variable; note will be the same fo rnow.

mod.out <- data.frame(doy=rep(days.use), 
                      resp=rep(rep(vars.resp, each=length(days.use)), length.out=length(days.use)*length(vars.resp)*length(vars.pred)),
                      pred=rep(vars.pred, each=length(days.use)*length(vars.resp)), 
                      t.stat=NA, p.val=NA, r.sq.m=NA)
mod.out$resp <- factor(mod.out$resp, levels=(vars.resp))
mod.out$pred <- factor(mod.out$pred, levels=vars.pred)
summary(mod.out)

# adding in date for later
mod.out$month <- month(ChicagolandSPINDVIVPD.all.ccNA$date[match(mod.out$doy, ChicagolandSPINDVIVPD.all.ccNA$doy)], label=F)
mod.out$month.name <- month(ChicagolandSPINDVIVPD.all.ccNA$date[match(mod.out$doy, ChicagolandSPINDVIVPD.all.ccNA$doy)], label=T, abbr=T)
date.temp <- as.Date(ChicagolandSPINDVIVPD.all.ccNA$date[match(mod.out$doy, ChicagolandSPINDVIVPD.all.ccNA$doy)])

mod.out$month.day <- format(as.Date(date.temp), "%d-%m")
head(mod.out)

# Looping through all of the models we could possibly want
pb <- txtProgressBar(min=0, max=nrow(mod.out), style = 3)
pb.ind <- 1
for(i in days.use){
  # Subset to just crossdated samples
  dat.tmp <- ChicagolandSPINDVIVPD.all.ccNA[ChicagolandSPINDVIVPD.all.ccNA$doy==i,]
  
  for(VAR in vars.pred){
    # dat.tmp$PRED <- dat.tmp[,VAR]
    dat.tmp$PRED <- dat.tmp[,VAR]
    dat.tmp <- dat.tmp[!is.na(dat.tmp$PRED),]
    if(VAR=="prcp.mm"){
      dat.tmp$PRED[dat.tmp$PRED<=1e-6] <- 1e-6
      dat.tmp$PRED <- log(dat.tmp$PRED)
    }
    
    
    for(RESP in vars.resp){
      # Update our Progress bar
      setTxtProgressBar(pb, pb.ind)
      pb.ind = pb.ind+1
      
      if(VAR==RESP) next
      
      out.ind <- which(mod.out$pred==VAR & mod.out$doy==i & mod.out$resp==RESP)
      
      if(!is.na(mod.out[out.ind, "t.stat"])) next
      
      # Set up the response variable for our model to make it generalzied
      dat.tmp$RESP <- dat.tmp[,RESP]
      
      # Rather than having true 0 in the response, make it very small
      dat.tmp <- dat.tmp[!is.na(dat.tmp$RESP),]
      # dat.tmp <- dat.tmp[dat.tmp$RWI>=0.1,] # At least for now; drop missing/VERY tiny rings
      # summary(dat.tmp[dat.tmp$RESP<0.01,])
      if(RESP=="rw"){
        dat.tmp$RESP <- log(dat.tmp$RESP)
      }
      
      
      # Run a simple mixed-effect model & save the summary so we can get the t-table
      # mod.var <- nlme::lme(RESP ~ PRED, random=list(TreeID=~1, CoreID=~1), data=dat.tmp[dat.tmp$CoreID!="608012B",], na.action=na.omit)
      
      # system.time(
      mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[!is.na(dat.tmp$RESP),], na.action=na.omit)
      # )
      mod.sum <- summary(mod.var)
      
      # Save our t-stat & pvalue for the climate predictor
      mod.out[out.ind, "t.stat"] <- mod.sum$tTable["PRED","t-value"]
      mod.out[out.ind, "p.val"] <- mod.sum$tTable["PRED","p-value"]
      mod.out[out.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    }
  }
}

# error 71%Error in chol.default((value + t(value))/2) : 
# the leading minor of order 1 is not positive
# In addition: Warning message:
#  'r.squaredGLMM' now calculates a revised statistic. See the help page. 

# retrying with only VPD added

# Remove unneeded columns
head(ChicagolandSPINDVIVPD.all.ccNA)

# Create data frame with NDVI and VPD
ChicagolandVPDNDVI.all.ccNA <- ChicagolandSPINDVIVPD.all.ccNA[,-c(2,3,4,5)]
head(ChicagolandVPDNDVI.all.ccNA)

# limiting days use to March - September
days.use <- unique(ChicagolandVPDNDVI.all.ccNA$doy[ndvi.all.cc$month >=3 & ndvi.all.cc$month <=9])
vars.resp <- names(ChicagolandVPDNDVI.all.ccNA)[!names(ChicagolandVPDNDVI.all.ccNA) %in% c("date", "doy", "year", "month")] # set response variable
vars.pred <- names(ChicagolandVPDNDVI.all.ccNA)[!names(ChicagolandVPDNDVI.all.ccNA) %in% c("date", "doy", "year", "month")] # set predictor variable; note will be the same fo rnow.

mod.out <- data.frame(doy=rep(days.use), 
                      resp=rep(rep(vars.resp, each=length(days.use)), length.out=length(days.use)*length(vars.resp)*length(vars.pred)),
                      pred=rep(vars.pred, each=length(days.use)*length(vars.resp)), 
                      t.stat=NA, p.val=NA, r.sq.m=NA)
mod.out$resp <- factor(mod.out$resp, levels=(vars.resp))
mod.out$pred <- factor(mod.out$pred, levels=vars.pred)
summary(mod.out)

# adding in date for later
mod.out$month <- month(ChicagolandVPDNDVI.all.ccNA$date[match(mod.out$doy, ChicagolandVPDNDVI.all.ccNA$doy)], label=F)
mod.out$month.name <- month(ChicagolandVPDNDVI.all.ccNA$date[match(mod.out$doy, ChicagolandVPDNDVI.all.ccNA$doy)], label=T, abbr=T)
date.temp <- as.Date(ChicagolandVPDNDVI.all.ccNA$date[match(mod.out$doy, ChicagolandVPDNDVI.all.ccNA$doy)])

mod.out$month.day <- format(as.Date(date.temp), "%d-%m")
head(mod.out)

# Looping through all of the models we could possibly want
pb <- txtProgressBar(min=0, max=nrow(mod.out), style = 3)
pb.ind <- 1
for(i in days.use){
  # Subset to just crossdated samples
  dat.tmp <- ChicagolandVPDNDVI.all.ccNA[ChicagolandVPDNDVI.all.ccNA$doy==i,]
  
  for(VAR in vars.pred){
    # dat.tmp$PRED <- dat.tmp[,VAR]
    dat.tmp$PRED <- dat.tmp[,VAR]
    dat.tmp <- dat.tmp[!is.na(dat.tmp$PRED),]
    if(VAR=="prcp.mm"){
      dat.tmp$PRED[dat.tmp$PRED<=1e-6] <- 1e-6
      dat.tmp$PRED <- log(dat.tmp$PRED)
    }
    
    
    for(RESP in vars.resp){
      # Update our Progress bar
      setTxtProgressBar(pb, pb.ind)
      pb.ind = pb.ind+1
      
      if(VAR==RESP) next
      
      out.ind <- which(mod.out$pred==VAR & mod.out$doy==i & mod.out$resp==RESP)
      
      if(!is.na(mod.out[out.ind, "t.stat"])) next
      
      # Set up the response variable for our model to make it generalzied
      dat.tmp$RESP <- dat.tmp[,RESP]
      
      # Rather than having true 0 in the response, make it very small
      dat.tmp <- dat.tmp[!is.na(dat.tmp$RESP),]
      # dat.tmp <- dat.tmp[dat.tmp$RWI>=0.1,] # At least for now; drop missing/VERY tiny rings
      # summary(dat.tmp[dat.tmp$RESP<0.01,])
      if(RESP=="rw"){
        dat.tmp$RESP <- log(dat.tmp$RESP)
      }
      
      
      # Run a simple mixed-effect model & save the summary so we can get the t-table
      # mod.var <- nlme::lme(RESP ~ PRED, random=list(TreeID=~1, CoreID=~1), data=dat.tmp[dat.tmp$CoreID!="608012B",], na.action=na.omit)
      
      # system.time(
      mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[!is.na(dat.tmp$RESP),], na.action=na.omit)
      # )
      mod.sum <- summary(mod.var)
      
      # Save our t-stat & pvalue for the climate predictor
      mod.out[out.ind, "t.stat"] <- mod.sum$tTable["PRED","t-value"]
      mod.out[out.ind, "p.val"] <- mod.sum$tTable["PRED","p-value"]
      mod.out[out.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
    }
  }
}

# received same error

summary(mod.out)
summary(mod.out[!is.na(mod.out$p.val),])
# saveRDS(mod.out, file.path(google.drive,"data/r_files/processed_files", "NDVI_daily_cors.RDS"))  
# ----------------------------------

# ----------------------------------
# Plotting and exploring the output
# ----------------------------------
mod.out <- readRDS(file.path(google.drive,"data/r_files/processed_files", "NDVI_daily_cors.RDS"))
summary(mod.out)

# creating a df of month breaks for plotting
month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


# resettign month.name to be ordered

# yrs.mark <- data.frame(Label=c("p.Oct 1", "Jan 1", "Apr 1", "Jul 1", "Oct 1"), 
#                        Date=c("2018-10-01", "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01"))
# yrs.mark$mark.doy <- lubridate::yday(yrs.mark$Date)
# yrs.mark$mark.doy[1] <- yrs.mark$mark.doy[1]-365

#Figures----
head(mod.out)
# ordering the TR data variables
# mod.out$resp <- factor(mod.out$resp, levels = c("lw", "ew", "rw"))

PRGn5 <- c("#7b3294", "#c2a5cf", "gray50", "#a6dba0", "#008837")


#pdf(file = file.path(google.drive,"data/r_files/figures/NDVICorr_r-val.pdf"), height=8, width=10)
#for(i in unique(mod.out$pred)){
#  print(
#    ggplot(data=mod.out[mod.out$pred==i & mod.out$p.val <0.05 & !is.na(mod.out$p.val),]) +
#      facet_grid(~pred, scales="free") +
#      ggtitle(paste0(i, " Landsat NDVI-NDVI temporal Corr.: r-value; alpha=0.05")) +
#      geom_tile(data=mod.out[mod.out$pred==i & mod.out$p.val > 0.05 & !is.na(mod.out$p.val),], aes(x=doy, y=resp), fill="grey50") +
#      geom_tile(aes(x=doy, y=resp, fill=sqrt(r.sq.m)*sign(t.stat))) +
#      scale_y_discrete(name="Land Cover Class", expand=c(0,0)) +
#      scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks$doy, labels = month.breaks$month) +
#      scale_fill_gradientn(name="Marginal\nR-value", colors=PRGn5, limits=max(sqrt(mod.out$r.sq.m))*c(-1,1))+
#      theme(legend.position="top")
#  )
#}
#dev.off()




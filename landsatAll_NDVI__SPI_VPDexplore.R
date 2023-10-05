# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))
head(ndvi.all)

unique(ndvi.all$type)

# for now let's just look at full years, so cutting 2023
ndvi.all <- ndvi.all[!ndvi.all$year %in% 2023,]

# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

# create column with date in ISO format
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")

# merge ChicagolandSPI and NDVIomitNA2022 by date columns
ChicagolandSPINDVI.all.cc <- merge (ChicagolandSPI, ndvi.all.cc, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVI.all.NA <- na.omit(ChicagolandSPINDVI.all.cc)

# reading in Trent's VPD data
ChicagolandVPD <- read.csv(file.path(google.drive, "data/data_sets/Chicagoland_Daily_VPD.csv"))

# create column with date in ISO format
ChicagolandVPD$date <- as.Date(ChicagolandVPD$Date, "%m/%d/%Y")

# merge ChicagolandVPD and ChicagolandSPINDVINA by date columns
ChicagolandSPINDVIVPD.all.cc <- merge (ChicagolandSPINDVI.all.ccNA <- na.omit(ChicagolandSPINDVI.all.cc), ChicagolandVPD, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVI.all.ccNA <- na.omit(ChicagolandSPINDVIVPD.all.cc)
head(ChicagolandSPINDVI.all.ccNA)

# Simplify column label to VPD
colnames(ChicagolandSPINDVI.all.ccNA)[18] = 'VPD'
head(ChicagolandSPINDVI.all.ccNA)

# Remove unneeded columns
ChicagolandSPINDVIVPD.all.ccNA <- ChicagolandSPINDVIVPD.all.ccNA[,-c(2,17)]
head(ChicagolandSPINDVIVPD.all.ccNA)

# limiting days use to March - september
days.use <- unique(ChicagolandSPINDVIVPD.all.ccNA$doy[ndvi.all.cc$month >=3 & ndvi.all.cc$month <=9])
vars.resp <- names(ChicagolandSPINDVIVPD.all.ccNA)[!names(ChicagolandSPINDVIVPD.all.ccNA) %in% c("date", "doy", "year", "month")] # set response variable
vars.pred <- names(ChicagolandSPINDVIVPD.all.ccNA)[!names(ChicagolandSPINDVIVPD.all.ccNA) %in% c("date", "doy", "year", "month")] # set predictor variable
  
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


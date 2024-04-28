# Creating a clean skip to re-do the daily correlation modeling and do some prediction from it



# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")


path.figs <- file.path(google.drive, "data/exploratory figures/daily models")
if(!dir.exists(path.figs)) dir.create(path.figs)
# path.landsat <- file.path(google.drive, "Neighborhood remote sensing analysis/Landsat NDVI")
# dir(path.landsat)
pathSave <- file.path(google.drive, "data/r_files/processed_files")

# Going back to the raw landsat data -- lookgin for somethign that has the satellite attached to know it's raw.
# # I don't know where this was created, but it's what we have -- we'll need to go back and figure this out at some point
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)
summary(ndvi.all)


# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))
ChicagolandSPEI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPEI.csv"))
ChicagolandTemp <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_Temps.csv"))

# create column with date in ISO format; making it lowercase "date" so that it merges easier
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
ChicagolandSPEI$date <- as.Date(ChicagolandSPEI$Date, "%m/%d/%Y")
ChicagolandTemp$date <- as.Date(ChicagolandTemp$Date, "%m/%d/%Y")
summary(ChicagolandSPI)
summary(ChicagolandSPEI)
summary(ChicagolandTemp)

dim(ChicagolandSPI); dim(ChicagolandSPEI); dim(ChicagolandTemp)


ndviMet <- merge(ndvi.all, ChicagolandTemp, all.x=T, all.y=F)
ndviMet <- merge(ndviMet, ChicagolandSPI, all.x=T, all.y=F)
ndviMet <- merge(ndviMet, ChicagolandSPEI, all.x=T, all.y=F)
summary(ndviMet)

# saving ndviMet to the data drive so that predictors are paired together with the NDVI data
saveRDS(ndviMet, file = file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_metVars_combined.RDS"))
write.csv(ndviMet, file = file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_metVars_combined.csv"), row.names=F)
#########################################
# Starting with just looking at urban medium through time ----
#########################################
for(LC in unique(ndviMet$type)){
  print(LC)
  # Subset the data 
  datLC <- ndviMet[ndviMet$type==LC,]
  
  # Checking the autocorrelation in NDVI
  head(datLC)
  acf(datLC$NDVI[!is.na(datLC$NDVI)])
  # acf(datLC$resid[!is.na(datLC$resid)]) # note: need to run below for this to work!
  
  
  # Creating a 14-day NDVI lag (day -14), that goes across satellites to try to bring in autocorrleation
  # May need a longer window, but we'll see
  datLC$NDVI.Lag14d <- NA
  for(i in 1:nrow(datLC)){
    rowLag <- which(datLC$date>=(datLC$date[i]-14) & datLC$date<datLC$date[i])
    
    if(length(rowLag)<1 ) next
    if(length(rowLag)==1) datLC$NDVI.Lag14d[i] <- datLC$NDVI[rowLag]
    if(length(rowLag)>1) datLC$NDVI.Lag14d[i] <- mean(datLC$NDVI[rowLag], na.rm=T)
    
  }
  summary(datLC)
  
  # ggplot(data=datLC[datLC$year==2020,]) +
    # ggtitle(paste0(LC, "; Year = 2012")) +
  #   # geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
  #   geom_line(aes(x=doy, y=NDVI, group=satellite), color="black") +
  #   geom_line(aes(x=doy, y=NDVI.Lag14d), color="gray50")
  
  
  # Do some quick graphs to check to make sure we can see a signal we're looking for
  # showing 2012 in black
  png(file.path(path.figs, paste0("NDVI_", LC, "_Raw_bySatellite_2012.png")), height=6, width=6, units="in", res=220)
  ggplot(data=datLC, aes(x=doy, y=NDVI)) +
    ggtitle(paste0(LC, "; Year = 2012")) +
    geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
    geom_line(data=datLC[datLC$year==2012,], aes(group=satellite), color="black")
  dev.off()
  
  png(file.path(path.figs,  paste0("NDVI_", LC, "_Raw_bySatellite_2005.png")), height=6, width=6, units="in", res=220)
  print(
    ggplot(data=datLC, aes(x=doy, y=NDVI)) +
    ggtitle(paste0(LC, "; Year = 2005")) +
    geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
    geom_line(data=datLC[datLC$year==2005,], aes(group=satellite), color="black")
  )
  dev.off()
  
  png(file.path(path.figs,  paste0("NDVI_", LC, "_Raw_bySatellite_2023.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC, aes(x=doy, y=NDVI)) +
    ggtitle(paste0(LC, "; Year = 2023")) +
    geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
    geom_line(data=datLC[datLC$year==2023,], aes(group=satellite), color="black"))
  dev.off()
  
  
  # Starting with doing a simple model of NDVI ~ 
  days.use <- 1:365
  modsList <- list()
  modsLag <- list()
  modsNorm <- list()
  
  mod.out <- data.frame(landcover=LC, yday=1:365, intercept=NA, coef.Lag=NA, coef.SPEI30=NA, coef.Tmin30=NA, pVal.Lag=NA, pVal.SPEI30=NA, pVal.Tmin30=NA, rSq.Process=NA, rSq.Lag=NA, rSq.Norm=NA) 
  
  datLC$NDVI.pred <- NA # Setting up a palceholder for predicted values
  datLC$NDVI.predLag <- NA # Setting up a palceholder for predicted values
  datLC$NDVI.predNorm <- NA # Setting up a palceholder for predicted values
  # row.ind = 0 # Setting up an index that will tell us what row to save things in; we should start with 0 because we haven't done anything yet
  pb <- txtProgressBar(min=0, max=nrow(mod.out), style=3)
  for(i in 1:nrow(mod.out)){
    setTxtProgressBar(pb, i)
    # For testing using i=185 (which is July 4; yday(as.Date("2021-07-04"))) -- this is a period that should have a decent SPI relationship based off of the initial corr plots
    # dayNOW <- days.use[i] # This is almost exactly the same as above, but now i will go from 1 to 215 (the number of unique days.use we have)
    dayNOW = i # Repurposing old code, so doing a clunky approach here
    
    ## Using an even-sided window to train the model for now to understand the relationships
    # Here we're subsetting our big data frame to the SMALL temporal window we want --> this should help with temporal stationarity in the effects of our predictors
    rowNow <- which(datLC$doy>=dayNOW-7 & datLC$doy<=dayNOW+7 )
    dat.tmp <- datLC[rowNow,] # Subsets things to a particular window (not just a single day); otherwise we were working with just 5 years of data, which isn't helpful
    # summary(dat.tmp)
    
    # Doing some graphing that we're not saving for our own sanity
    # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=NDVI, fill=satellite), scale="width")
    # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=X30d.SPI, fill=satellite), scale="width")
    # ggplot(data=dat.tmp, aes(x=X30d.SPI, y=NDVI)) + geom_point(aes(color=satellite)) + stat_smooth(method="lm")
     
    #Set up a normal (intercept-only) model
    modN <- nlme::lme(NDVI ~ 1, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsNorm[[i]] <- modN
    
    #Set up a lag-only model
    modL <- nlme::lme(NDVI ~NDVI.Lag14d, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsLag[[i]] <- modL
    
    # This is running a pretty basic model --> TMIN30d shouldn't have a big impact in the summer, but we'll keep it to see what happens
    modDay <- nlme::lme(NDVI ~ X30d.SPEI + TMIN30d + NDVI.Lag14d, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsList[[i]] <- modDay
    sumMod <- summary(modDay)
    # MuMIn::r.squaredGLMM(modDay)[,"R2m"]
  
    # Storing key stats about the model
    mod.out[i,"intercept"] <- modDay$coefficients$fixed["(Intercept)"]
    mod.out[i,"coef.Lag"] <- modDay$coefficients$fixed["NDVI.Lag14d"]
    mod.out[i,"coef.SPEI30"] <- modDay$coefficients$fixed["X30d.SPEI"]
    mod.out[i,"coef.Tmin30"] <- modDay$coefficients$fixed["TMIN30d"]
    mod.out[i,"tStat.Lag"] <- sumMod$tTable["NDVI.Lag14d", "t-value"]
    mod.out[i,"tStat.SPEI30"] <- sumMod$tTable["X30d.SPEI", "t-value"]
    mod.out[i,"tStat.Tmin30"] <- sumMod$tTable["TMIN30d", "t-value"]
    mod.out[i,"pVal.Lag"] <- sumMod$tTable["NDVI.Lag14d", "p-value"]
    mod.out[i,"pVal.SPEI30"] <- sumMod$tTable["X30d.SPEI", "p-value"]
    mod.out[i,"pVal.Tmin30"] <-sumMod$tTable["TMIN30d", "p-value"]
    mod.out[i, "rSq.Process"] <- MuMIn::r.squaredGLMM(modDay)[,"R2m"]
    mod.out[i, "rSq.Lag"] <- MuMIn::r.squaredGLMM(modL)[,"R2m"]
    mod.out[i, "rSq.Norm"] <- MuMIn::r.squaredGLMM(modN)[,"R2m"]
    
  }
  summary(mod.out)
  head(mod.out)
  
  write.csv(mod.out, file.path(pathSave, paste0("DailyModel_Stats_", LC, ".csv")), row.names=F)
  
  # Stacking so we can do a new daily corr figure
  effectStack <- stack(mod.out[,grep("tStat", names(mod.out))])
  names(effectStack) <- c("tStat", "effect")
  effectStack$doy <- mod.out$yday
  effectStack$effect <- gsub("tStat.", "", effectStack$effect) # making clean names
  effectStack$pVal <- stack(mod.out[,grep("pVal", names(mod.out))])[,"values"]
  effectStack$coef <- stack(mod.out[,grep("coef", names(mod.out))])[,"values"]
  
  summary(effectStack)
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Effects_SigOnly.png")), height=6, width=10, units="in", res=220)
  print(ggplot(data=effectStack[effectStack$pVal<0.05,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw())
  dev.off()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Effects_All.png")), height=6, width=10, units="in", res=220)
  print(ggplot(data=effectStack[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw())
  dev.off()
  
  
  # Now predicting from the models --> we need to do this separately from fitting because we want ONE prediction per obs
  # This could be made more efficient 
  for(DAY in unique(datLC$doy)){
    if(DAY == 366) next # Skip leap day
    rowNow <- which(datLC$doy==DAY & !is.na(datLC$X30d.SPI) & !is.na(datLC$TMAX30d) & !is.na(datLC$NDVI.Lag14d))
    
    if(length(rowNow)==0) next # Skip this row if we don't have the predictors we need
    
    datLC$NDVI.pred[rowNow] <- predict(modsList[[DAY]], newdata=datLC[rowNow,])
    datLC$NDVI.predLag[rowNow] <- predict(modsLag[[DAY]], newdata=datLC[rowNow,])
    datLC$NDVI.predNorm[rowNow] <- predict(modsNorm[[DAY]], newdata=datLC[rowNow,])
    
    
  }
  
  # Now looking at the output
  datLC$month <- lubridate::month(datLC$date)
  datLC$resid <- datLC$NDVI - datLC$NDVI.pred
  hist(datLC$resid)
  summary(datLC)
  
  write.csv(datLC, file.path(pathSave, paste0("DailyModel_NDVI-predict_", LC, ".csv")), row.names=F)
  
  
  # Doing some diagnostic plotting
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Residuals_byMonth.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC) +
    ggtitle(LC) +
    facet_wrap(~month) +
    geom_histogram(aes(x=resid)) +
    geom_vline(xintercept = 0, col="red2"))
  dev.off()
  
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Pred-Obs_byMonth.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC) +
    ggtitle(LC) +
    facet_wrap(~month) +
    geom_point(aes(x=NDVI.pred, y=NDVI)) +
    geom_abline(slope=1, intercept = 0, col="red2"))
  dev.off()
  
  png(file.path(path.figs, "NDVI-Model_datLC_SPEI30-Resid_byMonth.png"), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC) +
    ggtitle(LC) +
    facet_wrap(~month) +
    geom_point(aes(x=X30d.SPEI, y=resid)) +
    geom_hline(yintercept = 0, col="red2"))
  dev.off()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_NDVI_2005.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC[datLC$year==2005,]) +
    ggtitle(paste0(LC, " NDVI in Year 2005 (drought year)")) +
    stat_smooth(aes(x=doy, y=NDVI.predNorm, color="normal"), method="gam") +
    geom_point(aes(x=doy, y=NDVI, color="observed")) +
    geom_point(aes(x=doy, y=NDVI.pred, color="predicted-process")) +
    stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
    stat_smooth(aes(x=doy, y=NDVI.predLag, color="predicted-lag only"), method="gam") +
    stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted-process"), method="gam") +
    scale_color_manual(values=c("observed"="red4", "predicted-lag only"="salmon2", "predicted-process"="orange2", normal="black")) +
    scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
    theme_bw())
  dev.off()
  
  # ggplot(data=datLC[datLC$year==2021,]) +
  #   ggtitle(paste0(LC, " NDVI in Year 2021 (drought year)")) +
  #   stat_smooth(aes(x=doy, y=NDVI.predNorm, color="normal"), method="gam") +
  #   geom_point(aes(x=doy, y=NDVI, color="observed")) +
  #   geom_point(aes(x=doy, y=NDVI.pred, color="predicted-process")) +
  #   stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
  #   stat_smooth(aes(x=doy, y=NDVI.predLag, color="predicted-lag only"), method="gam") +
  #   stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted-process"), method="gam") +
  #   scale_color_manual(values=c("observed"="red4", "predicted-lag only"="salmon2", "predicted-process"="orange2", normal="black")) +
  #   scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
  #   theme_bw()
  
  
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_NDVI_2012.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC[datLC$year==2012,]) +
    ggtitle(paste0(LC, " NDVI in Year 2012 (drought year)")) +
    stat_smooth(aes(x=doy, y=NDVI.predNorm, color="normal"), method="gam") +
    geom_point(aes(x=doy, y=NDVI, color="observed")) +
    geom_point(aes(x=doy, y=NDVI.pred, color="predicted-process")) +
    stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
    stat_smooth(aes(x=doy, y=NDVI.predLag, color="predicted-lag only"), method="gam") +
    stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted-process"), method="gam") +
    scale_color_manual(values=c("observed"="red4", "predicted-lag only"="salmon2", "predicted-process"="orange2", normal="black")) +
    scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
    theme_bw())
  dev.off()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_NDVI_2020.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC[datLC$year==2020,]) +
    ggtitle(paste0(LC, " NDVI in Year 2020 (non-drought year)")) +
    stat_smooth(aes(x=doy, y=NDVI.predNorm, color="normal"), method="gam") +
    geom_point(aes(x=doy, y=NDVI, color="observed")) +
    geom_point(aes(x=doy, y=NDVI.pred, color="predicted-process")) +
    stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
    stat_smooth(aes(x=doy, y=NDVI.predLag, color="predicted-lag only"), method="gam") +
    stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted-process"), method="gam") +
    scale_color_manual(values=c("observed"="red4", "predicted-lag only"="salmon2", "predicted-process"="orange2", normal="black")) +
    scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
    theme_bw())
  dev.off()
  
  
  corPredObsJJA <- lm(NDVI ~ NDVI.pred, data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),])
  summary(corPredObsJJA)$r.squared
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Pred-Obs_JuneJulAug.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),], aes(x=NDVI.pred, y=NDVI)) +
    ggtitle(paste0(LC, ": June-July-August NDVI with 1:1 line (pseudo-R2=", round(summary(corPredObsJJA)$r.squared, 2), ")")) +
    geom_point() +
    geom_abline(slope=1, intercept=0, color="red2") +
    theme_bw())
  dev.off()
  
  print("") # Just kicking the label to a new line to make things cleaner
} 
#########################################

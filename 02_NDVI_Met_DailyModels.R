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
summary(ndviMet)

#########################################
# Starting with just looking at urban medium through time ----
#########################################
# Subset the data 
urbMed <- ndviMet[ndviMet$type=="urban-medium",]


# Do some quick graphs to check to make sure we can see a signal we're looking for
# showing 2012 in black
png(file.path(path.figs, "NDVI_UrbanMedium_Raw_bySatellite_2012.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed, aes(x=doy, y=NDVI)) +
  ggtitle("Urban Medium; Year = 2012") +
  geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
  geom_line(data=urbMed[urbMed$year==2012,], aes(group=satellite), color="black")
dev.off()

png(file.path(path.figs, "NDVI_UrbanMedium_Raw_bySatellite_2005.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed, aes(x=doy, y=NDVI)) +
  ggtitle("Year = 2005") +
  geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
  geom_line(data=urbMed[urbMed$year==2005,], aes(group=satellite), color="black")
dev.off()

png(file.path(path.figs, "NDVI_UrbanMedium_Raw_bySatellite_2023.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed, aes(x=doy, y=NDVI)) +
  ggtitle("Year = 2023") +
  geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
  geom_line(data=urbMed[urbMed$year==2023,], aes(group=satellite), color="black")
dev.off()


# Starting with doing a simple model of NDVI ~ 
days.use <- 1:365
modsList <- list()

mod.out <- data.frame(landcover="urban-medium", yday=1:365, intercept=NA, coef.SPI30=NA, coef.Tmin30=NA, pVal.SPI30=NA, pVal.Tmin30=NA, r.sq.m=NA) 

urbMed$NDVI.pred <- NA # Setting up a palceholder for predicted values
# row.ind = 0 # Setting up an index that will tell us what row to save things in; we should start with 0 because we haven't done anything yet
pb <- txtProgressBar(min=0, max=nrow(mod.out), style=3)
for(i in 1:nrow(mod.out)){
  setTxtProgressBar(pb, i)
  # For testing using i=185 (which is July 4; yday(as.Date("2021-07-04"))) -- this is a period that should ahve a decent SPI relationship based off of the initial corr plots
  # dayNOW <- days.use[i] # This is almost exactly the same as above, but now i will go from 1 to 215 (the number of unique days.use we have)
  dayNOW = i # Repurposing old code, so doing a clunky approach here
  
  ## Using an even-sided window to train the model for now to understand the relationships
  # Here we're subsetting our big data frame to the SMALL temporal window we want --> this should help with temporal stationarity in the effects of our predictors
  rowNow <- which(urbMed$doy>=dayNOW-7 & urbMed$doy<=dayNOW+7 )
  dat.tmp <- urbMed[rowNow,] # Subsets things to a particular window (not just a single day); otherwise we were working with just 5 years of data, which isn't helpful
  # summary(dat.tmp)
  
  # Doing some graphing that we're not saving for our own sanity
  # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=NDVI, fill=satellite), scale="width")
  # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=X30d.SPI, fill=satellite), scale="width")
  # ggplot(data=dat.tmp, aes(x=X30d.SPI, y=NDVI)) + geom_point(aes(color=satellite)) + stat_smooth(method="lm")
   
  
  # This is running a pretty basic model --> TMIN30d shouldn't have a big impact in the summer, but we'll keep it to see what happens
  modDay <- nlme::lme(NDVI ~ X30d.SPI + TMIN30d, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
  modsList[[i]] <- modDay
  sumMod <- summary(modDay)
  # MuMIn::r.squaredGLMM(modDay)[,"R2m"]

  # Storing key stats about the model
  mod.out[i,"intercept"] <- modDay$coefficients$fixed["(Intercept)"]
  mod.out[i,"coef.SPI30"] <- modDay$coefficients$fixed["X30d.SPI"]
  mod.out[i,"coef.Tmin30"] <- modDay$coefficients$fixed["TMIN30d"]
  mod.out[i,"pVal.SPI30"] <- sumMod$tTable["X30d.SPI", "p-value"]
  mod.out[i,"coef.Tmin30"] <-sumMod$tTable["TMIN30d", "p-value"]
  mod.out[i, "r.sq.m"] <- MuMIn::r.squaredGLMM(modDay)[,"R2m"]

}
summary(mod.out)
head(mod.out)


# Now predicting from the models --> we need to do this separately from fitting because we want ONE prediction per obs
# This could be made more efficient 
for(DAY in unique(urbMed$doy)){
  if(DAY == 366) next # Skip leap day
  rowNow <- which(urbMed$doy==DAY & !is.na(urbMed$X30d.SPI) & !is.na(urbMed$TMAX30d))
  
  if(length(rowNow)==0) next # Skip this row if we don't have the predictors we need
  
  modNow <- modsList[[DAY]]
  urbMed$NDVI.pred[rowNow] <- predict(modNow, newdata=urbMed[rowNow,])
  
  
}

# Now looking at the output
urbMed$month <- lubridate::month(urbMed$date)
urbMed$resid <- urbMed$NDVI - urbMed$NDVI.pred
hist(urbMed$resid)
summary(urbMed)

# Doing some diagnostic plotting
png(file.path(path.figs, "NDVI-Model_UrbMed_Residuals_byMonth.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed) +
  facet_wrap(~month) +
  geom_histogram(aes(x=resid)) +
  geom_vline(xintercept = 0, col="red2")
dev.off()


png(file.path(path.figs, "NDVI-Model_UrbMed_Pred-Obs_byMonth.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed) +
  facet_wrap(~month) +
  geom_point(aes(x=NDVI.pred, y=NDVI)) +
  geom_abline(slope=1, intercept = 0, col="red2")
dev.off()

png(file.path(path.figs, "NDVI-Model_UrbMed_SPI30-Resid_byMonth.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed) +
  facet_wrap(~month) +
  geom_point(aes(x=X30d.SPI, y=resid)) +
  geom_hline(yintercept = 0, col="red2")
dev.off()

png(file.path(path.figs, "NDVI-Model_UrbMed_NDVI_2005.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed[urbMed$year==2005,]) +
  ggtitle("NDVI in Year 2005 (drought year)") +
  stat_smooth(data=urbMed[,], aes(x=doy, y=NDVI.pred, color="normal"), method="gam") +
  geom_point(aes(x=doy, y=NDVI, color="observed")) +
  geom_point(aes(x=doy, y=NDVI.pred, color="predicted")) +
  stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
  stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted"), method="gam") +
  scale_color_manual(values=c("observed"="red4", "predicted"="orange2", normal="black")) +
  theme_bw()
dev.off()

png(file.path(path.figs, "NDVI-Model_UrbMed_NDVI_2012.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed[urbMed$year==2012,]) +
  ggtitle("NDVI in Year 2012 (drought year)") +
  stat_smooth(data=urbMed[,], aes(x=doy, y=NDVI.pred, color="normal"), method="gam") +
  geom_point(aes(x=doy, y=NDVI, color="observed")) +
  geom_point(aes(x=doy, y=NDVI.pred, color="predicted")) +
  stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
  stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted"), method="gam") +
  scale_color_manual(values=c("observed"="red4", "predicted"="orange2", normal="black")) +
  theme_bw()
dev.off()

png(file.path(path.figs, "NDVI-Model_UrbMed_NDVI_2020.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed[urbMed$year==2020,]) +
  ggtitle("NDVI in Year 2020 (non-drought year)") +
  stat_smooth(data=urbMed[,], aes(x=doy, y=NDVI.pred, color="normal"), method="gam") +
  geom_point(aes(x=doy, y=NDVI, color="observed")) +
  geom_point(aes(x=doy, y=NDVI.pred, color="predicted")) +
  stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
  stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted"), method="gam") +
  scale_color_manual(values=c("observed"="red4", "predicted"="dodgerblue2", normal="black")) +
  theme_bw()
dev.off()


corPredObsJJA <- lm(NDVI ~ NDVI.pred, data=urbMed[urbMed$doy>=yday(as.Date("2001-06-01")) & urbMed$doy<yday(as.Date("2001-09-01")),])
summary(corPredObsJJA)

png(file.path(path.figs, "NDVI-Model_UrbMed_Pred-Obs_JuneJulAug.png"), height=6, width=6, units="in", res=220)
ggplot(data=urbMed[urbMed$doy>=yday(as.Date("2001-06-01")) & urbMed$doy<yday(as.Date("2001-09-01")),], aes(x=NDVI.pred, y=NDVI)) +
  ggtitle("June-July-August NDVI with 1:1 line (pseudo-R2=0.37") +
  geom_point() +
  geom_abline(slope=1, intercept=0, color="red2") +
  theme_bw()
dev.off()
  
#########################################

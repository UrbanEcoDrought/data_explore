# cleaned Detrending code

# This runs the double detrending method that we use to create the NDVI anomalies time series
# To see the full messy workflow see '01_NDVI_detrending.R'

# script to detrend NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)
library(mgcv)
# library(dplR) # tree ring package I like to use to explore detrending sometimes
# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))


# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)

# wanting to see how many missing dates we actually have throughout the year- this can affect the VCI calculation.
# creating a continuous timeline of dates
time.all <- data.frame(date = seq(ymd('2001-01-05'), ymd(max(ndvi.all$date)), by="1 day"))
time.all$doy <- yday(time.all$date)
time.all$year <- year(time.all$date)
summary(time.all)

# merging together to look at missing data
time.check <- merge(ndvi.all, time.all, by=c("date", "doy", "year"), all=T)
summary(time.check)

time.check$NDVI[is.na(time.check$NDVI)] <- -99

ggplot(data=time.check) + facet_grid(type~year)+
  geom_point(aes(x=doy, y=NDVI, col=satellite))



length(time.check$NDVI[!is.na(time.check$type) & !is.na(time.check$satellite)])
length(time.check$NDVI[!is.na(time.check$type) & !is.na(time.check$satellite) & time.check$NDVI == -99])

dim(time.check)

# checking growing season data
time.check$month <- month(time.check$date)
tc.gs <- time.check[!is.na(time.check$type) & !is.na(time.check$satellite) & time.check$month %in% c(4:9),]

nrow(tc.gs)
nrow(tc.gs[tc.gs$NDVI==-99,])

ndvi.na.check <- aggregate(NDVI~doy+satellite, FUN=length, data=ndvi.all)

ggplot(data=ndvi.all) + facet_wrap(type~.)+
  geom_histogram(aes(x=doy, fill=satellite), binwidth = 1)


# seeing if we can correct for differences among satellites with referencing to lansat 8
ls8.dat <- ndvi.all[ndvi.all$satellite=="landsat 8",]
ls9.dat <- ndvi.all[ndvi.all$satellite=="landsat 9",]


ggplot() + facet_wrap(type~.)+ 
  labs(title="Landsat 8") +
  geom_line(data=ls8.dat, aes(x=doy, y=NDVI, col=as.factor(year)))


# seeing some differences across satellites. I think we will want to account for this when making an anomaly time series

summary(ndvi.all)

###############################
# Fancy detrending----


ndvi.gamm.step1 <- gamm(NDVI ~ type + s(doy, k=12, by=as.factor(type)), random=list(satellite=~1), data=ndvi.all, na.rm=T)

summary(ndvi.gamm.step1)
ndvi.gamm.step1

# creating placeholder dataframe because we were running into NA issues
ndvi.hold <- ndvi.all[!is.na(ndvi.all$NDVI),]

ndvi.hold$ndvi.gam.pred.step1 <- predict(ndvi.gamm.step1$gam) # this will predict the fixed effects only. The $gamm addition solves the version issue we were running into earlier.
ndvi.hold$ndvi.gam.step1.anomaly <- ndvi.hold$NDVI-ndvi.hold$ndvi.gam.pred.step1

ndvi.gamm.step2 <- gam(ndvi.gam.step1.anomaly~s(doy, k=12, by=satellite), data=ndvi.hold, na.rm=T)
ndvi.hold$ndvi.gam.pred.step2 <- predict(ndvi.gamm.step2)

ndvi.hold$ndvi.modeled <- ndvi.hold$ndvi.gam.pred.step1 + ndvi.hold$ndvi.gam.pred.step2

# creatign double detrend anomalies
ndvi.hold$ndvi.anomaly <- ndvi.hold$NDVI-ndvi.hold$ndvi.modeled

head(ndvi.hold)

ndvi.check.all <- merge(ndvi.hold, ndvi.all)
summary(ndvi.check.all)


# parsing down data frame and saving

summary(ndvi.check.all)

ndvi.detrend <- ndvi.check.all[,c("year", "doy", "type", "date", "NDVI", "ndvi.modeled", "ndvi.anomaly")]
names(ndvi.detrend) <- c("year", "doy", "type", "date", "ndvi.obs", "ndvi.modeled","ndvi.anomaly")

summary(ndvi.detrend)

# calculating a moving average by cover type from the left to help smooth out some of the jumpiness in the observations
library(zoo)
ndvi.detrend2 <- NULL
for(i in unique(ndvi.detrend$type)){
  temp <- ndvi.detrend[ndvi.detrend$type==i,]
  temp <- temp[order(temp$date, decreasing = F),]
  
  temp$obs.smooth <- rollapply(temp$ndvi.obs, width = 6, align="right", FUN=mean, na.rm=T, fill=NA) # not smoothing on a temporal window, but rather an observation frequency window
  temp$anom.smooth <- rollapply(temp$ndvi.anomaly, width = 6, align="right", FUN=mean, na.rm=T, fill=NA)
  
  if(is.null(ndvi.detrend2)) ndvi.detrend2 <- temp else ndvi.detrend2 <- rbind(ndvi.detrend2, temp)
}

summary(ndvi.detrend2)

png(filename = "figures/ndvi_obs_smooth.png", height = 11, width = 15, res = 300, units = "in")
ggplot(ndvi.detrend2[ndvi.detrend2$year %in% c(2011, 2012) & ndvi.detrend2$type=="forest",]) + facet_wrap(type~.) +
  labs(title="NDVI Observation series with 6obs smoother") +
  geom_vline(xintercept=as.Date("2012-01-01"), linetype="dashed", col="maroon")+
  geom_line(aes(x=date, y=ndvi.obs), col="black") +
  geom_line(aes(x=date, y=obs.smooth), col="dodgerblue", linewidth=1.2)+
  theme_bw()
dev.off()  

png(filename = "figures/ndvi_anomaly_smooth.png", height = 11, width = 15, res = 300, units = "in")
ggplot(ndvi.detrend2[ndvi.detrend2$year %in% c(2011, 2012) & ndvi.detrend2$type=="forest",]) + facet_wrap(type~.) +
  labs(title="NDVI Anomaly series with 6obs smoother") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept=as.Date("2012-01-01"), linetype="dashed", col="maroon") +
  geom_line(aes(x=date, y=ndvi.anomaly), col="black") +
  geom_line(aes(x=date, y=anom.smooth), col="dodgerblue", linewidth=1.2)+
  theme_bw()
dev.off() 

ggplot(ndvi.detrend2[ndvi.detrend2$year %in% c(2011, 2012) & ndvi.detrend2$type=="forest",]) + facet_wrap(type~.) +
  labs(title="NDVI Anomaly series with 6obs smoother") +
  geom_hline(yintercept = 1.0, linetype="dashed") +
  geom_vline(xintercept=as.Date("2012-01-01"), linetype="dashed", col="maroon") +
  geom_line(aes(x=date, y=ndvi.obs/ndvi.modeled), col="black") +
  #geom_line(aes(x=date, y=anom.smooth), col="dodgerblue", linewidth=1.2)+
  theme_bw()

#######################################################

# adding in steps to calculate the SD of each cover type to then standardize the data by the SD present in the cover type
sd.df <- data.frame(type = unique(ndvi.detrend$type))
for(i in unique(ndvi.detrend$type)){
  temp <- ndvi.detrend[ndvi.detrend$type==i,"ndvi.anomaly"]
  sd.df[sd.df$type==i, "ndvi.anom.sd"] <- sd(temp, na.rm=T)
  
}

# dividing the anomaly by the standard deviation
for(i in unique(sd.df$type)){
  temp <- ndvi.detrend[ndvi.detrend$type==i,]
  ndvi.detrend[ndvi.detrend$type==i, "ndvi.anom.scale"] <- temp$ndvi.anomaly/sd.df$ndvi.anom.sd[sd.df$type==i]
}

summary(ndvi.detrend)

# adding a month variable
ndvi.detrend$month <- month(ndvi.detrend$date)
saveRDS(ndvi.detrend, file = file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))


#####################
# Sanity Check and Diagnostic plots----

# some of the quick diagnostic plots
# plot of observed NDVI
ggplot(data = ndvi.detrend) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=ndvi.obs, col=as.factor(year)))

# looking at just urban-medium detrended with years broken out
ggplot(data = ndvi.detrend[ndvi.detrend$type=="urban-medium",]) + facet_wrap(year~.) +
  geom_line(aes(x=doy, y=ndvi.anomaly, col=as.factor(year))) +
  geom_hline(yintercept=0, linetype="dashed")+
  labs(title = "Urban Medium Land Cover NDVI Anomaly by Year")

# DIAGNOSTIC PLOT
# looking at raw NDVI and our double modeled NDVI
# NOTE: you can save a whole graph as an object to call later. See below
diag.scatter <- ggplot(data = ndvi.detrend) +
  geom_point(aes(x=ndvi.obs, y = ndvi.modeled), alpha=0.25) +
  geom_abline(slope = 1, intercept=0, col="red", linewidth=1.5)

# Adding graph only looking at the grow season
diag.scatter.grow.season <- ggplot(data = ndvi.detrend[ndvi.detrend$doy>60 & ndvi.detrend$doy<305,]) +
  geom_point(aes(x=ndvi.obs, y = ndvi.modeled), alpha=0.25) +
  geom_abline(slope = 1, intercept=0, col="red", linewidth=1.5)

# DIAGNOSTIC PLOT
# Showing modeled and observed time series
# allows us to see how well the modeled time series is doing compared with the observed for individual dates
# we can see areas where the observed is more/less than what the model predicted.
diag.ts <- ggplot(data = ndvi.detrend) + facet_grid(type~.) +
  geom_line(aes(x=date, y = ndvi.modeled, col="modeled"), linewidth=1) +
  geom_line(aes(x=date, y = ndvi.obs, col="observed")) +
  scale_color_manual(values=c("modeled" = "dodgerblue", "observed" = "black"))

# checking for heteroskedasticity in the residuals
# plotting anomalies on the y axis, observations on the x axis
diag.resid <- ggplot(data = ndvi.detrend) +
  geom_point(aes(x=ndvi.obs, y = ndvi.anomaly, col=type), alpha=0.25) +
  geom_hline(yintercept=0, linetype="dashed", color="orange2")

# plotting anomalies on the y axis, observations on the x axis for grow season only
diag.resid.grow.season <- ggplot(data = ndvi.detrend[ndvi.detrend$doy>60 & ndvi.detrend$doy<305,]) +
  geom_point(aes(x=ndvi.obs, y = ndvi.anomaly, col=type), alpha=0.25) +
  geom_hline(yintercept=0, linetype="dashed", color="orange2")


# saving diagnostic plots
# initiate link to a save file
png(filename=file.path(google.drive, "data/r_files/figures/diagnostic_plots/diagnostic_ndvi_timeseries.png"), height=8, width = 15, unit = "in", res=300)
diag.ts # call the graphic object
dev.off() # shut down the link to the save file

png(filename=file.path(google.drive, "data/r_files/figures/diagnostic_plots/diagnostic_ndvi_scatter.png"), height=8, width = 8, unit = "in", res=300)
diag.scatter # call the graphic object
dev.off() # shut down the link to the save file

png(filename=file.path(google.drive, "data/r_files/figures/diagnostic_plots/diagnostic_ndvi_scatter_grow_season.png"), height=8, width = 8, unit = "in", res=300)
diag.scatter.grow.season # call the graphic object
dev.off() # shut down the link to the save file

png(filename=file.path(google.drive, "data/r_files/figures/diagnostic_plots/diagnostic_ndvi_residuals.png"), height=8, width = 8, unit = "in", res=300)
diag.resid # call the graphic object
dev.off() # shut down the link to the save file

png(filename=file.path(google.drive, "data/r_files/figures/diagnostic_plots/diagnostic_ndvi_residuals_grow_season.png"), height=8, width = 8, unit = "in", res=300)
diag.resid.grow.season # call the graphic object
dev.off() # shut down the link to the save file

###################################################
# Looking at the anomaly time series
ggplot(data=ndvi.detrend) + facet_grid(type~.) +
  geom_hline(yintercept=0, linetype="dashed") +
  #geom_line(aes(x=date, y=ndvi.anomaly), col="blue")+
  geom_path(aes(x=date, y=ndvi.anom.scale), col="forestgreen")


# looking at 2005, 2012, and 2023

ggplot(data=ndvi.detrend[ndvi.detrend$year %in% c(2005, 2012, 2023) & ndvi.detrend$month %in% c(4:9),])+ facet_wrap(type~.) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(aes(x=doy, y=ndvi.obs, col=as.factor(year)))


ggplot(data=ndvi.detrend[ndvi.detrend$year %in% c(2005, 2012, 2023) & ndvi.detrend$month %in% c(4:9),])+ facet_wrap(type~.) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(aes(x=doy, y=ndvi.anom.scale, col=as.factor(year)))+
  geom_smooth(aes(x=doy, y=ndvi.anom.scale, col=as.factor(year)))

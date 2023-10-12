# cleaned Detrending code

# This runs the double detrending method that we use to create the NDVI anomalies time series
# To see the full messy workflow see '01_NDVI_detrending.R'

# script to detrend NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)
library(mgcv)
library(dplR) # tree ring package I like to use to explore detrending sometimes
# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))


# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)



################################
# Fancy detrending----


ndvi.gamm.step1 <- gamm(NDVI ~ type + s(doy, k=12, by=type), random=list(satellite=~1), data=ndvi.all, na.rm=T)

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

saveRDS(ndvi.detrend, file = file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))


#####################
# Sanity Check and Diagnostic plots----

ggplot(data = ndvi.detrend) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=ndvi.obs, col=as.factor(year)))

ggplot(data = ndvi.detrend[ndvi.detrend$type=="urban-medium",]) + facet_wrap(year~.) +
  geom_line(aes(x=doy, y=ndvi.anomaly, col=as.factor(year))) +
  geom_hline(yintercept=0, linetype="dashed")


# some of the quick diagnostic plots
# plot of observed NDVI
ggplot(data = ndvi.detrend) + facet_wrap(type~.) +
  geom_line(aes(x=doy, y=ndvi.obs, col=as.factor(year)))

# looking at just urban-medium detrended with years broken out
ggplot(data = ndvi.detrend[ndvi.detrend$type=="urban-medium",]) + facet_wrap(year~.) +
  geom_line(aes(x=doy, y=ndvi.anomaly, col=as.factor(year))) +
  geom_hline(yintercept=0, linetype="dashed")

# DIAGNOSTIC PLOT
# looking at raw NDVI and our double modeled NDVI
# NOTE: you can save a whole graph as an object to call later. See below
diag.scatter <- ggplot(data = ndvi.detrend) +
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

# saving diagnostic plots
# initiate link to a save file
png(filename=file.path(google.drive, "data/r_files/figures/diagnostic_plots/diagnostic_ndvi_timeseries.png"), height=8, width = 15, unit = "in", res=300)
diag.ts # call the graphic object
dev.off() # shut down the link to the save file

png(filename=file.path(google.drive, "data/r_files/figures/diagnostic_plots/diagnostic_ndvi_scatter.png"), height=8, width = 8, unit = "in", res=300)
diag.resid # call the graphic object
dev.off() # shut down the link to the save file


png(filename=file.path(google.drive, "data/r_files/figures/diagnostic_plots/diagnostic_ndvi_residuals.png"), height=8, width = 8, unit = "in", res=300)
diag.scatter # call the graphic object
dev.off() # shut down the link to the save file

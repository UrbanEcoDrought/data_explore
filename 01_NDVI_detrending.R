# script to detrend NDVI time series for the land cover classes of the chicago reigon
library(ggplot2)
library(lubridate)
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




# calculating mean time series fro each land cover type
# using all years available to calculate the mean
unique(ndvi.all$type)

ndvi.mean <- aggregate(NDVI~doy + type, FUN=mean, data=ndvi.all, na.rm=T)
head(ndvi.mean)


# calculating 95% CI to have for now
ndvi.mean[,"UB"] <- aggregate(NDVI~doy+type, FUN=quantile, prob=0.975, data=ndvi.all)[3] # taking the third column can creating a new variable in the ndvi.mean datframe
ndvi.mean[,"LB"] <- aggregate(NDVI~doy+type, FUN=quantile, prob=0.025, data=ndvi.all)[3] # taking the third column can creating a new variable in the ndvi.mean datframe
ndvi.mean[,"VAR"] <- aggregate(NDVI~doy+type, FUN=var, data=ndvi.all)[3]
names(ndvi.mean) <- c("doy", "type", "ndvi.mean", "UB", "LB", "VAR") # renaming variables to not be confusing later
head(ndvi.mean)


# plotting up mean NDVI time series with CI ribbon
ggplot(data=ndvi.mean) + facet_wrap(type~.) +
  geom_ribbon(aes(x=doy, ymin=LB, ymax=UB), alpha=0.5) +
  geom_line(aes(x=doy, y=ndvi.mean)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks$doy, labels = month.breaks$month) +
  theme_bw()

# plotting the variance just to see something
# The 'something': Just a ton of noise early in the year and late in the year. RA: I think we might be hitting detection limits of the sensor. Looking at maybe March-ish as the start time. Would like to check with Trent about how early we could be in drought.
# before truncating the series, lets see how things correlate with the met data early in the year.
ggplot(data=ndvi.mean) + facet_wrap(type~.) +
  # geom_ribbon(aes(x=doy, ymin=LB, ymax=UB), alpha=0.5) +
  geom_line(aes(x=doy, y=VAR)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks$doy, labels = month.breaks$month) +
    theme_bw()

# NDVI anomalies calcualtion----

# merging in the mean series into the ndvi.all data frame
ndvi.all2 <- merge(ndvi.all, ndvi.mean[,c("ndvi.mean", "doy", "type")], by=c("type", "doy"), all=T)
summary(ndvi.all2)    

# subtracting the mean series from teh raw series
ndvi.all2$anomaly.mean <- ndvi.all2$NDVI - ndvi.all2$ndvi.mean

# saving this output
saveRDS(ndvi.all2, file = file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))

# plotting to check that we have true anomaly series
# should be zero centered

ggplot(data=ndvi.all2) + facet_wrap(type~.)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=date, y=anomaly.mean))

ggplot(data=ndvi.all2[ndvi.all2$year %in% c(2005, 2012),]) + facet_grid(year~type)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=doy, y=anomaly.mean)) +
  # scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks$doy, labels = month.breaks$month) +
  theme_bw()



################################
# Fancy detrending----

# christy mentioned that there may be issues among the different satellites in terms of their sensitivity regarding NDVI
# to overcome this we want to account for some of that variance in the calculation of the mean (see above, where we have stepwise changes as years go by)

library(mgcv)

# enter Generalized additive mixed models

summary(ndvi.all2)

# setting up a gamm with land cover type as a fixed effect and the satellites as a random effect
# fitting a spline by day of year
ndvi.gamm <- gamm(NDVI ~ type + s(doy), random=list(satellite=~1), data=ndvi.all2, na.rm=T)
summary(ndvi.gamm)
ndvi.gamm

# creating placeholder dataframe because we were running into NA issues
ndvi.all.hold <- ndvi.all2[!is.na(ndvi.all2$NDVI),]

ndvi.all.hold$gamm.pred<- predict(ndvi.gamm, na.rm=T)
ndvi.all.hold$anomaly.gamm <- ndvi.all.hold$NDVI - ndvi.all.hold$gamm.pred

# merging placeholder data frame into the ndvi.all2 data frame
ndvi.all3 <- merge(ndvi.all2, ndvi.all.hold, all=T)
summary(ndvi.all3)
# wanting to see if the model really fit differently for each land cover type
ggplot(data=ndvi.all3) +
  geom_density(aes(x=gamm.pred, col=type))

# plotting the raw and the gamm prediction over top of one another
# still seem to be some off sets
## Christy: May want to look at this... I'm still rusty with the gams.
# The gamm misses a bit on the low end, but this might work as a sort of pseudo log transformation where the low values are emphasized a bit more in the detrending. So we'd be more sensitive to picking up low-greenness periods.
ggplot(data=ndvi.all3) + facet_wrap(type~.) +
  # geom_line(aes(x=date, y=NDVI)) +
  geom_line(aes(x=date, y=ndvi.mean), col="forestgreen", alpha=0.5)+
  geom_line(aes(x=date, y=gamm.pred), col="purple")

# plotting residuals
ggplot(data=ndvi.all3) + facet_wrap(type~.) +
  geom_hline(yintercept=0, linetype="dashed", col="forestgreen") +
  geom_line(aes(x=date, y=anomaly.gamm))

# looking at 2005 and 2012
ggplot(data=ndvi.all3[ndvi.all3$year %in% c(2005, 2012),]) + facet_grid(year~type)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=doy, y=anomaly.gamm)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks.short$doy, labels = month.breaks.short$month) +
  theme_bw()


# saving output with everything
saveRDS(ndvi.all3, file = file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))

library(mgcv)
library(ggplot2)
library(tibble)
library(dplyr)

# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

ndvi.latest <- read.csv(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract/NDVIall_latest.csv")) #load latest NDVI data
ndvi.latest$date <- as.Date(ndvi.latest$date)
ndvi.latest$type <- as.factor(ndvi.latest$type)
ndvi.latest$mission <- as.factor(ndvi.latest$mission)
summary(ndvi.latest)

ndvi.2024 <- ndvi.latest[ndvi.latest$year==2024,] #subset data to only contain the current year
ndvi.2024 <- ndvi.2024[ndvi.2024$type=='grassland',] #subset again to specifically grassland right now
dim(ndvi.2024)
summary(as.factor(ndvi.2024$mission))

ggplot(data=ndvi.2024[,], aes(x=yday,y=NDVI)) + geom_point() #quick plot to visualize

nmonths <- length(unique(lubridate::month(ndvi.2024$date))) # Number of knots per month
nObs <- nrow(ndvi.2024) # If we want to pick the number of knots based our the number of obs
gam_2024 <- gam(NDVI ~ s(yday, k=nmonths*2), data=ndvi.2024[,]) #create simple gam using the day of the year and NDVI
gam.check(gam_2024) #check accuracy of model
summary(gam_2024)

plot(gam_2024, residuals=TRUE)

#newDF <- with(ndvi.2024, data.frame(yday=seq(1, 254))) 
newDF <- data.frame(yday=seq(1,max(ndvi.2024$yday))) #create new data frame with column to represent day of year sequence
#y_pred <- predict(gam_2024, newDF, type='response')
ndata <- add_column(newDF, fit=predict(gam_2024, newdata=newDF,type='response')) #make continuous time series using predict
summary(ndata)

ggplot(ndata,aes(x=yday,y=fit)) + geom_line() + #quick plot to see fitted data
  geom_point(data=ndvi.2024, aes(y=NDVI, x=yday), color="blue2") # adding our observed data

#plot(NDVI ~ yday, data = ndvi.2024) #plot prediction data over raw data
#lines(y_pred ~ yday, data=newDF, col='red')

fam <- family(gam_2024) #use family argument to calculate CIs
fam
#str(fam)

ilink <- fam$linkinv #extract inverse of link function
ilink

ndata <- bind_cols(ndata, setNames(as_tibble(predict(gam_2024, ndata, se.fit=TRUE)[1:2]),c('fit_link','se_link'))) #generate fitted values & errors on link scale
ndata <- mutate(ndata, fit_resp = ilink(fit_link),right_upr = ilink(fit_link + (2 * se_link)), right_lwr = ilink(fit_link - (2 * se_link))) #compute 95% CI using above values and backtransform to response scale using ilink
summary(ndata)

ggplot(ndata,aes(x=yday,y=fit)) + geom_line() + 
  geom_point(data=ndvi.2024, aes(y=NDVI, x=yday), color="blue2") +
  geom_ribbon(data=ndata, aes(ymin=right_lwr, ymax=right_upr),alpha=0.1) #plot fitted values with 95% CI

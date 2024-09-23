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
dim(ndvi.2024)
summary(as.factor(ndvi.2024$mission))

# turn into loop later?
ndvi.2024.crop <- ndvi.2024[ndvi.2024$type=='crop',] #subset again for each LC type
ndvi.2024.forest <- ndvi.2024[ndvi.2024$type=='forest',]
ndvi.2024.grass <- ndvi.2024[ndvi.2024$type=='grassland',]
ndvi.2024.uh <- ndvi.2024[ndvi.2024$type=='urban-high',]
ndvi.2024.ul <- ndvi.2024[ndvi.2024$type=='urban-low',]
ndvi.2024.um <- ndvi.2024[ndvi.2024$type=='urban-medium',]
ndvi.2024.uo <- ndvi.2024[ndvi.2024$type=='urban-open',]

ggplot(data=ndvi.2024.grass[,], aes(x=yday,y=NDVI)) + geom_point() #quick plot to visualize, looking at grassland first

nmonths <- length(unique(lubridate::month(ndvi.2024.grass$date))) # Number of knots per month
nObs <- nrow(ndvi.2024.grass) # If we want to pick the number of knots based our the number of obs
gam_2024 <- gam(NDVI ~ s(yday, k=nmonths*2), data=ndvi.2024.grass[,]) #create simple gam using the day of the year and NDVI
gam.check(gam_2024) #check accuracy of model
summary(gam_2024)

plot(gam_2024, residuals=TRUE)

newDF <- data.frame(yday=seq(1,max(ndvi.2024.grass$yday))) #create new data frame with column to represent day of year sequence
ndata <- add_column(newDF, fit=predict(gam_2024, newdata=newDF,type='response')) #make continuous time series using predict
summary(ndata)
ggplot(ndata,aes(x=yday,y=fit)) + geom_line() + #quick plot to see fitted data
  geom_point(data=ndvi.2024, aes(y=NDVI, x=yday), color="blue2") # adding our observed data

first.diff <- diff(ndata$fit) #calculate slopes using first differenes approach + plot
diff_data <- as_tibble(first.diff)
diff_data <- add_column(diff_data,time_step=seq(1,(max(ndvi.2024.grass$yday)-1)))
ggplot(diff_data, aes(time_step, value)) +geom_line() + geom_hline(yintercept=0, color='blue')+
  labs(x="∆x", y="∆y") + ylim(-0.006,0.006)

#GAM CI
fam <- family(gam_2024) #use family argument to calculate CIs
fam

ilink <- fam$linkinv #extract inverse of link function
ilink

ndata <- bind_cols(ndata, setNames(as_tibble(predict(gam_2024, ndata, se.fit=TRUE)[1:2]),c('fit_link','se_link'))) #generate fitted values & errors on link scale
ndata <- mutate(ndata, fit_resp = ilink(fit_link),right_upr = ilink(fit_link + (2 * se_link)), right_lwr = ilink(fit_link - (2 * se_link))) #compute 95% CI using above values and backtransform to response scale using ilink
summary(ndata)

ggplot(ndata,aes(x=yday,y=fit)) + geom_line() + 
  geom_point(data=ndvi.2024.grass, aes(y=NDVI, x=yday), color="blue2") +
  geom_ribbon(data=ndata, aes(ymin=right_lwr, ymax=right_upr),alpha=0.1) #plot fitted values with 95% CI

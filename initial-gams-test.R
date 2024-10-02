library(mgcv)
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

######################
#read in gam derivs function
######################
source("~/work/MSB_Non-Stationarity/Example_Temporal_TreeRings/scripts/helper_functions/0_Calculate_GAMM_Derivs.R")
source("../../MSB_Non-Stationarity/Example_Temporal_TreeRings/scripts/helper_functions/0_Calculate_GAMM_Derivs.R")
# dir("../..")
######################

ndvi.latest <- read.csv(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract/NDVIall_latest.csv")) #load latest NDVI data
ndvi.latest$date <- as.Date(ndvi.latest$date)
ndvi.latest$type <- as.factor(ndvi.latest$type)
ndvi.latest$mission <- as.factor(ndvi.latest$mission)
summary(ndvi.latest)

######################
ndviUrbMed <- ndvi.latest <- ndvi.latest[ndvi.latest$type=="urban-medium",]
ggplot(data=ndviUrbMed[,], aes(x=yday,y=NDVI, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) 

gamUrbMed <- gam(NDVI ~ s(yday, k=18, by=mission) + mission-1, data=ndviUrbMed)
summary(gamUrbMed)
par(mfrow=c(2,2))
plot(gamUrbMed)
par(mfrow=c(1,1))


ndviUrbMed$predMean <- predict(gamUrbMed, newdata=ndviUrbMed)
ndviUrbMed$resid <- ndviUrbMed$NDVI - ndviUrbMed$predMean
head(ndviUrbMed)
tail(ndviUrbMed)

# Going to "reproject" the predicted mean/normal
ndviUrbMedDupe <- ndviUrbMed
ndviUrbMedDupe$mission <- "landsat 8"
head(ndviUrbMedDupe)
tail(ndviUrbMedDupe)

ndviUrbMed$predMean.reproj <- predict(gamUrbMed, newdata=ndviUrbMedDupe)
ndviUrbMed$NDVI.reproj <- ndviUrbMed$resid + ndviUrbMed$predMean.reproj
summary(ndviUrbMed)

ggplot(data=ndviUrbMed[,], aes(x=yday,y=NDVI.reproj, color=mission)) + 
  geom_point(size=0.1, alpha=0.3) +
  geom_smooth(method="gam") +
  scale_color_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) +
  scale_fill_manual(values=c("landsat 5"="#56b4e9", "landsat 7"="#0072B2", "landsat 8"="#E69F00", "landsat 9"="#D55E00")) 

ggplot(data=ndviUrbMed[,], aes(x=yday,y=NDVI)) + 
  ggtitle("raw ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbMed[ndviUrbMed$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 
  

ggplot(data=ndviUrbMed[,], aes(x=yday,y=NDVI.reproj)) + 
  ggtitle("reprojected ndvi") +
  geom_point(size=0.1, alpha=0.5, color="gray50") +
  geom_smooth(method="gam", aes(color="normal", fill="normal")) +
  geom_smooth(method="gam", data=ndviUrbMed[ndviUrbMed$year %in% c(2005, 2012, 2023),], aes(color=as.factor(year), fill=as.factor(year))) +
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#E69F00", "2023"="#CC79A7")) 

######################



######################
#fitting gams using by=mission, 2005
######################
ndvi.2005 <- ndvi.latest[ndvi.latest$year==2005,] #subset data to only contain specific year
dim(ndvi.2005)
summary(as.factor(ndvi.2005$mission))

ggplot(data=ndvi.2005[,], aes(x=yday,y=NDVI, color=type)) + geom_point() #quick plot to visualize

gam_2005stupid <- gam(NDVI ~ s(yday, k=nmonths*2) , data=ndvi.2005[,]) #create simple 
plot(gam_2005stupid)
summary(gam_2005stupid)

gam_2005Alls <- gam(NDVI ~ s(yday, k=nmonths*2, by=type), data=ndvi.2005[,]) #create simple 
summary(gam_2005Alls)
plot(gam_2005Alls)

ndvi.2005$pred1 <- predict(gam_2005Alls, newdata=ndvi.2005)
ndvi.2005$res1 <- ndvi.2005$NDVI-ndvi.2005$pred1
summary(ndvi.2005)

gam_2005All <- gam(NDVI ~ s(yday, k=nmonths*2, by=type) + type, data=ndvi.2005[,]) #create simple 
summary(gam_2005All)
plot(gam_2005All)

ndvi.2005$pred2 <- predict(gam_2005All, newdata=ndvi.2005)
ndvi.2005$res2 <- ndvi.2005$NDVI-ndvi.2005$pred2
summary(ndvi.2005)

ggplot(data=ndvi.2005[,], aes(x=NDVI,y=pred1, color=type)) + geom_point() + geom_abline(slope =1, intercept = 0) #quick plot to visualize
ggplot(data=ndvi.2005[,], aes(x=NDVI,y=pred2, color=type)) + geom_point() + geom_abline(slope =1, intercept = 0)  #quick plot to visualize


ndvi.2005Open <- ndvi.2005[ndvi.2005$type=='urban-open',] #replace with LC type

ggplot(data=ndvi.2005Open[,], aes(x=yday,y=NDVI, color=mission)) + geom_point() #quick plot to visualize

nmonths <- length(unique(lubridate::month(ndvi.2005$date))) # Number of knots per month
nObs <- nrow(ndvi.2005) # If we want to pick the number of knots based our the number of obs
gam_2005 <- gam(NDVI ~ s(yday, k=nmonths*2, by=mission) + mission, data=ndvi.2005[,]) #create simple gam using the day of the year and NDVI
gam.check(gam_2005) #check accuracy of model
summary(gam_2005)$r.sq # R-squared
summary(gam_2005)$dev.expl # explained deviance
AIC(gam_2005)
plot(gam_2005, residuals=TRUE)

newDF <- data.frame(yday=seq(1,max(ndvi.2005$yday))) #create new data frame with column to represent day of year sequence
var.tmp = data.frame(mission=unique(ndvi.2005[,3])[1])
newDF <- merge(newDF, var.tmp, all.x=T, all.y=T)
derivs <- calc.derivs(gam_2005, newdata = newDF, vars=c("yday"))
derivs$NDVI.pred <- predict(gam_2005, newdata=derivs)

ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  #geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=ndvi.2005[ndvi.2005$mission=="landsat 5",], aes(x=yday,y=NDVI, color="Landsat 5"))+
  geom_point(data=ndvi.2005[ndvi.2005$mission=="landsat 7",], aes(x=yday,y=NDVI, color="Landsat 7"))+
  labs(title="2005 urban-open",color="Legend") + ylab("NDVI") + scale_color_manual(" ", breaks=c("Landsat 5", "Landsat 7"), values = c("Landsat 5"="green","Landsat 7"="purple"))

ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  #geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean>0,], aes(x=yday, y=NDVI.pred), color="green3") +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean<0,], aes(x=yday, y=NDVI.pred), color="orange3") +
  labs(title="2005 crop CI")


#################
#2012
#################
ndvi.2012 <- ndvi.latest[ndvi.latest$year==2012,] #subset data to only contain specific year
dim(ndvi.2012)
summary(as.factor(ndvi.2012$mission))

ndvi.2012 <- ndvi.2012[ndvi.2012$type=='urban-open',] #replace with LC type

ggplot(data=ndvi.2012[,], aes(x=yday,y=NDVI)) + geom_point() #quick plot to visualize

nmonths <- length(unique(lubridate::month(ndvi.2012$date))) # Number of knots per month
nObs <- nrow(ndvi.2012) # If we want to pick the number of knots based our the number of obs
gam_2012 <- gam(NDVI ~ s(yday, k=nmonths*2), data=ndvi.2012[,]) #create simple gam using the day of the year and NDVI
gam.check(gam_2012) #check accuracy of model
summary(gam_2012)
plot(gam_2012, residuals=TRUE)

newDF <- data.frame(yday=seq(1,max(ndvi.2012$yday))) #create new data frame with column to represent day of year sequence
derivs <- calc.derivs(gam_2012, newdata = newDF, vars=names(newDF))
derivs$NDVI.pred <- predict(gam_2012, newdata=derivs)

ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  # geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=ndvi.2012[ndvi.2012$mission=="landsat 7",], aes(x=yday,y=NDVI, color="Landsat 7"))+
  #geom_point(data=ndvi.2005.crop[ndvi.2005.crop$mission=="landsat 7",], aes(x=yday,y=NDVI, color="Landsat 7"))+
  labs(title="2012 urban-open",color="Legend") + ylab("NDVI") + scale_color_manual(" ", breaks=c("Landsat 7"), values = c("Landsat 7"="purple"))

ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  #geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean>0,], aes(x=yday, y=NDVI.pred), color="green3") +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean<0,], aes(x=yday, y=NDVI.pred), color="orange3") +
  labs(title="2012 crop CI")

################
# 2005
#################
ndvi.2005 <- ndvi.latest[ndvi.latest$year==2005,] #subset data to only contain specific year
dim(ndvi.2005)

ndvi.2005 <- ndvi.2005[ndvi.2005$type=='crop',]
summary(as.factor(ndvi.2005$mission))

ggplot(data=ndvi.2005[,], aes(x=yday,y=NDVI)) + geom_point()

nmonths <- length(unique(lubridate::month(ndvi.2005$date))) # Number of knots per month
nObs <- nrow(ndvi.2005) # If we want to pick the number of knots based our the number of obs
gam_2005c <- gam(NDVI ~ s(yday, k=nmonths*2), data=ndvi.2005[,]) #create simple gam using the day of the year and NDVI
gam.check(gam_2005c) #check accuracy of model
summary(gam_2005c)
plot(gam_2005c, residuals=TRUE)

newDF <- data.frame(yday=seq(1,max(ndvi.2005$yday))) #create new data frame with column to represent day of year sequence
derivs <- calc.derivs(gam_2005c, newdata = newDF, vars=names(newDF))
derivs$NDVI.pred <- predict(gam_2005c, newdata=derivs)

ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  #geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=ndvi.2005[ndvi.2005$mission=="landsat 5",], aes(x=yday,y=NDVI, color="Landsat 5"))+
  geom_point(data=ndvi.2005[ndvi.2005$mission=="landsat 7",], aes(x=yday,y=NDVI, color="Landsat 7"))+
  labs(title="2005 urban-open",color="Legend") + ylab("NDVI") + scale_color_manual(" ", breaks=c("Landsat 5", "Landsat 7"), values = c("Landsat 5"="green","Landsat 7"="purple"))

ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  # geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean>0,], aes(x=yday, y=NDVI.pred), color="green3") +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean<0,], aes(x=yday, y=NDVI.pred), color="orange3") +
  labs(title="2005 crop CI")

###################
#ndvi.2023 <- ndvi.latest[ndvi.latest$year==2023,] #subset data to only contain specific year
#dim(ndvi.2023)
#summary(as.factor(ndvi.2023$mission))

# turn into loop later?
#ndvi.2023.crop <- ndvi.2023[ndvi.2023$type=='crop',] #subset again for each LC type
#ndvi.2023.forest <- ndvi.2023[ndvi.2023$type=='forest',]
#ndvi.2023.grass <- ndvi.2023[ndvi.2023$type=='grassland',]
#ndvi.2023.uh <- ndvi.2023[ndvi.2023$type=='urban-high',]
#ndvi.2023.ul <- ndvi.2023[ndvi.2023$type=='urban-low',]
#ndvi.2023.um <- ndvi.2023[ndvi.2023$type=='urban-medium',]
#ndvi.2023.uo <- ndvi.2023[ndvi.2023$type=='urban-open',]

#ggplot(data=ndvi.2023.grass[,], aes(x=yday,y=NDVI)) + geom_point() #quick plot to visualize, looking at grassland first

#nmonths <- length(unique(lubridate::month(ndvi.2023.forest$date))) # Number of knots per month
#nObs <- nrow(ndvi.2023.forest) # If we want to pick the number of knots based our the number of obs
#gam_2023 <- gam(NDVI ~ s(yday, k=nmonths*2), data=ndvi.2023.forest[,]) #create simple gam using the day of the year and NDVI
#gam.check(gam_2023) #check accuracy of model
#summary(gam_2023)
#plot(gam_2023, residuals=TRUE)

#newDF <- data.frame(yday=seq(1,max(ndvi.2023.forest$yday))) #create new data frame with column to represent day of year sequence

#derivs <- calc.derivs(gam_2023, newdata = newDF, vars=names(newDF))
#derivs$NDVI.pred <- predict(gam_2023, newdata=derivs)

#simLong <- stack(sim.tmp)
#simLong$yday <- df.tmp$yday
#summary(simLong)

#ggplot(data=simLong) +
# geom_line(aes(x=yday, y=values, group=ind), linewidth=0.1)

#ggplot(data=df.tmp) +
#geom_line(data=simLong, aes(x=yday, y=values, group=ind), linewidth=0.1, color="gray60") +
#geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
#geom_line(aes(x=yday, y=mean), color="black", linewdith=0.75) +
#geom_point(data=df.tmp[df.tmp$sig=="*",], aes(x=yday, y=mean), color="red2") +
#geom_hline(yintercept=0)

# adding the mean prediciton of NDVI
#df.tmp$NDVI.pred <- predict(gam_2023, newdata=df.tmp)
# summary(df.tmp)

#ggplot(data=df.tmp) +
#geom_line(aes(x=yday, y=NDVI.pred), color="black") +
# geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
#geom_point(data=df.tmp[df.tmp$sig=="*" & df.tmp$mean>0,], aes(x=yday, y=NDVI.pred), color="green3") +
#geom_point(data=df.tmp[df.tmp$sig=="*" & df.tmp$mean<0,], aes(x=yday, y=NDVI.pred), color="orange3") +
#geom_point(data=ndvi.2023.forest[ndvi.2023.forest$mission=="landsat 8",], aes(x=yday,y=NDVI), color="blue")+
#geom_point(data=ndvi.2023.forest[ndvi.2023.forest$mission=="landsat 9",], aes(x=yday,y=NDVI), color="red")+
#labs(title="2023 Forest", )

#ggplot(data=df.tmp) +
#geom_line(aes(x=yday, y=NDVI.pred), color="black") +
# geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
#geom_point(data=ndvi.2023.forest[ndvi.2023.forest$mission=="landsat 8",], aes(x=yday,y=NDVI, color="Landsat 8"))+
#geom_point(data=ndvi.2023.forest[ndvi.2023.forest$mission=="landsat 9",], aes(x=yday,y=NDVI, color="Landsat 9"))+
#labs(title="2023 Forest",color="Legend") + ylab("NDVI") + scale_color_manual(" ", breaks=c("Landsat 8", "Landsat 9"), values = c("Landsat 8"="red","Landsat 9"="blue"))

#ndata <- add_column(newDF, fit=predict(gam_2023, newdata=newDF,type='response')) #make continuous time series using predict
#summary(ndata)
#ggplot(ndata,aes(x=yday,y=fit)) + geom_line() + #quick plot to see fitted data
 # geom_point(data=ndvi.2023, aes(y=NDVI, x=yday), color="blue2") # adding our observed data

#first.diff <- diff(ndata$fit) #calculate slopes using first differenes approach + plot
#diff_data <- as_tibble(first.diff)
#diff_data <- add_column(diff_data,time_step=seq(1,(max(ndvi.2023.grass$yday)-1)))
#ggplot(diff_data, aes(time_step, value)) +geom_line() + geom_hline(yintercept=0, color='blue')+
 # labs(x="∆x", y="∆y") + ylim(-0.006,0.006)

#GAM CI
#fam <- family(gam_2023) #use family argument to calculate CIs
#fam

#ilink <- fam$linkinv #extract inverse of link function
#ilink

#ndata <- bind_cols(ndata, setNames(as_tibble(predict(gam_2023, ndata, se.fit=TRUE)[1:2]),c('fit_link','se_link'))) #generate fitted values & errors on link scale
#ndata <- mutate(ndata, fit_resp = ilink(fit_link),right_upr = ilink(fit_link + (2 * se_link)), right_lwr = ilink(fit_link - (2 * se_link))) #compute 95% CI using above values and backtransform to response scale using ilink
#summary(ndata)

#ggplot(ndata,aes(x=yday,y=fit)) + geom_line() + 
  #geom_point(data=ndvi.2023.grass, aes(y=NDVI, x=yday), color="blue2") +
  #geom_ribbon(data=ndata, aes(ymin=right_lwr, ymax=right_upr),alpha=0.1) #plot fitted values with 95% CI

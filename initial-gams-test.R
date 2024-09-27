library(mgcv)
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

ndvi.latest <- read.csv(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract/NDVIall_latest.csv")) #load latest NDVI data
ndvi.latest$date <- as.Date(ndvi.latest$date)
ndvi.latest$type <- as.factor(ndvi.latest$type)
ndvi.latest$mission <- as.factor(ndvi.latest$mission)
summary(ndvi.latest)

ndvi.2023 <- ndvi.latest[ndvi.latest$year==2023,] #subset data to only contain specific year
dim(ndvi.2023)
summary(as.factor(ndvi.2023$mission))

# turn into loop later?
ndvi.2023.crop <- ndvi.2023[ndvi.2023$type=='crop',] #subset again for each LC type
ndvi.2023.forest <- ndvi.2023[ndvi.2023$type=='forest',]
ndvi.2023.grass <- ndvi.2023[ndvi.2023$type=='grassland',]
ndvi.2023.uh <- ndvi.2023[ndvi.2023$type=='urban-high',]
ndvi.2023.ul <- ndvi.2023[ndvi.2023$type=='urban-low',]
ndvi.2023.um <- ndvi.2023[ndvi.2023$type=='urban-medium',]
ndvi.2023.uo <- ndvi.2023[ndvi.2023$type=='urban-open',]

ggplot(data=ndvi.2023.grass[,], aes(x=yday,y=NDVI)) + geom_point() #quick plot to visualize, looking at grassland first

nmonths <- length(unique(lubridate::month(ndvi.2023.forest$date))) # Number of knots per month
nObs <- nrow(ndvi.2023.forest) # If we want to pick the number of knots based our the number of obs
gam_2023 <- gam(NDVI ~ s(yday, k=nmonths*2), data=ndvi.2023.forest[,]) #create simple gam using the day of the year and NDVI
gam.check(gam_2023) #check accuracy of model
summary(gam_2023)
plot(gam_2023, residuals=TRUE)

set.seed(1124)
m.terms <- attr(terms(gam_2023), "term.labels") #model terms

alpha = 0.05 #95% CI
lwr=alpha/2
upr = 1-alpha/2


df.model <- model.frame(gam_2023) #finding which columns are numeric
cols.num <- vector()
for(j in 1:ncol(df.model)){
  if(is.numeric(df.model[,j])) cols.num <- c(cols.num, names(df.model)[j])
}

# Generate a random distribution of betas using the covariance matrix
coef.gam <- coef(gam_2023)
Rbeta <- mvrnorm(n=100, gam_2023$coefficients, gam_2023$Vp)

newDF <- data.frame(yday=seq(1,max(ndvi.2023.forest$yday))) #create new data frame with column to represent day of year sequence
X0 <- predict(gam_2023, newdata=newDF,type='lpmatrix') #create prediction matrices

newD <- newDF
newD[,m.terms[m.terms %in% cols.num]] <- newDF[,m.terms[m.terms %in% cols.num]]+1e-7
X1 <- predict(gam_2023, newdata=newD, type="lpmatrix")
Xp <- (X1 - X0) / 1e-7 # Change in Y per unit X
Xp.r <- NROW(Xp)
Xp.c <- NCOL(Xp)

for(v in names(newDF)) {
  Xi <- Xp * 0 # zeroing out our data frame 
  want <- which(substr(names(coef.gam),1,(nchar(v)+3))==paste0("s(",v,")")) # Finding which columns belong to this factor
  Xi[, want] <- Xp[, want]
  df <- Xi %*% coef(gam_2023)
  
  # Generating a distribution of simulated derivatives
  sim.tmp <- data.frame(Xp[,want] %*% t(Rbeta[,want]) )
  sim.mean <- apply(sim.tmp, 1, mean)
  sim.lwr <- apply(sim.tmp, 1, quantile, lwr)
  sim.upr <- apply(sim.tmp, 1, quantile, upr)
  sig <- as.factor(ifelse(sim.lwr*sim.upr>0, "*", NA))
  
  df.tmp <- data.frame(newDF, 
                       mean=sim.mean,
                       lwr=sim.lwr,
                       upr=sim.upr,
                       sig=sig,
                       var=as.factor(v))
  
  sim.tmp$var <- as.factor(v)
  
  if(v == newDF$yday[1]){ 
    df.out <- df.tmp 
    df.sim <- sim.tmp
  } else {
    df.out <- rbind(df.out, df.tmp)
    df.sim <- rbind(df.sim, sim.tmp)
  }
  
}
if(return.sims==T){
  out <- list()
  out[["ci"]]   <- df.out
  out[["sims"]] <- df.sim
} else {
  out <- df.out
}

simLong <- stack(sim.tmp)
simLong$yday <- df.tmp$yday
summary(simLong)

ggplot(data=simLong) +
  geom_line(aes(x=yday, y=values, group=ind), linewidth=0.1)

ggplot(data=df.tmp) +
  geom_line(data=simLong, aes(x=yday, y=values, group=ind), linewidth=0.1, color="gray60") +
  geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_line(aes(x=yday, y=mean), color="black", linewdith=0.75) +
  geom_point(data=df.tmp[df.tmp$sig=="*",], aes(x=yday, y=mean), color="red2") +
  geom_hline(yintercept=0)

# adding the mean prediciton of NDVI
df.tmp$NDVI.pred <- predict(gam_2023, newdata=df.tmp)
# summary(df.tmp)

ggplot(data=df.tmp) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  # geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=df.tmp[df.tmp$sig=="*" & df.tmp$mean>0,], aes(x=yday, y=NDVI.pred), color="green3") +
  geom_point(data=df.tmp[df.tmp$sig=="*" & df.tmp$mean<0,], aes(x=yday, y=NDVI.pred), color="orange3") +
  geom_point(data=ndvi.2023.forest[ndvi.2023.forest$mission=="landsat 8",], aes(x=yday,y=NDVI), color="blue")+
  geom_point(data=ndvi.2023.forest[ndvi.2023.forest$mission=="landsat 9",], aes(x=yday,y=NDVI), color="red")+
  labs(title="2023 Forest", )

ggplot(data=df.tmp) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  # geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=ndvi.2023.forest[ndvi.2023.forest$mission=="landsat 8",], aes(x=yday,y=NDVI, color="Landsat 8"))+
  geom_point(data=ndvi.2023.forest[ndvi.2023.forest$mission=="landsat 9",], aes(x=yday,y=NDVI, color="Landsat 9"))+
  labs(title="2023 Forest",color="Legend") + ylab("NDVI") + scale_color_manual(" ", breaks=c("Landsat 8", "Landsat 9"), values = c("Landsat 8"="red","Landsat 9"="blue"))

derivs <- calc.derivs(gam_2023, newdata = newDF, vars=names(newDF))
derivs$NDVI.pred <- predict(gam_2023, newdata=derivs)

#################
# Forest 2012
#################
ndvi.2012 <- ndvi.latest[ndvi.latest$year==2012,] #subset data to only contain specific year
dim(ndvi.2012)
summary(as.factor(ndvi.2012$mission))

ndvi.2012.forest <- ndvi.2012[ndvi.2012$type=='forest',]

ggplot(data=ndvi.2012.forest[,], aes(x=yday,y=NDVI)) + geom_point() #quick plot to visualize, looking at grassland first

nmonths <- length(unique(lubridate::month(ndvi.2012.forest$date))) # Number of knots per month
nObs <- nrow(ndvi.2012.forest) # If we want to pick the number of knots based our the number of obs
gam_2012f <- gam(NDVI ~ s(yday, k=nmonths*2), data=ndvi.2012.forest[,]) #create simple gam using the day of the year and NDVI
gam.check(gam_2012f) #check accuracy of model
summary(gam_2012f)
plot(gam_2012f, residuals=TRUE)

newDF <- data.frame(yday=seq(1,max(ndvi.2012.forest$yday))) #create new data frame with column to represent day of year sequence
derivs <- calc.derivs(gam_2012f, newdata = newDF, vars=names(newDF))
derivs$NDVI.pred <- predict(gam_2012f, newdata=derivs)

png("~/Google Drive/Shared drives/Urban Ecological Drought/data/r_files/figures/inital-gams-test figures/forest_2005.png", height=8, width=11, units="in", res=320)
ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  # geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=ndvi.2012.forest[ndvi.2012.forest$mission=="landsat 7",], aes(x=yday,y=NDVI, color="Landsat 7"))+
  #geom_point(data=ndvi.2005.forest[ndvi.2005.forest$mission=="landsat 7",], aes(x=yday,y=NDVI, color="Landsat 7"))+
  labs(title="2012 Forest",color="Legend") + ylab("NDVI") + scale_color_manual(" ", breaks=c("Landsat 7"), values = c("Landsat 7"="purple"))
dev.off()

ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  #geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean>0,], aes(x=yday, y=NDVI.pred), color="green3") +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean<0,], aes(x=yday, y=NDVI.pred), color="orange3") +
  labs(title="2012 Forest CI")

#################
# Forest 2005
#################
ndvi.2005 <- ndvi.latest[ndvi.latest$year==2005,] #subset data to only contain specific year
dim(ndvi.2005)
summary(as.factor(ndvi.2005$mission))

ndvi.2005.forest <- ndvi.2005[ndvi.2005$type=='forest',]

ggplot(data=ndvi.2005.forest[,], aes(x=yday,y=NDVI)) + geom_point() #quick plot to visualize, looking at grassland first

nmonths <- length(unique(lubridate::month(ndvi.2005.forest$date))) # Number of knots per month
nObs <- nrow(ndvi.2005.forest) # If we want to pick the number of knots based our the number of obs
gam_2005f <- gam(NDVI ~ s(yday, k=nmonths*2), data=ndvi.2005.forest[,]) #create simple gam using the day of the year and NDVI
gam.check(gam_2005f) #check accuracy of model
summary(gam_2005f)
plot(gam_2005f, residuals=TRUE)

newDF <- data.frame(yday=seq(1,max(ndvi.2005.forest$yday))) #create new data frame with column to represent day of year sequence
derivs <- calc.derivs(gam_2005f, newdata = newDF, vars=names(newDF))
derivs$NDVI.pred <- predict(gam_2005f, newdata=derivs)

png("~/Google Drive/Shared drives/Urban Ecological Drought/data/r_files/figures/inital-gams-test figures/forest_2005.png", height=8, width=11, units="in", res=320)
ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  #geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=ndvi.2005.forest[ndvi.2005.forest$mission=="landsat 5",], aes(x=yday,y=NDVI, color="Landsat 5"))+
  geom_point(data=ndvi.2005.forest[ndvi.2005.forest$mission=="landsat 7",], aes(x=yday,y=NDVI, color="Landsat 7"))+
  labs(title="2005 Forest",color="Legend") + ylab("NDVI") + scale_color_manual(" ", breaks=c("Landsat 5", "Landsat 7"), values = c("Landsat 5"="green","Landsat 7"="purple"))
dev.off()

ggplot(data=derivs) +
  geom_line(aes(x=yday, y=NDVI.pred), color="black") +
  # geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean>0,], aes(x=yday, y=NDVI.pred), color="green3") +
  geom_point(data=derivs[derivs$sig=="*" & derivs$mean<0,], aes(x=yday, y=NDVI.pred), color="orange3") +
  labs(title="2005 Forest CI")

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

# Looking at correlations with morton oak tree rings and some of the drought indices

library(ggplot2)
library(dplR)
library(lubridate)
library(dplyr)
# Tree ring data and detrending----
# Data from the oak fire study I did with Christy in 2018
# load("input_data/OER_full_dataset.Rdata")
# head(data.use)

Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# loading in morton oak series; will make into a chronology
mort.rw <- read.tucson(file.path(google.drive,"data/r_files/input_files/data_explore/OEM/mort_oaks11.rwl"))
mort.ew <- read.tucson(file.path(google.drive,"data/r_files/input_files/data_explore/OEM/mort_oaks_ew.rwl"))
mort.lw <- read.tucson(file.path(google.drive,"data/r_files/input_files/data_explore/OEM/mort_oaks_lww.rwl"))

# using a stright 2/3 spline for now. We're just exploring here and really just want the interannual variability
rw.detrend <- detrend(mort.rw, y.name="OEM", method="Spline")
ew.detrend <- detrend(mort.ew, y.name="OEM", method="Spline")
lw.detrend <- detrend(mort.lw, y.name="OEM", method="Spline")

rw.chron <- chron(rw.detrend, biweight=T)
ew.chron <- chron(ew.detrend, biweight=T)
lw.chron <- chron(lw.detrend, biweight=T)

plot(rw.chron)

# bringing in chrons into one file

mort.tr <- data.frame(year=row.names(rw.chron),
                      rw = rw.chron$std,
                      ew = ew.chron$std,
                      lw = lw.chron$std)

# Critical Period correlations----
# Analysis of timing of ciritical period for growth
library(ggplot2)

#path.google <- "/Volumes/GoogleDrive/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/"
#path.google <- "G:/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/"

# ----------------------------------
# 1. read in & format datasets: tree ring, climate----
# ----------------------------------
dat.tr <- mort.tr
dat.tr <- dat.tr[dat.tr$year>=1980,]
summary(dat.tr)
head(dat.tr)

# --------------
# Bring in drought indices----
# --------------
# data are at every 5 days or so.
dat.met <- read.csv(file.path(google.drive,"data/r_files/input_files/data_gathering_cleaning/Morton_Drought_Datasets - Morton_Drought_Datasets_mra.csv"), header=T)
summary(dat.met)
dat.met$month <- month(dat.met$date)
dat.met$month.name <- month(dat.met$date, label=T, abbr=T)
dat.met$doy <- yday(dat.met$date)
dat.met$year <- year(dat.met$date)

# need to flip EDDI and VDP to align with pos = wet, neg = dry. This will allow for all correlations to line up later
head(dat.met)

dat.met$eddi_14d <- dat.met$eddi_14d *-1 
dat.met$eddi_30d <- dat.met$eddi_30d *-1
dat.met$eddi_90d <- dat.met$eddi_90d *-1
dat.met$vpd..kpa. <- dat.met$vpd..kpa. *-1
dat.met$vpd.index.value <- dat.met$vpd.index.value *-1

# # Doing a 7-day smoothing of the met data; anchored on the date so that it's a cumulative effect
# # vars.pred <- c("prcp.mm", "tmax.C", "tmin.C", "vp.Pa", "srad.Wm2")
# vars.pred <- c("prcp.mm", "tmax.C")
# for(i in 1:length(vars.pred)){
#   dat.daymet[,paste0(vars.pred[i], ".wk")] <- zoo::rollapply(dat.daymet[,vars.pred[i]], width=7, align="right", FUN=mean, fill=NA)
#   
# }

# # Setting up a ~6-month lag (after the solstice)
# doy.solstice.sum <- lubridate::doy("2019-06-21")
# doy.equinox.aut <- lubridate::doy("2019-09-23")
# dat.lag <- dat.daymet[dat.daymet$doy>=doy.equinox.aut & dat.daymet$doy<366,]
# dat.lag$year <- dat.lag$year+1
# dat.lag$doy <- dat.lag$doy-365
# summary(dat.lag)
# 
# dat.daymet <- rbind(dat.daymet, dat.lag)
# summary(dat.daymet)
# --------------

# Bring the met data & ring data together----
dat.all <- merge(dat.tr, dat.met, by="year")
summary(dat.all)

# want to make sure that date is ordered past to present
dat.all <- dat.all[order(dat.all$date, decreasing = F),]
names(dat.all) <- tolower(names(dat.all))

# ----------------------------------


# ----------------------------------
# Running the analyses as a first pass----
# ----------------------------------
days.use <- unique(dat.all$doy)

# response variables will be total ring width = rw; earlywood = ew; latewood = lw 
vars.resp <- c("rw", "ew", "lw")
vars.pred <- tolower(names(dat.met)[!names(dat.met)%in%c("date", "month", "month.name", "doy", "year")])
# vars.resp="RWI"
mod.out <- data.frame(doy=rep(days.use), 
                      resp=rep(rep(vars.resp, each=length(days.use)), length.out=length(days.use)*length(vars.resp)*length(vars.pred)),
                      pred=rep(vars.pred, each=length(days.use)*length(vars.resp)), 
                      t.stat=NA, p.val=NA, r.sq.m=NA)
mod.out$resp <- factor(mod.out$resp, levels=(vars.resp))
mod.out$pred <- factor(mod.out$pred, levels=vars.pred)
summary(mod.out)

# adding in date for later
mod.out$month <- month(dat.all$date[match(mod.out$doy, dat.all$doy)], label=F)
mod.out$month.name <- month(dat.all$date[match(mod.out$doy, dat.all$doy)], label=T, abbr=T)
date.temp <- as.Date(dat.all$date[match(mod.out$doy, dat.all$doy)])

mod.out$month.day <- format(as.Date(date.temp), "%d-%m")
head(mod.out)

# Looping through all of the models we could possibly want
pb <- txtProgressBar(min=0, max=nrow(mod.out), style = 3)
pb.ind <- 1
for(i in days.use){
    # Subset to just crossdated samples
    dat.tmp <- dat.all[dat.all$doy==i,]
    
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
summary(mod.out)

summary(mod.out[!is.na(mod.out$p.val),])
write.csv(mod.out, file.path(google.drive,"data/r_files/processed_files", "tr_analyses","TR_ClimateCorrs_Daily.csv"), row.names=F)  
# ----------------------------------

# ----------------------------------
# Plotting and exploring the output
# ----------------------------------
mod.out <- read.csv(file.path(google.drive,"data/r_files/processed_files", "tr_analyses","TR_ClimateCorrs_Daily.csv"), header=T)
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
mod.out$resp <- factor(mod.out$resp, levels = c("lw", "ew", "rw"))

PRGn5 <- c("#7b3294", "#c2a5cf", "gray50", "#a6dba0", "#008837")

pdf("figures/CriticalPeriods_tr/ClimateCorr_0.01_t-stat_.pdf", height=8, width=11)
for(PLT in unique(mod.out$pred)){
  print(
    ggplot(data=mod.out[mod.out$pred==PLT,]) +
      facet_grid(~pred, scales="free") +
      ggtitle(paste0(PLT, " 5Day Morton Oaks Climate Corr.: Sig. T-stat; RWI, alpha=0.01")) +
      geom_tile(data=mod.out[mod.out$pred==PLT & mod.out$p.val>=0.01,], aes(x=doy, y=resp), fill="gray50") +
      geom_tile(data=mod.out[mod.out$pred==PLT & mod.out$p.val<0.01,], aes(x=doy, y=resp, fill=t.stat)) +
      # geom_tile(aes(x=doy, y=resp, fill=t.stat)) +
      #geom_vline(xintercept = 0, linetype="dashed") +
      scale_y_discrete(expand=c(0,0)) +
      scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks$doy, labels = month.breaks$month) +
      scale_fill_gradientn(name="t-stat", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
      theme(legend.position="top",
            axis.title.y=element_blank())
  )
}
dev.off()



pdf(file="figures/CriticalPeriods_tr/ClimateCorr_5day__t-stat.pdf", height=8, width=11)
for(i in unique(mod.out$pred)){
print(
  ggplot(data=mod.out[mod.out$pred==i,]) +
  facet_grid(~pred, scales="free") +
  ggtitle("5Day Morton Oaks Climate Corr.: Sig. T-stat; RWI; alpha=0.05") +
  geom_tile(data=mod.out[mod.out$pred==i & mod.out$p.val>=0.05,], aes(x=doy, y=resp), fill="gray50") +
  geom_tile(data=mod.out[mod.out$pred==i & mod.out$p.val<0.05,], aes(x=doy, y=resp, fill=t.stat)) +
  # geom_tile(aes(x=doy, y=resp, fill=t.stat)) +
  scale_y_discrete(name="Tree-Ring Component", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks$doy, labels = month.breaks$month) +
  scale_fill_gradientn(name="t-stat", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
  theme(legend.position="top")
)
}
dev.off()


# png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_5day_ringwidth_r-val.png"), height=10, width=8, units="in", res=120)
# ggplot(data=mod.out[mod.out$resp=="rw" & mod.out$p.val <0.05,]) +
#   #facet_grid(pred~., scales="free") +
#   ggtitle("Daily Climate Corr.: r-value; RWI") +
#   geom_tile(data=mod.out[mod.out$resp=="rw" & mod.out$p.val > 0.05,], aes(x=doy, y=pred), fill="grey50") +
#   geom_tile(aes(x=doy, y=pred, fill=sqrt(r.sq.m)*sign(t.stat))) +
#   scale_y_discrete(name="Drought Index", expand=c(0,0)) +
#   scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks$doy, labels = month.breaks$month) +
#   scale_fill_gradientn(name="Marginal\nR-value", colors=PRGn5, limits=max(sqrt(mod.out$r.sq.m))*c(-1,1))+
#   theme(legend.position="top")
# dev.off()

pdf(file = "figures/CriticalPeriods_tr/ClimateCorr_5day_r-val.pdf", height=8, width=10)
for(i in unique(mod.out$pred)){
print(
  ggplot(data=mod.out[mod.out$pred==i & mod.out$p.val <0.05,]) +
  facet_grid(~pred, scales="free") +
  ggtitle(paste0(i, " 5Day Morton Oaks Climate Corr.: r-value; alpha=0.05")) +
  geom_tile(data=mod.out[mod.out$pred==i & mod.out$p.val > 0.05,], aes(x=doy, y=resp), fill="grey50") +
  geom_tile(aes(x=doy, y=resp, fill=sqrt(r.sq.m)*sign(t.stat))) +
  scale_y_discrete(name="Drought Index", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=month.breaks$doy, labels = month.breaks$month) +
  scale_fill_gradientn(name="Marginal\nR-value", colors=PRGn5, limits=max(sqrt(mod.out$r.sq.m))*c(-1,1))+
  theme(legend.position="top")
  )
}
dev.off()



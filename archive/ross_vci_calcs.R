library(lubridate)
library(ggplot2)


google.drive <- "G:/Shared drives/Urban Ecological Drought"
#3dVCI data saved on Google Drive
#ChicagolandTempSPEISPINDVIVPDNA <- readRDS(file.path(google.drive, "data/data_sets/Chicagoland3dVCI.RDS"))

ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))
head(ndvi.all)

ndvi.all$date <- as.Date(ndvi.all$date)
head(ndvi.all)

# Don't want to deal with old detrendign here, so just keepign the observations for now.
ndvi.new <- ndvi.all[,!names(ndvi.all) %in% c("ndvi.modeled", "ndvi.anomaly")]

# creating month variable to make things a bit easier to subset
ndvi.new$month <- month(ndvi.new$date)

ndvi.short <- ndvi.new[ndvi.new$month %in% c(3:10),] # limiting data set to just the months adjacent to the growing season in Chicago
summary(ndvi.short)
# setting up a loop to calculate Vegetation Condition Index

# VCI calculation: 100*(NDVIobs - NDVImin)/NDVImax-NDVImin; (Kogan 1990)

class(ndvi.short$doy)


for(t in unique(ndvi.short$type)){
  ndvi.type <- ndvi.short[ndvi.short$type==t,]
  
  for(i in ndvi.type$date[ndvi.type$month %in% c(4:9)]){ # Wanting to target dates within the growing season
    
    
    doy <- ndvi.type$doy[ndvi.type$date==i]
    doy.range <- (doy-7):(doy+7) # setting a 15day window for DOY to calculate anomalies
    
    ndvi.obs <- ndvi.type$ndvi.obs[ndvi.type$date==i]
    ndvi.min <- min(ndvi.type[ndvi.type$doy %in%doy.range, "ndvi.obs"], na.rm=T)
    ndvi.max <- max(ndvi.type[ndvi.type$doy %in%doy.range, "ndvi.obs"], na.rm=T)
    
    ndvi.short[ndvi.short$type==t & ndvi.short$date==i, "vci.15"] <- 100*((ndvi.obs-ndvi.min)/(ndvi.max-ndvi.min))
      
  }
  
}

summary(ndvi.short[ndvi.short$month %in% c(4:9),])
dim(ndvi.short)
2492/11682

ggplot(ndvi.short[ndvi.short$type=="forest",])+ facet_wrap(type~year)+
  geom_line(aes(x=doy, y=vci.15))+
  geom_smooth(aes(x=doy, y=vci.15))


ggplot(ndvi.short[ndvi.short$type=="forest",])+ facet_wrap(type~year)+
  geom_point(aes(x=doy, y=ndvi.obs))+
  #geom_smooth(aes(x=doy, y=vci.15))



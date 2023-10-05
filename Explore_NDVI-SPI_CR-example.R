# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product --> our RESPONSE VARS would be 
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))
head(ndvi.all)

# Subsetting to a single landcover class for testing
ndvi.test <- ndvi.all[ndvi.all$type=="urban-medium",]
summary(ndvi.test)

# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)

# create column with date in ISO format

# merge ChicagolandSPI and NDVIomitNA2022 by date columns --> we dont' want to keep SPI for things we don't have NDVI for
ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

# Find out what days we want to model
ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.modeled.anomaly")
pred.vars <-c ("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

## Here's the basic model --> make this work otuside of a loop beforebuildign it into a loop
# specifying
dayNOW <- days.use[1]
RESP <- resp.vars[2]
PRED <- pred.vars[1]
 
dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,] # Subsets things to a particular window; otherwise we were worign with just 5 years of data, which isn't helpful
dat.tmp$RESP <- dat.tmp[,RESP] # This way of writing this creates a variable called RESP from the column that matches whatever value RESP is right now (e.g. ndvi.modeled.anomaly)
dat.tmp$PRED <- dat.tmp[,PRED] # This way of writing this creates a variable called PRED from the column that matches whatever value PRED is right now (e.g. X14d.SPI)
summary(dat.tmp) # Checkign to make sure it all looks okay
dim(dat.tmp)

mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[!is.na(dat.tmp$RESP),], na.action=na.omit)
# )
mod.sum <- summary(mod.var)
mod.sum

# Save our t-stat & pvalue for the climate predictor <- We'll need to figure out how to set up a dataframe to store our useful info, but for the moment, we may want to just set up a dataframe and use "rbind" for a small bit
mod.out[out.ind, "t.stat"] <- mod.sum$tTable["PRED","t-value"]
mod.out[out.ind, "p.val"] <- mod.sum$tTable["PRED","p-value"]
mod.out[out.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]



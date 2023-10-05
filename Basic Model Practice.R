# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# Read in the NDVI data
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))
head(ndvi.all)

# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))
head(ChicagolandSPI)

# create column with date in ISO format
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
head(ChicagolandSPI)

# merge ChicagolandSPI and NDVIomitNA2022 by date columns
ChicagolandSPINDVI.all <- merge (ChicagolandSPI, ndvi.all, by=c("date"), all.x=TRUE, all.y=TRUE)
head(ChicagolandSPINDVI.all)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVI.all.NA <- na.omit(ChicagolandSPINDVI.all)
head(ChicagolandSPINDVI.all.NA)
summary(ChicagolandSPINDVI.all.NA)

# reading in Trent's VPD data
ChicagolandVPD <- read.csv(file.path(google.drive, "data/data_sets/Chicagoland_Daily_VPD.csv"))

# create column with date in ISO format
ChicagolandVPD$date <- as.Date(ChicagolandVPD$Date, "%m/%d/%Y")

# merge ChicagolandVPD and ChicagolandSPINDVINA by date columns
ChicagolandSPINDVIVPD.all <- merge (ChicagolandSPINDVI.all.NA, ChicagolandVPD, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVIVPD.all.NA <- na.omit(ChicagolandSPINDVIVPD.all)
head(ChicagolandSPINDVIVPD.all.NA)

# Simplify column label to VPD
colnames(ChicagolandSPINDVIVPD.all.NA)[14] = 'VPD'
head(ChicagolandSPINDVIVPD.all.NA)

# Remove unneeded columns
ChicagolandSPINDVIVPD.all.NA <- ChicagolandSPINDVIVPD.all.NA[,-c(2,13)]
head(ChicagolandSPINDVIVPD.all.NA)

ChicagolandSPINDVIVPD.all.NA$RESP <- ChicagolandSPINDVIVPD.all.NA$ndvi.obs
ChicagolandSPINDVIVPD.all.NA$PRED <- ChicagolandSPINDVIVPD.all.NA$X60d.SPI

# Creating basic lme model
mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=ChicagolandSPINDVI.all.NA[ChicagolandSPINDVI.all.NA$type=="forest"&ChicagolandSPINDVI.all.NA$doy==99,], na.action=na.omit)
summary(mod.var)

# Save mod.var as its own object
mod.sum <- summary(mod.var)

# Save our t-stat & pvalue for the climate predictor
mod.out[out.ind, "t.stat"] <- mod.sum$tTable["PRED","t-value"]
mod.out[out.ind, "p.val"] <- mod.sum$tTable["PRED","p-value"]
mod.out[out.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
}


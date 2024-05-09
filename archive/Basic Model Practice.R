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

# Create response and predictor variable objects for modeling
ChicagolandSPINDVIVPD.all.NA$RESP <- ChicagolandSPINDVIVPD.all.NA$ndvi.obs
ChicagolandSPINDVIVPD.all.NA$PRED <- ChicagolandSPINDVIVPD.all.NA$X60d.SPI

# Creating basic lme model
mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=ChicagolandSPINDVIVPD.all.NA[ChicagolandSPINDVIVPD.all.NA$type=="forest"&ChicagolandSPINDVIVPD.all.NA$doy==150,], na.action=na.omit)
summary(mod.var)

# Save mod.var as its own object
mod.sum <- summary(mod.var)

# using all possible days
days.use <- unique(ChicagolandSPINDVIVPD.all.NA$doy[1:366])

# set response variable
vars.resp <- names(ChicagolandSPINDVIVPD.all.NA)[!names(ChicagolandSPINDVIVPD.all.NA) %in% c("date", "X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI", "year", "type", "doy", "year", "month", "VPD", "RESP", "PRED", "type.f")]

# set predictor variable
vars.pred <- names(ChicagolandSPINDVIVPD.all.NA)[!names(ChicagolandSPINDVIVPD.all.NA) %in% c("date", "year", "doy", "type", "ndvi.obs", "ndvi.modeled.anomaly", "ndvi.modeled", "RESP", "PRED", "type.f")]

mod.out <- data.frame(doy=rep(days.use), 
                      resp=rep(rep(vars.resp, each=length(days.use)), length.out=length(days.use)*length(vars.resp)*length(vars.pred)),
                      pred=rep(vars.pred, each=length(days.use)*length(vars.resp)), 
                      t.stat=NA, p.val=NA, r.sq.m=NA)

mod.out$resp <- factor(mod.out$resp, levels=(vars.resp))
mod.out$pred <- factor(mod.out$pred, levels=vars.pred)
summary(mod.out)

# Need help creating this object below. The example shows an if RESP==PRED condition which doesn't apply
# out.ind <-

# Save our t-stat & pvalue for the climate predictor
mod.out[out.ind, "t.stat"] <- mod.sum$tTable["PRED","t-value"]
mod.out[out.ind, "p.val"] <- mod.sum$tTable["PRED","p-value"]
mod.out[out.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]

# Creating lme model for forest land cover type with all response variables and all predictor variables
mod.var.forest <- nlme::lme(ndvi.obs + ndvi.modeled + ndvi.modeled.anomaly ~ X14d.SPI + X30d.SPI + X60d.SPI + X90d.SPI + VPD, random=list(year=~1), data=ChicagolandSPINDVIVPD.all.NA[ChicagolandSPINDVIVPD.all.NA$type=="forest",], na.action=na.omit)
summary(mod.var.forest)

# Create column with land cover type as factor
ChicagolandSPINDVIVPD.all.NA$type.f <- factor(ChicagolandSPINDVIVPD.all.NA$type, ordered = FALSE)
summary(ChicagolandSPINDVIVPD.all.NA)
is.factor(ChicagolandSPINDVIVPD.all.NA$type.f)
str(ChicagolandSPINDVIVPD.all.NA$type.f)

# Creating lme model with all response variables and all predictor variables and their interaction to land cover type
mod.var.alltypes <- nlme::lme(ndvi.obs + ndvi.modeled + ndvi.modeled.anomaly ~ X14d.SPI*type.f + X30d.SPI*type.f + X60d.SPI*type.f + X90d.SPI*type.f + VPD*type.f, random=list(year=~1), data=ChicagolandSPINDVIVPD.all.NA, na.action=na.omit)
summary(mod.var.alltypes)

# Creating lme model with observed ndvi response variable and all predictor variables and their interaction to land cover type
mod.var.alltypes.ndvi.obs <- nlme::lme(ndvi.obs ~ X14d.SPI*type.f + X30d.SPI*type.f + X60d.SPI*type.f + X90d.SPI*type.f + VPD*type.f, random=list(year=~1), data=ChicagolandSPINDVIVPD.all.NA, na.action=na.omit)
summary(mod.var.alltypes.ndvi.obs)

# Creating lme model with modeled ndvi response variable and all predictor variables and their interaction to land cover type
mod.var.alltypes.ndvi.modeled <- nlme::lme(ndvi.modeled ~ X14d.SPI*type.f + X30d.SPI*type.f + X60d.SPI*type.f + X90d.SPI*type.f + VPD*type.f, random=list(year=~1), data=ChicagolandSPINDVIVPD.all.NA, na.action=na.omit)
summary(mod.var.alltypes.ndvi.modeled)

# Creating lme model with modeled anomaly ndvi response variable and all predictor variables and their interaction to land cover type
mod.var.alltypes.ndvi.modeled.anomaly <- nlme::lme(ndvi.modeled.anomaly ~ X14d.SPI*type.f + X30d.SPI*type.f + X60d.SPI*type.f + X90d.SPI*type.f + VPD*type.f, random=list(year=~1), data=ChicagolandSPINDVIVPD.all.NA, na.action=na.omit)
summary(mod.var.alltypes.ndvi.modeled.anomaly)


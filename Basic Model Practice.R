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
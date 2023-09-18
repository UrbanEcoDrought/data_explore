# I changed some of the data set object names to help me better understand what they are while I'm working. The line following "some orienting plots" returned the following message "Warning message:
# Removed 931 rows containing non-finite values (`stat_smooth()`). Not sure if I messed it up renaming or if its a sources issue
# Call Ross's script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)

unique(ndvi.all$type)

# create data set with only full years, so 2001-2022, cutting 2023
ndvi.allFullYears <- ndvi.all[!ndvi.all$year %in% 2023,]

# some orienting plots
ggplot(data=ndvi.allFullYears[!ndvi.allFullYears$year %in% c(2005,2012),]) + facet_wrap(type~.) +
  stat_smooth(aes(x=doy, y=NDVI)) +
  geom_line(data=ndvi.allFullYears[ndvi.allFullYears$year %in% c(2005, 2012),], aes(x=doy, y=NDVI, col=as.factor(year)))


# we do have duplicates in dates as the collections were taken by different satellites
# running a brief script to take the average of the dates per cover type. Note: some of the measurements appear to be identical for replicate dates and others are close. Will take the mean for now


ndvi.allAgg <- aggregate(NDVI~date + type + doy, FUN=mean, data=ndvi.all)
head(ndvi.allAgg)
 
#Create a new table object with NA values removed from data.
NDVIomitNA <- na.omit(ndvi.allAgg)
summary(NDVIomitNA)

#Create a time series object with all NDVI data with NA values removed.
tsNDVIomitNA <- ts(NDVIomitNA, start=c(2001), frequency=366)

#Plot the time series object with all NDVI data with NA values removed.
ts.plot(tsNDVIomitNA)

library(dplyr)

#Add month column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Month = month(NDVIomitNA$date))
summary(NDVIomitNA)

#Add day column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Day = day(NDVIomitNA$date))

#Add year column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Year = year(NDVIomitNA$date))

#Create a new table object with years 2001-2022.
NDVIomitNA2022 <-NDVIomitNA[which(NDVIomitNA$Year %in% c(2001:2022)),]

#Aggregate points to get one NDVI per day of the year (doy)
NDVI_doy <- aggregate(NDVI ~ doy, data = NDVIomitNA2022, FUN = mean)
summary(NDVI_doy)
plot(NDVI_doy)

#Aggregate points to get one NDVI per day of the year with year as another variable
NDVI_doyYear <- aggregate(NDVI ~ doy + Year, data = NDVIomitNA2022, FUN = mean)
summary(NDVI_doyYear)

#Save graph of the average daily NDVI values for all years and land cover type (y) by the day of the year (x) as png.
png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/NDVI_doyGraph.png", unit="in", height = 5, width = 10, res = 300)
plot(NDVI_doyYear)
dev.off()

# Change each year line in the graph to a different color
NDVI_doyYear$Year <- factor(NDVI_doyYear$Year)

#Save graph of the yearly average daily NDVI values for all land cover types (y) by the day of the year (x) as png.
png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/NDVI_doyByYearGraph.png", unit="in", height=5, width=10, res = 300)
ggplot() + geom_line(data = NDVI_doyYear, aes(x = doy, y = NDVI, color = as.factor(Year))) # CR added as.factor so it wasn't being treated as a continuous variable
dev.off()

library(zoo)

#Create a mean NDVI value for each day of the year for all land cover types and years.
AggNDVI.all <- aggregate(ndvi.all$NDVI ~ ndvi.all$doy, FUN=mean, data=ndvi.all, na.rm=T)

#Create an object of the the maximum NDVI value.
MaxNDVI.all <-max(ndvi.all$NDVI, na.rm=TRUE)

#Create an object of the minimum NDVI value.
MinNDVI.all <-min(ndvi.all$NDVI, na.rm=TRUE)

#Create an object with the range between the MaxNDVI.all and MinNDVI.all
NDVI.allRange <- (max(ndvi.all$NDVI, na.rm=TRUE)-min(ndvi.all$NDVI, na.rm=TRUE))

#Create an object with the mean NDVI value.
NDVI.allMean <- mean(ndvi.all$NDVI, na.rm=TRUE)

#Create an object with the maximum NDVI for the average NDVI value for all land cover types for each date.
MaxNDVI.allbyDate <- max(NDVI_doyYear$NDVI, na.rm=TRUE)

#Create an object with the maximum NDVI for the average NDVI value for all land cover types for each date.
MinNDVI.allbyDate <- min(NDVI_doyYear$NDVI, na.rm=TRUE)

#Create an object with the range between the MaxNDVI.allbyDate and MinNDVI.allbyDate.
NDVI.allbyDateRange <- max(NDVI_doyYear$NDVI, na.rm=TRUE)-min(NDVI_doyYear$NDVI, na.rm=TRUE)

# Create function for moving average
moving_average <- function(x, n = 5) {
  stats::filter(x, rep(1 / n, n), sides = 2)
}

#Create an aggregate of the cleaned NDVI data.
AggNDVI.all$ma <-moving_average(AggNDVI.all$`ndvi.all$NDVI`, 5)

#Plot the aggregate of the cleaned NDVI data.
plot((AggNDVI.all$ma))
plot(AggNDVI.all$`ndvi.all$NDVI`)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

# create column with date in ISO format
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")

# merge ChicagolandSPI and NDVIomitNA2022 by date columns
ChicagolandSPINDVI <- merge (ChicagolandSPI, NDVIomitNA2022, by=c("date"), all.x=TRUE, all.y=TRUE)

# remove all NA values from dataframe (should be years before 2001)
ChicagolandSPINDVINA <- na.omit(ChicagolandSPINDVI)

# create basic linear model with NDVI as response variable and doy as predictor variable
NDVImodel <- lm(NDVI ~ doy, data=NDVIomitNA2022)
summary(NDVImodel)

# create basic linear model with NDVI as response variable and doy and SPI 14 day as predictor variables
SPINDVImodel <- lm(NDVI ~ doy + X14d.SPI, data = ChicagolandSPINDVINA)
summary(SPINDVImodel)

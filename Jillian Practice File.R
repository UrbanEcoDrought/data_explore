# 9/14/2023 Practice

#Create a new table object with NA values removed from data.
NDVIomitNA <- na.omit(ndvi.all)

#Create a time series object with all NDVI data with NA values removed.
tsNDVIomitNA <- ts(NDVIomitNA, start=c(2001), frequency=366)

#Plot the time series object with all NDVI data with NA values removed.
ts.plot(tsNDVIomitNA)

library(dplyr)
library(lubridate)
library(ggplot2)

#Add month column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Month = month(NDVIomitNA$date))

#Add day column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Day = day(NDVIomitNA$date))

#Add year column to data table.
NDVIomitNA<-mutate(NDVIomitNA, Year = year(NDVIomitNA$date))

#Create a new table object with years 2001-2022.
NDVIomitNA2022 <-NDVIomitNA[which(NDVIomitNA$Year %in% c(2001:2022)),]

#Aggregate points to get one NDVI per day of the year (doy)
NDVI_doy <- aggregate(NDVI ~ doy, data = NDVIomitNA2022, FUN = mean)
plot(NDVI_doy)

#Aggregate points to get one NDVI per day of the year with year as another variable
NDVI_doy <- aggregate(NDVI ~ doy + year, data = NDVIomitNA2022, FUN = mean)

#Save graph of the average daily NDVI values for all years and land cover type (y) by the day of the year (x) as png.
png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/NDVI_doyGraph.png", ggplot() + geom_line(data = NDVI_doy, aes(x = doy, y = NDVI, color = year)))
dev.off()

# Change each year line in the graph to a different color
NDVI_doy$year <- factor(NDVI_doy$year)

#Save graph of the yearly average daily NDVI values for all land cover types (y) by the day of the year (x) as png.
png(file="G:/Shared drives/Urban Ecological Drought/data/r_files/figures/NDVI_doyGraph.png",ggplot() + geom_line(data = NDVI_doy, aes(x = doy, y = NDVI, color = year)))
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
MaxNDVI.allbyDate <- max(NDVI_doy$NDVI, na.rm=TRUE)

#Create an object with the maximum NDVI for the average NDVI value for all land cover types for each date.
MinNDVI.allbyDate <- min(NDVI_doy$NDVI, na.rm=TRUE)

#Create an object with the range between the MaxNDVI.allbyDate and MinNDVI.allbyDate.
NDVI.allbyDateRange <- max(NDVI_doy$NDVI, na.rm=TRUE)-min(NDVI_doy$NDVI, na.rm=TRUE)

#Create an aggregate of the cleaned NDVI data.
AggNDVI.all$ma <-moving_average(AggNDVI.all$`ndvi.all$NDVI`, 5)

#Plot the aggregate of the cleaned NDVI data.
plot((AggNDVI.all$ma))
plot(AggNDVI.all$`ndvi.all$NDVI`)
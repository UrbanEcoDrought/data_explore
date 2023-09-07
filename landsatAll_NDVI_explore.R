# script to explore NDVI time series for the land cover classes of the chicago reigon
library(ggplot2)

Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)

unique(ndvi.all$type)

# for now let's just look at full years, so cutting 2023
ndvi.all <- ndvi.all[!ndvi.all$year %in% 2023,]

# some orienting plots
ggplot(data=ndvi.all[!ndvi.all$year %in% c(2005,2012),]) + facet_wrap(type~.) +
  stat_smooth(aes(x=doy, y=NDVI)) +
  geom_line(data=ndvi.all[ndvi.all$year %in% c(2005, 2012),], aes(x=doy, y=NDVI, col=as.factor(year)))


# we do have duplicates in dates as the collections were taken by different satellites
# running a brief script to take the average of the dates per cover type. Note: some of the measurements appear to be identical for replicate dates and others are close. Will take the mean for now


ndvi.all2 <- aggregate(NDVI~date + type, FUN=mean, data=ndvi.all)
head(ndvi.all2)

# separating ndvi object into individual land-cover objects
ndvi.crop <- ndvi.all2[ndvi.all2$type=="crop",]
ndvi.forest <- ndvi.all2[ndvi.all2$type=="forest",]
ndvi.grass <- ndvi.all2[ndvi.all2$type=="grassland",]
ndvi.hi <- ndvi.all2[ndvi.all2$type=="urban-high",]
ndvi.lo <- ndvi.all2[ndvi.all2$type=="urban-low",]
ndvi.med <- ndvi.all2[ndvi.all2$type=="urban-medium",]
ndvi.open <- ndvi.all2[ndvi.all2$type=="urban-open",]

summary(ndvi.open)


## Want to think about the above plot WRT creating an NDVI-anaomalies dataset
# missing days may prove to be an issue

# NDVI correlations----
# creating a correlation matrix for NDVI
# Crop and Forest
ndvi.cor <- merge(ndvi.crop[,c("date", "NDVI")], ndvi.forest[,c("date", "NDVI")], by="date", all=T)
summary(ndvi.cor)
names(ndvi.cor) <- c("date", "crop", "forest")

# adding grassland
ndvi.cor2 <- merge(ndvi.cor, ndvi.grass[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor2) <- paste(c(names(ndvi.cor),"grassland"))
head(ndvi.cor2)

# Adding medium urban
ndvi.cor3 <- merge(ndvi.cor2, ndvi.med[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor3) <- paste(c(names(ndvi.cor2),"urban.med"))
head(ndvi.cor3)

# adding med urban
ndvi.cor4 <- merge(ndvi.cor3, ndvi.lo[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor4) <- paste(c(names(ndvi.cor3),"urban.low"))
head(ndvi.cor4)

# Adding open urban
ndvi.cor5 <- merge(ndvi.cor4, ndvi.open[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor5) <- paste(c(names(ndvi.cor4),"urban.open"))
head(ndvi.cor5)

# Adding high urban
ndvi.cor6 <- merge(ndvi.cor5, ndvi.open[,c("date", "NDVI")], by="date", all=T)
names(ndvi.cor6) <- paste(c(names(ndvi.cor5),"urban.hi"))
head(ndvi.cor6)


ndvi.cor.cc <- complete.cases(ndvi.cor5)

length(unique(ndvi.cor5$date))
dim(ndvi.cor6)
head(ndvi.cor6)
tail(ndvi.cor6)

ndvi.cor.mat <- as.matrix(ndvi.cor6[,!names(ndvi.cor6) %in% "date"])
row.names(ndvi.cor.mat) <- ndvi.cor6$date

cor.ndvi <- cor(ndvi.cor.mat, use="pairwise.complete.obs")
heatmap(cor.ndvi)

cov.ndvi <- cov(ndvi.cor.mat, use="pairwise.complete.obs")
heatmap(cov.ndvi)

# trying this presentation of correlation out for now
# found here https://r-coder.com/correlation-plot-r/
library(PerformanceAnalytics)

chart.Correlation(ndvi.cor.mat, histogram = TRUE, method = "pearson")


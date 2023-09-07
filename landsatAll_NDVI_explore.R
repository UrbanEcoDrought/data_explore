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

# separating ndvi object into individual land-cover objects
ndvi.crop <- ndvi.all[ndvi.all$type=="crop",]
ndvi.forest <- ndvi.all[ndvi.all$type=="forest",]
ndvi.grass <- ndvi.all[ndvi.all$type=="grassland",]
ndvi.hi <- ndvi.all[ndvi.all$type=="urban-high",]
ndvi.lo <- ndvi.all[ndvi.all$type=="urban-low",]
ndvi.med <- ndvi.all[ndvi.all$type=="urban-medium",]
ndvi.open <- ndvi.all[ndvi.all$type=="urban-open",]

summary(ndvi.open)

# some orienting plots
ggplot(data=ndvi.open) +
  geom_line(aes(x=doy, y=NDVI, col=as.factor(year)))



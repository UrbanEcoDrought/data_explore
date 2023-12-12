# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
#Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)

#Subset each satellite out and see which year it encompasses
ndvi.sat5 <- ndvi.all[ndvi.all$satellite=="Landsat5",]
unique(ndvi.sat5$year)
ndvi.sat7 <- ndvi.all[ndvi.all$satellite=="Landsat7",]
unique(ndvi.sat7$year)
ndvi.sat8 <- ndvi.all[ndvi.all$satellite=="Landsat8",]
unique(ndvi.sat8$year)
ndvi.sat9 <- ndvi.all[ndvi.all$satellite=="Landsat9",]
unique(ndvi.sat9$year)

#Create a subset of satellites 5 and 7
ndvi.sats57<- subset(ndvi.all, subset = satellite %in% c("Landsat5", "Landsat7"))




# organizing the various NDVI products that Christy produced from GEE
# pulling in data from googledrive
library(ggplot2)
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")

# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")

google.drive <- Sys.getenv("GOOGLE_DRIVE")
pathDat <- file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract")

lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

ndviAll <- data.frame()
for(LCTYPE in lcnames){
  fileL8 <- dir(file.path(pathDat), paste0("Landsat8_", LCTYPE))[length(dir(pathDat, paste0("Landsat8_", LCTYPE)))]
  fileL9 <- dir(file.path(pathDat), paste0("Landsat9_", LCTYPE))[length(dir(pathDat, paste0("Landsat9_", LCTYPE)))]
  fileL7 <- dir(file.path(pathDat), paste0("Landsat7_", LCTYPE))[length(dir(pathDat, paste0("Landsat7_", LCTYPE)))]
  fileL5 <- dir(file.path(pathDat), paste0("Landsat5_", LCTYPE))[length(dir(pathDat, paste0("Landsat5_", LCTYPE)))]

  landsat8 <- read.csv(file.path(pathDat, fileL8))
  landsat9 <- read.csv(file.path(pathDat, fileL9))
  landsat7 <- read.csv(file.path(pathDat, fileL7))
  landsat5 <- read.csv(file.path(pathDat, fileL5))
  
  landsat8$satellite <- "landsat 8"
  landsat9$satellite <- "landsat 9"
  landsat7$satellite <- "landsat 7"
  landsat5$satellite <- "landsat 5"
  
  landsatAll <- rbind(landsat8, landsat9, landsat7, landsat5)
  # landsatAll <- rbind(landsat8, landsat9)
  landsatAll$type <- LCTYPE
  
  ndviAll <- rbind(ndviAll, landsatAll)
}
summary(ndviAll)


# setting date as date
ndviAll$date <- lubridate::as_date(ndviAll$date)

# creating a year variable for easier subsetting
ndviAll$year <- lubridate::year(ndviAll$date)

# creatign a day of year variable in case it is useful
ndviAll$doy <- lubridate::yday(ndviAll$date)
head(ndviAll)

ggplot(data=ndviAll[ndviAll$date>"2023-01-01",]) + facet_wrap(type~.) +
  geom_point(aes(x=date, y=NDVI, col=satellite))

saveRDS(ndviAll, file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))

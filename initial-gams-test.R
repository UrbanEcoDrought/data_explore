library(mgcv)
library(ggplot2)


Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

ndvi.latest <- read.csv(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract/NDVIall_latest.csv"))
ndvi.2024 <- ndvi.latest[ndvi.latest$year==2024,]
ndvi.2024 <- ndvi.2024[ndvi.2024$type=='grassland',]

ggplot(data=ndvi.2024[,], aes(x=yday,y=NDVI)) + geom_point()

gam_2024 <- gam(NDVI ~ s(yday), data=ndvi.2024)
gam.check(gam_2024)

newDF <- with(ndvi.2024, data.frame(yday=seq(1, 254)))
y_pred <- predict(gam_2024, newDF, type='response')

plot(NDVI ~ yday, data = ndvi.2024)
lines(y_pred ~ yday, data=newDF, col='red')

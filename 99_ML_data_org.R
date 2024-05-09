# pulling in the various data streams we have to combine into a single dataframe to work with in the ML framework

# loading in data from the daily models because we have the raw ndvi, met data, and the lagged calculation with residuals for each covery type.


lc.list <- c("urban-high", "urban-medium", "urban-low", "urban-open", "crop", "forest", "grassland")


# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.files <- file.path(google.drive, "data/r_files/processed_files/daily_models")

# wanting to only pull the non-summary files
file.list <- list.files(path.files)
file.list.dat <- file.list[grep("DailyModel_NDVI-", file.list)]


ndvi.all.ml <- NULL
pb <- txtProgressBar(min=0, max=length(file.list.dat), style=3)
pb.ind=1

for(i in file.list.dat){
  
  temp <- read.csv(file.path(google.drive, "data/r_files/processed_files/daily_models",i), header=T)
  
  # pulling out just the variables of interest
  temp2 <- temp[,!names(temp) %in% c("Date", "NDVI.pred", "NDVI.predNorm", "resid")]
  temp2$resid.NDVIlag <- temp2$NDVI - temp2$NDVI.predLag
  
  if(is.null(ndvi.all.ml)) ndvi.all.ml <- temp2 else ndvi.all.ml <- rbind(ndvi.all.ml,temp2)
  setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
}

ndvi.all.ml$date <- as.Date(ndvi.all.ml$date)
ndvi.all.ml$satellite <- as.factor(ndvi.all.ml$satellite)
ndvi.all.ml$type <- as.factor(ndvi.all.ml$type)
summary(ndvi.all.ml)

saveRDS(ndvi.all.ml, file.path(google.drive, "data/r_files/processed_files/ndviAll_ml_data.RDS"))

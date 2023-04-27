# exploring the dendrometer data a bit and calculating maximum swell differences for a given day.
library(ggplot2)

# loading in TOMST dendrometer data

dendro.dat <- readRDS("../data_gathering_cleaniing/processed_data/morton_dendrometer_cleaned.rds")

# have some time zone issues that I will need to talk through with Luke to make sure that the dates align
# shifting to be central time

dendro.dat$date.time.central <- lubridate::with_tz(dendro.dat$date.time, tzone="US/Central") # Shifting time to central time
dendro.dat$date.central <- lubridate::date(dendro.dat$date.time.central)
# Luke said that 2021 and 2022 data should be the most solid
dendro.dat2 <- dendro.dat[dendro.dat$date.central >= "2021-01-01",]

summary(dendro.dat2)
# some initial plots to see what the data are doing
head(dendro.dat2)

pdf(file="figures/dendrometer/dendrometer_qaqc.pdf", height= 8, width = 10)
for(i in unique(dendro.dat2$plotID)){
print(ggplot(dendro.dat2[dendro.dat2$plotID==i,]) + facet_grid(id.num~.) +
        geom_line(aes(x=date.time.central, y = t1)) + 
        labs(title = paste0(i, " TOMST Dendrometers 2021 -> Present"))
        
)
}
dev.off()


# getting the max variability for a given day

var.dat <- data.frame(date = unique(dendro.dat2$date.central))

pb <- txtProgressBar(min=0, max=length(unique(var.dat$date)), style=3)
pb.ind=1
for(i in var.dat$date){
  for(j in unique(dendro.dat2$id.num)){
    temp <- dendro.dat2[dendro.dat2$date.central==i & dendro.dat2$id.num==j, ]  
    max.temp <- max(temp$t1) # max value for the day
    min.temp <- min(temp$t1) # min value for the day
    
    range.temp <- max.temp - min.temp # calculating the range of the swell for each day
    
    var.dat[var.dat$date==i, j] <- range.temp # saving with dates as rows and sensors as columns
    
  }
  setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
}

summary(var.dat)
# need to stack data to be more easily read.

var.dat.stack <- stack(var.dat[,!names(var.dat) %in% "date"])
names(var.dat.stack) <- c("swell", "id.num")
head(var.dat.stack)
var.dat.stack$date <- var.dat$date

saveRDS(var.dat.stack, "processed_data/dendrometer_swell_variability.RDS")

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

pb <- txtProgressBar(min=0, max=length(unique(dendro.dat2$id.num)), style=3)
pb.ind=1
for(i in unique(dendro.dat2$id.num)){
    temp <- dendro.dat2[dendro.dat2$id.num==i, ]  
  
  for(j in unique(temp$date.central)){
    temp.time <- temp[temp$date.central==j,]
    
    max.temp <- max(temp.time$t1, na.rm=T) # max value for the day
    min.temp <- min(temp.time$t1, na.rm=T) # min value for the day
    
    range.temp <- max.temp - min.temp # calculating the range of the swell for each day
    
    var.dat[var.dat$date==j, i] <- range.temp # saving with dates as rows and sensors as columns
    
  }
  setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
}

summary(var.dat)
# need to stack data to be more easily read.

var.dat.stack <- stack(var.dat[,!names(var.dat) %in% "date"])
names(var.dat.stack) <- c("swell", "id.num")
head(var.dat.stack)
var.dat.stack$date <- var.dat$date

# bringing in the plot ID with the sensor number
var.dat.stack$plotID <- dendro.dat2$plotID[match(var.dat.stack$id.num, dendro.dat2$id.num)]
var.dat.stack$month <- lubridate::month(var.dat.stack$date)
var.dat.stack$month.name <- lubridate::month(var.dat.stack$date, abb=T, label=T)
var.dat.stack$year <- lubridate::year(var.dat.stack$date)
var.dat.stack$doy <- lubridate::yday(var.dat.stack$date)

saveRDS(var.dat.stack, "processed_data/dendrometer_swell_variability.RDS")

##############
# making plots of the swell


var.dat.stack <- readRDS("processed_data/dendrometer_swell_variability.RDS")
# going to truncate the data from may - august to avoid some of the weirdness

head(var.dat.stack)

var.dat.stack2 <- var.dat.stack[var.dat.stack$month >=4& var.dat.stack$month <=9,]

# bringing in daymet data
daymet.df <- readRDS("../data_gathering_cleaniing/processed_data/morton_daymet.RDS")
head(daymet.df)

daymet.df.short <- daymet.df[daymet.df$yday %in% var.dat.stack2$doy,]

# Plotting the swell by plot 
library(zoo)
pdf(file="figures/dendrometer/plot_swell.pdf", height= 8, width = 10)
for(i in unique(var.dat.stack2$plotID)){
  print(
    ggplot(data=var.dat.stack2[var.dat.stack2$plotID==i,]) + facet_grid(id.num~year) +
      geom_tile(data=daymet.df.short, aes(x=yday, y = 1, fill= vpd.kpa), height= 100)+
      geom_line(aes(x=doy, y=swell)) +
      geom_line(aes(x=doy, y=rollmean(swell, 10, na.pad=T, align="right")), col="#0072B2", linewidth=0.95) +
      scale_fill_gradient(high = "#d8b365", low= "#5ab4ac") +
      labs(title = paste0(i, " TOMST Dendrometers April-Sept; Daily Swell w/ 10day rolling average"), x = "DOY", y = "Swell (Daily Max - Daily Min)") +
    coord_cartesian(ylim=c(0,max(var.dat.stack2$swell, na.rm=T)))+
      scale_x_continuous(expand=c(0,0))
  )
}
dev.off()

ggplot(data= daymet.df.short) +
  geom_tile(aes(x=yday, y = 1, fill= vpd.kpa)) +
scale_fill_gradient(high = "#d8b365", low= "#5ab4ac")


#############################
# Correlation Analysis----

summary(var.dat.stack2)
summary(daymet.df.short)

# combining vpd and swell into a single data frame

cor.dat <- data.frame(year = var.dat.stack2$year,
                      doy = var.dat.stack2$doy,
                      swell = var.dat.stack2$swell,
                      plotID = var.dat.stack2$plotID)

cor.dat <- merge(cor.dat, daymet.df.short[,c("year", "yday", "vpd.kpa")], by.x=c("year", "doy"), by.y=c("year", "yday"))

head(cor.dat)

cor.output <- data.frame(plotID=unique(cor.dat$plotID),
                         r.value = NA,
                         p.value = NA)

for(i in unique(cor.dat$plotID)){
  test <- cor.dat[cor.dat$plotID==i,]
  
 meow <-  cor.test(test$swell, test$vpd.kpa, na.rm=T)
  
  cor.output[cor.output$plotID==i,"r.value"] <- meow$estimate
  cor.output[cor.output$plotID==i,"p.value"] <- meow$p.value
}

cor.output
cor.output$sig <- ifelse(cor.output$p.value <0.05, "YES", "NO")



cov.output <- data.frame(plotID=unique(cor.dat$plotID),
                         cov.value = NA,
                         p.value = NA)

for(i in unique(cor.dat$plotID)){
  test <- cor.dat[cor.dat$plotID==i,]
  
  meow <-  cov(test$swell, test$vpd.kpa, use="pairwise.complete.obs")
  
  cov.output[cov.output$plotID==i,"cov.value"] <- meow
  
}

cov.output
cov.output$sig <- ifelse(cov.output$p.value <0.05, "YES", "NO")


ggplot(data=cor.dat) + facet_wrap(plotID~.) +
  geom_point(aes(x=swell, y = vpd.kpa))+
  stat_smooth(aes(x=swell, y=vpd.kpa),method="lm")

plot.order <- cor.output[order(cor.output$r.value, decreasing = T),"plotID"]
cor.output$plotID <- factor(cor.output$plotID, levels = plot.order)

cor.output$spp <- substr(cor.output$plotID,1,4)

# Reading in some preliminary species drought tolerance designations
tol.dat <- read.csv("input_data/spp_drought_tolerance_MRA.csv", header=T)

cor.output$tolerance <- tol.dat$composite.tolerance[match(cor.output$spp, tol.dat$spp)]


png(filename = "figures/dendrometer/swell_vpd_cors.png", height = 8, width= 10, unit = "in", res=300)
ggplot(data=cor.output)+
  geom_point(aes(y=rev(plotID),x=r.value, col=rev(tolerance)), size=4) +
  labs(x= "Pearson's R", y = "PlotID", title= "Correlation between Swell and VPD at Morton Forestry Plots") +
  theme_bw()
dev.off()
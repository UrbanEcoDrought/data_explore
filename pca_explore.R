# Exploring christy's heat island dataset a bit more

library(ggplot2)
library(car)
require(plyr)
require(ggplot2)
require(RColorBrewer)
require(reshape)
require(scales)
require(zoo)
require(gridExtra)
require(grid)
require(ggsn)
require(sf)
library(vegan)
require(ggbiplot)
dat <- read.csv("G:/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis/data_processed/OLD_city_stats_all.csv", header=T)

# will need complete cases for a PCA analysis
dat.nobiome <- dat[!names(dat) %in% c("biome")]
dat2 <- dat.nobiome[complete.cases(dat.nobiome),]
summary(dat2)

dat2.short <- dat2[,!names(dat2) %in% c("ISOURBID", "ES00POP", "ISO3","URBID", "NAME", "LATITUDE", "LONGITUDE", "n.pixels")]
row.names(dat2.short) <- dat2$ISOURBID

# setting up for an ordination
test <- prcomp(dat2.short, scale. = T, center = T)
plot(test, type = "line", main="Scree Plot")
ggbiplot(test)

loadings <- data.frame(ISOURBID = dat2$ISOURBID,
                       pc1 = test$x[,1])

dat2.load <- merge(dat2, loadings, by="ISOURBID")
summary(dat2.load)

# setting color by sign on PC1
dat2.load$sign <- as.factor(ifelse(dat2.load$pc1 > 0, "Pos", "Neg"))


# Mapping the loadings around the world

worldmap2 <- map_data("world")
worldmap2$region <- as.factor(worldmap2$region)
worldmap2$subregion <- as.factor(worldmap2$subregion)

ggplot(data=dat2.load)+ 
  geom_point(aes(x=LONGITUDE, y = LATITUDE, color=sign)) +
  # scale_fill_gradient2(low="mediumpurple3", mid="white", high="forestgreen", na.value = NA) +
  #labs(fill="Diff from Mid-Century Chicago", x = "Latitude", y= "Longitude") +
  # scale_color_gradient2(low="mediumpurple3", high="forestgreen") +
  geom_path(data=worldmap2, aes(x=long, y=lat, group=group), color="black")+
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1)) +
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_text(size=14, face="bold"),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.text=element_text(face="bold", size=20),
        legend.position = "top",
        axis.title.x=element_text(color="black", size=14, face="bold"), 
        axis.title.y=element_text(color="black", size=14,face="bold"),  
        plot.title = element_text(face="bold", size = 22))
# 
#   coord_cartesian(xlim=range(dat2.load$LONGITUDE), ylim=range(dat2.load$LATITUDE)) + 
#   scale_y_continuous(limits=range(dat2.load$LATITUDE, na.rm=T) + c(-2,2), expand=c(0,0)) 

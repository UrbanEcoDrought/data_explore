# exploring the dendrometer data a bit and calculating maximum swell differences for a given day.
library(ggplot2)

# loading in TOMST dendrometer data

dendro.dat <- readRDS("../data_gathering_cleaniing/processed_data/morton_dendrometer_cleaned.rds")

# Luke said that 2021 and 2022 data should be the most solid
dendro.dat2 <- dendro.dat[dendro.dat$date > "2021-01-01",]

summary(dendro.dat2)
# some initial plots to see what the data are doing
head(dendro.dat2)

pdf(file="figures/dendrometer/dendrometer_qaqc.pdf", height= 8, width = 10)
for(i in unique(dendro.dat2$plotID)){
print(ggplot(dendro.dat2[dendro.dat2$plotID==i,]) + facet_grid(id.num~.) +
        geom_line(aes(x=date.time, y = t1)) + 
        labs(title = paste0(i, " TOMST Dendrometers 2021 -> Present"))
        
)
}
dev.off()

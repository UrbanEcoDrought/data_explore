# Data Extreme exploration

# Pulled some daymet weather data from climatEngine.org
# Pulled seasonal data: March, April May = MAM; June, July, August = JJA
# max temp; precip; vpd; Hargreves Potential Evapotranspiration
# Date range: 1980-2022

dat <- read.csv("G:/Shared drives/Urban Ecological Drought/data/r_files/input_files/data_explore/climate_engine_spring_summer_daymet_weather.csv", header=T)

head(dat)

mam.dat <- dat[dat$season=="MAM",]
jja.dat <- dat[dat$season=="JJA",]

# creating the 95%CI cut points
mam.temp.cuts <- quantile(mam.dat[,c("temp.c")], probs = c(0.025, 0.975))
mam.precip.cuts <- quantile(mam.dat[,c("precip.mm")], probs = c(0.025, 0.975))
mam.vpd.cuts <- quantile(mam.dat[,c("vpd.kpa")], probs = c(0.025, 0.975))
mam.pet.cuts <- quantile(mam.dat[,c("pet.hargreves")], probs = c(0.025, 0.975))

jja.temp.cuts <- quantile(jja.dat[,c("temp.c")], probs = c(0.025, 0.975))
jja.precip.cuts <- quantile(jja.dat[,c("precip.mm")], probs = c(0.025, 0.975))
jja.vpd.cuts <- quantile(jja.dat[,c("vpd.kpa")], probs = c(0.025, 0.975))
jja.pet.cuts <- quantile(jja.dat[,c("pet.hargreves")], probs = c(0.025, 0.975))

# MAM
mam.hot <- mam.dat[mam.dat$temp.c > mam.temp.cuts[2], c("year", "temp.c")]
names(mam.hot) <- c("year", "value")
mam.hot$var <- "temp.c"
mam.hot$type <- "high"

mam.cold <- mam.dat[mam.dat$temp.c < mam.temp.cuts[1], c("year", "temp.c")]
names(mam.cold) <- c("year", "value")
mam.cold$var <- "temp.c"
mam.cold$type <- "low"

mam.wet <- mam.dat[mam.dat$precip.mm > mam.precip.cuts[2], c("year", "precip.mm")]
names(mam.wet) <- c("year", "value")
mam.wet$var <- "precip.mm"
mam.wet$type <- "high"

mam.dry <- mam.dat[mam.dat$precip.mm < mam.precip.cuts[1], c("year", "precip.mm")]
names(mam.dry) <- c("year", "value")
mam.dry$var <- "precip.mm"
mam.dry$type <- "low"


mam.hi.vpd <- mam.dat[mam.dat$vpd.kpa > mam.vpd.cuts[2], c("year", "vpd.kpa")]
names(mam.hi.vpd) <- c("year", "value")
mam.hi.vpd$var <- "vpd.kpa"
mam.hi.vpd$type <- "high"

mam.low.vpd <- mam.dat[mam.dat$vpd.kpa < mam.vpd.cuts[1], c("year", "vpd.kpa")]
names(mam.low.vpd) <- c("year", "value")
mam.low.vpd$var <- "vpd.kpa"
mam.low.vpd$type <- "low"

mam.hi.pet <- mam.dat[mam.dat$pet.hargreves > mam.pet.cuts[2], c("year", "pet.hargreves")]
names(mam.hi.pet) <- c("year", "value")
mam.hi.pet$var <- "pet.hargreves"
mam.hi.pet$type <- "high"

mam.low.pet <- mam.dat[mam.dat$pet.hargreves < mam.pet.cuts[1], c("year", "pet.hargreves")]
names(mam.low.pet) <- c("year", "value")
mam.low.pet$var <- "pet.hargreves"
mam.low.pet$type <- "low"

# JJA
jja.hot <- jja.dat[jja.dat$temp.c > jja.temp.cuts[2], c("year", "temp.c")]
names(jja.hot) <- c("year", "value")
jja.hot$var <- "temp.c"
jja.hot$type <- "high"

jja.cold <- jja.dat[jja.dat$temp.c < jja.temp.cuts[1], c("year", "temp.c")]
names(jja.cold) <- c("year", "value")
jja.cold$var <- "temp.c"
jja.cold$type <- "low"

jja.wet <- jja.dat[jja.dat$precip.mm > jja.precip.cuts[2], c("year", "precip.mm")]
names(jja.wet) <- c("year", "value")
jja.wet$var <- "precip.mm"
jja.wet$type <- "high"

jja.dry <- jja.dat[jja.dat$precip.mm < jja.precip.cuts[1], c("year", "precip.mm")]
names(jja.dry) <- c("year", "value")
jja.dry$var <- "precip.mm"
jja.dry$type <- "low"

# Flipped VPD so that the colors would align in the plots
jja.hi.vpd <- jja.dat[jja.dat$vpd.kpa > jja.vpd.cuts[2], c("year", "vpd.kpa")]
names(jja.hi.vpd) <- c("year", "value")
jja.hi.vpd$var <- "vpd.kpa"
jja.hi.vpd$type <- "high"
# Flipped VPD so that the colors would align in the plots
jja.low.vpd <- jja.dat[jja.dat$vpd.kpa < jja.vpd.cuts[1], c("year", "vpd.kpa")]
names(jja.low.vpd) <- c("year", "value")
jja.low.vpd$var <- "vpd.kpa"
jja.low.vpd$type <- "low"


jja.hi.pet <- jja.dat[jja.dat$pet.hargreves > jja.pet.cuts[2], c("year", "pet.hargreves")]
names(jja.hi.pet) <- c("year", "value")
jja.hi.pet$var <- "pet.hargreves"
jja.hi.pet$type <- "high"

jja.low.pet <- jja.dat[jja.dat$pet.hargreves < jja.pet.cuts[1], c("year", "pet.hargreves")]
names(jja.low.pet) <- c("year", "value")
jja.low.pet$var <- "pet.hargreves"
jja.low.pet$type <- "low"

# putting extremes into seasonal data frames

jja.extreme <- rbind(jja.hot, jja.cold, jja.wet, jja.dry, jja.hi.vpd, jja.low.vpd, jja.hi.pet, jja.low.pet)
mam.extreme <- rbind(mam.hot, mam.cold, mam.wet, mam.dry, mam.hi.vpd, mam.low.vpd, mam.hi.pet, mam.low.pet)

# reshaping the seasonal data to show the extremes and to be easier to plot in ggplot

summary(mam.dat)
mam.dat.stack <- stack(mam.dat[,!names(mam.dat)%in% c("year", "season")])
head(mam.dat.stack)
names(mam.dat.stack) <- c("values", "variable")
mam.dat.stack$year <- mam.dat$year
mam.dat.stack$season <- as.factor("MAM")


summary(jja.dat)
jja.dat.stack <- stack(jja.dat[,!names(jja.dat)%in% c("year", "season")])
head(jja.dat.stack)
names(jja.dat.stack) <- c("values", "variable")
jja.dat.stack$year <- jja.dat$year
jja.dat.stack$season <- as.factor("JJA")

# putting in a variable for extreme years

for(i in mam.dat.stack$variable){
  temp <- mam.dat.stack[mam.dat.stack$variable==i,]
  extremes <- mam.extreme[mam.extreme$var==i,]
  for(j in mam.dat.stack$year){
    temp.year <- temp[temp$year==j,]
    
    mam.dat.stack[mam.dat.stack$variable==i & mam.dat.stack$year==j, "type"] <- ifelse(temp.year$year%in%extremes$year, extremes[extremes$year==j, "type"], "moderate")
    
    
  }
  
}
head(mam.dat.stack)

for(i in jja.dat.stack$variable){
  temp <- jja.dat.stack[jja.dat.stack$variable==i,]
  extremes <- jja.extreme[jja.extreme$var==i,]
  for(j in jja.dat.stack$year){
    temp.year <- temp[temp$year==j,]
    
    jja.dat.stack[jja.dat.stack$variable==i & jja.dat.stack$year==j, "type"] <- ifelse(temp.year$year%in%extremes$year, extremes[extremes$year==j, "type"], "moderate")
    
    
  }
  
}
head(jja.dat.stack)

# combining the stacked datasets together
chi.dat.stack <- rbind(jja.dat.stack, mam.dat.stack)

library(ggplot2)
library(gameofthrones)
chi.dat.stack$type <- factor(chi.dat.stack$type, levels = c("low", "moderate", "high"))

ggplot(data=chi.dat.stack) + facet_grid(variable~season, scales="free_y") +
  geom_boxplot(aes(x=type, y=values, fill=type)) +
  scale_fill_got_d(option="Daenerys")  

ggplot(data=chi.dat.stack) + facet_grid(variable~season, scales="free_y") +
  geom_line(aes(x=year, y=values), size=0.75, color="darkgrey") +
  geom_point(aes(x=year, y=values, color=type), size=3) +
  #geom_label(data=chi.dat.stack[!chi.dat.stack$type=="moderate",], aes(x=year, y=values, label=year), size=3)+
  scale_color_got_d(option="Tully") +
  theme_bw()


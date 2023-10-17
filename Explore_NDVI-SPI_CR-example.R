# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# reading in NDVI product --> our RESPONSE VARS would be 
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndvi_detrended_df.RDS"))
head(ndvi.all)

# Subsetting to a single landcover class for testing
LCtype <- "urban-medium"
ndvi.test <- ndvi.all[ndvi.all$type==LCtype,]
summary(ndvi.test)

# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))

# create column with date in ISO format
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
summary(ChicagolandSPI)

# merge ChicagolandSPI and NDVIomitNA2022 by date columns --> we dont' want to keep SPI for things we don't have NDVI for
ChicagolandSPINDVI <- merge (ChicagolandSPI, ndvi.test, by=c("date"), all.x=F, all.y=TRUE)
summary(ChicagolandSPINDVI)

# Find out what days we want to model --> setting this up now so we can work through it
ChicagolandSPINDVI$month <- lubridate::month(ChicagolandSPINDVI$date)
days.use <- unique(ChicagolandSPINDVI$doy[ChicagolandSPINDVI$month >=3 & ChicagolandSPINDVI$month <=9])
days.use 
resp.vars <- c("ndvi.obs", "ndvi.anomaly")
pred.vars <-c ("X14d.SPI", "X30d.SPI", "X60d.SPI", "X90d.SPI")

###################################
## Here's the BASIC model --> make this work outside of a loop before building it into a loop
###################################
# specifying our subsets for days/responses/predictors
## Each level of a loop will iterate through ONE of these at a time --> one loop will go through days; that would be nexted in a model that goes through predictors and that would be nested in one that goes through reponses
## But the base model just does ONE LEVLE OF EACH AT A TIME
RESP <- resp.vars[1] # ndvi.obs; NOTE: For some reason the anomaly wasn't working!  Very weird.
PRED <- pred.vars[1] # X14d.SPI 
dayNOW <- days.use[1] # Just starting with the first DOY for the year 

# Here we're subsetting our big data frame to the SMALL temporal window we want --> this should help with temporal stationarity in the effects of our predictors
dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,] # Subsets things to a particular window (not just a single day); otherwise we were working with just 5 years of data, which isn't helpful

# Selecting our predictors & responses and creating "dummy" columns called e.g. "RESP" that are filled with the values of whatever column RESP (as an object) is named
dat.tmp$RESP <- dat.tmp[,RESP] # This way of writing this creates a variable called RESP from the column that matches whatever value RESP is right now (e.g. ndvi.modeled.anomaly)
dat.tmp$PRED <- dat.tmp[,PRED] # This way of writing this creates a variable called PRED from the column that matches whatever value PRED is right now (e.g. X14d.SPI)
summary(dat.tmp) # Checking to make sure it all looks okay
dim(dat.tmp)

# This is running the BASIC model
mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
# )
mod.sum <- summary(mod.var)
mod.sum
MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
AIC(mod.var)


# Doing some diagnostic plots to check to make sure this model isn't fundamentally terrible
hist(resid(mod.var)) # Looking for a normal distribution
plot(resid(mod.var) ~ predict(mod.var)); abline(h=0, col="red") # Checking to make sure the values are evenly distributed around 0 (homoscedasticity)
plot(predict(mod.var)~dat.tmp[complete.cases(dat.tmp), "PRED"]); abline(a=0, b=1, col="red") # Predicted vs. observed with a 1:1 line added; ideally these would match well, but we want them to at least not show a clear bias across the range.  (Note: in this example, the effect of our predictor is non-significant and the R2 is terrible, so we won't see a clean relationship; we're working with like March 1, so this makes sense)

# Save our t-stat & pvalue for the climate predictor <- We'll need to figure out how to set up a dataframe to store our useful info, but for the moment, we may want to just set up a dataframe and use "rbind" for a small bit

# Creating a blank dataframe object with the columns we want to save
## NOTE: If running multiple models, we need to set this up BEFORE we get here otherwise it will get overwritten
## Each column is basically the different levels that we want to run the model on plus the data we want to save; We're just filling it with blank values for the moment
mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 

# Saving the info on the levels we ran
mod.out[1, "LandCover"] <- LCtype
mod.out[1, "PRED"] <- PRED
mod.out[1, "RESP"] <- RESP
mod.out[1, "DOY"] <- dayNOW

# Saving the key info from the model we ran --> this is what we will use to compare not just the "best fit" metrics, but also key standardized info on EFFECT SIZE & DIRECTION (i.e. the t-stat and parameter coeffecient)
mod.out[1,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
mod.out[1,"coef"] <- mod.sum$tTable["PRED","Value"]
mod.out[1,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
mod.out[1,"p.val"] <- mod.sum$tTable["PRED","p-value"]
mod.out[1, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
mod.out[1, "AIC"] <- AIC(mod.var) 

mod.out # Printing our dataframe to make sure everythign saved appropriately
###################################


# Okay, now that we have the basic model work, lets show how that works if we loop through day of year

###################################
## Iterating the model through the day of year, but keeping the pred & resp the same
###################################
# specifying our subsets for days/responses/predictors
## Each level of a loop will iterate through ONE of these at a time --> one loop will go through days; that would be nested in a model that goes through predictors and that would be nested in one that goes through responses
## But the base model just does ONE LEVEL OF EACH AT A TIME
PRED <- pred.vars[1] # X14d.SPI 
RESP <- resp.vars[1] # ndvi.obs; NOTE: For some reason the anomaly wasn't working!  Very weird.

# Creating a blank dataframe object with the columns we want to save
## NOTE: We're doing this first so we don't overwrite it every time in the loop
## Each column is basically the different levels that we want to run the model on plus the data we want to save; We're just filling it with blank values for the moment
mod.out <- data.frame(LandCover = NA, PRED=NA, RESP=NA, DOY=NA, intercept=NA, coef=NA, t.stat=NA, p.val=NA, r.sq.m=NA, AIC=NA) 

row.ind = 0 # Setting up an index that will tell us what row to save things in; we should start with 0 because we haven't done anything yet
for(i in 1:length(days.use)){
  dayNOW <- days.use[i] # This is almost exactly the same as above, but now i will go from 1 to 215 (the number of unique days.use we have)
  
  ## FROM HERE through the model IS IDENTICAL TO WHAT IS ABOVE FOR THE SINGLE EXAMPLE
  # Here we're subsetting our big data frame to the SMALL temporal window we want --> this should help with temporal stationarity in the effects of our predictors
  dat.tmp <- ChicagolandSPINDVI[ChicagolandSPINDVI$doy>=dayNOW-7 & ChicagolandSPINDVI$doy<=dayNOW+7 ,] # Subsets things to a particular window (not just a single day); otherwise we were working with just 5 years of data, which isn't helpful
  
  # Selecting our predictors & responses and creating "dummy" columns called e.g. "RESP" that are filled with the values of whatever column RESP (as an object) is named
  dat.tmp$RESP <- dat.tmp[,RESP] # This way of writing this creates a variable called RESP from the column that matches whatever value RESP is right now (e.g. ndvi.modeled.anomaly)
  dat.tmp$PRED <- dat.tmp[,PRED] # This way of writing this creates a variable called PRED from the column that matches whatever value PRED is right now (e.g. X14d.SPI)
  summary(dat.tmp) # Checking to make sure it all looks okay
  dim(dat.tmp)
  
  # This is running the BASIC model
  mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1), data=dat.tmp[,], na.action=na.omit)
  # )
  mod.sum <- summary(mod.var)
  ## Commenting out the checks because we know it works because we ran a small test
  # mod.sum
  # AIC(mod.var)
  
  # Saving the info on the levels we ran
  # # KEY DIFFERENCE: Here we need to specify which row the data will get saved in --> because this is a single level loop, we will only need as many rows as we have days of year
  # # # NOTE: WHen we start nesting loops, setting up this index will be more complicated
  row.ind = row.ind+1 # We want to save our output in the next row of the dataframe (the first one will be 0+1 = 1; the one after should become 2 and sowon)
  
  mod.out[row.ind, "LandCover"] <- LCtype
  mod.out[row.ind, "PRED"] <- PRED
  mod.out[row.ind, "RESP"] <- RESP
  mod.out[row.ind, "DOY"] <- dayNOW
  
  # Saving the key info from the model we ran --> this is what we will use to compare not just the "best fit" metrics, but also key standardized info on EFFECT SIZE & DIRECTION (i.e. the t-stat and parameter coeffecient)
  mod.out[row.ind,"intercept"] <- mod.sum$tTable["(Intercept)","Value"]
  mod.out[row.ind,"coef"] <- mod.sum$tTable["PRED","Value"]
  mod.out[row.ind,"t.stat"] <- mod.sum$tTable["PRED","t-value"]
  mod.out[row.ind,"p.val"] <- mod.sum$tTable["PRED","p-value"]
  mod.out[row.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
  mod.out[row.ind, "AIC"] <- AIC(mod.var) 
  
}
summary(mod.out)
head(mod.out)

# Making a quick graph to check and visually see if what we did is making sense --> JILLIAN: PLEASE SAVE THESE AS PNG FILES IN YOUR WORKFLOW
# This is just to look at the effect sizes as measured by the t-stat
ggplot(data=mod.out) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat))


# Same plot, but only showing statistically significant effects --> NOTE HOW FOR MOST OF THE SUMMER
ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat))


# Adding some intelligible breaks to the graph to help with interpretation
month.breaks <- data.frame(doy = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                           month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month.breaks.short <- data.frame(doy = c(1, 91, 182, 274),
                                 month = c("Jan", "Apr", "Jul", "Oct"))


# THIS IS THE MAIN GRAPH THAT WE WANT TO SAVE --> JILLIAN: PLEASE SAVE THESE AS PNG FILES IN YOUR WORKFLOW 
# (The others are shown just to show how we built to this)
ggplot(data=mod.out[mod.out$p.val<0.05,]) +
  facet_grid(.~RESP) +
  geom_tile(aes(x=DOY, y=PRED, fill=t.stat)) +
  scale_x_continuous(breaks=month.breaks$doy, labels=month.breaks$month)
###################################


###################################
# THE NEXT STEPS ARE to nest the DOY loop within a loop that iterates through our predictors
# Once we run the models and save the output, we can start to compare what correlates when and then look at the metrics of model performance to decide among predictrs
# This starts to get complicated, but we need to get to this stage quickly.
###################################


###################################


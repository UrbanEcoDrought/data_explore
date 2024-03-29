#Play with NDVI predictions with random forest
#By Lindsay Darling

  #Load libraries-----------

library(tidyverse)
library(tidylog)
library(randomForest)  #Random forests
library(units)         #Change units in spatial work
library(pdp)           #Partial dependence plots
library(vip)           #Variable importance plots
library(rpart)
library(rpart.plot)

#Set wd

#Pick computer

path <- 'D:/'
#path <- 'C:/Users/ledarlin/'

setwd(paste0(path,"Dropbox/Forest Composition/composition/Maps/shapefiles/UrbanDrought"))
wd <- setwd(paste0(path,"Dropbox/Forest Composition/composition/Maps/shapefiles/UrbanDrought"))

#Read data

df <- read_csv('landsat_ndvi_metVars_combined.csv') %>% 
  filter(type == 'urban-medium')

# Creating a 14-day NDVI lag (day -14), that goes across satellites to try to bring in autocorrleation
# May need a longer window, but we'll see
df$NDVI.Lag14d <- NA
for(i in 1:nrow(df)){
  rowLag <- which(df$date>=(df$date[i]-14) & df$date<df$date[i])
  
  if(length(rowLag)<1 ) next
  if(length(rowLag)==1) df$NDVI.Lag14d[i] <- df$NDVI[rowLag]
  if(length(rowLag)>1) df$NDVI.Lag14d[i] <- mean(df$NDVI[rowLag], na.rm=T)
  
}

norm <- df %>% 
  group_by(doy) %>% 
  summarize(norm = mean(NDVI, na.rm = TRUE))

df %<>%
  na.omit(.) %>% 
  #filter(between(doy, 100, 300)) %>% 
  left_join(., norm, by = 'doy') %>% 
  mutate(yearDay = paste0(year, doy)) 

set.seed(123)
randoAll<-randomForest(NDVI~., data=df[,-c(1,2,6,25)], mtry = 6, ntree = 500)
randoAll #90% variance explained

AllVIP<- vip(randoAll, include_type = TRUE, horizontal = TRUE, 
             aesthetics = list(fill = '#2a2e38'), num_features = 15) +
  theme_bw() +
  theme(axis.title.x = element_blank())

AllVIP

ggsave(paste0(path, "Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/AllVIP.png"), 
       AllVIP, width = 3, height = 2.25, unit = "in", dpi = 300)

#This pdp section is kind of obnoxious, but it makes really pretty plots.
#It makes each plot separately, combines them, then writes them out with
#a facet wrap. You need to specify which variables you want to do them for.
#I chose the top six from the VIP.

#There is a less verbose and less pretty way to make pdps at the end.

#Make top six pdps
pdp::partial(randoAll,
             pred.var = 'NDVI.Lag14d',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = TRUE,
             alpha = .1,
             parallel = TRUE) %>%
  as_tibble() %>%
  group_by(NDVI.Lag14d) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = NDVI.Lag14d) %>% 
  mutate(type = 'NDVI Lag 14d') -> NDVI.Lag14d_pdpAll;

pdp::partial(randoAll,
             pred.var = 'doy',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = TRUE,
             alpha = .1,
             parallel = TRUE) %>%
  as_tibble() %>%
  group_by(doy) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = doy) %>% 
  mutate(type = 'Day of year') -> doy_pdpAll;

pdp::partial(randoAll,
             pred.var = 'X90d.SPI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X90d.SPI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X90d.SPI) %>% 
  mutate(type = 'X90d.SPI') -> X90d.SPI_pdpAll;

pdp::partial(randoAll,
             pred.var = 'TMIN90d',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(TMIN90d) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = TMIN90d) %>% 
  mutate(type = '90 day T min') -> TMIN90d_pdpAll;

pdp::partial(randoAll,
             pred.var = 'TMIN30d',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(TMIN30d) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = TMIN30d) %>% 
  mutate(type = '30 day T min') -> TMIN30d_pdpAll;

pdp::partial(randoAll,
             pred.var = 'X60d.SPI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X60d.SPI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X60d.SPI) %>% 
  mutate(type = '60 day SPI') -> X60d.SPI_pdpAll;

pdp::partial(randoAll,
             pred.var = 'TMIN60d',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(TMIN60d) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = TMIN60d) %>% 
  mutate(type = '60 day T min') -> TMIN60d_pdpAll;

#Combine
comboAll <- rbind(doy_pdpAll, TMIN90d_pdpAll, 
                  TMIN30d_pdpAll, NDVI.Lag14d_pdpAll, TMIN60d_pdpAll, X90d.SPI_pdpAll)

#plot
allPDP <- ggplot(comboAll, aes(x = value, y = mean_pred)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = 'gray',alpha = .4) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #scale_x_continuous(labels = scales::dollar) +
  #ylim(c(.22,.3)) +
  ylab("NDVI") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()) +
  facet_wrap(~type, ncol = 3, scales = 'free')
allPDP

ggsave(paste0(path, "Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/allPDP.png"), 
       allPDP, width = 3.25, height = 2.05, unit = "in", dpi = 300)

#RF predict-----------

dfPred <- df %>% 
  filter(between(doy, 100, 300),
         type == 'urban-medium',
         year != 2020)#,
         satellite != 'Landsat7',
         satellite != 'Landsat5')

randoPred<-randomForest(NDVI~., data=dfPred[,-c(1,2,6,26)], ntree = 500)
randoPred #81% variance explained

dfPred2 <- df %>% 
  filter(between(doy, 100, 300),
    type == 'urban-medium',
    year == 2020)#,
    satellite != 'Landsat7',
    satellite != 'Landsat5') %>% 
  distinct()

pred <- predict(randoPred, dfPred2[,-c(1,2,6,26)]) %>% 
  cbind(dfPred2,.) 

ggplot(pred) +
  geom_smooth(aes(x = doy, y = NDVI), color = '#8a0f00')+
  geom_smooth(aes(x = doy, y = .), color = '#ee9a00') +
  geom_smooth(aes(x = doy, y = norm), color = 'black') +
  geom_point(aes(x = doy, y = NDVI), color = '#8a0f00', alpha = .3) +
  geom_point(aes(x = doy, y = .), color = '#ee9a00', alpha = .3)

#Less March----------------

#Read data

df <- read_csv('landsat_ndvi_metVars_combined.csv') %>% 
  na.omit(.) %>% 
  filter(between(doy, 150, 245)) %>% 
  filter(type == 'urban-medium')

set.seed(123)
randoShort<-randomForest(NDVI~., data=df[,-c(1,2,6)], mtry = 6, ntree = 500)
randoShort #60% variance explained

ShortVIP<- vip(randoShort, include_type = TRUE, horizontal = TRUE, 
             aesthetics = list(fill = '#2a2e38'), num_features = 15) +
  theme_bw() +
  theme(axis.title.x = element_blank())

ShortVIP

#ggsave(paste0(path, "Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/AllVIP.png"), 
#       AllVIP, width = 3, height = 2.25, unit = "in", dpi = 300)

#This pdp section is kind of obnoxious, but it makes really pretty plots.
#It makes each plot separately, combines them, then writes them out with
#a facet wrap. You need to specify which variables you want to do them for.
#I chose the top six from the VIP.

#There is a less verbose and less pretty way to make pdps at the end.

#Make top six pdps
pdp::partial(randoShort,
             pred.var = 'doy',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = TRUE,
             alpha = .1,
             parallel = TRUE) %>%
  as_tibble() %>%
  group_by(doy) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = doy) %>% 
  mutate(type = 'Day of year') -> doy_pdpShort;

pdp::partial(randoShort,
             pred.var = 'X90d.SPI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X90d.SPI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X90d.SPI) %>% 
  mutate(type = 'X90d.SPI') -> X90d.SPI_pdpShort;

pdp::partial(randoShort,
             pred.var = 'TMAX90d',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(TMAX90d) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = TMAX90d) %>% 
  mutate(type = '90 day T max') -> TMAX90d_pdpShort;

pdp::partial(randoShort,
             pred.var = 'X60d.SPI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X60d.SPI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X60d.SPI) %>% 
  mutate(type = '60 day SPI') -> X60d.SPI_pdpShort;

pdp::partial(randoShort,
             pred.var = 'X60d.SPEI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X60d.SPEI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X60d.SPEI) %>% 
  mutate(type = '60 day SPEI') -> X60d.SPEI_pdpShort;

pdp::partial(randoShort,
             pred.var = 'X90d.SPEI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X90d.SPEI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X90d.SPEI) %>% 
  mutate(type = '90 day SPEI') -> X90d.SPEI_pdpShort;

#Combine
comboAll <- rbind(doy_pdpShort, TMAX90d_pdpShort, 
                  X60d.SPI_pdpShort, X60d.SPEI_pdpShort, X90d.SPEI_pdpShort, X90d.SPI_pdpShort)

#plot
shortPDP <- ggplot(comboAll, aes(x = value, y = mean_pred)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = 'gray',alpha = .4) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #scale_x_continuous(labels = scales::dollar) +
  #ylim(c(.22,.3)) +
  ylab("NDVI") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()) +
  facet_wrap(~type, ncol = 3, scales = 'free')
shortPDP

#CART--------

set.seed(456)
tree1<-rpart(NDVI~., data=df[,-c(1,2,6)])
rpart.plot(tree1)


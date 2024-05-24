# testing out a few different variable selection methods
library(randomForest)
library(nnet)
library(caret)
library(neuralnet)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
# Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# load in the data----
ndvi.dat <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndviAll_ml_data.RDS"))

# parsing down dataset to the predictor and applicable response vars
vars.remove <- c("NDVI", "NDVI.predLag", "date", "year", "NDVI.Lag14d") # change vars here.

ndvi.dat.short <-ndvi.dat[complete.cases(ndvi.dat),!names(ndvi.dat) %in% vars.remove] # only working with complete cases to remove NA's
summary(ndvi.dat.short)

# general distribution plots
ndvi.dat.short.df <- stack(ndvi.dat.short)
names(ndvi.dat.short.df) <- c("values", "var")

ggplot(data=ndvi.dat.short.df) + facet_wrap(var~., scales="free") +
    geom_density(aes(x=values))

# looking at the correlation between variables for urban medium
library(ggcorrplot)
test <- ndvi.dat.short[ndvi.dat.short$type=="urban-medium",]
meow <- data.frame(cor(test[,c(3:ncol(test))]))

ggcorrplot(meow)
# Random Forests----
# pulling in Lindsay's code here
library(tidyverse)
library(tidylog)
library(randomForest)  #Random forests
library(units)         #Change units in spatial work
library(pdp)           #Partial dependence plots
library(vip)           #Variable importance plots
library(rpart)
library(rpart.plot)

randoAll<-randomForest(resid.NDVIlag~., data=ndvi.dat.short[ndvi.dat.short$type=="urban-medium",], mtry = 6, ntree = 500) # building a model off of the residual of obs-persistance model
randoAll # 31% variance explained

AllVIP<- vip(randoAll, include_type = TRUE, horizontal = TRUE, 
             aesthetics = list(fill = '#2a2e38'), num_features = 15) +
  theme_bw() +
  theme(axis.title.x = element_blank())

AllVIP


# gradient boosting methodology----
library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)  

# using XGBoost methodology

# setting a training and a testing data set
set.seed(123)
ndvi_split <- initial_split(ndvi.dat.short, prop = .7)
ndvi_train <- training(ndvi_split)
ndvi_test  <- testing(ndvi_split)
# variable names
features <- setdiff(names(ndvi_train), "resid.NDVIlag")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(ndvi_train, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)     

# Prepare the training data
features_train <- vtreat::prepare(treatplan, ndvi_train, varRestriction = new_vars) %>% as.matrix()
response_train <- ndvi_train$resid.NDVIlag

# Prepare the test data
features_test <- vtreat::prepare(treatplan, ndvi_test, varRestriction = new_vars) %>% as.matrix()
response_test <- ndvi_test$resid.NDVIlag

# dimensions of one-hot encoded data
dim(features_train)
## [1] 11854  29
dim(features_test)
## [1] 5081 29

# reproducibility
set.seed(123)

xgb.fit1 <- xgb.cv(
  data = features_train,
  label = response_train,
  nrounds = 1000,
  nfold = 5,
  objective = "reg:squarederror",  # for regression models
  verbose = 0               # silent,
)

# get number of trees that minimize error
xgb.fit1$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    rmse.train   = min(train_rmse_mean),
    ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
    rmse.test   = min(test_rmse_mean),
  )
##   ntrees.train rmse.train ntrees.test rmse.test
## 1          965  0.5022836          60  27572.31

# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")


# reproducibility
set.seed(123)

xgb.fit2 <- xgb.cv(
  data = features_train,
  label = response_train,
  nrounds = 1000,
  nfold = 5,
  objective = "reg:squarederror",  # for regression models
  verbose = 0,               # silent,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

# plot error vs number trees
ggplot(xgb.fit2$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")

# tune the model

# eta : controls the learning rate
# max_depth : tree depth
# min_child_weight : minimum number of observations required in each terminal node
# subsample : percent of training data to sample for each tree
# colsample_bytrees : percent of culumns to sample from for each tree

# create hyperparameter grid
hyper_grid <- expand.grid(
  eta = c(.01, .05, .1, .3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.65, .8, 1), 
  colsample_bytree = c(.8, .9, 1),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid)
## [1] 576

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 5000,
    nfold = 5,
    objective = "reg:squarederror",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)

# saving hypergrid for now
saveRDS(hyper_grid, "processed_data/gradient_boost_parameter_table_xgb.RDS")

# may want to do more grid searches to hone in on the optimal model, but moving along for now.

# look at hypergrid for the minimum RMSE to get the parameters

# parameter list
params <- list(
  eta = hyper_grid[hyper_grid$min_RMSE==min(hyper_grid$min_RMSE), "eta"],
  max_depth = hyper_grid[hyper_grid$min_RMSE==min(hyper_grid$min_RMSE), "max_depth"],
  min_child_weight = hyper_grid[hyper_grid$min_RMSE==min(hyper_grid$min_RMSE), "min_child_weight"],
  subsample = hyper_grid[hyper_grid$min_RMSE==min(hyper_grid$min_RMSE), "subsample"],
  colsample_bytree = hyper_grid[hyper_grid$min_RMSE==min(hyper_grid$min_RMSE), "colsample_bytree"]
)

# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = features_train,
  label = response_train,
  nrounds = 1576,
  objective = "reg:squarederror",
  verbose = 0
)


# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit.final)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

pdp <- xgb.fit.final %>%
  partial(pred.var = "X30d.SPI", n.trees = 1576, grid.resolution = 100, train = ndvi_train) %>%
  autoplot(rug = TRUE, train = ndvi_train) +
  scale_y_continuous(labels = "Precip Anom") +
  ggtitle("PDP")

ice <- xgb.fit.final %>%
  partial(pred.var = "X30d.SPI", n.trees = 1576, grid.resolution = 100, train = ndvi_train, ice = TRUE) %>%
  autoplot(rug = TRUE, train = ndvi_train, alpha = .1, center = TRUE) +
  scale_y_continuous(labels = Precip) +
  ggtitle("ICE")

gridExtra::grid.arrange(pdp, ice, nrow = 1)

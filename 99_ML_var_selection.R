# testing out a few different variable selection methods
library(randomForest)
library(nnet)
library(caret)
library(neuralnet)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# load in the data----
ndvi.dat <- readRDS(file.path(google.drive, "data/r_files/processed_files/ndviAll_ml_data.RDS"))

# parsing down dataset to the predictor and applicable response vars
vars.remove <- c("NDVI", "NDVI.predLag", "date", "year") # change vars here.

ndvi.dat.short <-ndvi.dat[complete.cases(ndvi.dat),!names(ndvi.dat) %in% vars.remove]
summary(ndvi.dat.short)

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

randoAll<-randomForest(resid.NDVIlag~., data=ndvi.dat.short[ndvi.dat.short$type=="forest" & ndvi.dat.short$doy %in% c(100),], mtry = 6, ntree = 500) # building a model off of the residual of obs-persistance model
randoAll #72% variance explained

AllVIP<- vip(randoAll, include_type = TRUE, horizontal = TRUE, 
             aesthetics = list(fill = '#2a2e38'), num_features = 15) +
  theme_bw() +
  theme(axis.title.x = element_blank())

AllVIP



# nnet package----
# Load required packages
library(nnet)
library(caret)

# caret has issues with factors. Need to create dummy variables for them
summary(ndvi.dat.short)

head(model.matrix(satellite~.,data=ndvi.dat.short))
dummy.sat <- dummyVars(satellite~.,data=ndvi.dat.short)
head(predict(dummy.sat, newdata=ndvi.dat.short))

predictors <- names(ndvi.dat.short)[!names(ndvi.dat.short) %in% c("resid.NDVIlag")]
res.var <- "resid.NDVIlag"
# Split data into training and testing sets

train_ann <- function(data, res.var ,hidden_layers = c(5), threshold = 0.01) {
  formula <- as.formula(paste(res.var, " ~", paste(names(data)[!names(data) %in% res.var], collapse = " + ")))
  nn <- neuralnet(formula, data, hidden = hidden_layers)
  return(nn)
}

nn <- train_ann(data=ndvi.dat.short, res.var = res.var)
# Extract weights
weights <- as.data.frame(nn$weights[[1]][1])

# Calculate variable importance using L1 regularization
variable_importance <- apply(abs(weights), 1, sum)

# Rank variables based on importance
ranked_variables <- names(variable_importance)[order(variable_importance, decreasing = TRUE)]


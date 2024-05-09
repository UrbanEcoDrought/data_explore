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
vars.remove <- c("NDVI", "NDVI.predLag", "date", "year", "satellite") # change vars here.

ndvi.dat.short <-ndvi.dat[ndvi.dat$type %in% c("urban-medium") & complete.cases(ndvi.dat),!names(ndvi.dat) %in% vars.remove]
summary(ndvi.dat.short)

ndvi.dat.short <- ndvi.dat.short[,!names(ndvi.dat.short) %in% "type"]
# Random Forests----


# nnet package----
# Load required packages
library(nnet)
library(caret)

# Define the formula 
# myformula <- resid.NDVIlag ~ .

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


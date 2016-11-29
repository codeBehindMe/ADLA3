require(caret)
# Read data
dt_ <- read.csv("../prostate.csv")
# Read utilitie functions.
source("../utilities.r")

# Make training and test sets
obj_ <- Udf.Utilities.PrepareTraining(dt_)
tr_ <- obj_$training
ts_ <- obj_$testing


# PCA
pc_ <- Udf.Utilities.Prcomp(tr_[,-ncol(tr_)],nComps = 2)

pcObj_ <- pc_$prcompObj # Get object 

tr_ <- cbind(tr_,pc_$components) # bind to features.

# Control method
ctrl_ <- trainControl(method = "cv", number = 5, sampling = "up")
mdl.xgb <- train(Result ~ . , data = tr_,method = "xgbTree", trControl = ctrl_)


mdl.gbm <- train(Result ~ . , data = tr_,method = "gbm", trControl = ctrl_)
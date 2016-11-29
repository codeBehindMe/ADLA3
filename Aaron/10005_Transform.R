require(h2o)
require(ggplot2)

# Read raw dataset
dt_ <- read.csv("../prostate.csv")
source("../utilities.r")

obj_ <- Udf.Utilities.PrepareTraining(dt_)
tr_ <- obj_$training
ts_ <- obj_$testing



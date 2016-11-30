require(h2o)
require(ggplot2)
require(caret)
# Read raw dataset
dt_ <- read.csv("../prostate.csv")
source("../utilities.r")

obj_ <- Udf.Utilities.PrepareTraining(dt_)
tr_ <- obj_$training
ts_ <- obj_$testing

#write.csv(tr_,"training.csv",row.names = FALSE)
#write.csv(ts_,"testing.csv",row.names = FALSE)


h2o.init(nthreads = -1)

tr_ <- read.csv("trFE.csv")
ts_ <- read.csv("tsFE.csv")
tr_$Result <- as.factor(tr_$Result)
ts_$Result <- as.factor(ts_$Result)

h2_tr <- as.h2o(tr_,"h2_tr")
h2_ts <- as.h2o(ts_,"h2_ts")


dl_ <- h2o.deeplearning(
    x = 1:(ncol(h2_tr) - 1),
    y = ncol(h2_tr),
    training_frame = h2_tr,
    validation_frame = h2_ts,
    distribution = "multinomial",
    activation = "Maxout",
    hidden = c(300,200,300),
    l1 = 1e-5,
    epochs = 20,
    nfolds = 5,
    balance_classes = TRUE
)

prd_ <- h2o.predict(dl_,h2_ts)
prd_ <- as.data.frame(prd_)
sum(as.numeric(prd_$predict) == as.numeric(feed.testing$Result))/599

h2o.performance(dl_,valid = TRUE)
## 68%
View(h2o.predict(dl_,h2_ts))


## Deeplearning with grid search

hidden_opt <- list(c(40,50,40),c(50,60,50),c(100,150,100))
l1_opt <- c(1e-5,1e-7)
hyper_params <- list(hidden = hidden_opt,l1 = l1_opt)

stop()
dl_grid <- h2o.grid(
    "deeplearning",
    hyper_params = hyper_params,
    x = 1:(ncol(h2_tr) - 1),
    y = ncol(h2_tr),
    distribution = "multinomial",
    activation = "Maxout",
    training_frame = h2_tr,
    validation_frame = h2_ts,
    balance_classes = TRUE,
    nfolds = 5,
    epochs = 10
)

for(model_id in dl_grid@model_ids){
    print(h2o.performance(h2o.getModel(model_id),valid = TRUE))
}

for(model_id in dl_grid@model_ids){
    
    prd_ <- h2o.predict(h2o.getModel(model_id),h2_ts)
    prd_ <- as.data.frame(prd_)
    print(sum(prd_$predict == ts_$Result)/599)
}



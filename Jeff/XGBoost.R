


self.xgboost <- function(Dataframe, target) {

    xgbrain <- Dataframe

	xgbTrain <- sparse.model.matrix(Result ~ ., data = xgbTrain)

	dtrain <- xgb.DMatrix(data=xgbTrain, label=Result)

	watchlist <- list(xgbTrain=dtrain)

	param <- list(  objective           = "binary:logistic", 
					booster             = "gbtree",
					eval_metric         = "auc",
					eta                 = 0.02,
					max_depth           = 25,
					subsample           = 0.6815,
					colsample_bytree    = 0.701
	)

	clf <- xgb.train(   params              = param, 
						data                = dtrain, 
						nrounds             = 570, 
						verbose             = 1,
						watchlist           = watchlist,
						maximize            = FALSE
	)

	xgbTest$shot_made_flag <- -1

	xgbTest <- sparse.model.matrix(shot_made_flag ~ ., data = xgbTest)




}

# Models

require(caret)


# Read data
dt_ <- read.csv("../prostate.csv")
# Read utilitie functions.
source("../utilities.r")

# Look at basic summary
str(dt_)
summary(dt_)


# Drop ID 
dt_ <- dt_[,-1]


# Factorise target variable.
dt_[,ncol(dt_)] <- as.factor(dt_[,ncol(dt_)])

# Plot class distribution.
ggplot(dt_, aes(dt_$Result)) + geom_bar() + ggtitle("Result Feature Class Observation Density") + xlab("Result") + ylab("Observation Count")


#Do principle component analysis
dt_pca <- Udf.Utilities.Prcomp(dt_[,  - ncol(dt_)])$components

# Plot PCA
ggplot(dt_pca,aes(dt_pca$PC1,dt_pca$PC2,col=as.factor(dt_$Result))) + geom_point() + xlab("PC1") + ylab("PC2") + ggtitle("Class Distribution on Principle Components") + scale_color_discrete(name = "Result")

## Segment training and testing.
trInd_ <- createDataPartition(dt_$Result,p = 0.8,list = FALSE, times =1 )
dt_tr <- dt_[trInd_,]
dt_ts <- dt_[-trInd_,]

# Visualise the class distributions of training and testing set.
Udf.Utilities.Multiplot(ggplot(dt_tr,aes(as.factor(dt_tr$Result))) + geom_bar() + xlab("Result") + ggtitle("Training Set Target Distribution"),ggplot(dt_ts,aes(as.factor(dt_ts$Result))) + geom_bar() + xlab("Result") + ggtitle("Test Set Target Distribution"))

# Set factor
dt_tr[,"Result"] <- Udf.Utilities.ForceAssertFactor(dt_tr[,"Result"])
dt_ts[,"Result"] <- Udf.Utilities.ForceAssertFactor(dt_ts[,"Result"])


## Extreme Boosted Tree
# Generate training control scheme.
# 10 fold cross validation with down sampling. 
ctrl_ <- trainControl(method = "cv", number = 10, sampling = "down")
tune_ <- expand.grid(eta = c(0.1,0.2,0.3,0.4),nrounds = 100,max_depth = 20,gamma = c(0,0.1,0.5),colsample_bytree = c(4,5,6,7),min_child_weight = 1)
mdl.xgb <- train(Result ~ . , data = dt_tr,method = "xgbTree", trControl = ctrl_ , tuneGrid = tune_)


prd_ <- predict(mdl,dt_ts[,-ncol(dt_ts)])



## Stochastic Gradient Boosting
mdl.gbm <- train(Result ~ . , data = dt_tr,method = "gbm", trControl = ctrl_)
















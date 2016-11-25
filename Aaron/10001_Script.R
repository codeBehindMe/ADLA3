## Data Visualisation.
require(ggplot2)
require(scales)
require(caret)


# setwd("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Applied Data Analysis/A3/Aaron")
source("../utilities.r")
dt_ <- read.csv("../prostate.csv")



str(dt_)
summary(dt_)


ggplot(dt_, aes(dt_$Result)) + geom_bar() + ggtitle("Result Feature Class Observation Density") + xlab("Result") + ylab("Observation Count")

## Classes 3 & 4 are misrepresented. Especially class 4.
# This prettey much means that simple models are straight away out since they will learn the bias to mispredict low occurance classes.


## PCA
dt_pca <- Udf.Utilities.Prcomp(dt_[, c(-1, - ncol(dt_))])$components

ggplot(dt_pca,aes(dt_pca$PC1,dt_pca$PC2,col=as.factor(dt_$Result))) + geom_point() + xlab("PC1") + ylab("PC2") + ggtitle("Class Distribution on Principle Components") + scale_color_discrete(name = "Result")

## Jitters
Udf.Utilities.Jitter(dt_,"ATT1","ATT2","Result")
Udf.Utilities.Jitter(dt_, "ATT2", "ATT3", "Result")
Udf.Utilities.Jitter(dt_, "ATT3", "ATT4", "Result")


# Train test split
dt_ <- dt_[,-1]
trInd_ <- createDataPartition(dt_$Result,p = 0.8,list = FALSE, times =1 )
dt_tr <- dt_[trInd_,]
dt_ts <- dt_[-trInd_,]

Udf.Utilities.Multiplot(ggplot(dt_tr,aes(as.factor(dt_tr$Result))) + geom_bar() + xlab("Result") + ggtitle("Training Set Target Distribution"),ggplot(dt_ts,aes(as.factor(dt_ts$Result))) + geom_bar() + xlab("Result") + ggtitle("Test Set Target Distribution"))

self.adaBoostedTree <- function(Dataframe, target) {

    warning("Application specific function call. May return incorrect values if used out of context.")

    require(caret)
    require(adabag)
    require(plyr)


    df_ <- Dataframe

    ctrl_ <- trainControl(method = "cv", number = 10, sampling = "down")


    df_[, target] <- as.factor(df_[, target])

    mdl_ <- train(target ~ . ,data = df_, method = 'AdaBoost.M1')

    return(mdl_)
}


dt_[,ncol(dt_)] <- as.factor(dt_[,ncol(dt_)])
myBoostModel <- train(Result ~ . , data = dt_,method = "AdaBoost.M1")

myBoostModel <- self.adaBoostedTree(dt_tr,"Result")

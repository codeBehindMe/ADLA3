## Data Visualisation.
require(ggplot2)
require(scales)

setwd("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Applied Data Analysis/A3/")
source("utilities.r")
dt_ <- read.csv("prostate.csv")



str(dt_)
summary(dt_)


ggplot(dt_, aes(dt_$Result)) + geom_bar() + ggtitle("Result Feature Class Observation Density") + xlab("Result") + ylab("Observation Count")

## Classes 3 & 4 are misrepresented. Especially class 4.
# This prettey much means that simple models are straight away out since they will learn the bias to mispredict low occurance classes.

## Jitters
ggplot(dt_, aes(as.factor(dt_$ATT1), as.factor(dt_$ATT2), col = as.factor(dt_$Result))) + geom_jitter() + xlab("ATT1") + ylab("ATT2") + ggtitle("Jitter Results ATT1 ~ ATT2 ")

## PCA
dt_pca <- Udf.Utilities.Prcomp(dt_[, c(-1, - ncol(dt_))])$components

ggplot(dt_pca,aes(dt_pca$PC1,dt_pca$PC2,col=as.factor(dt_$Result))) + geom_point() + xlab("PC1") + ylab("PC2") + ggtitle("Class Distribution on Principle Components") + scale_color_discrete(name = "Result")
require(ggplot2)
require(scales)
require(caret)
require(sqldf)

# Load the data.
source("../utilities.r")
source("../FE.r")
dt_ <- read.csv("../prostate.csv")



# try to find near zero variance predictors
nzv <- nearZeroVar(dt_[,- c(1,ncol(dt_))],saveMetrics = TRUE)

# Apparently all of them are required.

# Linear dependancies
cInf_ <- findLinearCombos(dt_[,- c(1,ncol(dt_))])

# No features to remove.


## Center and scale.
training <- Udf.Utilities.PrepareTraining(dt_)$training
testing <- Udf.Utilities.PrepareTraining(dt_)$testing

preProcessVal <- preProcess(training,method = c("center","scale"))

trXformed <- predict(preProcessVal,training)
tsXformed <- predict(preProcessVal,testing)


## PCA
pcaObj_ <- Udf.Utilities.Prcomp(trXformed[,-ncol(trXformed)],nComps = 2)
pcaRot_ <- pcaObj_$prcompObj$rotation # Rotation

pca2C_ <- pcaObj_$components # components for plot.

ggplot(pca2C_,aes(pca2C_$PC1,pca2C_$PC2,color = trXformed$Result)) + geom_point() # plot.


# amend pca components.
tmptr__ <- pcaRot_ * trXformed[,-ncol(trXformed)]
colnames(tmptr__)[c(1,2,3,4,5,6,7,8,9,10)] <- c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10")
trXformed <- cbind(trXformed,tmptr__[,c(1,2,3,4,5,6)])



tmpts__ <- pcaRot_ * tsXformed[,-ncol(tsXformed)]
colnames(tmpts__)[c(1,2,3,4,5,6,7,8,9,10)] <- c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10")
tsXformed <- cbind(tsXformed,tmpts__[,c(1,2,3,4,5,6)])


# xform + base
xPlusBaseTr <- cbind(trXformed,training[,-ncol(training)])
xPlusBaseTs <- cbind(tsXformed,testing[,-ncol(testing)])

# Move cols to end.
xPlusBaseTr <- Udf.Utilities.MoveColToEnd(xPlusBaseTr,"Result")
xPlusBaseTs <- Udf.Utilities.MoveColToEnd(xPlusBaseTs,"Result")


# Add in the sum of pca components as a feature.
xPlusBaseTrPCSum <- cbind(xPlusBaseTr,rowSums(tmptr__))
colnames(xPlusBaseTrPCSum)[ncol(xPlusBaseTrPCSum)] <- "SumPCA"
xPlusBaseTsPCSum <- cbind(xPlusBaseTs,rowSums(tmpts__))
colnames(xPlusBaseTsPCSum)[ncol(xPlusBaseTsPCSum)] <- "SumPCA"

# Move cols to end.
xPlusBaseTrPCSum <- Udf.Utilities.MoveColToEnd(xPlusBaseTrPCSum,"Result")
xPlusBaseTsPCSum <- Udf.Utilities.MoveColToEnd(xPlusBaseTsPCSum,"Result")
# 40% on validation set.

# feature sums
xpbtrpcs2s <- cbind(xPlusBaseTrPCSum,my.f2cnt(training,"ATT1","ATT2"))
colnames(xpbtrpcs2s)[ncol(xpbtrpcs2s)] <- "2Sums"
xpbtrpcs2s <- Udf.Utilities.MoveColToEnd(xpbtrpcs2s,"Result")
xpbtspcs2s <- cbind(xPlusBaseTsPCSum,my.f2cnt(testing,"ATT1","ATT2"))
colnames(xpbtspcs2s)[ncol(xpbtspcs2s)] <- "2Sums"
xpbtspcs2s <- Udf.Utilities.MoveColToEnd(xpbtspcs2s,"Result")
# 41.5% validation set svm

# 3 feature sums
xpbtrpcs3s <- cbind(xpbtrpcs2s,my.f3cnt(training,"ATT1","ATT2","ATT3"))
colnames(xpbtrpcs3s)[ncol(xpbtrpcs3s)] <- "3Sums"
xpbtrpcs3s <- Udf.Utilities.MoveColToEnd(xpbtrpcs3s,"Result")
xpbtspcs3s <- cbind(xpbtspcs2s,my.f3cnt(testing,"ATT1","ATT2","ATT3"))
colnames(xpbtspcs3s)[ncol(xpbtspcs3s)] <- "3Sums"
xpbtspcs3s <- Udf.Utilities.MoveColToEnd(xpbtspcs3s,"Result")


# original feature averages
rmeantr <- cbind(xpbtrpcs3s,rowMeans(training[,-ncol(training)]))
colnames(rmeantr)[ncol(rmeantr)] <- "RMean"
rmeantr <- Udf.Utilities.MoveColToEnd(rmeantr,"Result")
rmeants <- cbind(xpbtspcs3s,rowMeans(testing[,-ncol(testing)]))
colnames(rmeants)[ncol(rmeants)] <- "RMean"
rmeants <- Udf.Utilities.MoveColToEnd(rmeants,"Result")
# 27% / 37% on NN

stop()
## svmradial
ctrl_ <- trainControl(method = "cv", number = 5, sampling = "down")
svm_ <- train(Result ~ . , data = xPlusBaseTrPCSum, method = "svmRadial")
gbm_ <- train(Result ~ . , data = rmeantr, method = 'gbm',trControl = ctrl_)


svm_lin <- train(Result ~ . , data = xPlusBaseTrPCSum, method = "svmLinear")

svm_spectrum <- train(Result ~ . , data = xPlusBaseTrPCSum, method = "svmSpectrumString" )

svm_poly <- train(Result ~ . , data = trXformed, method = "svmPoly")


write.csv(rmeantr,"trFE.csv",row.names = FALSE)
write.csv(rmeants,"tsFE.csv",row.names = FALSE)

prd_ <- predict(gbm_,rmeants[,-ncol(rmeants)])
prd_ <- as.data.frame(prd_)
sum(prd_$prd_ == xpbtspcs3s$Result)/599

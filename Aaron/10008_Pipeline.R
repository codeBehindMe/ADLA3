## This file contains the deep learning pipeline.


#Description:
# This pipeline carries out the full transformation of data from the original dataset to the deeplearning data set. There are number of transformations that have been applied to boost the performance of the deep learning model.
# WARNING:: This model is built on H2O scalable desktop machine learning platform. There are a number of dependancies for this including 64bit JAVA. Ensure you have the relevante requirements before you begin.


## Required Packages ##
#######################
require(sqldf)
require(caret)
require(ggplot2)
require(h2o)


## Required Supporting Scripts ##
#################################
source("../utilities.r") # Utility functions and helpers.
source("../FE.r") # Feature engineering functions.


# Read in the original dataset.
dt_ <- read.csv("../prostate.csv")


# We have been told that all our features are numerical measurements and are rounded to the nearest integer. This already gives us some noise which makes it difficult to predict an outcome. Also given the mutlinomial nature this will be a difficult tast.


## Feature Engineering ##
#########################


# Look for some near zero variance predictors from the original dataset.
nzv <- caret::nearZeroVar(dt_[,- c(1,ncol(dt_))],saveMetrics = TRUE)
nzv

# We can see that we don't really have any zero variance predictors. Also all the features seem to have only about 30% unique data points out of the entire data set. The most frequent to the second most frequent value also seems to be spread across the feature space from around 1.2 to 1.1.
# This shows that the data is quite spread out. Unlikely that the features themselves would do anything.


# Next let's try to identify whether there are any linear dependancies between our features.
lc <- caret::findLinearCombos(dt_[,- c(1,ncol(dt_))])
lc

# As expected, there are no linear dependancies between our features. 

# Before move any further, let's generate a test and training set for our models to train on and later validate how well the model generalized. 
# To accomplish this we are going to use our utilities script. In this script the function PrepareTraining is what will be called.
# This function is essentially a wrapper function to the caret package's createDartaPartition method. The added functionality includes, it drops the ID column, and forces the response variable (Result) to take a factor form. The split also ensures that the training set response distribution and the test set response distribution is similar. Given that we have unbalanced classes, this is very important. The function returns a list, with embedded training and testing dataframes.

training <- Udf.Utilities.PrepareTraining(dt_)$training
testing <- Udf.Utilities.PrepareTraining(dt_)$testing


# Generally speaking, most algorithms, especially neural networks 0 scaled data. Why ? I don't know.
# To do this we are going to use a method called preProcess from the caret package. This is actually a robust method to apply center and scaling using the training set and apply the same scaling matrix to any unseen data. We will also input a YeoJohnson transformation as well.

# Get the pre processor.
fe.PreProcessor <- caret::preProcess(training,method = c("center","scale","YeoJohnson"))

## Step 1
# Use the preprocessor to transform the training and the test dataset. This preprocessor model will be used to transform all unseen data as well.
tr.csyj <- predict(fe.PreProcessor,training) # training set.
ts.csyj <- predict(fe.PreProcessor,testing) # testing set.

# Let's visualise the dataset using smoe principle component analysis to get the two major axis. We will again call on a wrapper from the utilities script to do the principle component analysis for us. Inside the Udf.Utilities.Prcomp method is a simple call to stats::prcomp method. 
ggplot(Udf.Utilities.Prcomp(tr.csyj[,-ncol(tr.csyj)],nComps = 2)$components,aes(PC1,PC2,color = tr.csyj$Result)) + geom_point() + scale_color_discrete("Result") + ggtitle("Prostate Cancer (Centered Scaled) on Principle Components.")

# We can see that there are no really distinct cluster groups, let's try using a spatial sign transform to project it radially outwards to see if we can improve the performance of the PCA.

## Step 2
tr.csyj.ss <- as.data.frame(caret::spatialSign(tr.csyj[,-ncol(tr.csyj)]))
ts.csyj.ss <- as.data.frame(caret::spatialSign(ts.csyj[,-ncol(ts.csyj)]))

# Let's revisualise to see if the PCA has improved.
ggplot(Udf.Utilities.Prcomp(tr.csyj.ss,nComps = 2)$components,aes(PC1,PC2,color = tr.csyj$Result)) + geom_point() + scale_color_discrete("Result") + ggtitle("Prostate Canceer (Centered, Scaled & Spatial Sign Transformed) on Principle Components")

# Hmm looks like it has spread out a little bit which is a good thing. 

# We are going to use l1 regularisation in our neural network, so it should automatically scale the parameters which are insiginificant down to zero. Therefore we will bind any additonal transformations to the centered and scaled data. Let's call these binding objects as feed(s). So feed.training and feed.testing. We will hold out the label until we are at the final stage.

# Original data centered and scaled with a YeaJohnson transform.
feed.training <- tr.csyj[,-ncol(tr.csyj)]
feed.testing <- ts.csyj[,-ncol(tr.csyj)]  

# Bind the new data and redo column names
feed.training <- cbind(feed.training,tr.csyj.ss)
colnames(feed.training) <- paste("FT",1:ncol(feed.training),sep="")

feed.testing <- cbind(feed.testing,ts.csyj.ss)
colnames(feed.testing) <- paste("FT",1:ncol(feed.testing),sep="")


## Step 3
# So far we have just been using PCA to visualise the data set. But let's include the soem of the results from the PCA as extra features.
pcaModel_ <- Udf.Utilities.Prcomp(feed.training,nComp = ncol(feed.training))
# Look at the components and their explanation of the overall variance in the dataset.
plot(pcaModel_$prcompObj,"Components Vs Variance Explained",type = "l")

# Looks like there is no 1 component which explains most of the variance. Remember our model is going to have l1 regularisation therefore it'll just simply scale the weights down to zero where variables don't actually help. So lets add the entire thing. 
# It is important that we keep the same rotation that we learned from our training set over to the validation sets. So let's extract the rotation matrix and then multiply our validation set to get the rotated values.

# multiply the rotation by the matrix which is spatial sign transformed to get the next step.


tr.csyj.ss.pca <- pcaModel_$prcompObj$rotation * feed.training
feed.training <- cbind(feed.training,tr.csyj.ss.pca)
colnames(feed.training) <- paste("FT",1:ncol(feed.training),sep="")

ts.csyj.ss.pca <- pcaModel_$prcompObj$rotation * feed.testing
feed.testing <- cbind(feed.testing,ts.csyj.ss.pca)
colnames(feed.testing) <- paste("FT",1:ncol(feed.testing),sep="")



## Step 4
# Another useful feature may be able to be captured by the top principle components. We will use the top 2 here.
tr.csyj.ss.pca.sum <- rowSums((pcaModel_$prcompObj$rotation * tr.csyj.ss.pca)[,1:2])
feed.training <- cbind(feed.training,tr.csyj.ss.pca.sum)
colnames(feed.training) <- paste("FT",1:ncol(feed.training),sep="")

ts.csyj.ss.pca.sum <- rowSums((pcaModel_$prcompObj$rotation * ts.csyj.ss.pca)[,1:2])
feed.testing <- cbind(feed.testing,ts.csyj.ss.pca.sum)
colnames(feed.testing) <- paste("FT",1:ncol(feed.testing),sep="")


## Step 5
# Another useful feature is the number of counts of some of the variables and how the occur on the dataset. Given that everything is rounded to an integer, we can assume they are nearly categorical. Lets see how we do with this. We are calling on one of our utility functions, which essentiall does a count and joins it using the sqldf library.
tr.colsum <- my.f2cnt(training,"ATT1","ATT2")
feed.training <- cbind(feed.training,tr.colsum)
colnames(feed.training) <- paste("FT",1:ncol(feed.training),sep="")

ts.colsum <- my.f2cnt(testing,"ATT1","ATT2")
feed.testing <- cbind(feed.testing,ts.colsum)
colnames(feed.testing) <- paste("FT",1:ncol(feed.testing),sep="")


## Step 6
# Finally scale and center everthing.
feed.training <- scale(feed.training)
feed.testing <- scale(feed.testing)


# Step 7
# rebind the labels.
feed.training <- as.data.frame(cbind(feed.training,training[,"Result"]))
colnames(feed.training)[ncol(feed.training)] <- "Result"
feed.training[,"Result"] <- as.factor(feed.training[,"Result"])
feed.testing <- as.data.frame(cbind(feed.testing,testing[,"Result"]))
colnames(feed.testing)[ncol(feed.testing)] <- "Result"
feed.testing[,"Result"] <- as.factor(feed.testing[,"Result"])

stop()
# Modelling

# Initialse h2o.
h2o.init(nthreads = -1)

# Cast the trainig and test sets as h2o objects.
h2_tr <- as.h2o(feed.training,"h2_tr")
h2_ts <- as.h2o(feed.testing,"h2_ts")


# Visualise using autoencoder.
NN.Autoencoder <- h2o.deeplearning(
    x = 1:(ncol(feed.training)-1),
    training_frame = h2_tr,
    hidden = c(33,23,13,3,2,3,13,23,33),
    epochs = 600,
    activation = "Tanh",
    autoencoder = TRUE,
    export_weights_and_biases = TRUE
)

tr_sup_ft <- h2o.deepfeatures(NN.Autoencoder,h2_tr,layer = 5)
pltData <- as.data.frame(tr_sup_ft)


ggplot(pltData,aes(pltData$DF.L5.C1,pltData$DF.L5.C2,color=feed.training$Result)) + geom_point()
plot_ly(x=pltData$DF.L3.C1,y=pltData$DF.L3.C2,z=pltData$DF.L3.C3, color = as.factor(feed.training$Result), mode = 'markers',type = "scatter3d",scale = 0.1)


## Some models.
dl_ <- h2o.deeplearning(
    x = 1:(ncol(h2_tr) - 1),
    y = ncol(h2_tr),
    training_frame = h2_tr,
    validation_frame = h2_ts,
    distribution = "multinomial",
    activation = "MaxoutWithDropout",
    hidden = c(200,300,200),
    l2 = 1e-5,
    epochs = 20,
    nfolds = 10,
    balance_classes = TRUE,
    input_dropout_ratio = 0.2,
    loss = "CrossEntropy",
    classification_stop = 0.45,
    variable_importances = TRUE
)

prd_ <- h2o.predict(dl_,h2_ts)
prd_ <- as.data.frame(prd_)
sum(as.numeric(prd_$predict) == as.numeric(feed.testing$Result))/599

h2o.saveModel(dl_,"mdl1")


dl_2 <- h2o.deeplearning(
    x = 1:(ncol(h2_tr) - 1),
    y = ncol(h2_tr),
    training_frame = h2_tr,
    validation_frame = h2_ts,
    distribution = "multinomial",
    activation = "MaxoutWithDropout",
    hidden = c(200,300,200),
    l2 = 1e-5,
    epochs = 20,
    nfolds = 10,
    balance_classes = TRUE,
    input_dropout_ratio = 0.3,
    loss = "CrossEntropy",
    classification_stop = 0.45,
    variable_importances = TRUE
)



prd_2 <- h2o.predict(dl_2,h2_ts)
prd_2 <- as.data.frame(prd_2)
sum(as.numeric(prd_2$predict) == as.numeric(feed.testing$Result))/599

h2o.saveModel(dl_2,"mdl1")




dl_3 <- h2o.deeplearning(
    x = 1:(ncol(h2_tr) - 1),
    y = ncol(h2_tr),
    training_frame = h2_tr,
    validation_frame = h2_ts,
    distribution = "multinomial",
    activation = "MaxoutWithDropout",
    hidden = c(200,300,200),
    l2 = 1e-5,
    epochs = 20,
    nfolds = 10,
    balance_classes = TRUE,
    input_dropout_ratio = 0.1,
    loss = "CrossEntropy",
    classification_stop = 0.45,
    variable_importances = TRUE
)


# 0.39
prd_3 <- h2o.predict(dl_3,h2_ts)
prd_3 <- as.data.frame(prd_3)
sum(as.numeric(prd_3$predict) == as.numeric(feed.testing$Result))/599

h2o.saveModel(dl_2, "mdl3")


dl_4 <- h2o.deeplearning(
    x = 1:(ncol(h2_tr) - 1),
    y = ncol(h2_tr),
    training_frame = h2_tr,
    validation_frame = h2_ts,
    distribution = "multinomial",
    activation = "MaxoutWithDropout",
    hidden = c(200,300,200),
    l2 = 1e-5,
    epochs = 20,
    nfolds = 10,
    balance_classes = TRUE,
    input_dropout_ratio = 0.05,
    loss = "CrossEntropy",
    classification_stop = 0.45,
    variable_importances = TRUE
)



prd_4 <- h2o.predict(dl_4,h2_ts)
prd_4 <- as.data.frame(prd_4)
sum(as.numeric(prd_4$predict) == as.numeric(feed.testing$Result))/599

h2o.saveModel(dl_2, "mdl4")




## Extracting Autoencoder Deepfeatures.

# We reduced the dimensions from our autoencoder earlier, althought it didn't give any good information in 2d, we can still see if they have an impact on our predictive power.
# Lets take layer 3, which has compression from 43 to 13.

BindDeepFeatures <- function(features,h2oEncoder){

    # Attach the autoencoder features.
    dFtMtx <- matrix(nrow = nrow(features),ncol = 13) # Genereate a container for the new features.

    ftMtx_ <- as.matrix(features) # Cast input as matrix.
    print("Binding Deep Features Input Features")
    pb_ <- txtProgressBar(style = 3) # Progress bar.
    for(i in 1:nrow(ftMtx_)){

        # Drop down through teh layers.
        tmp__ <- as.matrix(h2o.weights(h2oEncoder,1)) %*% ftMtx_[i,]
        tmp__ <- as.matrix(h2o.weights(h2oEncoder,2)) %*% tmp__
        tmp__ <- as.matrix(h2o.weights(h2oEncoder,3)) %*% tmp__

        tmp__ <- t(tmp__) # Transpose


        dFtMtx[i,] <- tmp__ # Concatenate
        setTxtProgressBar(pb_,i/nrow(ftMtx_))
        close(pb_)
    }

    boundMatrix_ <- cbind(ftMtx_,dFtMtx)

    return(as.data.frame(boundMatrix_))
}


# Steo 8
# Bind training set
feed.training.deep <- BindDeepFeatures(feed.training[,-ncol(feed.training)],NN.Autoencoder)
feed.testing.deep <- BindDeepFeatures(feed.testing[,-ncol(feed.testing)],NN.Autoencoder)


#Step 9
# rebind the labels.
feed.training.deep <- as.data.frame(cbind(feed.training.deep,training[,"Result"]))
colnames(feed.training.deep)[ncol(feed.training.deep)] <- "Result"
feed.training.deep[,"Result"] <- as.factor(feed.training.deep[,"Result"])
feed.testing.deep <- as.data.frame(cbind(feed.testing.deep,testing[,"Result"]))
colnames(feed.testing.deep)[ncol(feed.testing.deep)] <- "Result"
feed.testing.deep[,"Result"] <- as.factor(feed.testing.deep[,"Result"])

h2_tr <- as.h2o(feed.training.deep,"h2_tr")
h2_ts <- as.h2o(feed.testing.deep,"h2_ts")

dl_5 <- h2o.deeplearning(
    x = 1:(ncol(h2_tr) - 1),
    y = ncol(h2_tr),
    training_frame = h2_tr,
    validation_frame = h2_ts,
    distribution = "multinomial",
    activation = "MaxoutWithDropout",
    hidden = c(200,300,200),
    l2 = 1e-5,
    epochs = 20,
    nfolds = 10,
    balance_classes = TRUE,
    input_dropout_ratio = 0.1,
    loss = "CrossEntropy",
    classification_stop = 0.45,
    variable_importances = TRUE
)

prd_5 <- h2o.predict(dl_5,h2_ts)
prd_5 <- as.data.frame(prd_5)
sum(as.numeric(prd_5$predict) == as.numeric(feed.testing.deep$Result))/599

h2o.saveModel(dl_5, "mdl5")

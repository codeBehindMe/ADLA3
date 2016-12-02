predict.deep <- function(data,h2oModel = NULL) {
    
    # Input parameters.
    # data :: A dataframe of the input features. Does not take ID or Response variable.

    # Assert that input featurespace must be 10 wide.
    if (ncol(data) != 10) {
        stop("Input features do not match the prostate.csv feature length. Have you forgotton to remove ID and Result columns ?")
    }

    # Assert that h2o is available 
    require(h2o)
    require(caret)
    h2o.init(nthreads = -1)
    # Assert that h2o version is correct
    if (h2o.getVersion() != "3.10.0.8") {
        stop("You have a different version of H2O, get the version 3.10.0.8")
    }


    ## Start feature engineering.

    # Step 1 feature scaling and centering with preprocessor.
    fe.preProcessor <- readRDS("fePreProcessor.rds")

    tr.csyj <- predict(fe.preProcessor, data) # make prediction on new data.


    ## Step 2 :: Spatial Sign transform
    tr.csyj.ss <- as.data.frame(caret::spatialSign(tr.csyj))

    # Bind the new data and redo column names
    feed.training <- cbind(tr.csyj, tr.csyj.ss)
    colnames(feed.training) <- paste("FT", 1:ncol(feed.training), sep = "")

    ## Step 3 :: Add PCA results
    # Read in the model
    pcaModel_ <- readRDS("pcaModel.rds")

    tr.csyj.ss.pca <- pcaModel_$prcompObj$rotation * feed.training # matrix multiply the weights into the features.
    feed.training <- cbind(feed.training, tr.csyj.ss.pca) # Bind the existing data.
    colnames(feed.training) <- paste("FT", 1:ncol(feed.training), sep = "") # Generalise feature names.

    ## Step 4 :: Row sums of 2 PCA components.
    tr.csyj.ss.pca.sum <- rowSums((pcaModel_$prcompObj$rotation * tr.csyj.ss.pca)[, 1:2])
    feed.training <- cbind(feed.training, tr.csyj.ss.pca.sum)
    colnames(feed.training) <- paste("FT", 1:ncol(feed.training), sep = "")

    ## Step 5 :: Frequency values of features
    source("FE.r")
    tr.colsum <- my.f2cnt(data, "ATT1", "ATT2")
    feed.training <- cbind(feed.training, tr.colsum)
    colnames(feed.training) <- paste("FT", 1:ncol(feed.training), sep = "")

    # Step 6 :: Scale and center everything

    feed.training <- scale(feed.training)

    # Step 7 :: Bind autoencoder output

    # Read in the autoencoder weights
    ae.weights <- readRDS("AutoEncoderWeights.rds")

    # Helper to bind features.
    BindDeepFeatures <- function(features, weights) {

        # Attach the autoencoder features.
        dFtMtx <- matrix(nrow = nrow(features), ncol = 13) # Genereate a container for the new features.

        ftMtx_ <- as.matrix(features) # Cast input as matrix.
        for (i in 1:nrow(ftMtx_)) {

            # Drop down through teh layers.
            tmp__ <- weights[[1]] %*% ftMtx_[i,]
            tmp__ <- weights[[2]] %*% tmp__
            tmp__ <- weights[[3]] %*% tmp__

            tmp__ <- t(tmp__) # Transpose


            dFtMtx[i,] <- tmp__ # Concatenate
        }

        boundMatrix_ <- cbind(ftMtx_, dFtMtx)

        return(as.data.frame(boundMatrix_))
    }

    # bind features
    feed.training.deep <- BindDeepFeatures(feed.training, ae.weights)

    # Cast h2o object.
    h2_tr <- as.h2o(feed.training.deep, "h2_tr")

    # Get the neural network model or use passed model.
    if (is.null(h2oModel)) {
        mdl_ <- h2o.loadModel("DeepLearning_model_R_1480681170824_1")
    } else {
        mdl_ <- h2oModel;
    }

    pred <- as.data.frame(h2o.predict(mdl_, h2_tr)) # return predictions as dataframe.

    return(pred[,"predict"])
}
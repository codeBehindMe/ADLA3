Udf.Utilities.Multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    
    
    #####################################################
    # Multiplot for ggplot2                             
    # REFERENCE: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/            
    #####################################################
    
    
    # Multiple plot function
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                         ncol = cols, nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

Udf.Utilities.Prcomp <- function(Features,nComps = 2){

    ## Wrapper function for principle component analysis.

    if(ncol(Features) < 2){
        stop("Too low features.")
    } else if (nComps > ncol(Features)){
        stop("More components requested than features.")
    } else {
        ft_ <- scale(Features)
        pcObj <- prcomp(ft_)

        sumDev <- sum((pcObj$sdev)^2)
        actDev <- (pcObj$sdev)^2

        pComps <- as.data.frame(pcObj$x[,1:nComps])

        object <- list(prcompObj = pcObj, sDeviations = sumDev, actDeviations = actDev, components = pComps)

        return(object)
    }
    
}


Udf.Utilities.Jitter <- function(Dataframe,x,y,colBy=NULL){
    ### Wrapper function for ggplot.jitterplot()
    require(ggplot2)

    dt_ <- Dataframe 
    plotObj_ <- ggplot(dt_,aes(as.factor(dt_[,x]),as.factor(dt_[,y]))) # Generate ggplot object.

    if(!is.null(colBy)){ ## Assess the color scheme requirements. 
        col_ <- colBy
        ggtitle_ <- paste0("Jitter ",colBy," ",x," ~ ",y)
        plotObj_ <- plotObj_ + geom_jitter(aes(col = as.factor(dt_[,col_]))) + xlab(x) + ylab(y) + ggtitle(ggtitle_) + scale_color_discrete(name = colBy)
    } else {
        col_ <- "black"
        ggtitle_ <- paste0("Jitter ", x," ~ ", y)

         plotObj_ <- plotObj_ + geom_jitter() + xlab(x) + ylab(y) + ggtitle(ggtitle_)
    }
    return(plotObj_)
}


Udf.Utilities.Comboplot <- function(Dataframe,FUN,...) {

    stop("Not Implemented")
    # Generates objects from custom function calls on a dataframe columns.
    require(ggplot2)

    if(class(Dataframe) != "data.frame") stop (paste0("Don't know how to handle ",class(Dataframe)," type object!")) # Data frame validation.

    names_ <- names(Dataframe) # Get the names 
    combos_ <- combn(names,2) # Get all possible pairs combinations
    plots_ <- list() # Instantiate a list object to carry the plots
    for(i in 1:length(combos_)){




    }
}

Udf.Utilities.Scatter <- function(Dataframe,x,y,colBy=NULL){
    stop("Not Implemented")
    require(ggplot2)
    require(gridExtra)

    dt_ <- Dataframe 

    hist_top <- ggplot() + geom_histogram(aes(dt_[,x]))

    empty <- ggplot() + geom_point(aes(1,1),colour = "white") + theme(
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()
    )

    scatter <- ggplot() + geom_point(aes(dt_[,x],dt_[,y]))

    hist_right <- ggplot() + geom_histogram(aes(dt_[,y])) + coord_flip()

    gaObject <- grid.arrange(hist_top,empty,scatter,hist_right,ncol = 2, nrow = 2, widths = c(4,1), heights = c(1,4))

    return(gaObject)
}



Udf.Utilities.DownSample <- function(Dataframe,target){

    #stop("Not Implemented.")
    # Returns down sampled dataframe.
    
    # Check dataframe
    if(class(Dataframe) != "data.frame") stop(paste0("Don't know how to handle object of type ",class(Dataframe)))

    #dt_ <- Dataframe 

    #fLev <- levels(as.factor(Dataframe[,target]))

	set.seed(9560)
	down_train <- downSample(x = Dataframe[, -ncol(Dataframe)],
                         y = Dataframe$Result)
	# rename to Result (not sure why this method changes the col name to Class)
	names(down_train)[names(down_train) == 'Class'] <- 'Result'
	return(down_train)
	
}

Udf.Utilities.UpSample <- function(Dataframe,target){

    # Check dataframe
    if(class(Dataframe) != "data.frame") stop(paste0("Don't know how to handle object of type ",class(Dataframe)))

	set.seed(9560)
	up_train <- upSample(x = Dataframe[, -ncol(Dataframe)],
                         y = Dataframe$Result)
	# rename to Result (not sure why this method changes the col name to Class)
	names(up_train)[names(up_train) == 'Class'] <- 'Result'
	return(up_train)
	
}

Udf.Utilities.SmoteSample <- function(Dataframe){
		
    # Check dataframe
    if(class(Dataframe) != "data.frame") stop(paste0("Don't know how to handle object of type ",class(Dataframe)))
	
	set.seed(9560)
	smote_train <- SMOTE(Result ~ ., data = Dataframe)
	
	# rename to Result (not sure why this method changes the col name to Class)
	names(smote_train)[names(smote_train) == 'Class'] <- 'Result'                      
	
	return(smote_train)

}

Udf.Utilities.RoseSample <- function(Dataframe){
		
    # Check dataframe
    if(class(Dataframe) != "data.frame") stop(paste0("Don't know how to handle object of type ",class(Dataframe)))
	
	set.seed(9560)
	rose_train <- ROSE(Result ~ ., data = Dataframe)$data
	
	# rename to Result (not sure why this method changes the col name to Class)
	names(rose_train)[names(rose_train) == 'Class'] <- 'Result'                      
	
	return(rose_train)

}


Udf.Utilities.SplitLabel <- function(Dataframe,target){
    # Wrapper for splitting labels and features.

    # Remove label.
    features_ <- Dataframe[,-which(names(Dataframe) %in% target)] 
    # Get label
    labels_ <- Dataframe[,target]
    # Construct return object.
    returnObject <- list(features=features_,labels=labels_) 

    return(returnObject)
}


Udf.Utilities.ForceAssertFactor <- function(Vector){
    # Some models may want the labels as a factor for some internal routing. This function is called to convert a vector into a factor.

    factor_ <- as.factor(Vector)

    if(length(levels(factor_)) > 4){
        warn("Factor exceeds 4 levels.")
    }
    return(as.factor(Vector))
}

# Returns a dataframe with the classfication metrics given lists of predicted values and actuals
Udf.Utilities.ClassificationMetrics <- function(preds, actuals){

	metrics_df = data.frame(matrix(ncol = 2, nrow = 10))
	colnames(metrics_df) <- c('metric', 'value')

	cm <- as.matrix(table(Actual = actual, Predicted = pred)) # create the confusion matrix
	n <- sum(cm) # number of instances
	nc <- nrow(cm) # number of classes
	diag <- diag(cm) # number of correctly classified instances per class 
	rowsums <- apply(cm, 1, sum) # number of instances per class
	colsums <- apply(cm, 2, sum) # number of predictions per class
	p <- rowsums / n # distribution of instances over the actual classes
	q <- colsums / n # distribution of instances over the predicted classes
	
	accuracy <- sum(diag) / n 
	expAccuracy = sum(p*q)
	kappa <- (accuracy - expAccuracy) / (1 - expAccuracy)
		
	metrics_df[1,] <- c('Accuracy', accuracy)
	metrics_df[2,] <- c('Kappa', kappa)
		
	return(metrics_df)
}



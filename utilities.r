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

    dt_ <- Dataframe # Local Instance
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

    # Generates objects from custom function calls on a dataframe columns.
    require(ggplot2)

    if(class(Dataframe) != "data.frame") stop (paste0("Don't know how to handle ",class(Dataframe)," type object!")) # Data frame validation.

    names_ <- names(Dataframe) # Get the names 
    combos_ <- combn(names,2) # Get all possible pairs combinations
    plots_ <- list() # Instantiate a list object to carry the plots
    for(i in 1:length(combos_)){




    }







}
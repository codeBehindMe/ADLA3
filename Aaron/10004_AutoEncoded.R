require(h2o)
require(ggplot2)
require(plotly)


# Read raw dataset
dt_ <- read.csv("../prostate.csv")

# subset out id
dt_ <- dt_[,-1]


# Create training and testing partitions

trIndx_ <- createDataPartition(dt_$Result,p = 0.8, list = FALSE, times = 1)

dt_tr <- dt_[trIndx_,]
dt_ts <- dt_[-trIndx_,]


h2o.init(nthreads = -1)
# Deeplearning autoencoder

# H2o training frame
h2_tr <- h2o.importFile("../prostate.csv",header = TRUE,sep = ",")
h2_tr <- h2_tr[,-1]



NN.Autoencoder <- h2o.deeplearning(
    x = 1:(ncol(dt_)-1),
    training_frame = h2_tr,
    hidden = c(5,3,5),
    epochs = 600,
    activation = "Tanh",
    autoencoder = TRUE
)

tr_sup_ft <- h2o.deepfeatures(NN.Autoencoder,h2_tr,layer = 2)
pltData <- as.data.frame(tr_sup_ft)


ggplot(pltData,aes(pltData$DF.L2.C1,pltData$DF.L2.C2,col=as.factor(dt_$Result))) + geom_point()



plot_ly(x=pltData$DF.L2.C1,y=pltData$DF.L2.C2,z=pltData$DF.L2.C3, color = as.factor(dt_$Result), mode = 'markers',type = "scatter3d")

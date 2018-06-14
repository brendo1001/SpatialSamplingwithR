# load packages
library(spcosa)
library(sp)
# read data
load(file="Voorst.RData")

# compute compact geographical strata
# set random seed (for reproduction of results)
set.seed(31415)
#change class of grdVoorst to SpatialPixelsDataFrame
gridded(grdVoorst)<- ~s1+s2
myStrata <- stratify(object=grdVoorst,nStrata = 20, equalArea=F, nTry=1)
plot(myStrata)

# select randomly n locations from the strata
nh <- 4
mySample <- spsample(myStrata, n = nh)
plot(myStrata, mySample)

#overlay selected points with simulated field to extract variable of interest at sampling points
mySample <- as(mySample, "SpatialPoints")
mySampledata <- mySample%over%grdVoorst
mySampledf <- data.frame(as(mySample,"data.frame"),z=mySampledata$z)
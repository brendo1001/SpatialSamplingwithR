# load packages
library(sp)
library(spcosa)
# read data
load(file="Voorst.RData")

# compute compact geographical strata
# set random seed (for reproduction of results)
set.seed(314)
#change class of grdVoorst to SpatialPixelsDataFrame
gridded(grdVoorst)<- ~s1+s2
myStrata <- stratify(grdVoorst,nStrata = 20, equalArea=T, nTry=1)
plot(myStrata)

# select randomly n locations from the strata
nh <- 4
#mySample <- spsample(myStrata, n = nh, type="composite")
mySample <- spsample(myStrata, n = nh)
plot(myStrata, mySample)

#overlay selected points with simulated field to extract variable of interest at sampling points
mySample <- as(mySample, "SpatialPoints")
mySampledata <- mySample%over%grdVoorst
mySampledf <- data.frame(as(mySample,"data.frame"),z=mySampledata$z)

head(mySampledf)

#add column with identifier for composite
mySampledf$composite <- rep(1:nh)

#compute means for composites
compositemeans <- tapply(mySampledf$z,INDEX=mySampledf$composite,FUN=mean)

#Estimate mean for study area
(meanz <- mean(compositemeans))

#Estimate sampling variance of estimated mean
(varmeanz <- var(compositemeans)/nh)

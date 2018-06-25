library(sp)
library(spcosa)
library(ggplot2)

#load data.frame with coordinates (and other attributes) of fine grid (discretization of study area)
load("CovariatesThreeWoredasEthiopia.RData")

#load existing sampling points
load(file="DataThreeWoredasEthiopia.RData")

# plot prior points
ggplot(data = as.data.frame(d)) +
  geom_raster(data=grdEthiopia,mapping = aes(x = s1,y = s2),fill="grey") +
  geom_point(mapping = aes(x = s1,y = s2),size = 2) +
  scale_x_continuous(name = "Easting (km)") +
  scale_y_continuous(name = "Northing (km)") +
  coord_equal(ratio = 1)

#change iclass of grdEthiopia from data.frame to SpatialPixelsDataFrame
gridded(grdEthiopia)<-~s1+s2

#set number of new sampling locations to be selected
n<-100

#compute total sample size (existing points + new points)
ntot<-n+length(d)

#change class of d (existing points) from SpatialPointsDataFrame to SpatialPoints
d<-as(d,"SpatialPoints")

#Remove projection attributes of d
proj4string(d)<- NA_character_

#compute geostrata with argument priorPoints=d
set.seed(314)
myStrata <- stratify(grdEthiopia, nStrata = ntot, priorPoints=d, nTry=10)

#select sampling points of infill sample (centres of geostrata)
mySample <- spsample(myStrata)

#plot geostrata and sampling points (centres of geostrata)
plot(myStrata, mySample)

#select the new points from mySample
ids <- which(mySample@isPriorPoint==F)

#change class of mySample to data.frame
mySample <- as(mySample,"data.frame")
mySamplenew <- mySample[ids,]

#how many new points are selected?
nrow(mySamplenew)

#The reason that more than n new points are selected that there is 1 existing point outside the study area 
#Besides some existing points nearly coincide (multiple points per discretisation gridcell).
#Only 1 point per gridcell is kept, others are removed (see warning of stratify)

#change ntot so that exactly 100 new locations are selected
ntot <- ntot-(nrow(mySamplenew)-n)
set.seed(314)
myStrata <- stratify(grdEthiopia, nStrata = ntot, priorPoints=d, nTry=10)

#select sampling points of infill sample (centres of geostrata)
mySample <- spsample(myStrata)

ids <- which(mySample@isPriorPoint==F)
mySample <- as(mySample,"data.frame")
mySamplenew <- mySample[ids,]

nrow(mySamplenew)

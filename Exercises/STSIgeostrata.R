# load packages
library(spcosa)
library(rgdal)

# set random seed (for reproduction of results)
set.seed(31415)

# read field of interest
shpField <- readOGR(dsn = ".", layer = "farmsum")

# compute compact geographical strata; either use argument cellSize or nGridCells
myStrata <- stratify(shpField,nStrata = 10, cellSize=3, equalArea=TRUE, nTry=3)
#myStrata <- stratify(shpField,nStrata = 10, nGridCells = 2500, equalArea=TRUE, nTry=3)
plot(myStrata)

# obtain the surface areas of the strata
print(areaStrata<-getArea(myStrata))

# select randomly n locations from the strata
mySample <- spsample(myStrata, n = 2)
plot(myStrata, mySample)

# select randomly n locations from the strata to be used for bulking into n composites
myStrata <- stratify(shpField,nStrata = 10, cellSize = 3, equalArea=TRUE, nTry=3)
mySample <- spsample(myStrata, n = 2, type="composite")
plot(myStrata, mySample)

# change class of mySample
samplingPoints <- as(mySample, "SpatialPoints")

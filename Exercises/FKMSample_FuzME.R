# read data
load(file="HunterValley4Practicals.RData")

# read memberships computed with FuzMe
m <- read.csv(file="20_class_Maha.txt",sep="")
m <- m[,-c(1,2,3)]

# defuzzify, i.e. compute for each gridcell the cluster wth largest membership
grdHunterValley$cluster <- apply(m,MARGIN=1,which.max)

#select locations with largest membership in cluster 1...k
units <- apply(m,MARGIN=2,FUN=which.max)
myFKMSample <- grdHunterValley[units,]

#plot clusters and sampling points

ggplot(grdHunterValley) +
  geom_raster(mapping = aes(x = Easting/1000, y = Northing/1000, fill = factor(cluster))) +
  geom_point(data=myFKMSample,mapping=aes(x=Easting/1000,y=Northing/1000),size=2) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_fixed() +
  theme(legend.position="none")

ggplot(grdHunterValley) +
  geom_point(mapping=aes(x=ndvi,y=cti,colour=factor(cluster))) +
  geom_point(data=myFKMSample,mapping=aes(x=ndvi,y=cti),size=2) +
  scale_y_continuous(name = "ndvi") +
  scale_x_continuous(name = "cti") +
  theme(legend.position="none")

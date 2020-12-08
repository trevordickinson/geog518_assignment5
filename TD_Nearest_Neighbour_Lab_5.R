library(spatstat)
library(rgdal)
library(maptools)
library(raster)      
library(sp)           
library(plyr)                                                        
library(lubridate)

#make sure coordinates are correct
Coords <- elev[,c("grid_code")]
crs <- CRS("+init=epsg:26910") 
checkCRS <- proj4string(VRI.no0)
checkCRS
library("sf")

checkCRS <- st_crs(VRI.no0)
checkCRS 

# check clipped_elev and VRI.no0 are both class: spatial points data frame
class(elev)
class(VRI.no0)


#this plots the polygons and points as blk & white and difficult to look at.... need to apply more to it 

plot(VRI.no0)
plot(elev, col = "blue", add = TRUE)

#this just shows the number of occurances; which in our case is mostly 1 because they are unique elevation data points.... 
#convert the elevation data type to factor
elev@data$grid_code <- as.factor(elev@data$grid_code)
levels(elev$grid_code)

kma <- elev
kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]


kma

#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates

#This removes crimes that happened at the same spot- messes up data -
#investigate a way to keep duplicates 

zd <- zerodist(kma)
zd

#if there are duplicates, remove them
kma <- remove.duplicates(kma)

#crude method to create study area-> We should find a better way 
#create an "extent" object which can be used to create the observation window for spatstat

kma.ext <- as.matrix(extent(elev)) 

##################################################################


#create ppp object from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)


#observation window
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))


#Making this obj to run stats on 
#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)



#####
##K-FUNCTION 
#basic k-function (for a visual of how things change at different scales of view)
k.fun <- Kest(kma.ppp, correction = "Ripley")
plot(k.fun)

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(kma.ppp, Kest, nsim = 99, correction = "Ripley")
plot(k.fun.e)

#####

##Nearest Neighbour Distance

#calculate the distances between each point
nearestNeighbour <- nndist(kma.ppp)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))

##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"

##Calculate the nearest neighbor statistic to test for a random spatial distribution.
N.nnd <- sum(kma.ppp$n) #n = number of points in the dataset
N.nnd #378

#double check nrow is the same as N.nnd
nrow(elev)

#Mean nnd #double check if / 1000 is right
nnd = (((sum(nearestNeighbour$Distance))/N.nnd)/1000)
nnd  #0.423662
#SIMILAR TO quadrat analysis this will be for us to calculate
#mean nearest neighbour for random spatial distribution

#Define study area as VRI.no0 boundary
library(raster)

crs(VRI.no0)
studyArea <- area(VRI.no0) / 1000000
#View(studyArea)
studyArea.df <- as.data.frame(studyArea)
colnames(studyArea.df) <- c("area")
studyArea.df <- as.data.frame(studyArea.df)
studyArea.km2 <- sum(studyArea.df$area)
studyArea.km2 

mean.neighborhoodArea <- mean(studyArea.df$area)
mean.neighborhoodArea 


pointDensity <- N.nnd / (studyArea.km2) #number of points / area
pointDensity


r.nnd = ((1)/(2*(sqrt(pointDensity))))
r.nnd

d.nnd = ((1.07453)/(sqrt(pointDensity)))
d.nnd

R = (nnd / r.nnd)
R

SE.NND <- ((0.26136)/(sqrt((N.nnd)*(pointDensity))))
SE.NND

z = (((nnd) - r.nnd)/ (SE.NND))
z







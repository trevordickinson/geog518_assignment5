install.packages("spatstat")
install.packages("sp")
install.packages("plyr")
install.packages("maptools")
install.packages("readORG")

#####
#load required libraries
#####


library("readOGR")
library("maptools")
library("plyr")
library("sp")
library("spatstat")
library("rgdal")
library("tidyverse")
library("lubridate")
library("e1071")
library("gtable")
library("gridExtra")
library("maps")
library("grid")
library("ggplot2")
library("dplyr")
library("bcmaps")
library("tmap")
library("fBasics")
library("bcmapsdata")
library("raster")
library("maps")
library("tmap")
library("sf")




Coords <- elev_clean[,c("grid_code")]
crs <- CRS("+init=epsg:26910") 
checkCRS <- proj4string(vriClean)
checkCRS
library("sf")

checkCRS <- st_crs(vriClean)
checkCRS #epsg 4326


#create a file type called a SpatialPointsDataFrame
elev_points_PPA <- SpatialPointsDataFrame(coords = Coords, data = elev_clean, proj4string = crs)

#need to build ppp object 
#need list of x, y, and observation window


#transform the projection of both datasets to ensure that they are the same
elev_clean1 <- spTransform(elev_clean, CRS("+init=epsg:26910"))
vriClean1 <- spTransform(vriClean, CRS("+init=epsg:26910"))


#this plots the polygons and points as blk & white and difficult to look at.... need to apply more to it 

plot(vriClean1)
plot(elev_clean1, add = TRUE)



#this gets rid of elev points that fall outside of the polygon dataset 
#intersect the two datasets
elev_clean1 <- raster::intersect(elev_clean1, vriClean1)
plot(vriClean1)
plot(elev_clean1, add = TRUE)


#this just shows the number of occurances; which in our case is mostly 1 because they are unique elevation data points.... 
#convert the crimes data type to factor
elev_clean1@data$grid_code <- as.factor(elev_clean1@data$grid_code)
levels(elev_clean1$grid_code)
plot(elev_clean1$grid_code)






###########################################################################
###################################################################
########################################################
#############################################
##################################
######################
############
######



elevation <- "grid_code"


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






#Decide on our study area 
#Different crime types may end up with different subsets/ob windows that can impact my results
#can I subset before defining the window?


#observation window
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))


#Making this obj to run stats on 
#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)





####################

#Refer to notes to fill out gaps
##Nearest Neighbour Distance
###NEAREST NEIGHBOUR
nearestNeighbour <- nndist(kma.ppp)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"


##Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour
N.nnd <- sum(kma.ppp$n)
N.nnd

#check to make sure it was correct number of pts
nrow(elev)

nnd = ((sum(nearestNeighbour$Distance))/N.nnd)/1000
nnd


##################################
##################################
##################################


#mean nearest neighbour for random spatial distribution
  

study_area = vriClean$Area_sqkm <- area(vriClean)

VRI_Area

VRI@data[["Shape_Leng"]]

  library(raster)

  crs(vriClean)
  studyArea <- area(vriClean) / 1000000
  View(studyArea)
  studyArea.df <- as.data.frame(studyArea)
  colnames(studyArea.df) <- c("area")  
  studyArea.df <- as.data.frame(studyArea)
  studyArea.km2 <- sum(studyArea.df$area)
  studyArea.km2
  
  view(studyArea.df)
  
    
 
  pointDensity <- N / (M^2) #number of points / area
  
  r.nnd = ((1)/(2*(sqrt(pointDensity))))
  
  d.nnd = ((1.07453)/(sqrt(pointDensity)))
  
  R = (nnd / r.nnd)
  
  SE.NND <- (0.26136)/sqrt((N))*(pointDensity)
  
  z = ((nnd - r.nnd)/ (SE.NND))
  



  #####
  ##K-FUNCTION
  #basic k-function
  k.fun <- Kest(kma.ppp, correction = "Ripley")

  plot(k.fun)
  
  #use simulation to test the point pattern against CSR
  k.fun.e <- envelope(kma.ppp, Kest, nsim = 99, correction = "Ripley")
  plot(k.fun.e)
  
  
  #####
  ###KERNEL DENSITY ESTIMATION
  #2D (gaussian) kernel, compare how bandwidth (sigma) selection influences the point density estimates
  #since data are projected, sigma is represented in metres
  #eps is the width and height of the pixels (1000m X 1000m)
  #coerce to a SpatialGridDataFrame for plotting
  
  #CHANGE THE raster size here... not 1000 (that is a km x km)
  #then maybe run 4 different sigmas at the same cell size to figure out the impact of sigma
  kde.100 <- density(kma.ppp, sigma = 100, at = "pixels", eps = c(100, 100))
  kde.SG <- as(kde.100, "SpatialGridDataFrame")
  #kde.500 <- density(kma.ppp, sigma = 500, at = "pixels", eps = c(100, 100))
  #kde.SG <- cbind(kde.SG, as(kde.500, "SpatialGridDataFrame"))
  
  # he is pretty sure resolution for the scale here is points per square meter
  #***CHOOSE SOME OTHER SIGMA VALUES FOR SENSITIVITY ANALYSIS***
    
  names(kde.SG) <- c("Sigma Sensitivity Value")
  #plot
  x11() #opens a new plot window
  spplot(kde.SG)
  
  #can see how the bandwidth selection influences the density estimates
  summary(kde.SG)
  
  #use cross-validation to get the bandwidth that minimizes MSE
  #cross validation changes the resolution of sigma and show you how the mean square error changes over that range and then we try to pick a sigma value that minimizes mean square error
  bw.d <- bw.diggle(kma.ppp)
  #plot the "optimal" bandwidth
  plot(bw.d, ylim=c(-10, 10), main= "Cross Validation Sigma Values")
  
  #density using the cross-validation bandwidth - Here is where the size is controlled
  kde.bwo <- density(kma.ppp, sigma = bw.d, at = "pixels", eps = c(600, 600))
  plot(kde.bwo)
  
  

  
  
  
  
  
  
  
  
### SPATIAL AUTOCORRELATION ###

install.packages("spdep")
install.packages("raster")
install.packages("rgdal")
install.packages("tmap")
install.packages("shinyjs")

library(rgdal)
library(tmap)
library(spdep)
library(raster)
library(spatstat)
library(maptools)
library(sp)           
library(plyr)
library(lubridate)

########################
# First we will identify which polygons 'neighbour' each other

#Queens weight -Dominant Species
# Identify polygons that share edges (i.e. define the neighbourhoods)
vri.nb <- poly2nb(VRI.no0)

# Convert the neighbourhood lists from above into a 'line graph' or 'network graph.'
vri.net <- nb2lines(vri.nb, coords=coordinates(VRI.no0))
crs(vri.net) <- crs(VRI.no0)

#Visualize the network graph
tm_shape(VVRI.no0) + tm_borders(col='grey') +
  tm_layout(main.title = "Biomass \n Using Queen's Weight Connections",
            main.title.position = "center",
            main.title.color = "blue") + 
  tm_shape(vri.net) + tm_lines(col='black', lwd = 1) +
  tm_compass(type="arrow", position=c("LEFT", "top"), show.labels = 1) + #this adds a north arrow
  tm_scale_bar(position =c("left", "bottom")) # this adds a scale bar

# Rooks weight on top of Queen's weight connections for Forest Stand Height

# Identify polygons that share edges (i.e. define the neighbourhoods)
vri.nb2 <- poly2nb(VRI.no0, queen = FALSE)
# Note the default is for queen's weight for this function, so we have to use 'queen = FALSE' to specify rook's weighting.

# Convert the neighbourhood lists from above into a 'line graph' or 'network graph.'
vri.net2 <- nb2lines(vri.nb2, coords=coordinates(VRI.no0))
crs(vri.net2) <- crs(vriClean)

#Visualize the new network graph with Queen's and Rook's weights
tm_shape(VRI.no0) + tm_borders(col='lightgrey') +
  tm_layout(main.title = "Forest Stand Biomass \n Using Rook's and Queen's Weight Connections",
            main.title.position = "center",
            main.title.color = "black") + 
  tm_shape(vri.net) + tm_lines(col='black', lwd = 1) +
  tm_shape(vri.net2) + tm_lines(col='pink', lwd = 1) +
  tm_compass(type="arrow", position=c("LEFT", "top"), show.labels = 1) + #this adds a north arrow
  tm_scale_bar(position =c("left", "bottom")) # this adds a scale bar

########################

#Queens weight - Dominant Tree Stand Species
# Identify polygons that share edges (i.e. define the neighbourhoods)
vri.nb <- poly2nb(VRI.no0)

# Convert the neighbourhood lists from above into a 'line graph' or 'network graph.'
vri.net <- nb2lines(vri.nb, coords=coordinates(VRI.no0))
crs(vri.net) <- crs(VRI.no0)

#Visualize the network graph
tm_shape(VRI.no0) + tm_borders(col='lightgrey') +
  tm_layout(main.title = "Stand Biomass \n Using Queen's Weight Connections",
            main.title.position = "center",
            main.title.color = "black") + 
  tm_shape(vri.net) + tm_lines(col='blue', lwd = 1) +
  tm_compass(type="arrow", position=c("LEFT", "top"), show.labels = 1) + #this adds a north arrow
  tm_scale_bar(position =c("left", "bottom")) # this adds a scale bar

# Rooks weight on top of Queen's weight connections for Forest Stand Height

# Identify polygons that share edges (i.e. define the neighbourhoods)
vri.nb2 <- poly2nb(VRI.no0, queen = FALSE)
# Note the default is for queen's weight for this function, so we have to use 'queen = FALSE' to specify rook's weighting.

# Convert the neighbourhood lists from above into a 'line graph' or 'network graph.'
vri.net2 <- nb2lines(vri.nb2, coords=coordinates(VRI.no0))
crs(vri.net2) <- crs(VRI.no0)

#Visualize the new network graph with Queen'ss and Rook's weights
tm_shape(VRI.no0) + tm_borders(col='lightgrey') +
  tm_layout(main.title = "Forest Stand Biomass \n Using Rook's and Queen's Weight Connections",
            main.title.position = "center",
            main.title.color = "black") + 
  tm_shape(vri.net) + tm_lines(col='blue', lwd = 1) +
  tm_shape(vri.net2) + tm_lines(col='lightblue', lwd = 1) +
  tm_compass(type="arrow", position=c("LEFT", "top"), show.labels = 1) + #this adds a north arrow
  tm_scale_bar(position =c("left", "bottom")) # this adds a scale bar


########################

vri.lw <- nb2listw(vri.nb, zero.policy = TRUE, style = "W")

print.listw(vri.lw, zero.policy = TRUE)

########################

########################
#Calculate a Moran's I statistic which is the most common way for testing spatial autocorrelation. 
# i = an object in a landscape
# j = the object i's neighbours
# but who are your neighbours?

VRI.no0$Stand_StemBio <- as.numeric(VRI.no0$Stand_StemBio)

mi <- moran.test(VRI.no0$Stand_StemBio, vri.lw, zero.policy = TRUE)
mi


moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vri.lw)


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]

#not sure if below is correct
z <- ((mI - eI)/(sqrt((var*mI))))

########################  
#Run's the local moran's I on our data and gives the local weights matrix we've calculated.
lisa.test <- localmoran(VRI.no0$Stand_StemBio, vri.lw, zero.policy = TRUE)

#This extracts the relevant information
VRI.no0$Ii <- lisa.test[,1]
VRI.no0$E.Ii<- lisa.test[,2]
VRI.no0$Var.Ii<- lisa.test[,3]
#look up if this is a 1 or 2 tailed test, figure out how to get the test you want & make sure you know how to interpret it. 
VRI.no0$Z.Ii<- lisa.test[,4]
VRI.no0$P<- lisa.test[,5]
########################

#when [presenting the lisa test results and map, consider how do we communicate what we want to communicate. For instance how do the results sound to a reader that wants to understand. Dont just make a map showing all the I values... you need to compare them to something. Maybe compare Map of I value vs map of expected varaibles.... but then reader still has to visually compare. Jason said there may be a variable we have that is better for demonstrating whether there is spatial autocorrelation and more effectively communicates it to the reader. ]

map_LISA <- tm_shape(VRI.no0) + 
  tm_polygons(col = "P", 
              title = " LMI Z test P values", 
              breaks = c(0.000, 0.001, 0.01, 0.05, 0.07, 0.9, 1.000),
              style = "fixed", 
              palette = "Set2", n = 10, stretch = FALSE) +
  tm_layout(main.title = "LISA Test using Tree Stand Height (metres)",
            main.title.position = "center",
            main.title.color = "black") +   
  tm_compass(type="arrow", position=c("right", "top"), show.labels = 1) +
  tm_scale_bar(position = c("left", "top")) 


map_LISA


map_LISA2 <- tm_shape(VRI.no0) + 
  tm_polygons(col = "P", 
              title = "P values", 
              breaks = c(0.000, 0.05, 1.000),
              style = "fixed", 
              palette = "RdBu", stretch = FALSE) +
  tm_layout(main.title.position = "center",
            main.title.color = "black") +   
  tm_compass(type="arrow", position=c("right", "top"), show.labels = 1) +
  tm_scale_bar(position = c("left", "top")) 


map_LISA2




########################
#each point is a polygon and being compared to its neighbour. look at it in 4 quadrants. 'high high' 'low low'.. e.g. if polygon has a high value and its neighbors also have a high value then it is the high high . these two quadrants if populated show positive autocorrelation. 

# if you had a bunch of points in the other quadrants it would indicate negative autocorrelation (potentially)

# the dashed line
moran.plot(VRI.no0$Stand_StemBio, vri.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Dominant Tree Species Value", 
           ylab="Neighbour's Value", quiet=NULL)


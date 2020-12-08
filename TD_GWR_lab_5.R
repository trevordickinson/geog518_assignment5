####Geographically Weighted Regression

VRI.no0.coords <- sp::coordinates(VRI.no0)
#Observe the result:
head(VRI.no0.coords)
#Now add the coordinates back to the spatialpolygondataframe
#stores in our data
VRI.no0$X <- VRI.no0.coords[,1]
VRI.no0$Y <- VRI.no0.coords[,2]


VRI.no0.coords


########################################################




#The function finds a bandwidth for a given geographically weighted regression by optimzing a 
#selected function. For cross-validation, this scores the root mean square prediction error for the 
#geographically weighted regressions, choosing the bandwidth minimizing this quantity.


###Determine the bandwidth for GWR: take 5-10 minutes


GWRbandwidth <- gwr.sel(VRI.no0$Stand_StemBio ~ VRI.no0$Elev, 
                        data=VRI.no0, coords=cbind(VRI.no0$X, VRI.no0$Y),adapt=T) 


GWRbandwidth



warnings()


##############################################################################################################

###Perform GWR on the two variables with the bandwidth determined above
###This takes about 30 minutes

gwr.model = gwr(VRI.no0$Stand_StemBio ~ VRI.no0$Elev, 
                data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 


#############################RUNNING THE LINE ABOVE NOW- START TIME ~ 11:55 am until about 12:30 #############################################################

#Print the results of the model
gwr.model



#
#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
VRI.no0$localr <- results$localR2






tmap_mode("view")
tmap_mode("plot")


#good for the less than 0.00 in legend. 
#Create choropleth map of r-square values
map_r2 <- tm_shape(VRI.no0) +
  tm_polygons(col = "localr",
              title = "R2 values",
              breaks=c(-Inf, -0.1, 0.00, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
              palette = "-RdBu", n = 10)+
  tm_compass(type="arrow", position=c("right", "top"), show.labels = 1) +
  tm_scale_bar(position =c("left", "bottom")) 
map_r2







krig_elv_map

(names)

#Time for more magic. Let's map the coefficients
VRI.no0$coeff <- results$VRI.no0.Elev
#Create choropleth map of the coefficients
map_coef <- tm_shape(VRI.no0) +
  tm_polygons(col = "coeff",
              title = "Coefficients",
              breaks=c(-Inf, -50, -20, -10, -5, -2.5, 0, 2.5, 5,  10, 20, 50),
              #style = "fisher",
              palette = "-RdBu") +
  tm_compass(type="arrow", position=c("right", "top"), show.labels = 1) +
  tm_scale_bar(position =c("left", "bottom")) 
  
map_coef




citation("spgwr")




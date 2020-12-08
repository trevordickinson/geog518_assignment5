
library(rgdal)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(tmap)
library(gstat)
library(raster)    # Used to clip out thiessen polygons
library(sp)





#Examine the first several rows of the elev dataset. 
head(elev)






#Check outhe and observed elevation data sample points 
ml <- tm_shape(vriClean) + 
  tm_polygons() +
  tm_shape(elev) +
  tm_dots(n=10, col="grid_code", palette = "OrRd",  
          title="Sampled Elevation Points \n(in metres)", size=0.3) + 
  tm_compass(type="arrow", position=c("right", "top"), show.labels = 1) +
  tm_scale_bar(position =c("left", "bottom")) 
ml



#####################################################################################
########################Kriging Interpolation##############################################
###################################################################################################


f.0 <- as.formula(grid_code ~ 1) 

#Create variogram
var.smpl <- variogram(f.0, elev, cloud = FALSE) 
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=46000, model="Exp", range=6507, nugget=0))
plot(var.smpl, dat.fit)

#####################


# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige(f.0, elev, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, vriClean)

# Plot the map


tmaptools::palette_explorer() 


krig_elv_map <- tm_shape(r.m) + 
  tm_raster(n=25, palette="-YlGn",  
            title="Predicted Elevation \n(in metres)") +
  tm_shape(elev) + tm_dots(size=0.02) +
  tm_legend(legend.outside=TRUE)

krig_elv_map


###############VARIANCE#################################################################$$

r   <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, vriClean)



variance_map_krig <- tm_shape(r.m) + 
  tm_raster(style = "fixed",
            breaks = c(0, 100, 200, 500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000,20000),palette ="Reds",
            title="Variance map \n(in metres)") +tm_shape(elev) + tm_dots(size=0.02) +
  tm_legend(legend.outside=TRUE)

variance_map_krig
###############################CI

r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, vriClean)

CI_krig_map <- 
  tm_shape(r.m) + 
  tm_raster(n=10, palette ="Reds",
            title="95% CI map \n( in metres)") +tm_shape(elev) + tm_dots(size=0.02) +
  tm_legend(legend.outside=TRUE)

CI_krig_map
#######################################





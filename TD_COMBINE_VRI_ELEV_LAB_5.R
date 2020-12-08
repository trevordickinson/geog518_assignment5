#This is done after interpolation, and combines the raster elevation points with the VRI polygons 


tmap_mode("view")
tmap_mode("plot")



r <- raster(dat.krg)
r.m <- mask(r, vriClean)
sufaceMap <- tm_shape(r.m) + 
  tm_raster(n=5,palette = "viridis",
            title="Elev (m)") +
  tm_shape(elev) + tm_dots(size=0.02)
sufaceMap

#If you have too many cells, 
#you can reduce the number by aggregating values
#agg <- aggregate(yourRasterFromKriging, fact=??, fun=mean)

#Extract average elev for each polygon
vriClean$Elev <- extract(r, vriClean, fun = mean)[,1]




######Linear Regression##########

plot(vriClean$Stand_StemBio ~ vriClean$Elev)






#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
VRI.no0 <-  vriClean[which(vriClean$Stand_StemBio > 0), ]
VRI.no0 <-  VRI.no0[which(VRI.no0$Elev > 0), ]

#Now plot the data again
plot(VRI.no0$Stand_StemBio ~ VRI.no0$Elev)




#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(VRI.no0$Stand_StemBio ~ VRI.no0$Elev)

#Add the regression model to the plot you created- plots regression model to scatterplot
plot(VRI.no0$Stand_StemBio ~ VRI.no0$Elev)
abline(lm.model, col = "red")

#Get the summary of the results
summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
#fitted values is a list of predicted values for each polygon 
VRI.no0$predictlm <- lm.model$fitted.values

#open up the data and look at it to make sense of it

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
VRI.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(VRI.no0@data)

#Now, create choropleth map of residuals
map_resid <- tm_shape(VRI.no0) +
  tm_polygons(col = "residuals",
              title = "Stand Biomass Residuals",
              style = "jenks",
              palette = "viridis", n = 10)

map_resid










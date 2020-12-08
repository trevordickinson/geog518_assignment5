library("rgdal")
library("lubridate")
library("e1071")
library("gtable")
library("gridExtra")
library("grid")
library("ggplot2")
library("dplyr")
library("raster")
library("maps")
library("tmap")
library("rgeos")

##### Descriptive stats- Elevation ##########

#make elev numeric:
elev$grid_code <- as.numeric(elev$grid_code)

# Range of Elevation measurements total (some outside of watershed boundaries)
elev_range_all <- range(elev$grid_code)


# Sample size - elevation points
elev$grid_code <- as.numeric(elev$grid_code)
n_elev <- nrow(elev)
n_elev


# Range of Elevation measurements total (some outside of watershed boundaries)
elev_range_all <- range(elev$grid_code)


# Mean
elev_mean <- mean(elev$grid_code)
elev_mean 

#Median
elev_median <- median(elev$grid_code)
elev_median

#Mode
elev_mode <- as.numeric(names(sort(table(elev$grid_code), decreasing = TRUE))[1])
elev_mode 

#Standard Deviation
elev_sd <- sd(elev$grid_code, na.rm = TRUE) #Calculate the SD, ignoring NA values
elev_sd 

#Skewness
elev_skew <- skewness(elev$grid_code, na.rm = TRUE)[1]
#elev_skew <-formatC( round(elev_skew, 3 ), format='f', digits=2 )
elev_skew

#Kurtosis
elev_kurt <- kurtosis(elev$grid_code, na.rm = TRUE)[1]
#kurtPop <-formatC( round(kurtPop, 3 ), format='f', digits=2 )
elev_kurt

#CoV
elev_CoV <- (elev_sd / elev_mean) * 100
#CoVPop <-formatC( round(CoVPop, 3 ), format='f', digits=2 )
elev_CoV 

#Normal distribution test
elev_norm_PVAL <- shapiro.test(elev$grid_code)$p.value
#normPop_PVAL <- formatC(normPop_PVAL, format = "e", digits = 2) 
elev_norm_PVAL

######### BIOMASS DESCRIPTIVE STATS ############

#make Biomass estimates numeric:
VRI.no0$Stand_StemBio<- as.numeric(VRI.no0$Stand_StemBio)

# Sample size - polygons with Biomass measurements
VRI.no0$Stand_StemBio <- as.numeric(VRI.no0$Stand_StemBio)
n_Biomass <- nrow(VRI.no0) #MAYBE MAKE THIS WITHOUT NAs TOO??
n_Biomass

# Range of Biomass estimates
Biomass_range_all <- range(VRI.no0$Stand_StemBio)
Biomass_range_all

# Mean
Biomass_mean <- mean(VRI.no0$Stand_StemBio)
Biomass_mean 

#Median
Biomass_median <- median(VRI.no0$Stand_StemBio)
Biomass_median

#Mode
Biomass_mode <- as.numeric(names(sort(table(VRI.no0$Stand_StemBio), decreasing = TRUE))[1])
Biomass_mode 

#Standard Deviation
Biomass_sd <- sd(VRI.no0$Stand_StemBio, na.rm = TRUE) #Calculate the SD, ignoring NA values
Biomass_sd 

#Skewness
Biomass_skew <- skewness(VRI.no0$Stand_StemBio, na.rm = TRUE)[1]
#elev_skew <-formatC( round(elev_skew, 3 ), format='f', digits=2 )
Biomass_skew

#Kurtosis
Biomass_kurt <- kurtosis(VRI.no0$Stand_StemBio, na.rm = TRUE)[1]
#kurtPop <-formatC( round(kurtPop, 3 ), format='f', digits=2 )
Biomass_kurt

#CoV
Biomass_CoV <- (Biomass_sd / Biomass_mean) * 100
#CoVPop <-formatC( round(CoVPop, 3 ), format='f', digits=2 )
Biomass_CoV 

#Normal distribution test
Biomass_norm_PVAL <- shapiro.test(VRI.no0$Stand_StemBio)$p.value
#normPop_PVAL <- formatC(normPop_PVAL, format = "e", digits = 2) 
Biomass_norm_PVAL
#Error in shapiro.test(vriClean$Stand_StemBio) : sample size must be between 3 and 5000  ###THIS DOESNT WORK. SAMPLE SIZE TOO LARGE!!

#make a tables of descriptive stats using code from lab 1

Samples = c("Tree Biomass \n (tonnes/ha)", "Elevation \n (metres)") #Create an object for the labels
Samples
Mean = c(Biomass_mean, elev_mean) #Create an object for the means
Mean <- formatC( round(Mean, 3 ), format = 'f', digits = 2 )
SD = c(Biomass_sd, elev_sd) #Create an object for the standard deviations
SD <-formatC( round(SD, 3 ), format='f', digits=2 )
Median = c(Biomass_median, elev_median) #Create an object for the medians
Median 
Mode <- c(Biomass_mode, elev_mode) #Create an object for the modes
Mode
Skewness <- c(Biomass_skew, elev_skew) #Create an object for the skewness
Skewness
Kurtosis <- c(Biomass_kurt, elev_kurt) #Create an object for the kurtosis
Kurtosis
CoV<- c(Biomass_CoV, elev_CoV) #Create an object for the CoV
CoV <-formatC( round(CoV, 3 ), format='f', digits=2)
Normality <- c(Biomass_norm_PVAL, elev_norm_PVAL) #Create an object for the normality PVALUE #NOTE ELEV USED AS PLACEHOLDER FOR BIOMASS
Normality
Range <- c(Biomass_range_all , elev_range_all)
Range
SampleSize <- c(n_Biomass, n_elev)
SampleSize


####################################################################

######## HISTOGRAMS #######

####################################################################

#Transform elev from spatial data frame to regular dataframe 

elev1 <- as.data.frame(elev)




elev1


#GGplot histograms
hist_elev <- ggplot(elev1, aes(x = grid_code)) + 
  geom_histogram(bins = 10, color = "black", fill = "black") + labs(title = "Frequency of Elevations (metres)", x = "Elevation (metres)", y = "Frequency of occurrence") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) #set title to center 
#scale_x_continuous(breaks = seq(0, 8500, by = 100)))
hist_elev


###################BIOMASS#########################

#Transform VRI.no0 from spatial data frame to regular dataframe 

vriClean2 <- as.data.frame(VRI.no0)




View(vriClean2)


#GGplot histograms: VRI
hist_VRI <- ggplot(vriClean2, aes(x = Stand_StemBio)) +  
  
  geom_histogram(bins = 10, color = "black", fill = "orange", binwidth = 10) + labs(title = "Stand Stem Biomass Polygon Distribution", x = "Stand Stem Biomass (Tonnes)", y = "Frequency of occurrence") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) #set title to center 
  scale_x_continuous(breaks = seq(0, 8500, by = 100))
hist_VRI

#GGplot histograms: Elev

hist_elev <- ggplot(elev1, aes(x = grid_code)) +  
  
  geom_histogram(bins = 10, color = "black", fill = "orange", binwidth = 10) + labs(title = "Frequency Distribution of Elevation Points (Metres)", x = "Elevation in Metres", y = "Frequency of occurrence") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) #set title to center 
scale_x_continuous(breaks = seq(0, 8500, by = 100))
hist_elev




qqline(Biomass_skew$responsetime)

hist(Biomass_kurt$responsetime, breaks='fd')

print(Biomass_kurt)

plot(Biomass_kurt)

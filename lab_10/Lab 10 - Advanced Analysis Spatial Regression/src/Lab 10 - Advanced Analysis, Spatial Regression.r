###########################################################################
##
##	Lab 10 - Advanced Analysis, Spatial Regression 
## 
###########################################################################


###########################################################################
##	Required Libraries
##

library(sp)
#library(rgdal)
library(sf) #Replaces old rgdal package
library(spdep)
library(spatialreg)
library(lmtest)
library(RColorBrewer)

##
###########################################################################


###########################################################################
##	Data Preparation
##



##	Load the corrected Boston housing data provided in the spdep package:
data(boston)
ls()

##	Let's create  a spatial data frame object from the corrected boston.c
##		data.frame:
head(boston.c)

##	The data have longitude and latitude coordinates, so we'll extract 
##		those for our coordinate system and set the CRS to an unprojected,
##		geographic coordinate system:
boston_spdf <- st_as_sf(boston.c, coords = c("LON", "LAT"))
st_crs(boston_spdf) <- "+proj=longlat +ellps=WGS84"

##	Let's write out the unprojected points dataframe as a shapefile:
#st_write(obj=boston_spdf, dsn=".", layer="boston_spdf.shp", driver="ESRI Shapefile")


##	QUESTION 1:	 In order for us to use any kind of distance based 
##		estimates we need to project the data from a geographic lat/long
##		coordinate system into one with distance units.  Let's use the 
##		the Massachusetts state plane, NAD 1983 HARN, FIPS 2002 projection.
##		You can find information on it here:
##			http://www.spatialreference.org/ref/esri/102287/
##		Use the st_transform() function on the boston_spdf object
##		to assign the proj4 from the ESRI:102287 code.
##		Assign the reprojected spatial dataframe to an object named:
##			boston_spdf_prj
##		(10 points)
###########################################################################

boston_spdf_prj <- st_transform(boston_spdf, crs = 4152)


###########################################################################
##	Data Analysis

##	Generate a summary of our data set
head(boston_spdf_prj)
summary(boston_spdf_prj)


##	For more information on the Boston dataset and what each of the 
##		variables represent, please see the following:
##		http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html


##	Exploratory pairs plot of our response, the corrected median
##		house value per Boston township, and some of the other covariates
##		of interest:
##	NOTE: We can subset the spatial dataframe using a single square
##		bracket and a vector of its data.frame column names:
pairs(boston_spdf_prj[,c("CMEDV", "AGE", "DIS", "RAD", "TAX", "ZN", "CHAS"), drop=TRUE])


##	We know from prior research that most of the covariates we are
##		interested in using to explaing variability in median house
##		values are linearly related to the log() of median house values
##		so let's calculate that and add it to our spatial dataset:
boston_spdf_prj$LOGCMEDV <- log(boston_spdf_prj$CMEDV)


##	Plot a single comparison:
##	NOTE: Remember the difference between a single and double square
##		bracket index for Spatial*DataFrame objects?  The double square 
##		bracket or the $ returns a vector, the single [ returns another
##		Spatial*DataFrame object...
plot(y=boston_spdf_prj[["LOGCMEDV"]], x=boston_spdf_prj[["AGE"]])

##	Let's also explore how to create easy plots using R's formula syntax
##		instead of specifying x and y directly:
plot(boston_spdf_prj[["CMEDV"]] ~ boston_spdf_prj[["AGE"]])
plot(log(boston_spdf_prj$CMEDV) ~ boston_spdf_prj$AGE)

##	Can also plot more than one pair at once, and R will ask you to confirm
##		before plotting the next pair:
plot(boston_spdf_prj$LOGCMEDV ~ boston_spdf_prj$AGE + boston_spdf$DIS)

##	And if we use a factor in this formula syntax we can change the 
##		kind of plot we get (a box plot in this case):
plot(log(boston_spdf_prj$CMEDV) ~ boston_spdf_prj$CHAS)


##	QUESTION 2:	 Create a boxplot of DIS by CHAS and a scatterplot of 
##		LOGCMEDV vs. DIS, but color the points by the factor variable, CHAS,
##		indicating which points are on the Charles River and which aren't.
##		Save both to a PDF.
##		NOTE:  Make sure that your working directory is set to your /output
##			folder so you don't pollute your data or src folders...
##	(10 points)

plot(boston_spdf_prj$DIS~boston_spdf_prj$CHAS)
plot(boston_spdf_prj$LOGCMEDV~boston_spdf_prj$DIS,col = boston_spdf_prj$CHAS, pch = 18)


##-------------------------------------------------------------------------
## 1. Define neighbors and create weights matrix

##	Save off our new, projected coordinates for distance calculations:
coord_list_prj <- st_coordinates(boston_spdf_prj)

##	Calculate our neighborhood lists based on distance range of 
##		0.0 - 5000.0 m:
boston_kd1 <- dnearneigh(coord_list_prj, d1=0.0, d2=5000.0)

##	Plot out our data points and the neighborhood connectance:
##		Note the color definition is in # format, #RRGGBBAA, where AA is
##		the alpha transparency level and each two digit combination for 
##		the red, green, blue and alpha levels are hexadecimal values 0 - FF
##		or 0 - 255 in decimal:
plot(st_geometry(boston_spdf_prj))
plot(boston_kd1, coord_list_prj, pch=20, cex=0.5, col="#0000FF0F", add=T)


##	QUESTION 3:	 Create our weighted neighbor matrix from the neighbor list
##		by running the nb2listw() function.  Save the weighted neighbor 
##		matrix in an object named boston_kd1_wm.
##	(10 points)

boston_kd1_wm <- nb2listw(boston_kd1, style = "W")

##-------------------------------------------------------------------------


##-------------------------------------------------------------------------
## 2. See if there is any spatial structure to our response variable:

##	Calculate a global Moran's I value for the log() median house value:
moran.test(boston_spdf_prj$LOGCMEDV, listw=boston_kd1_wm)

##	Generate a Moran's plot of our response vs. the weighted sum of 
##		neighbor's values:
moran.plot(boston_spdf_prj$LOGCMEDV, boston_kd1_wm, labels=as.character(boston_spdf_prj$ID))

##	Let's construct the ordinary least squares (OLS) model we think will 
##		explain variance in house values, noting that our estimated model
##		uses no spatial component:
boston_lm <- lm(LOGCMEDV ~ RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston_spdf_prj)
summary(boston_lm)

##	The model does well and explains a large amount of the variability in
##		log() median house values, however, since we are concerned about 
##		spatial dependence in our model output, let's check our model
##		residuals for autocorrelation:

##	Save the model predictions and residuals by attaching them to our 
##		spatial dataframe:
boston_spdf_prj$lmpred <- predict(boston_lm)
boston_spdf_prj$lmresid <- residuals(boston_lm)

##	Calculate Moran's I on the model estimates:
lm.morantest(boston_lm, boston_kd1_wm)

##	NOTE: This can be done on the residuals directly, instead of using the
##		shortcut of the lm.morantest() function (and should give you the same
##		Moran's I statistic but because it is "naive" about the data being
##		residuals, the variance is inflated and your ability to detect
##		any residual spatial autocorrelation is diluted):
moran.test(residuals(boston_lm), boston_kd1_wm, randomisation=FALSE)


##	Let's map out the Local Moran's I estimates of autocorrelation for
##		each observation point:

## Local Autocorrelation: Local Moran's I (normality assumption)
localmoran1 <- localmoran(residuals(boston_lm), listw=boston_kd1_wm)

boston_spdf_prj$lmmoranz <- abs(localmoran1[,4]) ## Extract z-scores

lm.palette <- colorRampPalette(c("grey", "orange", "red"), space = "rgb")

spplot(as(boston_spdf_prj, "Spatial"), zcol="lmmoranz", col.regions=lm.palette(20), main="Local Moran's I (|z| scores)", pretty=T)


##	QUESTION 4:	 Create a new color ramp palette with colors of your 
##		choosing, but that make sense for displaying z-scores.  Next plot
##		the localmoran() tests for the original LOGCMEDV response variable
##		using your new color palette.
##	(20 points)



##-------------------------------------------------------------------------


##-------------------------------------------------------------------------
## 3. Determine the type of spatial dependence and hence the type
##		of spatial regression to apply:

##	We'll use the Lagrange Multiplier tests on our regression object
##		to discover the type of spatial model we might need to employ.  
##		You should do a perusal of the help for the lm.LMtests() function
##		to get a better feel for what the results indicate:
lm.LMtests(boston_lm, boston_kd1_wm, test="all")

##	Perform a test to see whether the residuals are heteroskedastic.  There
##		are multiple tests out there for doing this, but the studentized
##		Breusch-Pagan is relatively easy to understand and interpret:
bptest(boston_lm)


##	QUESTION 5:	 What do these tests tell us about the fit of the OLS model
##		and how might we address the problems indicated?
##	(10 points)

# Spatial Dependence: The OLS model fails to account for spatial autocorrelation in the data, 
  #both in the form of spatial error dependence and spatial lag dependence. 
  #This leads to inefficient and biased estimates. You should switch to a spatial regression model (SAR, SEM, or SARMA) to properly account for these spatial effects.

# Heteroskedasticity: The presence of heteroskedasticity in the residuals means that the OLS estimates may have biased standard errors, 
  #leading to unreliable hypothesis tests. 
  #To address this, you should use heteroskedasticity-robust standard errors (via vcovHC() or coeftest()) or consider using WLS or GLS methods.


##-------------------------------------------------------------------------


##-------------------------------------------------------------------------
## 4. Estimate a spatial error model:

boston_error <- errorsarlm(LOGCMEDV ~ RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston_spdf_prj, boston_kd1_wm)
summary(boston_error)

##	Re-run our tests for spatial autocorrelation and heterskedasticity:
resids <- residuals(boston_error)
moran.test(resids, boston_kd1_wm)
bptest.Sarlm(boston_error)

##	By looking at the diagnostics we can see that the spatial
##		error model removes the spatial autocorrelation.

##	Save the model predictions and residuals into our 
##		spatial dataframe:
boston_spdf_prj$errorpred <- predict(boston_error)
boston_spdf_prj$errorresid <- residuals(boston_error)

##-------------------------------------------------------------------------


##-------------------------------------------------------------------------
## 5. Estimate a spatial lag regression

boston_lag <- lagsarlm(LOGCMEDV ~ RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston_spdf_prj, boston_kd1_wm)
summary(boston_lag)

##	Re-run our tests for spatial autocorrelation and heteroskedasticity:
resids <- residuals(boston_lag)
moran.test(resids, boston_kd1_wm)
bptest.Sarlm(boston_lag)

##	Moran test suggests that we've successfully removed the spatial 
##		autocorrelation in our model estimates, however the BP test
##		indicates that there is remaining heteroskedasticity in the 
##		residuals, most likely due to model mis-specification...

##	Save the model predictions and residuals into our 
##		spatial dataframe:
boston_spdf_prj$lagpred <- predict(boston_lag)
boston_spdf_prj$lagresid <- residuals(boston_lag)

##-------------------------------------------------------------------------



##-------------------------------------------------------------------------
## 6. Summarize the output from our regression approaches

##	Plot a comparison of our spatial lag regression predictions and our
##		OLS predictions vs. the observed values along with a 1:1 line:
plot(boston_spdf_prj$LOGCMEDV, predict(boston_lag))
abline(a=0, b=1, lty=2, col="grey")
points(boston_spdf_prj$LOGCMEDV, predict(boston_lm), col="red", pch=20, cex=0.75)


##	QUESTION 6:	 Save this plot off as a PDF.
##	(5 points)



##	Let's plot out the Local Moran's I estimates of autocorrelation for
##		each observation point:

## Local Autocorrelation: Local Moran's I (normality assumption)
localmoran3 <- localmoran(residuals(boston_lag), listw=boston_kd1_wm)

boston_spdf_prj$lagmoranz <- abs(localmoran3[,4]) ## Extract z-scores


##	QUESTION 7:	 Just as you did with the local Moran's I estimates, map
##		the local Moran's I estimates on the residuals from the lm() model
##		and the spatial lag regression on side-by-side lattice plots in
##		order to visualize the reduction in spatial autocorrelation in 
##		the model errors.
##	(15 points)

##-------------------------------------------------------------------------

##
###########################################################################



###########################################################################
##
##	Data Saving and Clean-up 
##
###########################################################################

##	Using spplot() compare the residuals from the OLS regression and the 
##		spatial lag regression:

##	Construct north arrow and scale bar objects  for plotting in the 
##		spatial plot:
l1 <- list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(440000, 150000), scale = 4000)
l2 <- list("SpatialPolygonsRescale", layout.scale.bar(height=0.2), offset = c(438000, 148000), scale = 5000, fill=c("transparent","black"))
l3 <- list("sp.text", c(438000, 147000), "0", cex=0.7)
l4 <- list("sp.text", c(445000, 147000), "5000 m", cex=0.7)

##	Plot the data along with the other layout objects, specified in the 
##		sp.layout argument:
spplot(as(boston_spdf_prj, "Spatial")[c("lmresid", "lagresid")], sp.layout=list(l1, l2, l3, l4), col.regions=brewer.pal(9, "RdBu"), alpha=0.75, cuts=seq(-1, 1, by=0.25), scales=list(draw = TRUE))


##	QUESTION 8:	 Construct a plot of all three sets of residuals, OLS, 
##		spatial error regression, and spatial lag regression, but change
##		the color ramp from red-blue to some other scheme.
##		(10 points)


##	QUESTION 9:	Make sure your working directory is set and save your
##		boston_spdf_prj to an ESRI Shapefile for further use inside ArcGIS.
##		(10 points)

##		(Total: 100 points)

############################ End of file ##################################

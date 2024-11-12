##################################################
#######  Spatial Interpolation Methods  ##########
##################################################

library(sp)
library(sf)
#library(maptools)
library(gstat)
library(fields)
library(rasterVis)
library(classInt)
#library(SDMTools)
library(RColorBrewer)
library(raster)
library(plotfunctions)

###############################################START#########################################


# Read in prcp data
  # precip data for passage of rain in maria
prdata <- read.csv(file= "Lab 8 - Spatial Data Analyses/Interpolation Tutorial 2024/data/Maria Precip.csv", header=T)



#goal:
  # max rainfal of hurricane maria through out whole surface


# Set coordinates and thereby create spatial data object
  #creation of spatial points
coordinates(prdata) <- cbind(prdata$Long, prdata$Lat)


# Set mapping projection for spatial data object
  # http://spatialreference.org/
proj4string(prdata) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# Transform into projected coordinate system
prdata <- spTransform(prdata, CRS("+proj=utm +zone=20 +ellps=clrk66 +towgs84=11,72,-101,0,0,0,0 +units=m +no_defs "))
  #better to use EPSG

# Read in US state shapefile, select to PR, assign lat/long coord system
US <- st_read("Lab 8 - Spatial Data Analyses/Interpolation Tutorial 2024/data/cb_2017_us_state_5m.shp")

PR <- US[US$NAME=="Puerto Rico",]

st_transform(PR, 4326)

# Transform into projected coordinate system
  #PROJECTION OF ISLAND OF PUERTO RICO
PR <- st_transform(PR, 3920)

# Write out shapefile of PR
#st_write(PR,"/Volumes/KINGSTON UF/KINGSTON COPY/R Class/Lectures/Interpolation Tutorial 2023/data/PR Shapefile/PRBoundary.shp",layer="PRBoundary","ESRI Shapefile")



########################################
#### Prep for Spatial Interpolation ####
########################################

# YOU NEED TO HAVE A GRID TO PROJECT INTO AND DEFINE GRID TO DEFINE THAT RESOLUTION
  # you can do that by making st_make_grid
  # you need to make centers otherwise will default to polygons because you want points to interpolate into 
  
### Create prediction grid
# Create data frame of center points at a 500m x 500m resolution
grdpts <- st_make_grid(PR, cellsize = c(500, 500), what="centers")
# the PR is defining bounding box


# Transform the grid points into spatial points
spgrd <- as_Spatial(grdpts)
  #making our points into centroids 


# Convert to spatial pixels (spatial grid) object, selecting only points that fall within PR polygon
  # the grid is a box sized of PR this just shapes box into PR
PR <- as_Spatial(PR)
spgrdPR <- SpatialPixels(spgrd[PR,])


set.seed(42)

### Create training and test datasets for validation to interpolate across space

i <- sample(nrow(prdata),round(nrow(prdata)*20/100)) #exclude 20% of the data
training <- prdata[-i,]
test <- prdata[i,]


####################################
#### Inverse Distance Weighting ####
####################################


#### Perform Inverse Distance Weighting spatial interpolation on training sample
  #inverse distance weight in idw 
  #tilda one because we didnt add covariates in

idwPR <- idw(PRCP~1, training, spgrdPR) #can specify IDW power with "idp=" argument

# Plot the idw surface
brkpts <- c(100, 200, 300, 400, 500, 600, 700)

#png("/Volumes/KINGSTON/R Class/Lectures/Interpolation Tutorial/output/PR_PRCP_MARIA_IDW.png",6000,3000,res=600)

par(bg="white", mar=c(0,0,0,0))
image.plot(idwPR, zlim=c(0,800), axes=FALSE,xlab="", ylab="", graphics.reset=TRUE, horizontal=TRUE, 
	legend.shrink=0.3, col=brewer.pal('Blues', n=6), legend.lab="mm", breaks=brkpts)
#dev.off()

# Alternative way to plot
idwPR_raster <- raster(idwPR)
myTheme = rasterTheme(region=brewer.pal('Blues', n=6))
par(bg="white", mar=c(0,0,0,0), omi=c(0,0,0,0))
levelplot(idwPR_raster, par.settings=myTheme)
#dev.print(pdf, file = "/Volumes/KINGSTON/R Class/Lectures/Interpolation Tutorial/output/PR_PRCP_MARIA_IDW.pdf", width=6, height=4)
#dev.off()

### Assess interpolation ###

# Create spatial points of test dataset
sp.pt <- SpatialPoints(cbind(test$Long,test$Lat))

# Set mapping projection for spatial points
proj4string(sp.pt) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# Transform into projected coordinate system
sp.pt <- spTransform(sp.pt, CRS("+proj=utm +zone=20 +ellps=clrk66 +towgs84=11,72,-101,0,0,0,0 +units=m +no_defs "))

# Extract spatial test points from IDW raster
  #extracting them on interpolated surface and real point using extract value at that point
testidw <- extract(idwPR_raster, sp.pt)

### Calculate measures of fit ####

# Pearson's R Squared (square of Pearson correlation coefficient)
RSQR <- as.numeric(cor.test(test$PRCP, testidw)$estimate)^2
# Root Mean Square Deviation
RMSD <- sqrt(sum((test$PRCP-testidw)^2)/length(test$PRCP)) 
##  MAPE
MAPE <- mean(abs((test$PRCP-testidw)/test$PRCP)*100)



### Visualize error of IDW interpolation
# Take difference between test dataset and interpolated IDW surface
diff <- as.data.frame(abs(test$PRCP-testidw))


# Define same color palette as IDW surface and error color ramp
plotclr <- brewer.pal('Blues', n=6) 
nclr <- 5
errclr <- brewer.pal('Reds', n=5)

# Define class intervals for difference
errclass <- classIntervals(diff[,1], nclr, style="pretty")

# Assign R color codes to each class
errcolcode <- findColours(errclass, errclr)

# Assign coordinates to difference
coordinates(diff) <- coordinates(sp.pt)
# Add test points to IDW map, colored by interval
brkpts <- c(100, 200, 300, 400, 500, 600, 700)
plot(idwPR_raster, col=plotclr, breaks=brkpts)
points(diff, pch = 16, col= errcolcode)
# Add error legend to map
# Set points (boundary box) for placing legend on map
pnts = c(200000, 1960000, 230000, 1980000)
# Add legend with labels
gradientLegend(c(0,50), errclr, pos=pnts, coords=TRUE, labels="error")
text(215000, 1985000, "error")
# Save map as PDF
#par(bg="white", mar=c(0,0,0,0), omi=c(0,0,0,0))
#dev.print(pdf, file = "/Volumes/KINGSTON/R Class/Lectures/Interpolation Tutorial/output/PR_PRCP_MARIA_IDW_ERROR.pdf", width=10, height=4)
#dev.off()



##########################
#### Ordinary Kriging ####
##########################


### Perform Ordinary Kriging
# Variogram cloud
#PRCP is precipitation

v.prcp.c <- variogram(PRCP ~ 1, data=training, cloud=T) #specify "cutoff=" to limit spatial distance between point pairs 
plot(v.prcp.c)


# SemiVariogram
v.prcp <- variogram(PRCP  ~ 1, data=training)
plot(v.prcp)



# Select point pairs that have high/low spatial correlation
# will create cross hairs and click and once you make box and click esc will stop digitizing
#sel <- plot( variogram(PRCP ~ 1, training, cloud=TRUE), digitize=TRUE)

#plot(sel, training)

# Construct variogram model
mod <- vgm(c("Exp", "Mat", "Gau", "Sph"))
mod_reml <- vgm(psill=var(training$PRCP), model="Gau", range=sqrt(areaSpatialGrid(spgrdPR))/4, nugget=0)

# Fit variogram model, can choose between Restricted Maximum Likelihood (REML) and Ordinary Least Square (OLS) algorithms
fit_reml <- fit.variogram.reml(PRCP ~ 1, data=training, model = mod_reml)
plot(variogram(PRCP ~ 1, data=training), fit_reml, main="REML Model") 

# fit variogram model, ordianry least squares regression
fit_ols <- fit.variogram(variogram(PRCP ~ 1, data=training), model=mod)
plot(variogram(PRCP ~ 1, data=training), fit_ols, main="OLS Model")


# Krige using better variogram model with preferred model
  #prefered model is OLS

map <- krige(PRCP ~ 1, locations=training, model=fit_ols, newdata=spgrdPR)
spplot(map, "var1.pred", col.regions=brewer.pal('Blues', n=6), cuts=5, at=brkpts)


# Validation with test subset
krigtest <- krige(PRCP ~ 1, locations=training, model=fit_ols, newdata=test)
plot(test$PRCP, krigtest$var1.pred, asp=1, xlab="Observed", ylab="Predicted", xlim=c(100,600), ylim=c(100,600))
abline(0,1,col="red",cex=0.5)


# Difference between predictions and test values
diff <- krigtest$var1.pred - test$PRCP
summary(diff)
# RMSE (precision)
sqrt(sum(diff^2)/length(diff))
# Mean Error (bias)
sum(diff)/length(diff)


##########################
####### Co Kriging #######
##########################
# lets see if elevation makes model better  
# Read in PR DEM
PRDEM <- raster("Lab 8 - Spatial Data Analyses/Interpolation Tutorial 2024/data/DEM/PR_Elevation_Pro.tif")

# Transform into projected coordinate system
PRDEM <- projectRaster(PRDEM, crs="+proj=utm +zone=20 +ellps=clrk66 +towgs84=11,72,-101,0,0,0,0 +units=m +no_defs ")
#writeRaster(PRDEM, filename="/Volumes/KINGSTON/R Class/Lectures/Interpolation/data/DEM/#PR_Elevation_Pro_Proj.tif", overwrite=TRUE) 


# For Co-kriging we need to obtain the value of the covariate for each observation
  #pulling out elevation for all training points used to do the krigging 
elev <- extract(PRDEM,training)
training@data <- data.frame(training@data, elev)

# Exploratory plot of precip vs. elevation
plot(PRCP ~ elev, data=training@data)
# Pearson's product-moment correlation
cor(training@data$PRCP, elev)
cor.test(training@data$PRCP, elev)

# Explore semivariogram of elevation
  # before merging vars we need to check co-vars 
v.elev <- variogram(elev ~ 1, training)
plot(v.elev)

# Fit variogram model
mod <- vgm(c("Exp", "Mat", "Gau", "Sph"))
fit_ols <- fit.variogram(variogram(elev ~ 1, data=training), model=mod)
plot(v.elev, pl=T, model=fit_ols, main="OLS Model")

# Try REML fit
m.elev <- vgm(psill=var(training$elev), model="Sph", range=sqrt(areaSpatialGrid(spgrdPR))/4, nugget=0)
fit_reml <- fit.variogram.reml(elev ~ 1, data=training, model = m.elev) 
plot(variogram(elev ~ 1, data=training), fit_reml, main="REML Model") 

# Create an object with the function gstat() that contains both the variable and the covariate
  #making e object and writing over it to combine precip and elevation
e <- gstat(id="PRCP", formula=PRCP~1, data=training)
e <- gstat(e, id="elev", formula=elev~1, data=training)

# Plot the 2 direct variograms and cross-variogram
vg <- variogram(e)
plot(vg)

# Fit linear model of co-regionalization
e_sph <- fit.lmc(vg,e,model=vgm(psill=cov(training$PRCP,training$elev),model="Sph",range=sqrt(areaSpatialGrid(spgrdPR))/4,nugget=0), correct.diagonal=1.01)
plot(vg, e_sph$model)

e_gau <- fit.lmc(vg,e,model=vgm(psill=cov(training$PRCP,training$elev),model="Gau",range=sqrt(areaSpatialGrid(spgrdPR))/4,nugget=0), correct.diagonal=1.01)
plot(vg, e_gau$model)

e_mat <- fit.lmc(vg,e,model=vgm(psill=cov(training$PRCP,training$elev),model="Mat",range=sqrt(areaSpatialGrid(spgrdPR))/4,nugget=0), correct.diagonal=1.01)
plot(vg, e_mat$model)

#try all of them and sees what lowers your error




# Predict PRCP over prediction grid using lmc
k <- predict(e_gau, spgrdPR)

# Summarize predictions and errors
summary(k$PRCP.pred)
summary(k$PRCP.var)


# Plot prediction map
#png("/Volumes/KINGSTON/R Class/Lectures/Interpolation Tutorial/output/PR_CK_DEM.png",2200,1200,res=300)
spplot(k,"PRCP.pred",col.regions=terrain.colors(50),main="Prediction Map",scales=list(draw=T))
#dev.off()

# Plot error map
#png("/Volumes/KINGSTON/R Class/Lectures/Interpolation Tutorial/output/PR_CK_DEM_Error.png",2200,1200,res=300)
spplot(k,"PRCP.var",col.regions=heat.colors(50),main="Error Map",scales=list(draw=T))
#dev.off()

### Validation with test subset
krige_cross <- predict(e_gau, test)
str(krige_cross)

### Goodness of fit indexes
diff <- krige_cross@data$PRCP.pred - test$PRCP
summary(diff)
# RMS error (precision)
sqrt(sum(diff^2)/length(diff))   
# mean error (bias)
sum(diff)/length(diff)              
# Pearson's R Squared
RSQR <- as.numeric(cor.test(test$PRCP, krige_cross@data$PRCP.pred)$estimate)^2  						
# Root Mean Square Deviation
RMSD <- sqrt(sum((test$PRCP - krige_cross@data$PRCP.pred)^2)/length(test$PRCP))  
# Plot of observed vs predicted
plot(test$PRCP, krige_cross@data$PRCP.pred,asp=1, xlab="Observed", ylab="Predicted", xlim=c(100,600), ylim=c(100,600))
abline(0,1,col="red",cex=0.5)

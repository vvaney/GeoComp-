## Laboratory 1
##	Intro to Geostatistics:
##	David Keellings
##  Adapted From:
##  Forrest R. Stevens




##########
## Install and load required packages:

##	The "foreign" library is required for the read/write of files
##		like DBF, CSV and Excel:
library(foreign)

##	The "ape" package provides us with several spatial statistics
##		tools, including Moran.I, useful for testing for spatial
##		autocorrelation in regression residuals:
#install.packages("ape")         
library(ape)

##	The "pgirmess" library gives us corellogram functions, useful
##		for looking at spatial autocorrelation over various enforced
##		distances:
#install.packages("pgirmess")
library(pgirmess)

##	The "sp" library is critical for loading and manipulating a
##		variety of spatial data formats:
#install.packages("sp")
library(sp)

##	The "lattice" package gives us access to several advanced plot
##		styles and visualization techniques:
library(lattice)

##	The "rgdal" package gives us access to spatial tools for reading, 
##		writing, and projecting spatial data including shapefiles:
library(rgdal)

library(sf)

##########



##########
##	Load and prepare required data:

##	Change the working directory to the folder where we know our
##		data of interest to be located and where we want output 
##		to be saved by default:
setwd("/Volumes/KINGSTON UF/KINGSTON COPY/R Class/IntroR /Lab 1 - Intro to Geostatistics/data")

## TIPS
##  On Mac right-click on folder then hold down option to copy path
##  On Windows can either copy file path from windows explorer address bar or 
##  hold the shift key and right click on the folder to get path, to flip slashes use
##  path <- readClipboard()



## Read in sample shapefile.shp ArcGIS shapefile,
##		look at structure, and plot
sample <- st_read("sample.shp")
str(sample)
plot(sample)
plot(sample['response'])

##	Use the read.dbf() function from the "foreign" library to load
##		the dbf file from the sample.shp ArcGIS shapefile:
sample_data <- read.dbf(file="sample.dbf")
##########



##########
##	Explore and do sample statistics:

##	Explore the contents of sample_data:
head(sample_data)
sample_data
summary(sample_data)
class(sample_data)
class(sample_data$x)


##	Attach the sample_data data.frame, which gives us access and 
##		creates object names from the column names of each column
##		from the data.frame object:
attach(sample_data)

response
hist(response)
plot(x=x, y=response)
plot(x=y, y=response)
plot(x=covariate, y=response)
pairs(data.frame(x,y,covariate,response))


##	Using the linear model function, we are going to regress our
##		response variable on our covariate:
model1 <- lm(response ~ covariate)
model1
summary(model1)


##	Now let's test for spatial autocorrelation in our regression
##		residuals:
resid1 = resid(model1)

## We must generate a distance matrix, then take the inverse of 
##		the matrix values and replace the diagonal entries with zero:
dists = as.matrix(dist(cbind(x, y)))

dists_inv <- 1 / dists
diag(dists_inv) <- 0
	
dists_inv[1:5, 1:5]
Moran.I(response, dists_inv)
Moran.I(resid1, dists_inv)

##	Since there's statistical evidence for spatial autocorrelation
##		in our original model, let's try including lat/long in the
##		model covariates:
model2 <- lm(response ~ covariate + x + y)
model2
summary(model2)

##	And test for spatial autocorrelation again:
Moran.I(resid(model2), dists_inv)


##	Let's plot a correlogram of our response to see whether we can
##		detect local spatial autocorrelation in our response, such
##		that we might have expected the problem in model1's residuals:
coords <- data.frame("x"=x, "y"=y)
correlogram <- correlog(coords, response)
plot(correlogram)


##	We are going to create a SpatialPointsDataFrame in order to use
##		R's tools for plotting and analyzing this one type of spatial
##		data:
sp_data <- SpatialPointsDataFrame(coords, data = sample_data)
spplot(sp_data, "response")

sp_data$resid1 <- resid(model1)
sp_data$resid2 <- resid(model2)

bubble(sp_data, "resid1")
bubble(sp_data, "resid2")

##########



##########
##	Write data and clean-up:

##	Let's write the residuals back to our ArcGIS shapefile attribute
##		table so we can continue to analyze the results in our GIS of
##		choice:
sample_data$resid1 = resid(model1)
sample_data$resid2 = resid(model2)

write.dbf(sample_data, file="sample.dbf")

#####

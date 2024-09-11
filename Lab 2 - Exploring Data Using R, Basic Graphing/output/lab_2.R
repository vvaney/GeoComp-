##################################
# Name: Valentina Rollemberg Vaney
# Date :9/11/2024
###################################
library(dplyr)
library(lctools)
library(sp)



gro <- read.csv("Lab 2 - Exploring Data Using R, Basic Graphing/data/growth.csv",stringsAsFactors = T)
nitro <-  read.csv("Lab 2 - Exploring Data Using R, Basic Graphing/data/nitrogen.csv",stringsAsFactors = T)
station_id <- read.csv("Lab 2 - Exploring Data Using R, Basic Graphing/data/station_id.csv")


#------------------------------------------

head(gro)
class(gro)
str(gro)
summary(gro)



both <- merge(gro,nitro, by = c("station_id","timestamp"))

all_data <- merge(both, station_id, by = c("station_id"))


# shows where NAs are 
all_data[is.na(all_data$x) | is.na(all_data$y),]

plot(x=all_data$x, y=all_data$y)
text(x=all_data$x, y=all_data$y, labels=all_data$station_id, cex=0.6, pos=4)

#editing the the df manually like an animal
all_data$x <- edit(all_data$x)
all_data$y <- edit(all_data$y)


#replacing all NA's with missing coords

all_data$x[is.na(all_data$x)] <- 5
all_data$y[is.na(all_data$y)] <- 1

all_data[is.na(all_data$x) | is.na(all_data$y),]

#### deleting obs #####
ls()
rm(gro)
rm(nitro)
ls()


# attach makes everything into an object

attach(all_data)
growth[1:10]

detach(all_data)
rm(station_id)
attach(all_data)



new_timestamp <- as.character(all_data$timestamp)
head(new_timestamp)
new_timestamp <- as.POSIXct(new_timestamp, format="%m/%d/%Y %H:%M", tz="GMT")


plot(x=new_timestamp, y=growth)

#converting time stamp to data frame

all_data$timestamp = new_timestamp
pairs(all_data)

#saving our work
write.csv(all_data, file="Lab 2 - Exploring Data Using R, Basic Graphing/output/all_data.csv")
save(all_data, file="Lab 2 - Exploring Data Using R, Basic Graphing/output/all_data.RData")
save.image(file="Lab 2 - Exploring Data Using R, Basic Graphing/output/lab2.Rdata")


#---------------------------------------------------------------------------

#looking at spatial correlation via timestamps

#converting df into sf
coords <- cbind(all_data$x, all_data$y)
alldata_spdf <- SpatialPointsDataFrame( coords=coords, data=all_data)


#We can then select just the last time stamp:
lastob <- alldata_spdf[alldata_spdf$timestamp==alldata_spdf$timestamp[2400],]


# We can visualize nitrogen in the last time stamped observation:
lm.palette <- colorRampPalette(c("green", "yellow", "orange", "red"), space = "rgb")

spplot(lastob, zcol="nitrogen", col.regions=lm.palette(5), main="Dissolved Organic Nitrogen")

lmoran <- l.moransI(coords, 8, lastob$growth)

# this looks at clusters 
lastob$lmorancluster <- lmoran$Cluster

spplot(lastob, zcol="lmorancluster", col.regions=c("grey", "red", "blue", "green", "orange"), main="Local Moran’s I Clusters", cuts=c(0,1,2,3,4,5))

#calculating the morans for the last one 


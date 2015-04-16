# Code used to prepare the example sparrow datasets in Rdistance
# Jason Carlisle
# Last updated 4/16/2015

# See latest updates at bottom.

# This code calculates a bunch of covariates that we're not using at this time.  The only one
# included in the package's example data is a categorical covariate of sagebrush cover (low or high)

# Load required packages
#require(unmarked)
require(sp)
require(rgdal)
require(rgeos)
require(raster)
require(RODBC)




# Connect to SQL Server database on desktop PC
chan <- odbcDriverConnect(connection='driver={SQL Server Native Client 11.0};
                server=CARLISLEMULE,1433; database=LineTransectData;
                          Uid=jasontemp; Pwd=temppassword', believeNRows=FALSE)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# PART 1:  PREPARE RAW BIRD COUNT DATA ------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# read in dataframe of observations on transects that don't have "Z" in the transect ID
# We surveyed 8 "Z" transects which were sited differently and are not being analyzed here
birds <- sqlQuery(chan, query="SELECT * FROM [LineTransectData].[dbo].[Avian_Data] WHERE TranID NOT LIKE 'Z%'")
birds <- birds[ , -10]  # drop comments columns
birds <- birds[order(birds$TranID), ]  # order by transect ID
#birds <- data.frame(birds, row.names=NULL)  # drop row.names

# first, calculate the perpendicular distance of the bird to the transect line based on
# distance to observer and bearing from line observed
# x=r*sin(theta), where x is the distance of the bird perpendicular to the line
# r is the observed distance of the bird from the observer, and theta is the sighting
# angle, or bearing from the line (0-90)
birds$PerpDist <- round(sight2perpdist(sightdist=birds$SightDist, sightangle=birds$SightAngle), digits=1)

# now select the species of interest
#brsp <- subset(birds, subset=Spp=="BRSP", select=c(TranID, PerpDist, ObsType, Number))
#hola <- subset(birds, subset=Spp=="HOLA", select=c(PerpDist, TranID))
#sath <- subset(birds, subset=Spp=="SATH", select=c(PerpDist, TranID))
#vesp <- subset(birds, subset=Spp=="VESP", select=c(PerpDist, TranID))
#sags <- subset(birds, subset=Spp=="SAGS", select=c(PerpDist, TranID))
#weme <- subset(birds, subset=Spp=="WEME", select=c(PerpDist, TranID))


# plot histogram
#hist(brsp$PerpDist, breaks=20, col="dark gray", main="BRSP Distances")
#hist(hola$PerpDist, breaks=20)
#hist(sath$PerpDist, breaks=20)
#hist(vesp$PerpDist, breaks=20)
#hist(sags$PerpDist, breaks=20)
#hist(weme$PerpDist, breaks=20)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# PART 2:  PREPARE TRANSECT-LEVEL COVARIATES ------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Read in covariate data collected while surveying
effort <- sqlQuery(chan, query="SELECT * FROM [LineTransectData].[dbo].[Avian_Effort] WHERE TranID NOT LIKE 'Z%'")
effort <- effort[order(effort$TranID), ]  # order by transect ID
#effort <- data.frame(effort, row.names=NULL)  # drop row.names
#odbcClose(chan)  # close the connection to the SQL Database


# Read in transect locations
trans <- readOGR(dsn="C:/Users/jcarlis3/Box Sync/GIS/Field_GIS_Data/x2012", layer="Transects_atts_wylam")
length(trans)
trans@data[ncol(trans@data)] <- NULL  # get rid of last column with redundant info
trans@data  # look at attribute table

# drop 8 transects that start with "Z"
trans@data[grepl(pattern="Z", x=trans@data$TransectID), ]  # find row numbers
trans <- trans[-c(9, 10, 23, 28, 57, 62, 63, 64), ]
trans@data[grepl(pattern="Z", x=trans@data$TransectID), ]  # check that "Z"s gone
length(trans)  # should now be 72


# Find all .img raster files in the raster directory that were processed to have masked values==0
rastdir <- "F:/umbrella_GIS/LandcoverData/usgs_wyo_sagebrush_jan2012/ProcessedRasters"
dlist <- dir(path=rastdir, pattern="0s.img$")
(dlist <- dlist[c(1, 4, 5, 7, 6, 8)])  # keep only the ones I'm interested in

# Loop through the list and read in the rasters
rlist<- list()
for(i in 1:length(dlist)){
    rlist<- c(rlist, raster(paste(rastdir, dlist[i], sep="/")))
}

# Stack the rasters. They must have the exact same properties (proj, origin, res, extent)
rs<- stack(rlist)
names(rs)  # Double check all rasters of interest made it into the stack
#plot(rs)

# Crop down to a 5km buffer from transects
rs <- crop(x=rs, y=gBuffer(trans, width=5000))
plot(rs)


# Create a 100m buffer around each transect line
polys <- gBuffer(trans, width=100, capStyle="ROUND", byid=TRUE)
plot(polys[1:4, ]); plot(trans, add=TRUE)  # plot a few to see

# Take the mean of each raster within the buffered polygon of each line
xmean <- extract(x=rs, y=polys, fun=mean, df=TRUE)  # mean
xsd <- extract(x=rs, y=polys, fun=sd, df=TRUE)  # standard deviation
xmax <- extract(x=rs, y=polys, fun=max, df=TRUE)  # maximum
xmin <- extract(x=rs, y=polys, fun=min, df=TRUE)  # minimum

# Remove temporary raster files stored on hard drive
showTmpFiles()
removeTmpFiles(h=0.01)

# Give columns meaningful names and append to one data.frame
names(xmean)[2:7] <- c("BareMean", "HerbMean", "LitterMean", "ShrubMean", "SageMean", "HeightMean")
names(xsd)[2:7] <- c("BareSD", "HerbSD", "LitterSD", "ShrubSD", "SageSD", "HeightSD")
names(xmax)[2:7] <- c("BareMax", "HerbMax", "LitterMax", "ShrubMax", "SageMax", "HeightMax")
names(xmin)[2:7] <- c("BareMin", "HerbMin", "LitterMin", "ShrubMin", "SageMin", "HeightMin")
xdf <- cbind(xmean, xsd, xmax, xmin)  # append
xdf <- xdf[ , -c(8, 15, 22)]  # remove redundant ID columns
rm(xmean, xsd, xmax, xmin)  # tidy up workspace


# Append to data from shapefile attribute table to make covariate data.frame
sdat <- trans@data
sdat <- sdat[order(sdat$TransectID), ]  # order by transect ID
covs <- data.frame(cbind(sdat, xdf), row.names=NULL)  # append
covs$ID <- NULL  # remove ID field after ensuring match with TranID order
rm(sdat, xdf)  # tidy up workspace


# Make sure data types are correct
covs$Tier <- as.factor(covs$Tier)  # change to factor
covs$Core <- as.factor(covs$Core)  # change to factor
covs <- droplevels(covs)  # drops unused factor levels in TranID






# Grouse fecal pellet data
# Calculate the number of clusters per transect
p1 <- sqlQuery(chan, query="SELECT TranID, COUNT(NULLIF(Fecal, 0)) FROM Grouse_Data WHERE TranID NOT LIKE 'Z%' GROUP BY TranID")

# Calculate the number of pellets per transect
p2 <- sqlQuery(chan, query="SELECT TranID, SUM(Fecal) FROM Grouse_Data WHERE TranID NOT LIKE 'Z%' GROUP BY TranID")

# Create data.frame of poop data
poop <- data.frame(p1[1], p1[2], p2[2])
names(poop) <- c("TranID", "Clusters", "Pellets")

# Calculate the average cluster size (number of pellets) per transect
poop$ClusterMean <- round(poop$Pellets / poop$Clusters, digits=1)
poop$ClusterMean[poop$Clusters==0] <- 0  # replace NaN values with 0


# Append poop data to other covariates
covs <- cbind(covs, poop)
covs$TransectID == covs$TranID  # check rows match up
covs <- covs[c(28, 2:27, 29:31)]



# Code Doherty Tier as dummy variables
# Tier 0 will be baseline, initialize other columns
covs$T1 <- 0
covs$T2 <- 0
covs$T3 <- 0
covs$T4 <- 0

covs[covs$Tier==1, "T1"] <- 1
covs[covs$Tier==2, "T2"] <- 1
covs[covs$Tier==3, "T3"] <- 1
covs[covs$Tier==4, "T4"] <- 1





# Keep only BRSP records
brsp <- birds[birds$Spp=="BRSP", ]


brsp <- brsp[, c(1, 8, 5,6 )]





covs <- covs[, c("TranID", "SageMean", "Clusters")]


# Add binary presence of grouse pellets
covs$Grouse <- 0
covs[covs$Clusters > 0, "Grouse"] <- 1
nrow(covs[covs$Clusters > 0, ])


hist(covs$SageMean)
summary(covs$SageMean)


# Add categorical sagebrush cover (low being 10% or less)
covs$SageClass <- "High"
covs[covs$SageMean <= 10, "SageClass"] <- "Low"
covs <- covs[, c(1, 5, 2, 4)]

# Keep continuous sagebrush cover (round and rename)
covs$SageMean <- round(covs$SageMean, 1)
names(covs)[3] <- "SageCont"

# Check some summary stats
summary(covs[covs$SageClass=="Low", ])
summary(covs[covs$SageClass=="High", ])

sparrows.counts <- data.frame(brsp, row.names=NULL)
sparrows.covs <- data.frame(covs, row.names=NULL)




# Save as csv (for my reference) and as rda (for package data)
write.csv(sparrows.counts, "C:/Users/jcarlis3/Box Sync/Classes/s8_Spr_15/DistanceSampling/ExampleVignette/sparrows.counts.csv", row.names=FALSE)
write.csv(sparrows.covs, "C:/Users/jcarlis3/Box Sync/Classes/s8_Spr_15/DistanceSampling/ExampleVignette/sparrows.covs.csv", row.names=FALSE)

save(sparrows.counts, sparrows.covs, file="C:/Users/jcarlis3/Box Sync/Classes/s8_Spr_15/DistanceSampling/ExampleVignette/sparrows.rda")

#Note, in the example vignette, NAs will be added in the counts file for transects where no BRSPs were observed
#nobrsp <- c("B2", "D1", "D2", "D4", "K1", "L3", "N2", "P3", "Q3", "Q4")


load("C:/Users/jcarlis3/Box Sync/Classes/s8_Spr_15/DistanceSampling/ExampleVignette/sparrows.rda")




# Update 2/23/15 ----------------------------------------------------------

# In order to streamline NA-handling in RDistance, Trent and I decided to use empty factor levels to represent
# sites where no detections were made (instead of using NAs in the vector of distances etc.)

# load in the data file prepped previously (without having to re-run the code above)
load("C:/R_Code/Rdistance/Rdistance/data/sparrows.rda")

# drop the continuous sage and grouse covariates -- just stick with SageClass for now
sparrows.covs <- sparrows.covs[1:2]


# drop the empty factor levels for site in counts
# (to replicate data just entered for tutorial's sake, the empty levels will be added or dealt with in the tutorial)
?droplevels
sparrows.counts <- droplevels(sparrows.counts)

save(sparrows.counts, sparrows.covs, file="C:/R_Code/Rdistance/Rdistance/data/sparrows.rda")


# Update 2/26/2015 --------------------------------------------------------
# Renaming some columns and adding transect lengths

# Goal for two required two data.frames:
# a distance data.frame with at least $distance, $siteID, and $groupsize
# a site-covariate data.frame with at least $siteID and $length

# load in the data file prepped previously (without having to re-run the code above)
load("C:/R_Code/Rdistance/Rdistance/data/sparrows.rda")

# rename
dists.df <- sparrows.counts
covs.df <- sparrows.covs
rm(sparrows.counts, sparrows.covs)

# add length, rename, and rearrange
covs.df$length <- 500
names(covs.df)[1:2] <- c("siteID", "sage")
covs.df <- covs.df[c(1, 3, 2)]

# rename
names(dists.df) <- c("siteID", "groupsize", "sightdist", "sightangle")

# save
save(dists.df, covs.df, file="C:/R_Code/Rdistance/Rdistance/data/sparrows.rda")



# Update 3/30/2015 ----
# Change names of data.frames and save as two objects, not one

# load in the data file prepped previously (without having to re-run the code above)
load("C:/R_Code/Rdistance/Rdistance/data/sparrows.rda")

sparrow.dists <- dists.df
sparrow.covs <- covs.df

# save
save(sparrow.dists, file="C:/R_Code/Rdistance/Rdistance/data/sparrow.dists.rda")
save(sparrow.covs, file="C:/R_Code/Rdistance/Rdistance/data/sparrow.covs.rda")


# make the sage covariate a factor
load("C:/R_Code/Rdistance/Rdistance/data/sparrow.covs.rda")
sparrow.covs$sage <- as.factor(sparrow.covs$sage)
save(sparrow.covs, file="C:/R_Code/Rdistance/Rdistance/data/sparrow.covs.rda")

# Update 4/16/2015 for version 1.2.2 ----
# load in the data file prepped previously (without having to re-run the code above)
load("C:/R_Code/Rdistance/Rdistance/data/sparrow.dists.rda")
load("C:/R_Code/Rdistance/Rdistance/data/sparrow.covs.rda")

sparrow.detections <- sparrow.dists
sparrow.transects <- sparrow.covs
rm(sparrow.dists, sparrow.covs)

save(sparrow.detections, file="C:/R_Code/Rdistance/Rdistance/data/sparrow.detections.rda")
save(sparrow.transects, file="C:/R_Code/Rdistance/Rdistance/data/sparrow.transects.rda")

# Code used to prepare the example songbird datasets in Rdistance
# Jason Carlisle
# Last updated 6/19/2017


# Creating two example datasets, each will include two tables:
# 1)  Line transect example (BRSP)
      # a detection data.frame with at least $dist, $siteID, and $groupsize
      # a site data.frame with at least $siteID and $length (and we're adding some other covariates)
# 2)  Point transect example (SATH)
      # a detection data.frame with at least $dist, $siteID, and $groupsize
      # a site data.frame with at least $siteID (and we're adding some other covariates)


# Load required packages
# require(Rdistance)  # build from development
require(sp)
require(rgdal)
require(rgeos)
require(raster)
require(RODBC)
require(lubridate)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# PART 1:  PREPARE THE DETECTIONS DATASETS (RAW BIRD COUNT DATA) -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Data sources
# Connect to SQL Server database on CarlisleMule Desktop
# chan <- odbcDriverConnect(connection='driver={SQL Server Native Client 11.0}; server=CARLISLEMULE,1433;
#                           database=LineTransectData; Uid=jasontemp; Pwd=temppassword', believeNRows=FALSE)

# Path to Access databases
path <- "C:/Users/jcarlisle/Google Drive/Dissertation/Data/"

tchan <- odbcConnectAccess2007(access.file=paste0(path, "Umbrella_MasterData_LineTransect_25May2017.accdb"))

pchan  <- odbcConnectAccess2007(access.file=paste0(path, "Umbrella_MasterData_PointCount_25May2017.accdb"))



# Read in dataframe of observations on transects that don't have "Z" in the transect ID
# We surveyed 8 "Z" transects which were sited differently and are not being analyzed here
dB <- sqlQuery(tchan, query="SELECT * FROM [Avian_Data] WHERE TranID NOT LIKE 'Z%'")
dB$Comments <- NULL  # drop comments columns
dB <- dB[order(dB$TranID), ]  # order by transect ID


# Keep only Brewer's Sparrow data from 2012
dB <- dB[dB$Spp=="BRSP" & dB$Year==2012, ]


# Keep only necessary columns and rename them to Rdistance conventions
dB <- dB[, c("TranID", "Number", "SightDist", "SightAngle")]
names(dB) <- c("siteID", "groupsize", "sightdist", "sightangle")


# Compute perpendicular, off-transect distances from the observer's sight distance and angle
dB$dist <- round(perp.dists(s.dist="sightdist", s.angle="sightangle", data=dB), 1)


# Plot histogram
hist(dB$dist, breaks=25, col="dark gray", main="Sparrow Distances", xlab="Distance (m)")






# SATH point count data

# Read in dataframe of observations on transects that don't have "Z" in the transect ID
dS <- sqlQuery(pchan, query="SELECT * FROM [Point_Data]")
dS$Comments <- NULL  # drop comments columns
dS <- dS[order(dS$PtID), ]  # order by transect ID


# Keep only Sage Thrasher data from 2013
dS <- dS[dS$Spp=="SATH" & dS$Year==2013, ]


# Keep only necessary columns and rename them to Rdistance conventions
dS <- dS[, c("PtID", "GrpSize", "Dist")]
names(dS) <- c("siteID", "groupsize", "dist")




# Plot histogram
hist(dS$dist, breaks=10, col="dark gray", main="Thrasher Distances", xlab="Distance (m)")
abline(0, 1)






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# PART 2:  PREPARE THE SITE DATASETS (TRANSECT- (OR POINT-) LEVEL DATA) -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Transect shapefile
trandir <- "D:/GIS/Field_GIS_Data/x2012"  # External drive
# trandir <- "C:/Users/Jason/Box Sync/GIS/Field_GIS_Data/x2012"  # ThinkPad Laptop
# trandir <- "C:/Users/jcarlis3/Box Sync/GIS/Field_GIS_Data/x2012"  # CarlisleMule Desktop

# Point count shapefile
ptdir <- "D:/GIS/Field_GIS_Data/x2013/TrtPointCounts"  # External drive

# Homer raster data
rastdir <- "D:/UWyo/Ch3_FocalRaster/Oct2016/CovariateRasters/Veg0s"  # External drive
# rastdir <- "C:/GIS_data/HomerData/ProcessedRasters"  # ThinkPad Laptop
# rastdir <- "F:/umbrella_GIS/LandcoverData/usgs_wyo_sagebrush_jan2012/ProcessedRasters"#  CarlisleMule Desktop



# BRSP line transect data
# Read in transect information recorded while surveying (from SQL database)
tB <- sqlQuery(tchan, query="SELECT * FROM [Avian_Effort] WHERE TranID NOT LIKE 'Z%'")
tB <- tB[order(tB$TranID), ]  # order by transect ID
# Keep only the 2012 surveys
tB <- tB[year(tB$SampDateStart) == 2012, ]
# Add transect length (all 500 m)
tB$length <- 500


# SATH point transect data
tS <- sqlQuery(pchan, query="SELECT * FROM [Point_Effort]")
tS <- tS[order(tS$PtID), ]  # order by transect ID
# Keep only the 2013 surveys
tS <- tS[tS$Year == 2013, ]


tB <- droplevels(tB)
tS <- droplevels(tS)

odbcCloseAll()



# Keep only necessary columns and rename
tB <- tB[, c("TranID", "Observer", "length")]
names(tB) <- c("siteID", "observer", "length")

tS <- tS[, c("PtID", "Observer")]
names(tS) <- c("siteID", "observer")


# Rename observers
levels(tB$observer) <- c("obs1", "obs2", "obs3", "obs4", "obs5")
levels(tS$observer) <- c("obs1", "obs2", "obs3", "obs4", "obs5", "obs6")


table(tB$observer)
table(tS$observer)


# Reorder columns
tB <- tB[, c("siteID", "length", "observer")]





# Calculate some covariates with GIS
# Read in transect locations
trans <- readOGR(dsn=trandir, layer="Transects_atts_wylam")
length(trans)
trans@data[ncol(trans@data)] <- NULL  # get rid of last column with redundant info
trans@data  # look at attribute table

# drop 8 transects that start with "Z"
trans <- trans[!grepl(pattern="Z", x=trans@data$TransectID), ]
trans@data[grepl(pattern="Z", x=trans@data$TransectID), ]  # check that "Z"s gone
length(trans)  # should now be 72




# Read in point count locations
pts <- readOGR(dsn=ptdir, layer="PointCountLocations_surveyed")
plot(pts)
length(pts)
pts@data

# Project points from NAD 83 to Wylam
proj4string(trans)
proj4string(pts)

pts <- spTransform(pts, proj4string(trans))
identicalCRS(pts, trans)

# Find all .img raster files in the raster directory that were processed to have masked values==0
(dlist <- dir(path=rastdir, pattern="0s.img$"))
(dlist <- dlist[c(1, 4, 7, 8)])  # keep only the ones I'm interested in

# Loop through the list and read in the rasters
rlist <- list()
for(i in 1:length(dlist)){
    rlist<- c(rlist, raster(paste(rastdir, dlist[i], sep="/")))
}

# Stack the rasters. They must have the exact same properties (proj, origin, res, extent)
rs <- stack(rlist)
names(rs)  # Double check all rasters of interest made it into the stack
#plot(rs)

# Crop down to a 5km buffer from transects
rs <- crop(x=rs, y=gBuffer(trans, width=5000))
plot(rs)


# Create a 100m buffer around each transect line
polys <- gBuffer(trans, width=100, capStyle="FLAT", byid=TRUE)
plot(polys[1:4, ]); plot(trans, add=TRUE)  # plot a few to see

# Take the mean of each raster within the buffered polygon of each line
xmean <- extract(x=rs, y=polys, fun=mean, df=TRUE)  # mean


# Give columns meaningful names and append to one data.frame
names(xmean)[2:ncol(xmean)] <- c("bare", "herb", "shrub", "height")
xmean[2:ncol(xmean)] <- round(xmean[2:ncol(xmean)], 1)
plot(xmean)

# Append to data from shapefile attribute table to make covariate data.frame
(sdf <- trans@data)
(sdf <- data.frame(cbind(sdf, xmean), row.names=NULL))  # append, they're already in the right order
sdf$ID <- NULL  # remove ID field after ensuring match with TranID order
sdf$Tier <- sdf$Core <- NULL  # extra cols

# Merge to existing transect data
tB <- merge(tB, sdf, by.x="siteID", by.y="TransectID")
rm(sdf)
tB <- droplevels(tB)  # drops unused factor levels in sdf (specifically TranID)


# Add categorical sagebrush cover (low being less than 10%)
tB$shrubclass <- "Low"
tB[tB$shrub >= 10, "shrubclass"] <- "High"
tB$shrubclass <- factor(tB$shrubclass, levels=c("Low", "High"))





# For points
# Take the mean of each raster within 100 m of each point
xmean <- extract(x=rs, y=pts, fun=mean, buffer=100, df=TRUE)  # mean


# Give columns meaningful names and append to one data.frame
names(xmean)[2:ncol(xmean)] <- c("bare", "herb", "shrub", "height")
xmean[2:ncol(xmean)] <- round(xmean[2:ncol(xmean)], 1)
plot(xmean)

# Append to data from shapefile attribute table to make covariate data.frame
(sdf <- pts@data)
(sdf <- data.frame(cbind(sdf, xmean), row.names=NULL))  # append, they're already in the right order
sdf$ID <- NULL  # remove ID field after ensuring match with TranID order


# Merge to existing transect data
tS <- merge(tS, sdf, by.x="siteID", by.y="Pt_ID")
rm(sdf)
tS <- droplevels(tS)
tS <- tS[, c("siteID", "observer", "bare", "herb", "shrub", "height")]





# Make sure pt sites has full complement of site levels
length(levels(tS$siteID))
levels(tS$siteID)
length(levels(dS$siteID))
levels(dS$siteID)

table(tS$siteID)
table(dS$siteID)

dS$siteID <- factor(dS$siteID)
length(levels(dS$siteID))

# The two levels with no detections, add back as levels
levels(dS$siteID) <- c(levels(dS$siteID), "C1X03", "C1X07")

table(dS$siteID)




# Rename
sparrow.detections <- data.frame(dB, row.names=NULL)
sparrow.sites <- data.frame(tB, row.names=NULL)

thrasher.detections <- data.frame(dS, row.names=NULL)
thrasher.sites <- data.frame(tS, row.names=NULL)

# Erase everything but the detection and transect datasets
rm(list=setdiff(ls(), c("sparrow.detections", "sparrow.sites",
                        "thrasher.detections", "thrasher.sites")))




# Save .rda files to package directory
save(sparrow.detections, file="C:/R_Code/Rdistance/data/sparrow.detections.rda")
save(sparrow.sites, file="C:/R_Code/Rdistance/data/sparrow.sites.rda")
save(thrasher.detections, file="C:/R_Code/Rdistance/data/thrasher.detections.rda")
save(thrasher.sites, file="C:/R_Code/Rdistance/data/thrasher.sites.rda")





# Added in Sept 2017 for version 2.0 release
# Rename objects and .rda files

load("C:/R_Code/Rdistance/data/sparrow.detections.rda")
sparrowDetectionData <- sparrow.detections
save(sparrowDetectionData, file="C:/R_Code/Rdistance/data/sparrowDetectionData.rda")

load("C:/R_Code/Rdistance/data/sparrow.sites.rda")
sparrowSiteData <- sparrow.sites
save(sparrowSiteData, file="C:/R_Code/Rdistance/data/sparrowSiteData.rda")


load("C:/R_Code/Rdistance/data/thrasher.detections.rda")
thrasherDetectionData <- thrasher.detections
save(thrasherDetectionData, file="C:/R_Code/Rdistance/data/thrasherDetectionData.rda")

load("C:/R_Code/Rdistance/data/thrasher.sites.rda")
thrasherSiteData <- thrasher.sites
save(thrasherSiteData, file="C:/R_Code/Rdistance/data/thrasherSiteData.rda")

# END
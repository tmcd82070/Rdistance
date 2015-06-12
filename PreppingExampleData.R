# Code used to prepare the example sparrow datasets in Rdistance
# Jason Carlisle
# Last updated 5/21/2015

# Goal for two required two data.frames:
# a detection data.frame with at least $dist, $siteID, and $groupsize
# a transect data.frame with at least $siteID and $length (and we're adding some other covariates)

# This code calculates a bunch of covariates that we're not using at this time.  The only one
# included in the Rdistance example data (as of v 1.2.2) is a categorical covariate of sagebrush cover (low or high)

# Load required packages
require(Rdistance)
require(sp)
require(rgdal)
require(rgeos)
require(raster)
require(RODBC)


# Data sources
# Connect to SQL Server database on CarlisleMule Desktop
chan <- odbcDriverConnect(connection='driver={SQL Server Native Client 11.0}; server=CARLISLEMULE,1433;
                          database=LineTransectData; Uid=jasontemp; Pwd=temppassword', believeNRows=FALSE)

# Transect shapefile
trandir <- "C:/Users/Jason/Box Sync/GIS/Field_GIS_Data/x2012"  # ThinkPadLaptop
# trandir <- "C:/Users/jcarlis3/Box Sync/GIS/Field_GIS_Data/x2012"  # CarlisleMule Desktop

# Homer raster data
rastdir <- "C:/GIS_data/HomerData/ProcessedRasters"  # ThinkPad Laptop
# rastdir <- "F:/umbrella_GIS/LandcoverData/usgs_wyo_sagebrush_jan2012/ProcessedRasters"#  CarlisleMule Desktop


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# PART 1:  PREPARE THE DETECTIONS DATASET (RAW BIRD COUNT DATA) -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Read in dataframe of observations on transects that don't have "Z" in the transect ID
# We surveyed 8 "Z" transects which were sited differently and are not being analyzed here
ddf <- sqlQuery(chan, query="SELECT * FROM [LineTransectData].[dbo].[Avian_Data] WHERE TranID NOT LIKE 'Z%'")
ddf <- ddf[ , -10]  # drop comments columns
ddf <- ddf[order(ddf$TranID), ]  # order by transect ID


# Keep only Brewer's Sparrow data from 2012
ddf <- ddf[ddf$Spp=="BRSP" & ddf$Year==2012, ]


# Keep only necessary columns and rename them to Rdistance conventions
ddf <- ddf[, c("TranID", "Number", "SightDist", "SightAngle")]
names(ddf) <- c("siteID", "groupsize", "sightdist", "sightangle")


# Compute perpendicular, off-transect distances from the observer's sight distance and angle
# Note, the current version of Rdistance (v 1.2.2) doesn't have the perpendicular distances calculated
# within the example dataset.
ddf$dist <- perp.dists(obs.dist=ddf$sightdist, obs.angle=ddf$sightangle)


# Plot histogram
hist(ddf$dist, breaks=25, col="dark gray", main="Sparrow Distances", xlab="Distance (m)")




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# PART 2:  PREPARE THE TRANSECTS DATASET (TRANSECT-LEVEL DATA) -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Read in transect information recorded while surveying (from SQL database)
tdf <- sqlQuery(chan, query="SELECT * FROM [LineTransectData].[dbo].[Avian_Effort] WHERE TranID NOT LIKE 'Z%'")
tdf <- tdf[order(tdf$TranID), ]  # order by transect ID

# Add transect length (all 500 m)
tdf$length <- 500


# Keep only necessary columns and rename
tdf <- tdf[, c("TranID", "Observer", "length")]
names(tdf) <- c("siteID", "observer", "length")


# Rename observers
levels(tdf$observer) <- c("obs1", "obs2", "obs3", "obs4", "obs5")

# Reorder columns
tdf <- tdf[, c("siteID", "length", "observer")]


# Calculate some covariates with GIS
# Read in transect locations
trans <- readOGR(dsn=trandir, layer="Transects_atts_wylam")
length(trans)
trans@data[ncol(trans@data)] <- NULL  # get rid of last column with redundant info
trans@data  # look at attribute table

# drop 8 transects that start with "Z"
trans@data[grepl(pattern="Z", x=trans@data$TransectID), ]  # find row numbers
trans <- trans[-c(9, 10, 23, 28, 57, 62, 63, 64), ]  # these should be row numbers +1
trans@data[grepl(pattern="Z", x=trans@data$TransectID), ]  # check that "Z"s gone
length(trans)  # should now be 72


# Find all .img raster files in the raster directory that were processed to have masked values==0
dlist <- dir(path=rastdir, pattern="0s.img$")
(dlist <- dlist[c(1, 4, 5, 7, 6, 8)])  # keep only the ones I'm interested in

# Loop through the list and read in the rasters
rlist<- list()
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
polys <- gBuffer(trans, width=100, capStyle="ROUND", byid=TRUE)
plot(polys[1:4, ]); plot(trans, add=TRUE)  # plot a few to see

# Take the mean of each raster within the buffered polygon of each line
xmean <- extract(x=rs, y=polys, fun=mean, df=TRUE)  # mean


# Give columns meaningful names and append to one data.frame
names(xmean)[2:7] <- c("BareMean", "HerbMean", "LitterMean", "ShrubMean", "SageMean", "HeightMean")


# Append to data from shapefile attribute table to make covariate data.frame
(sdf <- trans@data)
(sdf <- data.frame(cbind(sdf, xmean), row.names=NULL))  # append, they're already in the right order
sdf$ID <- NULL  # remove ID field after ensuring match with TranID order


# Make sure data types are correct
sdf$Tier <- as.factor(sdf$Tier)  # change to factor
sdf$Core <- as.factor(sdf$Core)  # change to factor
sdf <- droplevels(sdf)  # drops unused factor levels in sdf (specifically TranID)


# Add categorical sagebrush cover (low being less than 10%)
sdf$SageClass <- "Low"
sdf[sdf$SageMean >= 10, "SageClass"] <- "High"
sdf$SageClass <- factor(sdf$SageClass, levels=c("Low", "High"))



# Round mean sagebrush
sdf$SageMean <- round(sdf$SageMean, 1)


# Keep only a few of the spatial variables for now and rename
sdf <- sdf[, c("TransectID", "SageMean", "SageClass")]
names(sdf) <- c("siteID", "sagemean", "sage")


# Merge
(tdf <- merge(tdf, sdf, by="siteID"))


# Rename
sparrow.detections <- data.frame(ddf, row.names=NULL)
sparrow.transects <- data.frame(tdf, row.names=NULL)


# Erase everything but the detection and transect datasets
rm(list=setdiff(ls(), c("sparrow.detections", "sparrow.transects")))




# Save .rda files to package directory
save(sparrow.detections, file="C:/R_Code/Rdistance/data/sparrow.detections.rda")
save(sparrow.transects, file="C:/R_Code/Rdistance/data/sparrow.transects.rda")

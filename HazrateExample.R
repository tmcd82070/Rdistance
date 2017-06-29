# Example of covariates on hazard-rate function


require(Rdistance)
require(unmarked)



# Note, all packages are used to fit a hazard-rate detection function
# to the Rdistance sparrows data with distances > 150 truncated
trunc <- 150






# Read in example sparrow datasets and remove extra columns
data(package="Rdistance", sparrow.detections)
sparrow.detections <- sparrow.detections[, -which(names(sparrow.detections) %in% c("sightdist", "sightangle"))]

data(package="Rdistance", sparrow.transects)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Rdistance -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# NO COVARIATE
# Fit model and estimate abundance (density per ha)
r.dfunc <- F.dfunc.estim(sparrow.detections, likelihood = "hazrate", w.hi=trunc)
r.fit <- F.abund.estim(r.dfunc, detection.data=sparrow.detections, transect.data=sparrow.transects, area=10000,
                       R=500, ci=0.95, plot.bs=TRUE)

r.fit

# The two parameters of the hazard rate detection function
# Could also be extracted with coef(r.fit)
r.fit$parameters[[1]]  # we call this Sigma for hazrate
r.fit$parameters[[2]]  # we call this Beta for hazrate

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# # Distance -----
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# # NO COVARIATE
# # Format detection data
# d.data <- sparrow.detections[3:2]
# names(d.data) <- c("distance", "size")  # meet naming conventions
# d.data$object <- 1:nrow(d.data)  # add object ID
# 
# # Produce and format 3 other required data.frames
# d.region <- data.frame(Region.Label="main", Area=10000)
# 
# d.sample <- sparrow.transects[1:2]
# names(d.sample)[1:2] <- c("Sample.Label", "Effort")
# d.sample$Region.Label <- "main"
# 
# d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=sparrow.detections$siteID)
# 
# # Fit model and estimate abundance
# (ds.fit <- ds(data=d.data, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
#               truncation=trunc, transect="line", key="hr", adjustment=NULL, dht.group=FALSE))
# 
# plot(ds.fit)
# print(ds.fit)
# summary(ds.fit)
# 
# # Extract output
# # They're output is messy, and I think the hazard-rate is parameterized differently in each package?
# names(ds.fit)
# names(ds.fit$ddf)
# names(ds.fit$dht)
# str(ds.fit)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# unmarked -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ONE COVARIATE
# unmarked uses a multinomial-Poisson mixture model to estimate abundance
# They have a great vignette at http://cran.r-project.org/web/packages/unmarked/vignettes/distsamp.pdf


# Convert to from individual-level format to transect-level format required by distsamp
u.dists <- sparrow.detections

u.dists <- u.dists[u.dists$dist < trunc, ]  # manually truncate
#hist(u.dists$dist, breaks=30)
summary(u.dists$dist)

# Format (bin) into a matrix with dimensions of number of sites (M or R) X number of distance intervals (J)
# Each cell of the matrix is the number of observations at that distance interval for that transect
cp15 <- seq(0, trunc, by=15)
u.mat <- formatDistData(distData=u.dists, distCol="dist", transectNameCol="siteID", dist.breaks=cp15)

# Likely too wide to print cleanly, but have a look
u.mat

# covariate data
u.covs <- sparrow.transects

# Organize distance data along with covariates and metadata
u.dist <- unmarkedFrameDS(y=u.mat, siteCovs=u.covs, dist.breaks=cp15,
                          tlength=rep(500, nrow(u.covs)), survey="line", unitsIn="m")

u.dist  # look at the data
summary(u.dist)  # get a summary of the new object


# Fit hazard-rate detection function with covariate on detection
u.fit <- distsamp(~sagemean ~1, u.dist, keyfun="hazard", output="density", unitsOut="ha")


u.fit



# Plot detection function for two different values of the covariate
# The shape parameter is the only one that changes

# When sagebrush cover is 5%
shape5 <- backTransform(linearComb(u.fit["det"], c(1, 5)))@estimate
scale <- backTransform(u.fit["scale"])@estimate
plot(function(x) gxhaz(x, shape=shape5, scale=scale), 0, trunc,
     ylab="Detection probability", xlab="Distance (m)", lwd=2)

# When sagebrush cover is 20%
shape20 <- backTransform(linearComb(u.fit["det"], c(1, 20)))@estimate
plot(function(x) gxhaz(x, shape=shape20, scale=scale), 0, trunc,
     lty=2, lwd=2, add=TRUE)

legend("topright", c("5% sagebrush cover", "20% sagebrush cover"), lty=c(1, 2))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


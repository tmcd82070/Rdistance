# Comparing the output of three R packages for distance-sampling analyis
# Rdistance, Distance, and unmarked

# Jason Carlisle
# Wyoming Cooperative Fish & Wildlife Research Unit
# jason.d.carlisle@gmail.com

# last updated 4/8/2015

# Trent, once the three packages are installed, this code should run self-contained as is
# Run the entire script, then see the object named "output" for a summary table of results by package.
# "output" has NAs where I could not figure out how to calculate that value for that package.

# Note, all packages are used to fit a half-normal detection function
# to the Rdistance sparrows data with distances > 150 truncated
trunc <- 150


# Load packages (looks like no functions are masked)
require(Rdistance)
require(Distance)
require(unmarked)


# Prep data.frame to store results in
output <- data.frame(pkg=c("Rdistance", "Distance", "unmarked"),
                     Nhat=NA,
                     Nlow=NA,
                     Nupp=NA,
                     Sigma=NA,
                     ESW=NA,
                     LogLike=NA,
                     AIC=NA)


# Read in example sparrow datasets and calculate perpendicular distance
data(package="Rdistance", sparrow.dists)
sparrow.dists$dists <- perp.dists(obs.dist=sparrow.dists$sightdist, obs.angle=sparrow.dists$sightangle)
sparrow.dists <- sparrow.dists[, -which(names(sparrow.dists) %in% c("sightdist", "sightangle"))]

data(package="Rdistance", sparrow.covs)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Rdistance -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Fit model and estimate abundance (density per ha)
r.dfunc <- F.dfunc.estim(sparrow.dists, likelihood = "halfnorm", w.hi=trunc)
r.fit <- F.abund.estim(r.dfunc, distdata=sparrow.dists, covdata=sparrow.covs, area=10000,
                       R=1000, ci=0.95, plot.bs=TRUE, bs.method="transects")

r.fit

# Extract output
output[1, "Sigma"] <- r.fit$parameters[[1]]
output[1, "Nhat"] <- r.fit$n.hat
output[1, "Nlow"] <- r.fit$ci[[1]]
output[1, "Nupp"] <- r.fit$ci[[2]]
output[1, "ESW"] <- ESW(r.fit)
output[1, "LogLike"] <- r.fit$loglik
output[1, "AIC"] <- AIC.dfunc(r.fit)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Distance -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Format detection data
d.data <- sparrow.dists[3:2]
names(d.data) <- c("distance", "size")  # meet naming conventions
d.data$object <- 1:nrow(d.data)  # add object ID

# Produce and format 3 other required data.frames
d.region <- data.frame(Region.Label="main", Area=10000)

d.sample <- sparrow.covs[1:2]
names(d.sample)[1:2] <- c("Sample.Label", "Effort")
d.sample$Region.Label <- "main"

d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=sparrow.dists$siteID)

# Fit model and estimate abundance
(ds.fit <- ds(data=d.data, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
              truncation=trunc, transect="line", key="hn", adjustment=NULL, dht.group=FALSE))

plot(ds.fit)
print(ds.fit)
summary(ds.fit)

# Extract output
names(ds.fit)
names(ds.fit$ddf)
names(ds.fit$dht)
str(ds.fit)
# The Nhat and CI are copied from the printed output, not sure where stored in ds.fit
output[2, "Sigma"] <- NA  # Don't know if/where this is stored
output[2, "Nhat"] <- 0.8265172  # differs from ds.fit$ddf$Nhat
output[2, "Nlow"] <- 0.6717755
output[2, "Nupp"] <- 1.016903
output[2, "ESW"] <- NA  # Don't know if/where this is stored
output[2, "LogLike"] <- ds.fit$ddf$lnl
output[2, "AIC"] <- ds.fit$ddf$criterion

# Questions:
# How are transects with no birds accounted for?
# Why do the printed abundance and what's stored in ds.fit$ddf$Nhat differ?
# Monotonicity parameters?
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# unmarked -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# unmarked uses a multinomial-Poisson mixture model to estimate abundance
# They have a great vignette at http://cran.r-project.org/web/packages/unmarked/vignettes/distsamp.pdf
# Here, I used 15 m bins, but my experience is that different bin size affects the results quite a bit.

# Convert to from individual-level format to transect-level format required by distsamp
u.dists <- sparrow.dists

u.dists <- u.dists[u.dists$dists < trunc, ]  # manually truncate
#hist(u.dists$dists, breaks=30)
summary(u.dists$dists)

# Add factor levels for transects where no sparrows were observed (needs to be 72 total levels)
length(levels(u.dists$siteID))
levels(u.dists$siteID) <- c(levels(u.dists$siteID),
                           c("B2", "D1", "D2", "D4", "K1", "L3", "N2", "P3", "P4", "Q3", "Q4"))
length(levels(u.dists$siteID))
table(u.dists$siteID)

# Format (bin) into a matrix with dimensions of number of sites (M or R) X number of distance intervals (J)
# Each cell of the matrix is the number of observations at that distance interval for that transect
# cp5 <- seq(0, trunc, by=5)  # create 5 m bins from 0-trunc
# cp10 <- seq(0, trunc, by=10)  # 10 m bins
# cp20 <- seq(0, trunc, by=20)  # 20 m bins
cp15 <- seq(0, trunc, by=15)

# u.mat.5 <- formatDistData(distData=u.dists, distCol="dists", transectNameCol="siteID", dist.breaks=cp5)
# u.mat.10 <- formatDistData(distData=u.dists, distCol="dists", transectNameCol="siteID", dist.breaks=cp10)
# u.mat.20 <- formatDistData(distData=u.dists, distCol="dists", transectNameCol="siteID", dist.breaks=cp20)
u.mat <- formatDistData(distData=u.dists, distCol="dists", transectNameCol="siteID", dist.breaks=cp15)

# Likely too wide to print cleanly, but have a look
u.mat

# covariate data
u.covs <- sparrow.covs[1]

# Organize distance data along with covariates and metadata
# u.dist.5 <- unmarkedFrameDS(y=u.mat.5, siteCovs=u.covs, dist.breaks=cp5,
#                              tlength=rep(500, nrow(u.covs)), survey="line", unitsIn="m")
# u.dist.10 <- unmarkedFrameDS(y=u.mat.10, siteCovs=u.covs, dist.breaks=cp10,
#                             tlength=rep(500, nrow(u.covs)), survey="line", unitsIn="m")
# u.dist.20 <- unmarkedFrameDS(y=u.mat.20, siteCovs=u.covs, dist.breaks=cp20,
#                             tlength=rep(500, nrow(u.covs)), survey="line", unitsIn="m")
u.dist <- unmarkedFrameDS(y=u.mat, siteCovs=u.covs, dist.breaks=cp15,
                            tlength=rep(500, nrow(u.covs)), survey="line", unitsIn="m")

u.dist  # look at the data
summary(u.dist)  # get a summary of the new object

# Fit half-normal detection function with no covariates for detection or density
# u.fit.5 <- distsamp(~1 ~1, u.dist.5, keyfun="halfnorm", output="density", unitsOut="ha")
# u.fit.10 <- distsamp(~1 ~1, u.dist.10, keyfun="halfnorm", output="density", unitsOut="ha")
# u.fit.20 <- distsamp(~1 ~1, u.dist.20, keyfun="halfnorm", output="density", unitsOut="ha")
u.fit <- distsamp(~1 ~1, u.dist, keyfun="halfnorm", output="density", unitsOut="ha")

# u.fit.5
# u.fit.10
# u.fit.20
u.fit

# Plot detection function
hist(u.fit, xlab="Distance (m)")


# Back-transformed output
backTransform(u.fit, type="state")
backTransform(u.fit, type="det")

# Extract output
names(u.fit)
u.fit@estimates
str(u.fit)
output[3, "Sigma"] <- backTransform(u.fit, type="det")@estimate
output[3, "Nhat"] <- backTransform(u.fit, type="state")@estimate
output[3, "Nlow"] <- predict(u.fit, type="state")[1, "lower"]
output[3, "Nupp"] <- predict(u.fit, type="state")[1, "upper"]
output[3, "ESW"] <- NA  # There are methods for computing ESW, see ?distsamp example
output[3, "LogLike"] <- u.fit@negLogLike
output[3, "AIC"] <- u.fit@AIC

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
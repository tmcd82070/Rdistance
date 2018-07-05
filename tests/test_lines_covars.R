#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance:  an R package for distance-sampling analysis

# This script tests the line-transect workflow, with covariates

# Jason D. Carlisle
# WEST, Inc.
# jcarlisle@west-inc.com
# Last updated 9/21/2017

# This demo was tested using the following:
# Rdistance version 2.0.0

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Outline -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# This script contains the following sections:
# 1) Install and load Rdistance
# 2) Read in input data
# 3) Fit a detection function
# 4) Estimate abundance given the detection function
# 5) Use AICc to select a detection function and estimate abundance
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



# #////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# # 1) Install and load Rdistance -----
# #////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# # As of publication, the Rdistance package is not on CRAN, but can be installed from a GitHub repository, which will
# # require using the devtools package.  The Rdistance package may be further developed after publication, but this demo
# # is current and compatible with Rdistance version 1.3.2.
# 
# # Check if Rdistance is installed.  If yes, load it.  If not, install it, then load it.
# # Rdistance depends only on base R packages
# 
# if("Rdistance" %in% rownames(installed.packages()) == FALSE){
#   install.packages("Rdistance")
# }
require(Rdistance)
# 
# 
# # View help documentation for the Rdistance package
# ?Rdistance
# 
# # View help documentation for key Rdistance functions
# ?dfuncEstim
# ?abundEstim
# ?F.automated.CDA
# 
# # View help documentation for Rdistance example datasets
# ?sparrowDetectionData
# ?sparrowSiteData
# #////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 2) Read in input data -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance requires two input datasets.  These can be prepared outside of R and read in as data.frames using,
# for example, read.csv.  We make use of the sparrow example datasets already contained within Rdistance.


# The first required dataset is a detection data.frame
# Each row is a detection, and the siteID, groupsize, and dist columns are required (as named)
data(sparrowDetectionData)
head(sparrowDetectionData)



# If the observers recorded sighting distance and sighting angle instead of perpendicular distance (as is often common
# in line transect surveys), you can use the `perp.dists` function (detailed in Section 3) to calculate the perpendicular
# distances based on the sighting distances and sighting angles.



# The second required dataset is a transect data.frame
# Each row is a transect, and the siteID and length columns are required (as named)
# Other columns (e.g., transect-level covariates) are ignored, but may be useful in modeling abundance later
data(sparrowSiteData)
head(sparrowSiteData)
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 3) Fit a detection function
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 3: Fit a detection function
# After prepping the input data, the first step is to explore your data and fit a detection function.


# Distance-sampling analysis is done on perpendicular distances (i.e., the distance from each detected group to the
# transect, not to the observer).  We have provided the perpendicular distances (named `dist`) in the example data, but
# observers originally recorded sighting distances and sighting angles.  Here we use the `perp.dists` function to
# (re)calculate the perpendicular distances (`dist`) and remove the `sightdist` and `sightangle` columns.  See the help
# documentation for `perp.dists` for details.


# sparrowDetectionData$dist2 <- perp.dists(s.dist="sightdist", s.angle="sightangle", data=sparrowDetectionData)
# 
# sparrowDetectionData <- sparrowDetectionData[, -which(names(sparrowDetectionData) %in% c("sightdist", "sightangle", "dist2"))]                                                                  
# head(sparrowDetectionData)



# Explore the distribution of distances.
hist(sparrowDetectionData$dist, col="grey", main="", xlab="Distance (m)", breaks=20)
rug(sparrowDetectionData$dist)
summary(sparrowDetectionData$dist)


sparrowSiteData$zBare <- with(sparrowSiteData, (bare - (mean(bare)))/sd(bare))
hist(sparrowSiteData$zBare)


# Fit detection function

trunc <- 100
sparrow.dfunc <- dfuncEstim(formula=dist~zBare, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                            likelihood="halfnorm", w.hi=trunc)

sparrow.dfunc

# Coefficients:
#   Estimate  SE          z          p(>|z|)    
# (Intercept)  3.880301  0.06298025  61.611392  0.000000000
# zBare        0.109066  0.04173286   2.613432  0.008963801



plot(sparrow.dfunc)
# Plot for different covar values
plot(sparrow.dfunc, newdata=data.frame(zBare=seq(min(sparrowSiteData$zBare), max(sparrowSiteData$zBare), length=4)))



## Two covariates
sparrow2.dfunc <- dfuncEstim(formula=dist~zBare+observer, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                            likelihood="halfnorm", w.hi=trunc)

# Call: dfuncEstim(formula = dist ~ zBare + observer, detectionData = sparrowDetectionData,     siteData = sparrowSiteData, likelihood = "halfnorm", w.hi = trunc)
# 
# Coefficients:
#   Estimate      SE         z            p(>|z|)      
# (Intercept)    3.925780615  0.1277937  30.71967079  3.108922e-207
# zBare          0.092367294  0.0456892   2.02164389   4.321315e-02
# observerobs2  -0.007432956  0.2041106  -0.03641631   9.709504e-01
# observerobs3  -0.022169883  0.1699543  -0.13044616   8.962135e-01
# observerobs4  -0.168769696  0.1784444  -0.94578287   3.442593e-01
# observerobs5  -0.087602596  0.1729181  -0.50661318   6.124263e-01
# 
# Convergence: Success
# Function: HALFNORM  
# Strip: 0 to 100 
# Average effective strip width (ESW): 56.18571 
# Average probability of detection: 0.5618571 
# Scaling: g(0) = 1
# Log likelihood: 1480.274 
# AICc: 2972.806

(newdata <- data.frame(zBare=rep(0, 5),
                       observer=paste0("obs",1:5)))

plot(sparrow2.dfunc, newdata=newdata)

# Turn the legend off with "legend = FALSE" in the plot function



(newdata <- data.frame(zBare=c(-1,0,1),
                       observer=factor("obs1", levels=paste0("obs",1:5))))

plot(sparrow2.dfunc, newdata=newdata)



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 4) Estimate abundance given the detection function -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Estimating abundance requires the additional information contained in the second required dataset, described earlier,
# where each row represents one transect. Load the example dataset of surveyed sparrow transects from the package.


# Next, estimate abundance (or density in this case) using `abundEstim`.  If `area`=1, then density is given in the
# squared units of the distance measurements --- in this case, sparrows per square meter.  Instead, we set `area`=10000
# in order to convert to sparrows per hectare (1 ha == 10,000 m^2^).  The equation used to calculate the abundance
# estimate is detailed in the help documentation for `abundEstim`.

# Confidence intervals for abundance are calculated using a bias-corrected bootstrapping method (see `abundEstim`),
# and the detection function fit in each iteration of the bootstrap is plotted as a blue line (if `plot.bs=TRUE`).
# Note that, as with all bootstrapping procedures, there may be slight differences in the confidence intervals between
# runs due to so-called 'simulation slop'.  Increasing the number of bootstrap iterations (`R` = 100 used here) may be
# necessary to stabilize CI estimates.


fit <- abundEstim(dfunc=sparrow.dfunc, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                  area=10000, R=5, ci=0.95, plot.bs=TRUE)
fit

# Abundance estimate:  0.8753123 ;  95% CI=( 0.8558303 to 0.9078753 )




# Results of interest (such as the abundance estimate and confidence interval) can be extracted from the resulting
# object (here called `fit`).
fit$n.hat
fit$ci


fitBy <- abundEstim(dfunc=sparrow.dfunc, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                       area=10000, ci=NULL, bySite=TRUE)
head(fitBy)

mean(fitBy$density)
fit$n.hat


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 5) Use AICc to select a detection function and estimate abundance -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Alternatively, steps 3 (fitting a detection function) and 4 (estimating abundance) can be automated using the
# function `F.automated.CDA`.  This function attempts to fit multiple detection functions, uses AICc (by default, but
# see help documentation for `AIC.dfunc` for other options) to find the 'best' detection function, then proceeds to
# estimate abundance using that detection function.  By default, `F.automated.CDA` tries a large subset of `Rdistance`'s
# built-in detection functions, but you can control exactly which detection functions are attempted (see help
# documentation for `F.automated.CDA`).  Specifying `plot=TRUE` would return a plot of each detection function.  In
# this example, we attempt to fit the default detection functions (n = 41), and we don't plot each (`plot=FALSE`).

auto <- autoDistSamp(formula=dist~zBare, 
                     detectionData=sparrowDetectionData, 
                     siteData=sparrowSiteData,
                     w.hi=trunc, 
                     plot=FALSE, 
                     area=10000, 
                     R=10, 
                     ci=0.95, 
                     plot.bs=FALSE, 
                     showProgress = TRUE,
                     likelihoods=c("halfnorm", "hazrate","uniform","negexp"),
                     expansions=0)

# Call: dfuncEstim(formula = formula, detectionData = detectionData,     siteData = siteData, likelihood = fit.table$like[1], pointSurvey = pointSurvey,     w.lo = w.lo, w.hi = w.hi, expansions = fit.table$expansions[1],     series = fit.table$series[1])
# 
# Coefficients:
#   Estimate    SE           z           p(>|z|)     
# (Intercept)  0.92822452  3.503407115   0.2649491  7.910487e-01
# zBare        3.49451884  3.488491936   1.0017277  3.164751e-01
# Knee         0.03291271  0.003075251  10.7024472  9.912513e-27
# 
# Convergence: Success
# Function: UNIFORM  
# Strip: 0 to 100 
# Average effective strip width (ESW): 49.14188 
# Average probability of detection: 0.4914188 
# Scaling: g(0) = 1
# Log likelihood: 1475.271 
# AICc: 2956.615
# 
# Abundance estimate:  1.080316 ;  95% CI=( 0.8651121 to 1.251669 )
# CI based on 6 of 10 successful bootstrap iterations

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#






# Compare to Distance
require(Distance)

# Format detection data
sparrow.merge <- merge(sparrowDetectionData, sparrowSiteData, by="siteID")
d.data <- sparrow.merge[c("dist", "groupsize", "bare", "zBare")]
names(d.data) <- c("distance", "size", "bare", "zBare")  # meet naming conventions
d.data$object <- 1:nrow(d.data)  # add object ID

# Produce and format 3 other required data.frames
d.region <- data.frame(Region.Label="main", Area=10000)

d.sample <- sparrowSiteData[c("siteID", "length")]
names(d.sample)[1:2] <- c("Sample.Label", "Effort")
d.sample$Region.Label <- "main"

d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=sparrowDetectionData$siteID)

# Fit model and estimate abundance
(ds.fit <- ds(data=d.data, formula= ~zBare, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
              truncation=trunc, transect="line", key="hn", adjustment=NULL, dht.group=FALSE))

plot(ds.fit)
print(ds.fit)
summary(ds.fit)


# # Trent's RDistance fitting
# fit <- dfuncEstim(formula = dist ~ bare, detectionData = sparrowDetectionData,
#                   siteData = sparrowSiteData, likelihood = "halfnorm")
# fit <- abundEstim(fit, sparrowDetectionData, sparrowSiteData, ci=.95, 
#                    R=100, area=10000)


# With bare as covariate
# Very very similar results, but don't match exactly
# with Standardized zBare, results match to 5 decimals.
# AIC
AIC(fit)  # Rdistance = 2965.787
AIC(ds.fit)  # Distance = 2965.751

# Abund
fit$n.hat  # 0.8753123
fit$ci  # depends on the boostrap, not getting enough iterations to converge
ds.fit$dht$individuals$N  # 0.8753126 (0.6918557 - 1.107416)
# ds.fit$dht$individuals$N$Estimate
# ds.fit$dht$individuals$N$lcl
# ds.fit$dht$individuals$N$ucl

# Sigma
coef(fit)  # (Intercept)=3.880301; zBare=0.109066 
     
ds.fit$ddf$ds$aux$ddfobj$scale$parameters  # Intercept =  3.8802998 , zBare = 0.1090674



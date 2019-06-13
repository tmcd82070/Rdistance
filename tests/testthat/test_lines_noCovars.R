#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance:  an R package for distance-sampling analysis

# This script tests the line-transect workflow, with no covariates

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
# ?F.dfunc.estim
# ?F.abund.estim
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
context("Test the lines_noCovars() function")

test_that("lines_noCovars Covars() operates as it should", {

  data(sparrowDetectionData)
  data(sparrowSiteData)
  trunc <- 100
  sparrow.dfunc <- dfuncEstim(formula=dist~1, detectionData=sparrowDetectionData, likelihood="halfnorm", w.hi=trunc)
  plot(sparrow.dfunc)
  sparrow.dfunc
  
  
  ESW(sparrow.dfunc)  # 56.30098


  fit <- abundEstim(dfunc=sparrow.dfunc, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                       area=10000, R=500, ci=NULL, plot.bs=TRUE)
  
  fit
  
  # Abundance estimate:  0.863415 ;  95% CI=( 0.6673237 to 1.091665 )

  # Results of interest (such as the abundance estimate and confidence interval) can be extracted from the resulting
  # object (here called `fit`).
  fit$n.hat








# Toggle on bySite option, to estimate abundance for each transect

fitSite <- abundEstim(dfunc=sparrow.dfunc, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                      area=10000, ci=NULL, bySite=TRUE)
head(fitSite)

# plot(fitSite)

mean(fitSite$density)
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


auto <- autoDistSamp(formula=dist~1, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                        w.hi=trunc, plot=FALSE, area=10000, R=50, ci=NULL, plot.bs=TRUE)





# Compare to Distance
require(Distance)

# Format detection data
d.data <- sparrowDetectionData[c("dist", "groupsize")]
names(d.data) <- c("distance", "size")  # meet naming conventions
d.data$object <- 1:nrow(d.data)  # add object ID

# Produce and format 3 other required data.frames
d.region <- data.frame(Region.Label="main", Area=10000)

d.sample <- sparrowSiteData[c("siteID", "length")]
names(d.sample)[1:2] <- c("Sample.Label", "Effort")
d.sample$Region.Label <- "main"

d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=sparrowDetectionData$siteID)

# Fit model and estimate abundance
(ds.fit <- ds(data=d.data, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
              truncation=trunc, transect="line", key="hn", adjustment=NULL, dht.group=FALSE))

plot(ds.fit)
print(ds.fit)
summary(ds.fit)
# See Abundance estimate in Summery for individuals section



# Results very, very similar
# AIC
AIC(fit, criterion="AIC")  # Rdistance = 2970.593
AIC(ds.fit)  # Distance = 2970.594

# Abund
fit$n.hat  # 0.863415
fit$ci  # depends on the bootstrap, about 0.680 - 1.09
ds.fit$dht$individuals$N  # 0.8634164 (0.6943432 - 1.073659)
# ds.fit$dht$individuals$N$Estimate
# ds.fit$dht$individuals$N$lcl
# ds.fit$dht$individuals$N$ucl

# Sigma
coef(fit)  # 46.35884
exp(ds.fit$ddf$ds$aux$ddfobj$scale$parameters)  # 46.35865
})

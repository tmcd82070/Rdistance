#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance:  an R package for distance-sampling analysis

# This demo accompanies the Ecography Software Note on Rdistance.
# After loading the Rdistance package, this demo is self-contained, including example datasets.

# Jason D. Carlisle
# Wyoming Cooperative Fish & Wildlife Research Unit, University of Wyoming
# jason.d.carlisle@gmail.com
# Last updated 11/5/2015

# This demo was tested using the following:
# Rdistance version 1.3.2
# Windows 8.1 Pro - 64 bit
# R version 3.2.2
# RStudio version 0.99.482
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
# 6) Model abundance using a simple linear model
# 7) Model abundance using a generalized linear mixed-effects model
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 1) Install and load Rdistance -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# As of publication, the Rdistance package is not on CRAN, but can be installed from a GitHub repository, which will
# require using the devtools package.  The Rdistance package may be further developed after publication, but this demo
# is current and compatible with Rdistance version 1.3.2.

# Check if Rdistance is installed.  If yes, load it.  If not, install it, then load it.
# Rdistance depends only on base R packages

if("Rdistance" %in% rownames(installed.packages()) == FALSE){
  install.packages("Rdistance")
}
require(Rdistance)


# View help documentation for the Rdistance package
?Rdistance

# View help documentation for key Rdistance functions
?F.dfunc.estim
?F.abund.estim
?F.automated.CDA

# View help documentation for Rdistance example datasets
?sparrow.detections
?sparrow.transects
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 2) Read in input data -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance requires two input datasets.  These can be prepared outside of R and read in as data.frames using,
# for example, read.csv.  We make use of the sparrow example datasets already contained within Rdistance.


# The first required dataset is a detection data.frame
# Each row is a detection, and the siteID, groupsize, and dist columns are required (as named)
data(sparrow.detections)
head(sparrow.detections)



# If the observers recorded sighting distance and sighting angle instead of perpendicular distance (as is often common
# in line transect surveys), you can use the `perp.dists` function (detailed in Section 3) to calculate the perpendicular
# distances based on the sighting distances and sighting angles.



# The second required dataset is a transect data.frame
# Each row is a transect, and the siteID and length columns are required (as named)
# Other columns (e.g., transect-level covariates) are ignored, but may be useful in modeling abundance later
data(sparrow.transects)
head(sparrow.transects)
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


sparrow.detections$dist <- perp.dists(obs.dist=sparrow.detections$sightdist, obs.angle=sparrow.detections$sightangle)

sparrow.detections <- sparrow.detections[, -which(names(sparrow.detections) %in% c("sightdist", "sightangle"))]                                                                  
head(sparrow.detections)



# Explore the distribution of distances.
hist(sparrow.detections$dist, col="grey", main="", xlab="distance (m)")
rug(sparrow.detections$dist)
summary(sparrow.detections$dist)


# Next, fit a detection function (plotted as a red line) using `F.dfunc.estim`.  For now, we will proceed using the
# half-normal likelihood as the detection function, but in Section 5 of this tutorial, we demonstrate how to run an
# automated process that fits multiple detection functions and compares them using AICc.  Note that distances greater
# than 150 m are quite sparse, so here we right-truncate the data, tossing out detections where `dist` > 150.
sparrow.dfunc <- F.dfunc.estim(sparrow.detections, likelihood="halfnorm", w.hi=150)
plot(sparrow.dfunc)
sparrow.dfunc


# The effective strip width (ESW) is the key information from the detection function that will be used to next estimate
# abundance (or density).  The ESW is calculated by integrating under the detection function.  A survey with imperfect
# detection and ESW equal to *X* effectively covers the same area as a study with perfect detection out to a distance
# of *X*.  See the help documentation for `ESW` for details.
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 4) Estimate abundance given the detection function -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Estimating abundance requires the additional information contained in the second required dataset, described earlier,
# where each row represents one transect. Load the example dataset of surveyed sparrow transects from the package.


# Next, estimate abundance (or density in this case) using `F.abund.estim`.  If `area`=1, then density is given in the
# squared units of the distance measurements --- in this case, sparrows per square meter.  Instead, we set `area`=10000
# in order to convert to sparrows per hectare (1 ha == 10,000 m^2^).  The equation used to calculate the abundance
# estimate is detailed in the help documentation for `F.abund.estim`.

# Confidence intervals for abundance are calculated using a bias-corrected bootstrapping method (see `F.abund.estim`),
# and the detection function fit in each iteration of the bootstrap is plotted as a blue line (if `plot.bs=TRUE`).
# Note that, as with all bootstrapping procedures, there may be slight differences in the confidence intervals between
# runs due to so-called 'simulation slop'.  Increasing the number of bootstrap iterations (`R` = 100 used here) may be
# necessary to stabilize CI estimates.

fit <- F.abund.estim(sparrow.dfunc, detection.data=sparrow.detections, transect.data=sparrow.transects,
                     area=10000, R=100, ci=0.95, plot.bs=TRUE)
fit



# Results of interest (such as the abundance estimate and confidence interval) can be extracted from the resulting
# object (here called `fit`).
fit$n.hat
fit$ci

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


auto <- F.automated.CDA(detection.data=sparrow.detections, transect.data=sparrow.transects,
                        w.hi=150, plot=FALSE, area=10000, R=100, ci=0.95, plot.bs=TRUE)


# You can see that the detection function with the lowest AICc value (and thus selected as the 'best') is the negative
# exponential likelihood, with one cosine expansion.

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 6) Model abundance using a simple linear model -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# `Rdistance` can provide you with site-level estimates of abundance that are corrected for imperfect detection. You can
# then use these estimates as the response variable in one of the many modeling frameworks available in Program R that
# you're likely already familiar with (e.g., `lm`, `glm`, `lme4`, etc.).

# Estimate abundance at each site
# The process of generating an overall, study-area estimate of abundance is outlined in the **Rdistance Tutorial for Beginners**.  Use the `by.id` argument (`by.id=TRUE`) within `F.abund.estim` or `F.automated.CDA` to estimate abundance for each site surveyed.


auto <- F.automated.CDA(detection.data=sparrow.detections, transect.data=sparrow.transects,
                        w.hi=150, plot=FALSE, area=10000, R=100, ci=0.95, plot.bs=TRUE, by.id=TRUE)

# Because `by.id=TRUE`, a data.frame of the site-level abundances is stored in the `auto` object under the name `nhat.df`:


head(auto$nhat.df)


# Because distances were measured in meters and `area=10000`, abundance (density) estimates are given as the number of sparrows per hectare.

# Model abundance estimates

# The original transect dataset contained a variable named `sagemean` that represents the average sagebrush cover at each transect.  Suppose we want to examine the relationship between sagebrush cover and sparrow abundance.  First, merge the new data.frame of site-level abundance estimates back to the orignial transect data.frame that includes `sagemean`.



mydata <- merge(sparrow.transects, auto$nhat.df, by="siteID")
head(mydata)

# 
# 
# # We now have both variables in the same data.frame.  For the sake of demonstration, we will fit a simple linear regression model using the `lm` function to assess the relationship between sagebrush cover and sparrow density (per ha).  We see that sparrow density increases with sagebrush cover, and density nearly quadruples across the range of sagebrush cover surveyed.
# 
# 
# mod <- lm(nhat ~ sagemean, data=mydata)
# summary(mod)
# 
# 
# 
# plot(mydata$nhat ~ mydata$sagemean, xlab="Sagebrush Cover (%)", ylab="Sparrow Density (birds per ha +/- 95% CI)")
# 
# pred.sage <- seq(min(mydata$sagemean), max(mydata$sagemean), by=0.5)
# pred.nhat <- data.frame(predict(mod, newdata=data.frame(sagemean=pred.sage), interval="confidence"))
# 
# lines(pred.sage, pred.nhat$fit, col="blue", lwd=3)
# lines(pred.sage, pred.nhat$lwr, col="blue", lwd=2, lty="dashed")
# lines(pred.sage, pred.nhat$upr, col="blue", lwd=2, lty="dashed")



# Conclusion
# By using `Rdistance` to estimate site-level abundance, you can account for imperfect detection, and use the modeling framework you choose to then relate site-level covariates to abundance.  We demonstrated this workflow using `Rdistance`'s `F.automated.CDA` function to fit a detection function and provide abundance estimates for each site, and the standard `lm` function to model the relationship between a habitat covariate and sparrow density.

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 7) Model abundance using a generalized linear mixed-effects model -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# The variance-mean ratio is much larger than 1 (the expectation for Poisson), so we choose negative-binomial
var(mydata$rawcount) / mean(mydata$rawcount)
# mean(mydata$rawcount)^2/(var(mydata$rawcount)-mean(mydata$rawcount))

# Add transect cluster group to transect data.frame
sparrow.transects$grp <- as.factor(substring(sparrow.transects$siteID, 1, 1))



# Merge sage data to detection data (sage will be the strata)
sparrow.detections <- merge(sparrow.detections, sparrow.transects[, c("siteID", "sage")])


# Create strata for dfuncs
(strata <- unique(sparrow.detections[, "sage"]))


# Initiate output table to store Rdistance output
out.tab <- data.frame(sage=NA, esw=NA, w.hi=NA, prob=NA, model=NA)
nhats <- data.frame(siteID=NA, rawcount=NA, nhat=NA, strata=NA)


# Loop through strata and fit dfuncs, estimate density
for(i in 1:length(strata)){
  # Subset detection and transect data to current strata
  d <- sparrow.detections[sparrow.detections$sage==strata[i], ]
  t <- sparrow.transects[sparrow.transects$sage==strata[i], ]
  
  # Pick dfunc using AICc and estimate transect-level density (per ha if area=1e4, per km2 if area=1e6)
  ans <- F.automated.CDA(d, t, w.hi=quantile(d$dist, 0.95), area=1e4, x.scl="max",
                         expansions=0:2, ci=NULL, plot=FALSE, plot.bs=TRUE, by.id=TRUE)
  
  # Store n.hats
  tmp <- ans$nhat.df
  tmp$strata <- strata[i]
  # nhats <- merge(nhats, tmp, by="siteID", all.x=TRUE)
  nhats <- rbind(nhats, tmp)
  nhats <- na.omit(nhats)
  
  
  # Store other output
  out.tab[i, "sage"] <- as.character(strata[i])
  out.tab[i, "esw"] <- round(ans$esw, 1)
  out.tab[i, "w.hi"] <- round(unname(ans$w.hi), 1)
  out.tab[i, "prob"] <- round(out.tab[i, "esw"] / out.tab[i, "w.hi"], 3)
  out.tab[i, "model"] <- paste(ans$like.form, ans$series, ans$expansions, sep=" ")
  
}

# View results
out.tab
head(nhats)






# Create one data frame for regression
mydata <- merge(sparrow.transects, nhats, by="siteID")
head(mydata)


plot(rawcount ~ sagemean, mydata)
plot(nhat ~ sagemean, mydata)





# Store the effective strip width
mydata <- merge(mydata, out.tab[c("sage", "esw", "prob")])








# GLM

require(MASS)
mydata$Lesw <- log(mydata$esw)

fit.glm <- glm.nb(rawcount ~ offset(Lesw) + sagemean, data=mydata)
summary(fit.glm)


pred.data <- data.frame(sagemean= seq(min(mydata$sagemean), max(mydata$sagemean), length.out=100))

pred.data$Lesw <- 3.6788
pred.data[pred.data$sagemean < 10, "Lesw"] <- 4.1682

pred.data <- cbind(pred.data, predict(fit.glm, pred.data, type="link", se.fit=TRUE))
pred.data <- within(pred.data, {
  Count <- exp(fit)
  Low <- exp(fit - 1.96 * se.fit)
  Upp <- exp(fit + 1.96 * se.fit)
})


# Plotting count
plot(mydata$rawcount ~ mydata$sagemean, xlab="Sagebrush Cover (%)", ylab="Sparrow Count")
lines(pred.data$sagemean, pred.data$Count, col="red", lwd=3)
lines(pred.data$sagemean, pred.data$Low, col="red", lwd=2, lty="dashed")
lines(pred.data$sagemean, pred.data$Upp, col="red", lwd=2, lty="dashed")
abline(v=10, lty="dotted", col="blue", lwd=3)
text(5.5, 14.5, "Low Sage", col="blue")
text(16, 14.5, "High Sage", col="blue")


# Note that the abrupt step is due to different detection probabilities (and ESWs) between low and high sagebrush cover
# Raw counts
plot(mydata$rawcount ~ mydata$sagemean, xlab="Sagebrush Cover (%)", ylab="Raw Count")
abline(v=10, lty="dotted", col="blue", lwd=3)
text(5.5, 14.5, "Low Sage", col="blue")
text(16, 14.5, "High Sage", col="blue")

# Densities (low sage gets deflated)
plot(mydata$nhat ~ mydata$sagemean, xlab="Sagebrush Cover (%)", ylab="Density (birds/ha)")
abline(v=10, lty="dotted", col="blue", lwd=3)
text(5.5, 3.65, "Low Sage", col="blue")
text(16, 3.65, "High Sage", col="blue")
arrows(5.5, 3.1, 5.5, 2.2, col="blue", lwd=2)

# Converting predicted count to density and plotting
pred.data$D <- (pred.data$Count*1e4)/(2*exp(pred.data$Lesw)*500)
pred.data$DLow <- (pred.data$Low*1e4)/(2*exp(pred.data$Lesw)*500)
pred.data$DUpp <- (pred.data$Upp*1e4)/(2*exp(pred.data$Lesw)*500)

plot(mydata$nhat ~ mydata$sagemean, xlab="Sagebrush Cover (%)", ylab="Density (birds/ha)")
lines(pred.data$sagemean, pred.data$D, col="red", lwd=3)
lines(pred.data$sagemean, pred.data$DLow, col="red", lwd=2, lty="dashed")
lines(pred.data$sagemean, pred.data$DUpp, col="red", lwd=2, lty="dashed")
abline(v=10, lty="dotted", col="blue", lwd=3)
text(5.5, 3.65, "Low Sage", col="blue")
text(16, 3.65, "High Sage", col="blue")






# GLMM

require(lme4)
fit.mixed <- glmer.nb(rawcount ~ offset(Lesw) + sagemean + (1|grp), data=mydata)
summary(fit.mixed, cor=FALSE)
plot(fit.mixed)


plot(mydata$nhat ~ mydata$sagemean, xlab="Sagebrush Cover (%)", ylab="Sparrow Density (birds per ha +/- 95% CI)")








pred.data <- data.frame(sagemean= seq(min(mydata$sagemean), max(mydata$sagemean), length.out=100))

pred.data$Lesw <- 3.6788
pred.data[pred.data$sagemean < 10, "Lesw"] <- 4.1682

pred.data <- cbind(pred.data, predict(fit.mixed, pred.data, type="link", se.fit=TRUE))
pred.data <- within(pred.data, {
  Count <- exp(fit)
  Low <- exp(fit - 1.96 * se.fit)
  Upp <- exp(fit + 1.96 * se.fit)
})










pred.sage <- data.frame(sagemean = seq(min(mydata$sagemean), max(mydata$sagemean), by=0.5))
mod.pred <- bootMer(fit.mixed, # model object
                    FUN = function(x) (predict(x, pred.sage, re.form=NA)), # predict function; re.form = NA means will only give output for fixed effects
                    use.u = FALSE, # change to TRUE to only boots. fixed effects and keep randoms constant. FALSE is more conservative and what Bolker recommends.
                    nsim=1000, # # of simulations for parametric bootstrap
                    type = "parametric") # parametric bootstrap



pred.nhat <- data.frame(predict(fit.mixed, newdata=data.frame(sagemean=pred.sage), interval="confidence"))

lines(pred.sage, pred.nhat$fit, col="red", lwd=3)
lines(pred.sage, pred.nhat$lwr, col="red", lwd=2, lty="dashed")
lines(pred.sage, pred.nhat$upr, col="red", lwd=2, lty="dashed")



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#













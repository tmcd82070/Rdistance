## ---- Rdistance test file - line transects without covariates ----

require(Rdistance)

trunc <- 100
sparrowSiteData$zBare <- with(sparrowSiteData, (bare - (mean(bare)))/sd(bare))
sparrow.dfunc.zBare <- dfuncEstim(formula=dist~zBare, 
                            detectionData=sparrowDetectionData, 
                            siteData = sparrowSiteData,
                            likelihood="halfnorm", 
                            w.hi=trunc)
fit.zBare <- abundEstim(dfunc=sparrow.dfunc.zBare, 
                  detectionData=sparrowDetectionData, 
                  siteData=sparrowSiteData,
                  area=10000, ci=NULL, plot.bs=FALSE)

# -------------------------------------------------
# context("Transects, one covariate, bySite=FALSE")


test_that("Coefficients are correct", {
  expect_equal( round(coefficients(sparrow.dfunc.zBare),4) , c("(Intercept)" = 3.8804, "zBare" = 0.1092 ))
})


test_that("Standard errors are correct", {
  expect_equal( round(sqrt(diag(sparrow.dfunc.zBare$varcovar)), 5), c(0.06302, 0.04175))
})

test_that("AIC is correct", {
  expect_equal( round(AIC(sparrow.dfunc.zBare)[1], 3), 2965.787)
})

test_that("ESW is correct", {
  eswVals <- c(45.675, 46.942, 47.583, 48.067, 48.472, 
               48.879, 49.328, 53.019, 54.743, 54.827, 
               54.996, 55.165, 55.461, 55.503, 55.756, 
               56.263, 56.432, 56.475, 56.559, 56.855, 
               56.940, 57.151, 57.193, 57.362, 57.658, 
               57.785, 57.912, 57.996, 58.123, 58.249, 
               58.545, 59.093, 59.135, 59.850, 60.396, 
               60.480, 60.606, 60.648, 61.442, 61.484, 
               62.025, 62.066, 62.482, 62.978, 63.473, 
               63.760, 63.924, 64.129, 64.537, 64.619, 
               64.985, 65.066, 65.672, 65.833, 66.514)
  expect_equal( sort(unique(round(ESW(sparrow.dfunc.zBare), 3))), eswVals)
})

test_that("Abundance is correct",{
  expect_equal(round(fit.zBare$n.hat, 4), 0.8753)
})


# -------------------------------------------------------------

# context("Transects, two covariates, bySite=FALSE")

sparrow.dfunc <- dfuncEstim(formula=dist~zBare + observer, 
                            detectionData=sparrowDetectionData, 
                            siteData = sparrowSiteData,
                            likelihood="halfnorm", 
                            w.hi=trunc)

test_that("Coefficients are correct", {
  BETA <- c("(Intercept)" = 3.92592, 
            "zBare" = 0.09245, 
            "observerobs2" = -0.00733, 
            "observerobs3" = -0.02242, 
            "observerobs4" = -0.16883, 
            "observerobs5" = -0.08784)
  beta <- coefficients(sparrow.dfunc)
  expect_equal(round(beta,5), BETA)
})

test_that("Standard errors are correct", {
  seCoef <- sqrt(diag(sparrow.dfunc$varcovar))
  SECOEF <- c(0.12917, 0.04569, 0.20532, 0.17118, 0.17947, 0.17411)
  expect_equal(round(seCoef,5), SECOEF)
})

test_that("Avg ESW is correct",{
  expect_equal(round(mean(ESW(sparrow.dfunc)),3), 56.186)
})


test_that("Observer predictions are correct", {
  newdata <- data.frame(zBare=rep(0, 5),
                       observer=paste0("obs",1:5))
  params <- predict(sparrow.dfunc, newdata = newdata)
  PARAMS <- matrix(c(50.700, 50.329, 49.576, 42.824, 46.436), 5, 1)
  dimnames(PARAMS) <- list(c("1","2","3","4","5"), NULL)
  expect_equal(round(params,3), PARAMS)
})

test_that("zBare predictions are correct", {
  newdata <- data.frame(zBare=c(-1,0,1),
                        observer=factor("obs1", levels=paste0("obs",1:5)))
  params <- predict(sparrow.dfunc, newdata = newdata)
  PARAMS <- matrix(c(46.223, 50.7, 55.61), 3, 1)
  dimnames(PARAMS) <- list(c("1","2","3"), NULL)
  expect_equal(round(params,3), PARAMS)
})


test_that("plot returns", {
  newdata <- data.frame(zBare=c(-1,0,1),
                        observer=factor("obs1", levels=paste0("obs",1:5)))
  tmp <- plot(sparrow.dfunc, newdata = newdata)
  expect_s3_class(tmp, "dfunc")
})


set.seed(88888)
tmp <- capture_output(
  fit <- abundEstim(dfunc=sparrow.dfunc, 
                    detectionData=sparrowDetectionData, 
                    siteData=sparrowSiteData,
                    area=10000, R=5, ci=0.95, plot.bs=TRUE)
)

test_that("Bootstrap CI correct",{
  CI <- c("7.307443%" = 0.75218, "99.3181%" = 0.98943)
  expect_equal(round(fit$ci, 5), CI)
})


# ---------------------------------------------
# context("Transects, two covariates, bySite=TRUE")

fitBy <- abundEstim(dfunc=sparrow.dfunc, 
                    detectionData=sparrowDetectionData, 
                    siteData=sparrowSiteData,
                    area=10000, ci=NULL, bySite=TRUE,
                    plot.bs = FALSE)

test_that("dim of return is correct", {
  expect_equal(dim(fitBy), c(72, 15))
})

test_that("mean density is correct", {
  expect_equal(round(mean(fitBy$density), 5), round(fit$n.hat, 5))
})

tmp <- capture_output(
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
                     likelihoods=c("halfnorm", "hazrate","negexp"),
#                     likelihoods=c("halfnorm", "hazrate","uniform","negexp"),
                     expansions=0)
)

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




# ==============================================================
# context("Comparison to Distance")

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
tmp <- capture.output(
  ds.fit <- ds(data=d.data, 
             formula= ~zBare, 
             region.table=d.region, 
             sample.table=d.sample, 
             obs.table=d.obs,
             truncation=trunc, 
             transect="line", 
             key="hn", 
             adjustment=NULL, 
             dht.group=FALSE)
)

test_that("AICs match", {
  expect_equal(round(AIC(sparrow.dfunc.zBare)[1],1), round(AIC(ds.fit)$AIC,1))
})

test_that("Abundances match", {
  expect_lt( abs(fit.zBare$n.hat - ds.fit$dht$individuals$N$Estimate), 3.5e-6 )
})

# ds.fit$dht$individuals$N$Estimate
# ds.fit$dht$individuals$N$lcl
# ds.fit$dht$individuals$N$ucl

test_that("Coefficients match", {
  expect_true( all( abs(coef(fit.zBare) - 
             ds.fit$ddf$ds$aux$ddfobj$scale$parameters) <
             c(0.0002, 0.0002)) )

})



## ----- Rdistance test file -------

require(Rdistance)

data(sparrowDetectionData)
data(sparrowSiteData)

# --------------------------------------------------------
context("bySite with site-level covs")

# Fit detection function with site-level covar
dfunc <- dfuncEstim(dist ~ observer,
                    likelihood="halfnorm", 
                    w.hi=150,
                    detectionData=sparrowDetectionData,
                    siteData=sparrowSiteData)

fit <- abundEstim(dfunc = dfunc,
                  detectionData = sparrowDetectionData,
                  siteData = sparrowSiteData,
                  area = 1e4,
                  ci = NULL,
                  bySite=TRUE)

test_that("dim of returned df correct", {
  expect_equal(dim(fit), c(72,14))
})

test_that("no missing densities", {
  expect_equal(sum(is.na(fit$density)), 0)
})

test_that("abundance correct", {
  expect_equal(round(sum(fit$density), 2), 60.39)
})


# --------------------------------------------------------
context("bySite with detection-level covars")

# groupsize is detection-level covariate
dfunc <- dfuncEstim(dist ~ groupsize,
                    likelihood="halfnorm", w.hi=150,
                    detectionData=sparrowDetectionData,
                    siteData=sparrowSiteData)

test_that("parameter estimates correct", {
  expect_equal(round(dfunc$parameters, 3), c("(Intercept)" = 3.762, groupsize = 0.140))
})

test_that("bySite == TRUE errors out", {
  # Should not work (error), 
  expect_error(
      abundEstim(dfunc = dfunc,
                    detectionData = sparrowDetectionData,
                    siteData = sparrowSiteData,
                    area = 1e4,
                    ci = NULL,
                    bySite=TRUE), 
      "Cannot estimate site-level abundance")
})



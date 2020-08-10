##line-transect workflow, no covariates

require(Rdistance)

context("Test the line-transect workflow no covars")

## load example data and assign variables needed for testing
data(sparrowDetectionData)
data(sparrowSiteData)

trunc <- 100

sparrow.dfunc <- dfuncEstim(
  formula=dist~1, 
  detectionData=sparrowDetectionData, 
  likelihood="halfnorm", 
  w.hi=trunc)

fit <- abundEstim(dfunc=sparrow.dfunc, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                  area=10000, ci=NULL, plot.bs=FALSE)

fitSite <- abundEstim(dfunc=sparrow.dfunc, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                      area=10000, ci=NULL, bySite=TRUE)

invisible(capture.output(auto <- autoDistSamp(formula=dist~1, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                     w.hi=trunc, plot=FALSE, area=10000, ci=NULL, plot.bs=FALSE)))

##-----Test abundEstim

test_that("ESW is calculated correctly", {
  
  expect_equal(ESW(sparrow.dfunc), 56.30084, tolerance = 0.00001)
})
  
test_that("abundance estimate correct", {  
  
  expect_equal(fit$n.hat, 0.8634171)
  
})

##-----Test fit by site
  
test_that("fit by site operates correctly", {
  
  expect_equal(mean(fitSite$density), 0.8634171 )
})

##-----Test autoDistSamp

test_that("autoDistSamp ESW correct", {
  
  expect_equal(auto$esw, 42.07846, tolerance = 0.00001)
})

test_that("autoDistSamp function selection is correct", {
  
  expect_match(auto$like.form, "negexp")
})
  
test_that("dim of returned autoDistSamp df correct", {
  expect_equal(dim(auto$fitTable), c(41, 6))
})
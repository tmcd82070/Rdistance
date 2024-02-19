# test_effectiveDistance.R
library(Rdistance)
# context("effectiveDistance")

# effectiveDistance is a wrapper for ESW.R and EDR.R
# test will check that effectiveDistance properly routes objects 

# point-transect detection function
data("thrasherDetectionData")
data("thrasherSiteData")
dfunc_point <- dfuncEstim(formula = dist ~ 1,
                          detectionData = thrasherDetectionData,
                          pointSurvey = TRUE,
                          likelihood = "halfnorm")

# line-transect detection function
data("sparrowDetectionData")
data("sparrowSiteData")
dfunc_line <- dfuncEstim(formula = dist ~ 1,
                         detectionData = sparrowDetectionData,
                         siteData = sparrowSiteData,
                         likelihood = "halfnorm", 
                         w.hi = 150)

test_that("pointSurvey object goes to EDR function", {
  expect_equal(effectiveDistance(dfunc_point), EDR(dfunc_point))
})

test_that("lineSurvey object goes to ESW function", {
  expect_equal(effectiveDistance(dfunc_line), ESW(dfunc_line))
})
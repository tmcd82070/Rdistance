# test_ESW.r
library(Rdistance)
context("ESW")

# point transect survey detection function
data(thrasherDetectionData)
dfunc_point <- dfuncEstim(formula = dist ~ 1,
                          detectionData = thrasherDetectionData,
                          likelihood = "halfnorm", 
                          w.hi = 175, 
                          pointSurvey = TRUE)

# line-transect detection function
data("sparrowDetectionData")
data("sparrowSiteData")
dfunc_line <- dfuncEstim(formula = dist ~ 1,
                         detectionData = sparrowDetectionData,
                         siteData = sparrowSiteData,
                         likelihood = "halfnorm", 
                         w.hi = 150)

test_that("pointSurvey object throws an error", {
  expect_error(ESW(dfunc_point), "ESW is for line transects only.  See EDR for the point-transect equivalent.")
})

# target first if statement: hazrate at 0
dfunc_point_case_1 <- dfuncEstim(formula = dist ~ 1,
                                 detectionData = sparrowDetectionData,
                                 likelihood = "hazrate", 
                                 w.hi = 175)

# dfunc_point_case_1.rds stores previously returned object from ESW(dfunc_point_case_1)
# file is located in testthat folder; delete file to reset stored value
# dfunc_point_case_1 enters 1st if statement in ESW
test_that("ESW(dfunc_point_case_1) returns equivalent obj as it did previously", {
  expect_equal_to_reference(ESW(dfunc_point_case_1), "esw_dfunc_point_case_1.rds")
})
 
dfunc_point_covar <- dfuncEstim(formula = dist ~ shrubclass,
                                detectionData = sparrowDetectionData,
                                siteData = sparrowSiteData,
                                likelihood = "halfnorm",
                                w.hi = 150)

newdata <- data.frame(shrubclass=levels(sparrowSiteData$shrubclass))

# skip missing data case
test_that("ESW(dfunc_point_covar, newdata) returns equivalent obj as it did previously", {
  expect_equal_to_reference(ESW(dfunc_point_covar, newdata), "esw_dfunc_point_covar_newdata.rds")
})

# enter missing data case
test_that("ESW(dfunc_point_covar) returns equivalent obj as it did previously", {
  expect_equal_to_reference(ESW(dfunc_point_covar), "esw_dfunc_point_covar.rds")
})

# target if case: is.null( obj$g.x.scl )
dfunc_point_covar$g.x.scl <- NULL
test_that("ESW(dfunc_point_covar g.x.scl=NULL) produces expected warning", {
  expect_warning(ESW(dfunc_point_covar), "g0 unspecified.  Assumed 1.")
})

# nesting equal to reference expectation within warning expectation to get around warning 
# from testthat when reference expectation is run:
# as this is expect_equal_to_reference, we will see a warning when .rds file is initialized on 1st run
dfunc_point_covar$g.x.scl <- NULL
test_that("ESW(dfunc_point_covar g.x.scl=NULL) produces expected warning", {
  expect_warning(expect_equal_to_reference(ESW(dfunc_point_covar), "esw_dfunc_point_covar_null.rds"), "g0 unspecified.  Assumed 1.")
})
# test_EDR.R
library(Rdistance)
# context("EDR")

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

test_that("lineSurvey object throws an error", {
  expect_error(EDR(dfunc_line), "EDR is for point transects only.  See ESW for the line-transect equivalent.")
})

# target first if statement: hazrate at 0
dfunc_point_case_1 <- dfuncEstim(formula = dist ~ 1,
                          detectionData = thrasherDetectionData,
                          likelihood = "hazrate", 
                          w.hi = 175, 
                          pointSurvey = TRUE)

# dfunc_point_case_1.rds stores previously returned object from EDR(dfunc_point_case_1)
# file is located in testthat folder; delete file to reset stored value
# dfunc_point_case_1 enters 1st if statement in EDR
test_that("EDR(dfunc_point_case_1) returns equivalent obj as it did previously", {
  expect_equal_to_reference(EDR(dfunc_point_case_1), "edr_dfunc_point_case_1.rds")
})

data("thrasherDetectionData")
data("thrasherSiteData")
dfunc_point_covar <- dfuncEstim(formula = dist ~ observer,
                          detectionData = thrasherDetectionData,
                          siteData = thrasherSiteData,
                          likelihood = "halfnorm",
                          w.hi = 175,
                          pointSurvey = TRUE)

newdata <- data.frame(observer=factor(c("obs1", "obs2"), levels = paste0("obs", 1:6)))

# skip missing data case
test_that("EDR(dfunc_point_covar, newdata) returns equivalent obj as it did previously", {
  expect_equal_to_reference(EDR(dfunc_point_covar, newdata), "edr_dfunc_point_covar_newdata.rds")
})
# enter missing data case
test_that("EDR(dfunc_point_covar) returns equivalent obj as it did previously", {
  expect_equal_to_reference(EDR(dfunc_point_covar), "edr_dfunc_point_covar.rds")
})
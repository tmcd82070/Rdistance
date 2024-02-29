# Test parseModel
library(Rdistance)

sparrowDf <- Rdistance::RdistDf(sparrowSiteData, sparrowDetectionData)
is.RdistDf(sparrowDf)

parseModel(sparrowDf, dist ~ 1)

# no units on dist

# no units on w.lo

# w.lo equal 0 with no units

# no units on w.hi

# w.hi equal NULL

testthat::test_that("Missing formula errors", {
  testthat::expect_error(parseModel(sparrowDf), "'formula' is required")
})

test_that("AIC(dfunc, criterion=BIC) returns equivalent obj as it did previously", {
  expect_equal_to_reference(AIC(dfunc, criterion="BIC"), "AIC_dfunc_BIC.rds")
})

test_that("AIC(dfunc, criterion=other) returns equivalent obj as it did previously", {
  expect_equal_to_reference(AIC(dfunc, criterion="other"), "AIC_dfunc_AICc.rds")
})
# test_predict.dfunc.r
library(Rdistance)
context("predict.dfunc")

data("sparrowDetectionData")
data("sparrowSiteData")
dfuncObs <- dfuncEstim(formula=dist~observer,
                    detectionData=sparrowDetectionData,
                    siteData = sparrowSiteData,
                    likelihood="halfnorm", w.hi=100, pointSurvey = FALSE,
                    control=RdistanceControls(maxIter=1000))
newdata=data.frame(observer=levels(sparrowSiteData$observer))

test_that("dfuncEstim returns equivalent object as it did previously", {
  expect_equal_to_reference(predict.dfunc(dfuncObs, newdata, "parameters"), "predict.dfunc.rds")
})

x <- 5
test_that("dfuncEstim(non-dfunc object) generates expected error", {
  expect_error(predict.dfunc(x, newdata, "parameters"), "object is not a dfunc")
})

dfuncObs$covars <- NULL
test_that("dfuncEstim(covars = NULL) returns equivalent object as it did previously", {
  expect_equal_to_reference(predict.dfunc(dfuncObs, newdata, "parameters"), "predict.dfunc_covars_null.rds")
})
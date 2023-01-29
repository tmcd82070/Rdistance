# test_print.dfunc.r
library(Rdistance)
# context("print.dfunc")

data(sparrowDetectionData)
dfunc <- dfuncEstim(formula=dist~1,
                    detectionData=sparrowDetectionData,
                    likelihood="halfnorm", w.hi=100, pointSurvey=FALSE)

test_that("print.dfunc(dfunc, criterion=AICc) returns equivalent object as it did previously", {
  invisible(capture.output(expect_equal_to_reference(print.dfunc(dfunc, criterion="AICc"), "print.dfunc_AICc.rds")))
}) 

test_that("print.dfunc(dfunc, criterion=AIC) returns equivalent object as it did previously", {
  invisible(capture.output(expect_equal_to_reference(print.dfunc(dfunc, criterion="AIC"), "print.dfunc_AIC.rds")))
}) 

test_that("print.dfunc(dfunc, criterion=BIC) returns equivalent object as it did previously", {
  invisible(capture.output(expect_equal_to_reference(print.dfunc(dfunc, criterion="BIC"), "print.dfunc_BIC.rds")))
}) 

test_that("print.dfunc(dfunc, criterion=AICc pointSurvey=TRUE) returns equivalent object as it did previously", {
  dfunc$pointSurvey <- TRUE
  invisible(capture.output(expect_equal_to_reference(print.dfunc(dfunc, criterion="AICc"), "print.dfunc_AICc_pointSurvey.rds")))
})

data(sparrowSiteData)
dfuncSmu <- dfuncSmu(dist~1, sparrowDetectionData, w.hi=150)

test_that("print.dfunc(dfuncSmu) returns equivalent object as it did previously", {
  invisible(capture.output(expect_equal_to_reference(print.dfunc(dfuncSmu, criterion="AICc"), "print.dfunc_AICc_dfuncSmu.rds")))
}) 

dfuncObs <- dfuncEstim(formula=dist~observer,
                       detectionData=sparrowDetectionData,
                       siteData=sparrowSiteData,
                       likelihood="halfnorm", w.hi=100, pointSurvey=FALSE,
                       control=RdistanceControls(maxIter=1000))

test_that("print.dfunc(dfuncObs) returns equivalent object as it did previously", {
  invisible(capture.output(expect_equal_to_reference(print.dfunc(dfuncObs, criterion="AICc"), "print.dfunc_AICc_dfuncObs.rds")))
}) 

test_that("print.dfunc(dfuncObs pointSurvey=TRUE) returns equivalent object as it did previously", {
  dfuncObs$pointSurvey <- TRUE
  invisible(capture.output(expect_equal_to_reference(print.dfunc(dfuncObs, criterion="AICc"), "print.dfunc_AICc_dfuncObs_pointSurvey.rds")))
}) 


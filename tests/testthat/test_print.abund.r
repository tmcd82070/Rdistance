# test_print.abund.r
library(Rdistance)
context("print.abund")

data(sparrowDetectionData)
data(sparrowSiteData)
dfunc <- dfuncEstim(formula=dist~1,
                    detectionData=sparrowDetectionData,
                    likelihood="halfnorm", w.hi=100, pointSurvey=FALSE)
fit <- abundEstim(dfunc, detectionData=sparrowDetectionData,
                  siteData=sparrowSiteData, area=10000, R=20, ci=0.95,
                  plot.bs=TRUE, bySite=FALSE)

fit$nItersConverged <- 2
test_that("print.abund(fit: nItersConverged < length(B)) returns expected warning", {
  expect_warning(capture.output(print.abund(fit)), "The proportion of non-convergent bootstrap iterations is high.")
})
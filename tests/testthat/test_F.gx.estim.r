# test_F.gx.estim.r
library(Rdistance)
# context("F.gx.estim.r")

data(sparrowDetectionData)
data(sparrowSiteData)

test_that("F.gx.extim(hn.dfunc) returns equivalent ojbect as it did previously", {
  hn.dfunc <- dfuncEstim(formula=dist~1,
                      detectionData=sparrowDetectionData,
                      likelihood="halfnorm", w.hi=100)
  expect_equal_to_reference(
    F.gx.estim(hn.dfunc),
    "F.gx.estim_halfnorm.rds")
})

test_that("F.gx.extim(hn.dfunc x.scl < w.lo) returns equivalent ojbect as it did previously", {
  hn.dfunc <- dfuncEstim(formula=dist~1,
                      detectionData=sparrowDetectionData,
                      likelihood="halfnorm", w.hi=100)
  hn.dfunc$w.lo <- 1 
  expect_equal_to_reference(
    expect_warning(F.gx.estim(hn.dfunc)),
    "F.gx.estim_halfnorm_w.lo.rds")
})
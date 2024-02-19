# test_F.nLL.r
library(Rdistance)
# context("F.nLL")

data(sparrowDetectionData)
data(sparrowSiteData)
dfunc <- dfuncEstim(formula=dist~1,
                    detectionData=sparrowDetectionData,
                    likelihood="halfnorm", w.hi=100)

test_that("F.nLL(...for.optim=F) returns equivalent object as it did previously", {
  expect_equal_to_reference(F.nLL(dfunc$parameters, dfunc$dist, covars = NULL, dfunc$like.form, w.lo=0, w.hi=max(dfunc$dist), dfunc$series, expansions=0, dfunc$pointSurvey, for.optim = F), "F.nLL_optim_F.rds")
}) 

test_that("F.nLL(...for.optim=T) returns equivalent object as it did previously", {
  expect_equal_to_reference(F.nLL(dfunc$parameters, dfunc$dist, covars = NULL, dfunc$like.form, w.lo=0, w.hi=max(dfunc$dist), dfunc$series, expansions=0, dfunc$pointSurvey, for.optim = T), "F.nLL_optim_T.rds")
}) 
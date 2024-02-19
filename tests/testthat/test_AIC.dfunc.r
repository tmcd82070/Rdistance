# test_AIC.dfunc.r
library(Rdistance)
# context("AIC.dfunc")

data(sparrowDetectionData)
   
dfunc <- dfuncEstim(dist~1,
         detectionData=sparrowDetectionData, 
         w.hi=150)
   
test_that("AIC(dfunc, criterion=AIC) returns equivalent obj as it did previously", {
  expect_equal_to_reference(AIC(dfunc, criterion="AIC"), "AIC_dfunc_AIC.rds")
})

test_that("AIC(dfunc, criterion=BIC) returns equivalent obj as it did previously", {
  expect_equal_to_reference(AIC(dfunc, criterion="BIC"), "AIC_dfunc_BIC.rds")
})

test_that("AIC(dfunc, criterion=other) returns equivalent obj as it did previously", {
  expect_equal_to_reference(AIC(dfunc, criterion="other"), "AIC_dfunc_AICc.rds")
})
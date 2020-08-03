# test_F.maximize.g.r
library(Rdistance)
context("F.maximize.g.r")

test_that("F.maximize.g returns equivalent object as it did previously", {
  dfunc <- dfuncEstim(formula=dist~1,detectionData=sparrowDetectionData,likelihood="halfnorm", w.hi=100)

  result <- F.maximize.g(dfunc) 
    
  expect_equal_to_reference(result, "F.maximize.g.rds")
})
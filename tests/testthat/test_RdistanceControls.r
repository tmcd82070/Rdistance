# test_RdistanceControls.r
library(Rdistance)
# context("RdistanceControls")

test_that("RdistanceControls() returns equivalent ojbect as it did previously", {
  expect_equal_to_reference(RdistanceControls(), "RdistanceControls.rds")
})
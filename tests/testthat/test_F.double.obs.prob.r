# test_F.double.obs.prob.r
library(Rdistance)

set.seed(538392)
obsrv <- data.frame( obsby.1=rbinom(100,1,.75), obsby.2=rbinom(100,1,.5) )
F.double.obs.prob( obsrv, observer=1 )
x<-F.double.obs.prob( obsrv, observer=2 )
x<-F.double.obs.prob( obsrv, observer="both" )

# using hard coded values rather than .rds file here 
# as data set is generated using seeded random functions (so not likely to change)

test_that("F.double.obs.prob(obsrv, observer=1) returns expected value for data given", {
  expect_equal(F.double.obs.prob(obsrv, observer=1), 0.6226415)
})

test_that("F.double.obs.prob(obsrv, observer=2) returns expected value for data given", {
  expect_equal(F.double.obs.prob(obsrv, observer=2), 0.478260869565217)
})

test_that("F.double.obs.prob(obsrv, observer=both) returns expected value for data given", {
  expect_equal(F.double.obs.prob(obsrv, observer="both"), 0.803117309269893)
})

obsrv_b <- data.frame( obsby.1=rbinom(100,1,.75), obsby.3=rbinom(100,1,.5) )

test_that("F.double.obs.prob(obsrv_b, observer=1 generates appropriate error message", {
  expect_error(F.double.obs.prob(obsrv_b, observer=1), "Variables 'obsby.1' and 'obsby.2' not found in input data frame.")
})

test_that("F.double.obs.prob(obsrv, observer=foo generates appropriate error message", {
  expect_error(F.double.obs.prob(obsrv, observer="foo"), "Inappropriate 'observer' parameter")
})

test_that("F.double.obs.prob(obsrv, observer=3 generates appropriate error message", {
  expect_error(F.double.obs.prob(obsrv, observer=3), "Inappropriate 'observer' parameter")
})
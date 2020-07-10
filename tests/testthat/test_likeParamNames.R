# test_likeParamNames.R
library(Rdistance)
context("likeParamNames")

test_that("input=halfnorm outputs=halfnorm sigma", {
  expect_equal(likeParamNames("halfnorm"), c("Sigma"))
})

test_that("input=hazrate outputs=Sigma Beta", {
  expect_equal(likeParamNames("hazrate"), c("Sigma", "Beta"))
})

test_that("input=uniform outputs=Threshold Knee", {
  expect_equal(likeParamNames("uniform"), c("Threshold", "Knee"))
})

test_that("input=negexp outputs=Beta", {
  expect_equal(likeParamNames("negexp"), c("Beta"))
})

test_that("input=Gamma outputs=Shape Scale", {
  expect_equal(likeParamNames("Gamma"), c("Shape", "Scale"))
})

# generate user defined function for testing 
triangular.like <- function(a, dist, covars=NULL, 
                            pointSurvey=FALSE, w.lo=0, w.hi, 
                            series="", expansions=0, scale=TRUE){
  L <- (2/a)*(1 - dist/a)
  L[ L < 0 ] <- 0
  L
}

triangular.start.limits <- function(dist, expansions, w.lo, w.hi){
  list(start=max(dist)*.75,
       lowlimit=w.lo,
       highlimit=w.hi,
       names="Max")
}

test_that("input=triangular outputs=Max", {
  expect_equal(likeParamNames("triangular"), c("Max"))
})


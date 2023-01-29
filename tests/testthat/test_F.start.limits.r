# F.start.limits.r
library(Rdistance)
# context("F.start.limits")

test_that("F.start.limits(uniform) returns type=list", {
  expect_type(F.start.limits("uniform", 1, 0, 1000), "list")
})

test_that("F.start.limits(halfnorm) returns type=list", {
  expect_type(F.start.limits("halfnorm", 1, 0, 1000, 500*runif(100)), "list")
})

test_that("F.start.limits(hazrate) returns type=list", {
  expect_type(F.start.limits("hazrate", 1, 0, 1000), "list")
})

test_that("F.start.limits(negexp) returns type=list", {
  expect_type(F.start.limits("negexp", 1, 0, 1000), "list")
})

test_that("F.start.limits(Gamma) returns type=list", {
  expect_type(F.start.limits("Gamma", 0, 0, 1000, 1000*runif(100)), "list")
})

test_that("F.start.limits(hazrate + covars) returns type=list", {
  expect_type(F.start.limits("hazrate", 1, 0, 1000, covars = cbind(c(1,2,3),c(4,5,6))), "list")
})

test_that("F.start.limits(uniform + covars) returns type=list", {
  expect_type(F.start.limits("uniform", 1, 0, 1000, covars = cbind(c(1,2,3),c(4,5,6))), "list")
})

test_that("F.start.limits(negexp + covars) returns type=list", {
  expect_type(F.start.limits("negexp", 1, 0, 1000, covars = cbind(c(1,2,3),c(4,5,6))), "list")
})

test_that("F.start.limits(Gamma + covars) returns type=list", {
  expect_type(F.start.limits("Gamma", 0, 0, 1000, 1000*runif(100), covars = cbind(c(1,2,3),c(4,5,6))), "list")
})

test_that("F.start.limits(user_def + covars) returns type=list", {
  expect_error(F.start.limits("user_def", 1, 0, 1000))
})
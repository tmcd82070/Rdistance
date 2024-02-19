#test_secondDeriv.r
library(Rdistance)
# context("secondDeriv")

func <- function(x){-x*x} # second derivative should be -2

test_that("secondDeriv(0,function(x){-x*x}) equals -2", {
  expect_equal(secondDeriv(0,func)[1,1], -2)
})

# set eps=2 to trigger case: length(eps) > d
test_that("secondDeriv(0,function(x){-x*x},eps=2) equals -2", {
  expect_equal(secondDeriv(0,func,eps=c(2,0))[1,1], -2)
})

func <- function(x){3 + 5*x^2 + 2*x^3} # second derivative should be 10+12x

test_that("secondDeriv(0,function(x){3 + 5*x^2 + 2*x^3}) equals 10", {
  expect_equal(secondDeriv(0,func)[1,1], 10)
})

func <- function(x){x[1]^2 + 5*x[2]^2} # should be rbind(c(2,0),c(0,10))
out <- cbind(c(2,0),c(0,10))

test_that("secondDeriv(c(1,1),function(x){x[1]^2 + 5*x[2]^2}) equals var out", {
  expect_equal(secondDeriv(c(1,1),func), out)
})
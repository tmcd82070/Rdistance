# test_smu.like.r
library(Rdistance)
# context("smu.like")

set.seed(238642)
d <- abs(rnorm(100))
dfunc <- dfuncSmu(d~1)
 
L <- smu.like(a=dfunc$parameters, 
        dist=dfunc$dist, 
        w.lo=dfunc$w.lo, 
        w.hi=dfunc$w.hi, 
        scale=TRUE)

test_that("smu.like returns expected result", {
  expect_equal(smu.like(a=dfunc$parameters, 
                        dist=dfunc$dist,
                        w.lo=dfunc$w.lo,
                        w.hi=dfunc$w.hi,
                        scale=TRUE), L)
})
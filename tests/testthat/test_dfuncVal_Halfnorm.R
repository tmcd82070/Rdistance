#' @description Test specific values of parameters. Other tests, 
#' i.e., test_dfuncEstim, simply test whether the inputs to dfuncEstim 
#' cause errors.  These other routines do not check that routines arrive
#' at the same answer. 
#' 
library(Rdistance)
data("sparrowDetectionData")
data("sparrowSiteData")

w.lo <- 0
w.hi <- units::set_units(150, "m")

fit <- dfuncEstim(formula = dist ~ 1 + offset(groupsize)
                 , detectionData = sparrowDetectionData
                 , likelihood = "halfnorm"
                 , w.lo = w.lo
                 , w.hi = w.hi
                 , pointSurvey = FALSE
                 , expansions = 0
                 , series = "cosine"
                 , x.scl = 0
                 , g.x.scl = 1
                 , outputUnits = "m"
      )

nhat <- abundEstim(fit
                   , sparrowDetectionData
                   , sparrowSiteData
                   , area = units::set_units(10, "ha")
                   , ci = NULL)

correct.params <- c(Sigma = 49.87369,  4.056892 )
correct.esw <- units::set_units(62.34277, "m")
correct.nHat <- 8.265237
tol <- 0.00001

testthat::test_that("Halfnorm:params", {
  testthat::expect_equal(object = c(fit$parameters, fit$varcovar)
                         , expected = correct.params 
                         , tolerance = tol)
})

testthat::test_that("Halfnorm:esw", {
  testthat::expect_equal(object = effectiveDistance(fit)
                         , expected = correct.esw
                         , tolerance = tol)
})

testthat::test_that("Halfnorm:nHat", {
  testthat::expect_equal(object = nhat$n.hat
                         , expected = correct.nHat
                         , tolerance = tol)
})


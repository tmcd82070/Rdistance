#' @description Test specific values of parameters. Other tests, 
#' i.e., test_dfuncEstim, simply test whether the inputs to dfuncEstim 
#' cause errors.  These other routines do not check that routines arrive
#' at the same answer. 
#' 
library(Rdistance)
library(testthat)
data("sparrowDetectionData")
data("sparrowSiteData")

w.lo <- 0
w.hi <- units::set_units(150, "m")

sparrowDf <- RdistDf(sparrowSiteData
                   , sparrowDetectionData)

# No covariates ----
testthat::test_that("triangle w/ no covariates, same value",{
  intercept <- c(
    "(Intercept)" = 4.9836579648961771838
  )
  loglik <-  -1647.2329735461735254

  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = "triangle"
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_equal(fit$par, intercept)
  testthat::expect_equal(fit$loglik, loglik)}
)

# Continuous covariate ----

testthat::test_that("triangle w/ cont covar, same value", {
  intercept <- c(
    "(Intercept)" = 4.4377817190722748108556,
    "bare"        = 0.0079119179363036033076
  )
  loglik <- -1659.4548951814647353
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = "triangle"
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_equal(fit$par, intercept)
  testthat::expect_equal(fit$loglik, loglik)
  })

# Factor Covariate ----
testthat::test_that("triangle w/ factor covar, same value",{
  intercept <- c(
    "(Intercept)" =  4.8442653498491576868901,
    "observerobs2" =  0.1955710420447245023734,
    "observerobs3" = -0.0060148279270844972125,
    "observerobs4" = -0.2754127763996739308716,
    "observerobs5" =  0.0866599375800209820175
  )
  loglik <- -1657.2413886513998023
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
                                 , likelihood = "triangle"
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_equal(fit$par, intercept)
  testthat::expect_equal(fit$loglik, loglik)
})


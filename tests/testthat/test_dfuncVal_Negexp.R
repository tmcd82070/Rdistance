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
testthat::test_that("negexp w/ no covariates, same value",{
  intercept <- c(
    "(Intercept)" = -3.760954
    )
  loglik <- -1630.8929773267443
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = "negexp"
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_equal(round(fit$par,6), 
                         round(intercept,6))
  testthat::expect_equal(fit$loglik, loglik)}
)

# Continuous covariate ----

testthat::test_that("negexp w/ cont covar, same value", {
  intercept <- c(
    "(Intercept)" = -2.98953184,
    "bare"        = -0.01359075
    )
  loglik <- -1626.39078111867
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = "negexp"
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_equal(round(fit$par, 6), 
                         round(intercept, 6))
  testthat::expect_equal(fit$loglik, loglik)
  })

# Factor Covariate ----
testthat::test_that("negexp w/ factor covar, same value",{
  intercept <- c(
  "(Intercept)" =   -3.813825,
  "observerobs2"= -0.149295,
  "observerobs3"=  0.048917,
  "observerobs4"=  0.365778,
  "observerobs5"=  0.019506
    )
  loglik <- -1628.3243420088
    fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
                                   , likelihood = "negexp"
                                   , w.lo = w.lo
                                   , w.hi = w.hi
                                   , expansions = 0
                                   , series = "cosine"
                                   , x.scl = 0
                                   , g.x.scl = 1
                                   , outputUnits = "m"
    )
    testthat::expect_equal(round(fit$par,6), 
                           round(intercept,6))
    testthat::expect_equal(fit$loglik, loglik)
})


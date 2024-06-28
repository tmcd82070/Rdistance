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
testthat::test_that("Hazrate w/ no covariates, same value",{
  intercept <- c(
    "(Intercept)" = 3.774728574,
    "k"           = 2.405957093    )
  loglik <- -1631.7955344829936166
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = "hazrate"
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

testthat::test_that("hazrate w/ cont covar, same value", {
  intercept <- c(
    "(Intercept)" =3.06243871,
    "bare"        =0.01325053,
    "k"           =2.54787816
    )
  loglik <- -1626.9196695394841754
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = "hazrate"
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
testthat::test_that("hazrate w/ factor covar, same value",{
  intercept <- c(
  "(Intercept)" =  3.91558993,
  "observerobs2"=  0.17191702,
  "observerobs3"=  0.06574510,
  "observerobs4"= -0.38573966,
  "observerobs5"= -0.09584550,
  "k"           =  2.76471844
    )
  loglik <- -1627.508575018499414
    fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
                                   , likelihood = "hazrate"
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


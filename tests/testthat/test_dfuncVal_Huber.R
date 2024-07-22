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
w.hi <- units::set_units(207, "m")

sparrowDf <- RdistDf(sparrowSiteData
                   , sparrowDetectionData)

# No covariates ----
testthat::test_that("huber w/ no covariates, same value",{
  intercept <- c(
    "(Intercept)" =   4.6460624332947544346,
    "gamma" = 103.5331816147906067727
  )
  loglik <-  -1765.8472363686191784

  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = "huber"
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

testthat::test_that("huber w/ cont covar, same value", {
  intercept <- c(
    "(Intercept)" =   1.34163764681409469759,
    "bare" =   0.02913847238739412232,
    "gamma" = 181.31394868220903049405
  )
  loglik <- -1737.282516063277626
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = "huber"
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
testthat::test_that("huber w/ factor covar, same value",{
  intercept <- c(
    "(Intercept)" =   4.68114048692660222883,
    "observerobs2" =  -0.76031636986890249741,
    "observerobs3" =  -1.55534338537169247729,
    "observerobs4" =  -2.30958961083673486314,
    "observerobs5" =  -1.11636260899878592134,
    "gamma" = 102.65664765844631745040
  )
  loglik <- -1709.6456601883908206
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
                                 , likelihood = "huber"
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


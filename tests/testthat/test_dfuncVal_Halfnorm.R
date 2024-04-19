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
testthat::test_that("Halfnorm w/ no covariates, same value",{
  intercept <- c("(Intercept)" = 3.9094997233683392857)
  loglik <- -1630.7159607863527526
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = "halfnorm"
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

testthat::test_that("Halfnorm w/ cont cov, same value", {
  intercept <- c("(Intercept)" = 3.346183910,
                 "bare" = 0.009715925)
  loglik <- -1624.3506723274776959
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = "halfnorm"
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
testthat::test_that("Halfnorm w/ no covariates, same value",{
  intercept <- c(
    "(Intercept)" =  3.92800543,
    "observerobs2"=  0.17073246,
    "observerobs3"= -0.01647247,
    "observerobs4"= -0.26739638,
    "observerobs5"= -0.02839298    )
  loglik <- -1626.1415857120343844
    fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
                                   , likelihood = "halfnorm"
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

  
# snapshot testing is the way to go here; but, I cannot get the 
# following to create snapshot files. Hence, cannot test
# testthat::test_that("Halfnorm no covariates same value",{
#   fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
#                                  , likelihood = "halfnorm"
#                                  , w.lo = w.lo
#                                  , w.hi = w.hi
#                                  , expansions = 0
#                                  , series = "cosine"
#                                  , x.scl = 0
#                                  , g.x.scl = 1
#                                  , outputUnits = "m"
#   )
#   testthat::expect_snapshot_value(fit, style = "serialize")}
# )
# 
# 
# # Continuous covariate
# testthat::expect_snapshot_value({
#   fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
#                                  , likelihood = "halfnorm"
#                                  , w.lo = w.lo
#                                  , w.hi = w.hi
#                                  , expansions = 0
#                                  , series = "cosine"
#                                  , x.scl = 0
#                                  , g.x.scl = 1
#                                  , outputUnits = "m"
#   )
# }, style = "json2")
# 
# # Factor Covariate
# testthat::expect_snapshot_value({
#   fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
#                                  , likelihood = "halfnorm"
#                                  , w.lo = w.lo
#                                  , w.hi = w.hi
#                                  , expansions = 0
#                                  , series = "cosine"
#                                  , x.scl = 0
#                                  , g.x.scl = 1
#                                  , outputUnits = "m"
#   )
# }, style = "serialize")

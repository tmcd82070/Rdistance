#' @description Test specific values of parameters. Other tests, 
#' i.e., test_dfuncEstim, simply test whether the inputs to dfuncEstim 
#' cause errors.  These other routines do not check that routines arrive
#' at the same answer. 
#' 

w.lo <- 0
w.hi <- units::set_units(150, "m")

sparrowDf <- RdistDf(sparrowSiteData
                   , sparrowDetectionData)

# No covariates ----
testthat::test_that("Halfnorm w/ no covariates, same value",{
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
  testthat::expect_snapshot(print.default(fit)
                          , transform = scrub_environ)
})

# Continuous covariate ----

testthat::test_that("Halfnorm w/ cont cov, same value", {

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
  testthat::expect_snapshot(print.default(fit)
                            , transform = scrub_environ)
  })

# Factor Covariate ----
testthat::test_that("Halfnorm w/ no covariates, same value",{
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
    testthat::expect_snapshot(print.default(fit)
                              , transform = scrub_environ)
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

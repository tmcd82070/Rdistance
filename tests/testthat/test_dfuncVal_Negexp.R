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
testthat::test_that("negexp w/ no covariates, same value",{
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
  testthat::expect_snapshot(print.default(fit)
                            , transform = scrub_environ)
})

# Continuous covariate ----

testthat::test_that("negexp w/ cont covar, same value", {
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
  testthat::expect_snapshot(print.default(fit)
                            , transform = scrub_environ)
})

# Factor Covariate ----
testthat::test_that("negexp w/ factor covar, same value",{
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
    testthat::expect_snapshot(print.default(fit)
                              , transform = scrub_environ)
})


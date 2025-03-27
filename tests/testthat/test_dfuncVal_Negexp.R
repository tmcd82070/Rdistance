#' @description Snapshot testing of basic functionality    
#' 

w.lo  <- 0
w.20  <- units::set_units(2, "m")
w.hi  <- units::set_units(150, "m")
sArea <- units::set_units(4105, "km^2")
lhood <- "negexp"
xScl  <- units::set_units(0, "m")
gXscl <- 0.75

sparrowDf <- RdistDf(sparrowSiteData
                   , sparrowDetectionData)

# Min inputs ----
testthat::test_that(paste0(lhood, "-MinimumInputs"),{
  fit <- sparrowDf |> 
    dfuncEstim(formula = dist ~ groupsize(groupsize)
             , likelihood = lhood) |> 
    abundEstim( area = sArea
                , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})


# No covariates ----
testthat::test_that(paste0(lhood, "-NoCovar"),{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
                                 ) |> 
  abundEstim( area = sArea
              , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                          , transform = scrub_environ)
})

# Continuous covariate ----
testthat::test_that(paste0(lhood, "-ContinuousCovar"), {

  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = lhood
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
                                 ) |> 
  abundEstim( area = sArea
            , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
  })

# Factor Covariate ----
testthat::test_that( paste0(lhood, "-FactorCovar"),{
    fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
                                   , likelihood = lhood
                                   , w.lo = w.lo
                                   , w.hi = w.hi
                                   , expansions = 0
                                   , series = "cosine"
                                   , x.scl = 0
                                   , g.x.scl = 1
                                   , outputUnits = "m"
                                   ) |> 
    abundEstim( area = sArea
              , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
})


# Wlo and Whi, no covar ----
testthat::test_that( paste0(lhood, "-NoCovarWloWhi"),{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , w.lo = w.20
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = w.20
                                 , g.x.scl = 1
                                 , outputUnits = "m"
                                 ) |> 
  abundEstim( area = sArea
            , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                          , transform = scrub_environ)
  }
)

# WLow, no covar ----
testthat::test_that( paste0(lhood, "-NoCovarWlo"),{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , w.lo = w.20
                                 # , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = w.20
                                 , g.x.scl = 1
                                 , outputUnits = "m"
                                 ) |> 
    abundEstim( area = sArea
                , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)

# Feet, no covar ----
testthat::test_that( paste0(lhood,  "-NoCovarsFt"),{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , outputUnits = "ft"
                                 ) |> 
    abundEstim( area = sArea
                , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)

# Expansions, no covar ----
testthat::test_that( paste0(lhood, "-NoCovarExpansions"),{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , expansions = 2
                                 , outputUnits = "m"
                                 ) |> 
    abundEstim( area = sArea
                , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)

# Continuous covariate, expansions ----

testthat::test_that( paste0(lhood, "-ContCovarExpansions"),{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = lhood
                                 , expansions = 2
                                 , outputUnits = "m"
                                 ) |> 
    abundEstim( area = sArea
                , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)


# Scaling ----

testthat::test_that( paste0(lhood, "-ContCovarExpansionsScaling"),{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = lhood
                                 , expansions = 2
                                 , outputUnits = "m"
                                 , x.scl = xScl
                                 , g.x.scl = gXscl
                                 ) |> 
    abundEstim( area = sArea
                , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)


# Bootstraps ----

set.seed(4784523)
testthat::test_that( paste0(lhood, "-Bootstraps"),{
  fit <- sparrowDf |> 
    dfuncEstim(formula = dist ~ groupsize(groupsize)
               , likelihood = lhood) |> 
    abundEstim( area = sArea
                , ci = .95
                , R = 20)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)

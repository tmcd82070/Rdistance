#' @title Snapshot testing of basic Rdistance functioning
#' 
#' @description A function that performs snapshot testing 
#' of basic Rdistance functioning.  Most combination of input
#' parameters to dfuncEstim are tested. 
#' 
#' @param lhood The likelihood to test. All tests use this likelihood.
#' 
#' @param w.lo Lowest lower limit to use, typically 0.
#' 
#' @param w.20 A lower limit greater than 0 to test left truncation.
#' Some tests are run at \code{w.lo}, some at \code{w.20}.
#' 
#' @param w.hi A upper limit to use when testing right truncation. 
#' Some tests are run at the maximum distance, no specification of 
#' right truncation, some are run at \code{w.hi}.
#' 
#' @param sArea Study area to use in all tests.
#' 
#' @param xScl X location to scale when scaling is tested. 
#' 
#' @param gXscl Height to scale to, when scaling is tested. 
#' 
#' @param dataDf The data frame to use for all tests.  Must be a 
#' RdistDf. NOTE: this routine assumes that distances are stored in 
#' column 'dist', and that variables 'bare', 'observer', and 
#' 'groupsize' exist in this data frame to use as covariates. 
#' 
#' 


# cut down printed digits so snapshots don't fail with changes in 7th or 8th digit
options(digits = 4)

testthat::set_max_fails(Inf)

test_dfuncVal <- function(
      lhood 
    , w.lo  
    , w.20  
    , w.hi  
    , sArea 
    , xScl  
    , gXscl 
    , dataDf
  ){

  # Min inputs ----
  testthat::test_that(paste0(lhood, "-MinimumInputs"),{
    fit <- dataDf |> 
      dfuncEstim(formula = dist ~ groupsize(groupsize)
                 , likelihood = lhood) |> 
      abundEstim( area = sArea
                  , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
  })
  
  
  # No covariates ----
  testthat::test_that(paste0(lhood, "-NoCovar"),{
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
    
    fit <- dataDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
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
    fit <- dataDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
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
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                   , likelihood = lhood
                                   , outputUnits = "ft"
    ) |> 
      abundEstim( area = sArea
                  , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
  }
  )
  
  # Cosine Expansions, no covar ----
  testthat::test_that( paste0(lhood, "-NoCovarCosExpansions"),{
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                   , likelihood = lhood
                                   , expansions = 2
                                   , outputUnits = "m"
                                   , series = "cosine"
    ) |> 
      abundEstim( area = sArea
                  , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
  }
  )
  
  # Sine Expansions, no covar ----
  testthat::test_that( paste0(lhood, "-NoCovarSinExpansions"),{
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                   , likelihood = lhood
                                   , expansions = 2
                                   , outputUnits = "m"
                                   , series = "sine"
    ) |> 
      abundEstim( area = sArea
                  , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
  }
  )
  
  # Hermite Expansions, no covar ----
  testthat::test_that( paste0(lhood, "-NoCovarHermExpansions"),{
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                   , likelihood = lhood
                                   , expansions = 2
                                   , outputUnits = "m"
                                   , series = "hermite"
    ) |> 
      abundEstim( area = sArea
                  , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
  }
  )
  
  # Simple Expansions, no covar ----
  testthat::test_that( paste0(lhood, "-NoCovarSimpExpansions"),{
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                   , likelihood = lhood
                                   , expansions = 2
                                   , outputUnits = "m"
                                   , series = "simple"
    ) |> 
      abundEstim( area = sArea
                  , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
  }
  )
  
  # BSpline Expansions, no covar ----
  testthat::test_that( paste0(lhood, "-NoCovarBSplineExpansions"),{
    fit <- dataDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                   , likelihood = lhood
                                   , expansions = 2
                                   , outputUnits = "m"
                                   , series = "bspline"
    ) |> 
      abundEstim( area = sArea
                  , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
  }
  )
  
  # Continuous covariate, expansions ----
  
  testthat::test_that( paste0(lhood, "-ContCovarExpansions"),{
    fit <- dataDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                   , likelihood = lhood
                                   , expansions = 2
                                   , outputUnits = "m"
    ) |> 
      abundEstim( area = sArea
                  , ci = NULL)
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
    
    # Simple Plot ----
    test_that(paste(lhood, "plot"), {
      expect_snapshot_plot(paste0("dfuncVal_", lhood), plot(fit))
    })
    
  }
  )
  
  # Scaling ----
  
  testthat::test_that( paste0(lhood, "-ContCovarExpansionsWScaling"),{
    fit <- dataDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
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
  
  fit <- dataDf |> 
    dfuncEstim(formula = dist ~ groupsize(groupsize)
               , likelihood = lhood) 
  
  testthat::test_that( paste0(lhood, "-SerialBootstraps"),{
    abun <- fit |> abundEstim( area = sArea
                               , ci = .95
                               , R = 20
                               , parallel = FALSE)
    testthat::expect_snapshot(summary(abun)
                              , transform = scrub_environ)
  }
  )
  
  ciRegEx <- "95% CI: \\d+(\\.\\d+)? to \\d+(\\.\\d+)?"
  
  testthat::test_that( paste0(lhood, "-2CoreBootstraps"),{
    abun <- fit |> abundEstim( area = sArea
                               , ci = .95
                               , R = 20
                               , parallel = 2)
    testthat::expect_output(summary(abun)
                            , regexp = ciRegEx)
  } 
  )
  
  testthat::test_that( paste0(lhood, "-FullCoreBootstraps"),{
    abun <- fit |> abundEstim( area = sArea
                               , ci = .95
                               , R = 20
                               , parallel = TRUE)
    testthat::expect_output(summary(abun)
                            , regexp = ciRegEx)
  }
  )
  
}  


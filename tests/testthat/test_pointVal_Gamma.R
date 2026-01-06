#' @description Snapshot testing of basic point transect functionality    
#' 

w.lo  <- 0
w.20  <- units::set_units(65.6168, "ft")
w.hi  <- units::set_units(200, "m")
sArea <- units::set_units(4105, "km^2")
lhood <- "Gamma"
xScl  <- units::set_units(0, "m")
gXscl <- 0.75

# will use thrasherDF

# Min inputs ----
testthat::test_that(paste0(lhood, "-MinimumInputs"),{
  fit <- thrasherDf |> 
    dfuncEstim(formula = dist ~ groupsize(groupsize)
             , likelihood = lhood) |> 
    abundEstim( area = sArea
                , ci = NULL)
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})


# No covariates ----
testthat::test_that(paste0(lhood, "-NoCovar"),{
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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

  fit <- thrasherDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
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
    fit <- thrasherDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
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
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
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

# Continuous covariate, expansions ----

testthat::test_that( paste0(lhood, "-ContCovarExpansions"),{
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
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

# Simple Plot ----
testthat::test_that( paste0(lhood, "-Plot"),{
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                  , likelihood = lhood
  )
  
  expect_snapshot_plot("pointVal_Gamma"
                       , plot(fit
                          , newdata=data.frame(bare = c(20, 30, 40))
                          , nbins = 40 
                       ))
  
}
)




# Scaling ----

testthat::test_that( paste0(lhood, "-ContCovarExpansions"),{
  fit <- thrasherDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
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
# no snapshot testing of bootstraps, only that they execute

# set.seed(4784523)
# testthat::test_that( paste0(lhood, "-Bootstraps"),{
#   fit <- thrasherDf |> 
#     dfuncEstim(formula = dist ~ groupsize(groupsize)
#                , likelihood = lhood) |> 
#     abundEstim( area = sArea
#                 , ci = .95
#                 , R = 20)
#   testthat::expect_snapshot(summary(fit)
#                             , transform = scrub_environ)
# }
# )

fit <- thrasherDf |> 
  dfuncEstim(formula = dist ~ groupsize(groupsize)
             , likelihood = lhood) 
  
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

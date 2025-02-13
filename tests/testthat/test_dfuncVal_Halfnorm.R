#' @description Test specific values of parameters. Other tests, 
#' i.e., test_dfuncEstim, simply test whether the inputs to dfuncEstim 
#' cause errors.  These other routines do not check that routines arrive
#' at the same answer. 
#' 

w.lo <- 0
w.20 <- units::set_units(65.6168, "ft")
w.hi <- units::set_units(150, "m")
lhood <- "halfnorm"

sparrowDf <- RdistDf(sparrowSiteData
                   , sparrowDetectionData)

# Min inputs ----
testthat::test_that("Halfnorm Minimum inputs",{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ groupsize(groupsize)
                                 , likelihood = lhood
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})


# No covariates ----
testthat::test_that("Halfnorm w/ no covariates, same value",{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_snapshot(summary(fit)
                          , transform = scrub_environ)
})

# Continuous covariate ----

testthat::test_that("Halfnorm w/ cont cov, same value", {

  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = lhood
                                 , w.lo = w.lo
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = 0
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
  })

# Factor Covariate ----
testthat::test_that("Halfnorm w/ factor covariates, same value",{
    fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
                                   , likelihood = lhood
                                   , w.lo = w.lo
                                   , w.hi = w.hi
                                   , expansions = 0
                                   , series = "cosine"
                                   , x.scl = 0
                                   , g.x.scl = 1
                                   , outputUnits = "m"
    )
    testthat::expect_snapshot(summary(fit)
                              , transform = scrub_environ)
})

  
testthat::test_that("Halfnorm, no covar, wlo 20, whi 150",{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , w.lo = w.20
                                 , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = w.20
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_snapshot(summary(fit)
                          , transform = scrub_environ)
  }
)

testthat::test_that("Halfnorm, no covar, wlo 20, whi high",{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , w.lo = w.20
                                 # , w.hi = w.hi
                                 , expansions = 0
                                 , series = "cosine"
                                 , x.scl = w.20
                                 , g.x.scl = 1
                                 , outputUnits = "m"
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)

testthat::test_that("Halfnorm, no covar, ft",{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , outputUnits = "ft"
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)

# Expansions, no covar ----
testthat::test_that("Halfnorm, no covar, expansions",{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , expansions = 2
                                 , outputUnits = "m"
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)

# Continuous covariate, expansions ----
#
# FIGURE OUT WHY THIS TAKES SO LONG TO CONVERGE (~1.5 MINS), 
# BUT APPEARS TO CONVERGE TO THE CORRECT ANSWER.
#
testthat::test_that("Halfnorm, w/ cont covar, expansions",{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = lhood
                                 , expansions = 2
                                 , outputUnits = "m"
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
}
)

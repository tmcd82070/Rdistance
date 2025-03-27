#' @description Test specific values of parameters. Other tests, 
#' i.e., test_dfuncEstim, simply test whether the inputs to dfuncEstim 
#' cause errors.  These other routines do not check that routines arrive
#' at the same answer. 
#' 

w.lo <- 0
w.20 <- units::set_units(80, "ft")
w.hi <- units::set_units(150, "m")
lhood <- "huber"

sparrowDf <- RdistDf(sparrowSiteData
                   , sparrowDetectionData)

# No covariates ----
testthat::test_that("Huber w/ no covariates, same value",{
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
  testthat::expect_snapshot(print.default(fit)
                          , transform = scrub_environ)
})

# Continuous covariate ----

testthat::test_that("Huber w/ cont cov, same value", {

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
  testthat::expect_snapshot(print.default(fit)
                            , transform = scrub_environ)
  })

# Factor Covariate ----
testthat::test_that("Huber w/ factor covariates, same value",{
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
    testthat::expect_snapshot(print.default(fit)
                              , transform = scrub_environ)
})

  
testthat::test_that("Huber, no covar, wlo 20, whi 150",{
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
  testthat::expect_snapshot(print.default(fit)
                          , transform = scrub_environ)
  }
)

testthat::test_that("Huber, no covar, wlo 20, whi high",{
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
  testthat::expect_snapshot(print.default(fit)
                            , transform = scrub_environ)
}
)

testthat::test_that("Huber, no covar, ft",{
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = lhood
                                 , outputUnits = "ft"
  )
  testthat::expect_snapshot(print.default(fit)
                            , transform = scrub_environ)
}
)



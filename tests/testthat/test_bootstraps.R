#' @description Snapshot testing bootstrapping    
#' 

w.lo  <- 0
w.hi  <- setUnits(200, "m")
sArea <- setUnits(1, "mi^2")
lhood <- "halfnorm"
RR <- 50  # iterations for tests

set.seed(23095843)

fit <- sparrowDf |> 
  dfuncEstim(formula = dist ~ groupsize(groupsize)
             , likelihood = lhood
             , w.hi = w.hi
             ) 
  

# no bootstrapping ----
testthat::test_that("No bootstrapping",{
  abn <- abundEstim( fit
                   , area = sArea
                   , ci = NULL
                   , R=RR
                   , parallel = F
                   , showProgress = F
                   , plot.bs = T
                   )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})

# non-Parallel bootstrapping ----
testthat::test_that("Serial BS; show T; plot T",{
  abn <- abundEstim( fit
                     , area = sArea
                     , propUnitSurveyed = 1
                     , ci = 0.95
                     , R = RR
                     , showProgress = T
                     , plot.bs = T
                     , parallel = F
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})

testthat::test_that("Serial BS; show F; plot T",{
  abn <- abundEstim( fit
                     , area = sArea
                     , propUnitSurveyed = 1
                     , ci = 0.95
                     , R = RR
                     , showProgress = F
                     , plot.bs = T
                     , parallel = F
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})


testthat::test_that("Serial BS; show T; plot F",{
  abn <- abundEstim( fit
                     , area = sArea
                     , propUnitSurveyed = 1
                     , ci = 0.95
                     , R = RR
                     , showProgress = T
                     , plot.bs = F
                     , parallel = F
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})

testthat::test_that("Serial BS; show F; plot F",{
  abn <- abundEstim( fit
                     , area = sArea
                     , propUnitSurveyed = 1
                     , ci = 0.95
                     , R = RR
                     , showProgress = F
                     , plot.bs = F
                     , parallel = F
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})

# Parallel operations ----

testthat::test_that("Parallel BS; Cores=Max-1",{
  abn <- abundEstim( fit
                     , area = sArea
                     , propUnitSurveyed = 1
                     , ci = 0.95
                     , R = RR
                     , showProgress = F
                     , plot.bs = F
                     , parallel = T
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})


testthat::test_that("Parallel BS; Cores=2",{
  abn <- abundEstim( fit
                     , area = sArea
                     , propUnitSurveyed = 1
                     , ci = 0.95
                     , R = RR
                     , showProgress = F
                     , plot.bs = F
                     , parallel = 2
  )
  testthat::expect_snapshot(summary(fit)
                            , transform = scrub_environ)
})

# Check warnings ----
testthat::test_that("Parallel BS; Cores=-1",{
  testthat::expect_warning(
  { abn <- abundEstim( fit
                     , area = sArea
                     , propUnitSurveyed = 1
                     , ci = 0.95
                     , R = parallel::detectCores()
                     , showProgress = T
                     , plot.bs = F
                     , parallel = -1
  )
  }
  , regexp = "cores must be >= 1"
  )
})

testthat::test_that("Parallel BS; Cores=100",{
  testthat::expect_warning(
    { abn <- abundEstim( fit
                         , area = sArea
                         , propUnitSurveyed = 1
                         , ci = 0.95
                         , R = parallel::detectCores()
                         , showProgress = T
                         , plot.bs = F
                         , parallel = 100
    )
    }
    , regexp = "cores must be <="
  )
})

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
testthat::test_that("Logistic w/ no covariates, same value",{
  # Dunno where I got these values, but keep em just for reference
  # intercept <- c(
  #   "(Intercept)" = 3.32165487382,
  #   "Knee"        = 0.03399299704    )
  # loglik <-  -1628.346016788130
  intercept <- c(
    "(Intercept)" = 3.20217204, 
    "Knee"        = 0.03332738
  )
  loglik <-  -1660.637620398593
  

  fit <- sparrowDf |> dfuncEstim(formula = dist ~ 1 + groupsize(groupsize)
                                 , likelihood = "logistic"
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

testthat::test_that("Logistic w/ cont covar, same value", {
  # intercept <- c(
  #   "(Intercept)" = -2.521333130818637524584,
  #   "bare"        =  0.091346920209716561678,
  #   "Knee"        =  0.035560913708471635075
  #   )
  # loglik <- -1619.48655874189717
  intercept <- c(
    "(Intercept)" =  3.39840608499118035368 ,
    "bare"        = -0.31287446971086901870 ,
    "Knee"        =  0.02954315224286400976 
  )
  loglik <- -1661.1858793231822347
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
                                 , likelihood = "logistic"
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
testthat::test_that("Logistic w/ factor covar, same value",{
  # intercept <- c(
  # "(Intercept)" =  3.777423121715529852338,
  # "observerobs2"=  0.255745620894587299965,
  # "observerobs3"= -0.084801457130264004669,
  # "observerobs4"= -4.584902504534079881182,
  # "observerobs5"= -0.104474733024009655558,
  # "Knee"        =  0.037237108416373940201
  #   )
  # loglik <- -1625.7604647182586177
  intercept <- c(
    "(Intercept)"  =   3.840776931931665583164 ,
    "observerobs2" =   0.195657101189449345036 ,
    "observerobs3" =  -0.268178559451367204414 ,
    "observerobs4" = -11.714737998263288432099 ,
    "observerobs5" =  -0.264092763353868775766 ,
    "Knee"         =   0.036498590717327895894
  )
  loglik <- -1657.7104011586800425
  fit <- sparrowDf |> dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
                                 , likelihood = "logistic"
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


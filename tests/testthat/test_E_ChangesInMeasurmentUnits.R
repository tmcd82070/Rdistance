# 
# Check for equality of results under different input units
#

library(units)

# ---- Lines ----
data("sparrowDetectionData")
data("sparrowSiteData")
detectDf <- sparrowDetectionData
siteDf <- sparrowSiteData

# detection distances are already "m"
fit.m.01 <- dfuncEstim(formula = dist ~ 1
                    , detectionData = detectDf
                    , w.lo = units::set_units(10, "m") 
                    , w.hi = units::set_units(200, "m")
                    , x.scl = units::set_units(10, "m"))

# ---- Change units on distances ----

units(detectDf$dist) <- "ft"
fit.m.02 <- dfuncEstim(formula = dist ~ 1
                    , detectionData = detectDf
                    , w.lo = units::set_units(10, "m") 
                    , w.hi = units::set_units(200, "m")
                    , x.scl = units::set_units(10, "m"))

test_that("m2ft Dist - Params equal", {
  sig01 <- set_units(coef(fit.m.01), "m")
  sig02 <- set_units(coef(fit.m.02), "ft")
  names(sig01) <- NULL
  expect_equal( sig01, set_units(sig02, "m"))
})

test_that("m2ft Dist - ESW equal",{
  sig01 <- effectiveDistance(fit.m.01)
  sig02 <- effectiveDistance(fit.m.02)
  expect_equal( sig01, set_units(sig02, "m"))
})


# ---- Change units on w.hi ----

detectDf <- sparrowDetectionData

fit.m.02 <- dfuncEstim(formula = dist ~ 1
                       , detectionData = detectDf
                       , w.lo = units::set_units(10, "m") 
                       , w.hi = units::set_units(656.168, "ft")
                       , x.scl = units::set_units(10, "m"))

test_that("m2ft w.hi - Params equal", {
  expect_equal( coef(fit.m.01), coef(fit.m.02) )
})

test_that("m2ft w.hi - ESW equal",{
  expect_equal( ESW(fit.m.01), ESW(fit.m.02) )
})

test_that("m2ft w.hi - AIC equal",{
  expect_equal( AIC(fit.m.01), AIC(fit.m.02))
})

# ---- Change units on w.lo ----
# Note there are two observations at exactly 10 m.  So, need high precision 
# on w.lo
fit.m.02 <- dfuncEstim(formula = dist ~ 1
                       , detectionData = detectDf
                       , w.lo = units::set_units(393.7007874015747575, "in") 
                       , w.hi = units::set_units(656.168, "ft")
                       , x.scl = units::set_units(393.7007874015747575, "in"))

test_that("m2ft w.hi - Params equal", {
  expect_equal( coef(fit.m.01), coef(fit.m.02) )
})

test_that("m2ft w.hi - ESW equal",{
  expect_equal( ESW(fit.m.01), ESW(fit.m.02) )
})

test_that("m2ft w.hi - AIC equal",{
  expect_equal( AIC(fit.m.01), AIC(fit.m.02))
})


# ---- Check abundance ----

abund01 <- abundEstim(fit.m.01
                      , siteData = siteDf
                      , detectionData = detectDf
                      , area = set_units(100, "km^2")
                      , ci = NULL)

units(siteDf$length) <- "mi"

abund02 <- abundEstim(fit.m.01
                      , siteData = siteDf
                      , detectionData = detectDf
                      , area = set_units(100, "km^2")
                      , ci = NULL)
test_that("Abundances equal",{
  expect_equal( abund01$n.hat, abund02$n.hat ) 
})

abund02 <- abundEstim(fit.m.01
                      , siteData = siteDf
                      , detectionData = detectDf
                      , area = set_units(24710.439304662791983, "acres")
                      , ci = NULL)
test_that("Abundances equal",{
  expect_equal( abund01$n.hat, abund02$n.hat ) 
})


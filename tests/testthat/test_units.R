#
# Test measurement unit facilities
#
library(Rdistance)

data("sparrowDetectionData")
data("sparrowSiteData")

context("Measurement Units same as input")

tmp <- units::as_units(150, "m")
x.scl <- units::as_units(10, "ft")
x.scl.m <- x.scl
units(x.scl.m) <- "m"

dfunc_line <- dfuncEstim(formula = dist ~ 1,
                         detectionData = sparrowDetectionData,
                         siteData = sparrowSiteData,
                         likelihood = "halfnorm", 
                         w.hi = tmp, 
                         x.scl = x.scl )
sigma_m <- coef(dfunc_line)
units(sigma_m) <- "m"

test_that("Units on distance are meters", {
  expect_equal( units(dfunc_line$dist), units(tmp) )
})

test_that("Units on w.lo are meters", {
  expect_equal( units(dfunc_line$w.lo), units(tmp) )
})

test_that("Units on w.hi are meters", {
  expect_equal( units(dfunc_line$w.hi), units(tmp) )
})

test_that("Units on x.scl are meters", {
  expect_equal( units(dfunc_line$x.scl), units(tmp) )
})

test_that("x.scl is 10 ft in meters", {
  expect_equal( round(dfunc_line$x.scl,3), round(x.scl.m,3) )
})

test_that("dfuncEstim produced correct ESW with units", {
  expect_equal(round(ESW(dfunc_line),3), as_units(62.459, "m"))
})

# ------------------------------------------------

context("Measurement Units different from input")

dfunc_line <- dfuncEstim(formula = dist ~ 1,
                         detectionData = sparrowDetectionData,
                         siteData = sparrowSiteData,
                         likelihood = "halfnorm", 
                         w.hi = tmp, 
                         x.scl=x.scl,  
                         outputUnits = "ft")
units(tmp) <- "ft"
sigma_ft <- coef(dfunc_line)
units(sigma_ft) <- "ft"

test_that("Units on distance are feet", {
  expect_equal( units(dfunc_line$dist), units(tmp) )
})

test_that("Units on w.lo are feet", {
  expect_equal( units(dfunc_line$w.lo), units(tmp) )
})

test_that("Units on w.hi are feet", {
  expect_equal( units(dfunc_line$w.hi), units(tmp) )
})

test_that("Units on x.scl are feet", {
  expect_equal( units(dfunc_line$x.scl), units(tmp) )
})

test_that("x.scl is 10 ft", {
  expect_equal( round(dfunc_line$x.scl,3), round(x.scl,3) )
})

test_that("dfuncEstim produced correct ESW with units", {
  expect_equal(round(ESW(dfunc_line),3), as_units(204.919, "ft"))
})

# ------------------------------------------------

context("Internal unit conversions produce same result")

names(sigma_m) <- NULL  # must do this because conversion in next line removes names
units(sigma_ft) <- "m"

test_that("Sigma_ft and Sigma_m are equal", {
  expect_equal( round(sigma_m,3), round(sigma_ft,3) )
})



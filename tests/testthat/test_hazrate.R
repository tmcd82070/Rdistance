library(Rdistance)
# context("Hazrate distance function")

# Load example sparrow data (line transect survey type)
data(sparrowDetectionData)

# Default hazrate ----
dfuncFit <- dfuncEstim(formula=dist~1,
                       detectionData=sparrowDetectionData,
                       likelihood="hazrate", 
                       w.lo = 0,
                       w.hi = units::as_units(100, "m"), 
                       pointSurvey=FALSE, 
                       expansions = 0, 
                       x.scl = units::as_units(0, "m"), 
                       g.x.scl = 1)

test_that("hazrate Sigma = 39.263040", {
  expect_equal(round(coef(dfuncFit)[["Sigma"]], 5), round(39.263040, 5))
})

test_that("hazrate ESW = 55.33783 [m]", {
  expect_equal(round(ESW(dfuncFit), 5), units::as_units(55.33783, "m"))
})

aicRef <- c(2972.533)
attr(aicRef, "criterion") <- "AICc"

test_that("hazrate AIC = 2972.533", {
  expect_equal(round(AIC(dfuncFit), 3), aicRef)
})

test_that("hazrate prints", {
  expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
})

# Devise some method to test plots
test_that("hazrate plots", {
  expect_output_file({
    png(filename = "trash.png")
    plot(dfuncFit)
    dev.off(dev.cur())},
    "trash.png")
})


# Left truncation ----
test_that("Error: No units on w.lo", {
  expect_error(
    dfuncFit <- dfuncEstim(formula=dist~1,
                           detectionData=sparrowDetectionData,
                           likelihood="hazrate", 
                           w.lo = 25,
                           w.hi = units::as_units(100, "m"), 
                           pointSurvey=FALSE, 
                           expansions = 0, 
                           x.scl = units::as_units(0, "m"), 
                           g.x.scl = 1)
  )
})

dfuncFit <- dfuncEstim(formula=dist~1,
                       detectionData=sparrowDetectionData,
                       likelihood="hazrate", 
                       w.lo = units::as_units(25, "m"),
                       w.hi = units::as_units(100, "m"), 
                       pointSurvey=FALSE, 
                       expansions = 0, 
                       x.scl = units::as_units(25, "m"), 
                       g.x.scl = 1)



test_that("hazrate left truncation Sigma = 47.478043", {
  expect_equal(round(coef(dfuncFit)[["Sigma"]], 5), round(47.478043, 5))
})


test_that("hazrate left truncation ESW = 56.30084 [m]", {
  expect_equal(round(ESW(dfuncFit), 5), units::as_units(56.30084, "m"))
})

aicRef <- c(1546.942)
attr(aicRef, "criterion") <- "AICc"

test_that("hazrate AIC = 1546.942", {
  expect_equal(round(AIC(dfuncFit), 3), aicRef)
})

test_that("hazrate left truncation prints", {
  expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
})

# Devise some method to test plots
test_that("hazrate left truncation plots", {
  expect_output_file({
    png(filename = "trash.png")
    plot(dfuncFit)
    dev.off(dev.cur())},
    "trash.png")
})


# Positive x.scl ----
test_that("Error: No units on x.scl", {
  expect_error(
    dfuncFit <- dfuncEstim(formula=dist~1,
                           detectionData=sparrowDetectionData,
                           likelihood="hazrate", 
                           w.lo = units::as_units(0, "m"),
                           w.hi = units::as_units(100, "m"), 
                           pointSurvey=FALSE, 
                           expansions = 0, 
                           x.scl = 0, 
                           g.x.scl = 1)
  )
})

dfuncFit <- dfuncEstim(formula=dist~1,
                       detectionData=sparrowDetectionData,
                       likelihood="hazrate", 
                       w.lo = units::as_units(0, "m"),
                       w.hi = units::as_units(100, "m"), 
                       pointSurvey=FALSE, 
                       expansions = 0, 
                       x.scl = units::as_units(25, "m"), 
                       g.x.scl = 1)

test_that("hazrate positive x.scl Sigma = 46.3587", {
  expect_equal(round(coef(dfuncFit)[["Sigma"]], 4), round(46.3587, 4))
})


test_that("hazrate positive x.scl ESW = 65.11255 [m]", {
  expect_equal(round(ESW(dfuncFit), 5), units::as_units(65.11255, "m"))
})

aicRef <- c(2970.605)
attr(aicRef, "criterion") <- "AICc"

test_that("hazrate positive x.scl AIC = 2970.605", {
  expect_equal(round(AIC(dfuncFit), 3), aicRef)
})

test_that("hazrate positive x.scl prints", {
  expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
})

# Devise some method to test plots
test_that("hazrate positive x.scl plots", {
  expect_output_file({
    png(filename = "trash.png")
    plot(dfuncFit)
    dev.off(dev.cur())},
    "trash.png")
})


# g(x.scl) < 1 ----

dfuncFit <- dfuncEstim(formula=dist~1,
                       detectionData=sparrowDetectionData,
                       likelihood="hazrate", 
                       w.lo = units::as_units(0, "m"),
                       w.hi = units::as_units(100, "m"), 
                       pointSurvey=FALSE, 
                       expansions = 0, 
                       x.scl = units::as_units(25, "m"), 
                       g.x.scl = 0.8)

test_that("hazrate g.x.scl < 1 Sigma = 46.3587", {
  expect_equal(round(coef(dfuncFit)[["Sigma"]], 4), round(46.3587, 4))
})


test_that("hazrate g.x.scl < 1 ESW =  52.09004 [m]", {
  expect_equal(round(ESW(dfuncFit), 5), units::as_units( 52.09004, "m"))
})

aicRef <- c(2970.605)
attr(aicRef, "criterion") <- "AICc"

test_that("hazrate g.x.scl < 1 AIC = 2970.605", {
  expect_equal(round(AIC(dfuncFit), 3), aicRef)
})

test_that("hazrate g.x.scl < 1 prints", {
  expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
})

# Devise some method to test plots
test_that("hazrate g.x.scl < 1 plots", {
  expect_output_file({
    png(filename = "trash.png")
    plot(dfuncFit)
    dev.off(dev.cur())},
    "trash.png")
})


# ---- Hazrate test ----
hazRate <- dfuncEstim(formula=dist~1,
                      detectionData=sparrowDetectionData,
                      likelihood="hazrate", 
                      w.lo = 0,
                      w.hi = units::as_units(100, "m"), 
                      pointSurvey=FALSE, 
                      expansions = 0, 
                      x.scl = units::as_units(0, "m"), 
                      g.x.scl = 1)

test_that("Hazrate Sigma = ???", {
  expect_equal(round(coef(hazRate)[["Sigma"]], 5), round(46.3586986528704, 5))
})

# ---- Negexp test ----
negExp <- dfuncEstim(formula=dist~1,
                     detectionData=sparrowDetectionData,
                     likelihood="negexp", 
                     w.lo = 0,
                     w.hi = units::as_units(100, "m"), 
                     pointSurvey=FALSE, 
                     expansions = 0, 
                     x.scl = units::as_units(0, "m"), 
                     g.x.scl = 1)

test_that("Negexp Sigma = ???", {
  expect_equal(round(coef(negExp)[["Sigma"]], 5), round(46.3586986528704, 5))
})

# ---- Uniform test ----
Uniform <- dfuncEstim(formula=dist~1,
                      detectionData=sparrowDetectionData,
                      likelihood="uniform", 
                      w.lo = 0,
                      w.hi = units::as_units(100, "m"), 
                      pointSurvey=FALSE, 
                      expansions = 0, 
                      x.scl = units::as_units(0, "m"), 
                      g.x.scl = 1)

test_that("Uniform Sigma = ???", {
  expect_equal(round(coef(Uniform)[["Sigma"]], 5), round(46.3586986528704, 5))
})


library(Rdistance)
# context("Halfnorm distance function")

# Load example sparrow data (line transect survey type)
data(sparrowDetectionData)

# Default Halfnorm ----
dfuncFit <- dfuncEstim(formula=dist~1,
                    detectionData=sparrowDetectionData,
                    likelihood="halfnorm", 
                    w.lo = 0,
                    w.hi = units::as_units(100, "m"), 
                    pointSurvey=FALSE, 
                    expansions = 0, 
                    x.scl = units::as_units(0, "m"), 
                    g.x.scl = 1 
                    )

test_that("Halfnorm default prints", {
  expect_output(print(dfuncFit), regexp = "AICc:")
})

test_that("Halfnorm Sigma = 46.35870", {
  expect_equal(round(coef(dfuncFit)[["Sigma"]], 5), round(46.3586986528704, 5))
})

test_that("Halfnorm ESW = 56.30084 [m]", {
  expect_equal(round(ESW(dfuncFit), 5), units::as_units(56.30084, "m"))
})

aicRef <- c(2970.605)
attr(aicRef, "criterion") <- "AICc"

test_that("Halfnorm AIC = 2970.605", {
  expect_equal(round(AIC(dfuncFit), 3), aicRef)
})

test_that("Halfnorm prints", {
  expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
})

# Devise some method to test plots
test_that("Halfnorm plots", {
  expect_silent(tmp <- plot(dfuncFit))
  expect_gt(tmp$yscl, 0)
  })


# Left truncation ----
test_that("Error: No units on w.lo", {
  expect_error(
    dfuncFit <- dfuncEstim(formula=dist~1,
                       detectionData=sparrowDetectionData,
                       likelihood="halfnorm", 
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
                       likelihood="halfnorm", 
                       w.lo = units::as_units(25, "m"),
                       w.hi = units::as_units(100, "m"), 
                       pointSurvey=FALSE, 
                       expansions = 0, 
                       x.scl = units::as_units(25, "m"), 
                       g.x.scl = 1)

test_that("Halfnorm Left Trunc prints", {
  expect_output(print(dfuncFit), regexp = "Strip: 25 \\[m\\]")
})


test_that("Halfnorm left truncation Sigma = 49.19439", {
  expect_equal(round(coef(dfuncFit)[["Sigma"]], 5), round(49.19439, 5))
})


test_that("Halfnorm left truncation ESW = 56.30084 [m]", {
  expect_equal(round(ESW(dfuncFit), 5), units::as_units(56.30084, "m"))
})

aicRef <- c(1544.182)
attr(aicRef, "criterion") <- "AICc"

test_that("Halfnorm AIC = 1544.182", {
  expect_equal(round(AIC(dfuncFit), 3), aicRef)
})

test_that("Halfnorm left truncation prints", {
  expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
})

# Devise some method to test plots
test_that("Halfnorm left truncation plots", {
  expect_silent(tmp <- plot(dfuncFit))
  expect_gt(tmp$yscl, 0)
})


# Positive x.scl ----
test_that("Error: No units on x.scl", {
  expect_error(
    dfuncFit <- dfuncEstim(formula=dist~1,
                           detectionData=sparrowDetectionData,
                           likelihood="halfnorm", 
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
                       likelihood="halfnorm", 
                       w.lo = units::as_units(0, "m"),
                       w.hi = units::as_units(100, "m"), 
                       pointSurvey=FALSE, 
                       expansions = 0, 
                       x.scl = units::as_units(25, "m"), 
                       g.x.scl = 1)

test_that("Halfnorm Positive x.scl prints", {
  expect_output(print(dfuncFit), regexp = "25 \\[m\\]")
})

test_that("Halfnorm positive x.scl Sigma = 46.3587", {
  expect_equal(round(coef(dfuncFit)[["Sigma"]], 4), round(46.3587, 4))
})


test_that("Halfnorm positive x.scl ESW = 65.11255 [m]", {
  expect_equal(round(ESW(dfuncFit), 5), units::as_units(65.11255, "m"))
})

aicRef <- c(2970.605)
attr(aicRef, "criterion") <- "AICc"

test_that("Halfnorm positive x.scl AIC = 2970.605", {
  expect_equal(round(AIC(dfuncFit), 3), aicRef)
})

test_that("Halfnorm positive x.scl prints", {
  expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
})

# Devise some method to test plots
test_that("Halfnorm positive x.scl plots", {
  expect_silent(tmp <- plot(dfuncFit))
  expect_gt(tmp$yscl, 0)
})


# g(x.scl) < 1 ----

dfuncFit <- dfuncEstim(formula=dist~1,
                       detectionData=sparrowDetectionData,
                       likelihood="halfnorm", 
                       w.lo = units::as_units(0, "m"),
                       w.hi = units::as_units(100, "m"), 
                       pointSurvey=FALSE, 
                       expansions = 0, 
                       x.scl = units::as_units(25, "m"), 
                       g.x.scl = 0.8)

test_that("Halfnorm g(x.sclll) < 1 prints", {
  expect_output(print(dfuncFit), regexp = "= 0.8")
})


test_that("Halfnorm g.x.scl < 1 Sigma = 46.3587", {
  expect_equal(round(coef(dfuncFit)[["Sigma"]], 4), round(46.3587, 4))
})


test_that("Halfnorm g.x.scl < 1 ESW =  52.09004 [m]", {
  expect_equal(round(ESW(dfuncFit), 5), units::as_units( 52.09004, "m"))
})

aicRef <- c(2970.605)
attr(aicRef, "criterion") <- "AICc"

test_that("Halfnorm g.x.scl < 1 AIC = 2970.605", {
  expect_equal(round(AIC(dfuncFit), 3), aicRef)
})

test_that("Halfnorm g.x.scl < 1 prints", {
  expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
})

# Devise some method to test plots
test_that("Halfnorm g.x.scl < 1 plots", {
  expect_silent(tmp <- plot(dfuncFit))
  expect_gt(tmp$yscl, 0)
})

test_that("Halfnorm prints", {
  expect_output(print(dfuncFit), regexp = "AICc:")
})

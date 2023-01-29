##Test perpDists function
library(Rdistance)

## Create some arbitrary data frame with columns radial distance, and sighting
## angle

testData <- data.frame(distance.radial = c(3, 9, 27), angle.degrees = c(30, 45, 60))

## Create a test data.frame with an unintended NA in distance.radial and angle.degrees for error
## catch testing
testDataNA <- data.frame(distance.radial = c(3, NA, 27), angle.degrees = c(NA, 45, 60))
## Context
# context("testing perpDists()")

test_that("perpDists() operates appropriatly", {
  
  ## Make sure error catch works for NAs in testDataNA
  expect_error(perpDists(sightDist = "distance.radial", sightAngle = "angle.degrees",  
  testDataNA), "Please remove detections for which sightDist and/or sightAngle is NA.")
  
  ## Verify that calculation to find perpendicular distance given hypotenuse and
  ## sight angle in degrees is calculated correctly with testData
  expect_equal(perpDists("distance.radial", "angle.degrees", testData), c(1.5, 6.363961, 23.3826859))
  
})
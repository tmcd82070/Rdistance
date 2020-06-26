##Test perpDists function 
library(Rdistance)


##Create some arbitrary data.frame with columns radial distance and the sighting angle

testData <- data.frame(distance.radial =c(3,9,27), angle.degrees =c(30, 45, 60))

##      distance.radial       angle.degrees
#  1               3            30
#  2               9            45
#  3              27            60


##Create a test data.frame with an unintended NA in distance.radial for error catch testing
testDataNA <- data.frame(distance.radial = c(3, NA, 27), angle.degrees = c(30, 45, 60))
##Context
context("testing perpDists()")

test_that("perpDists() operates appropriatly", {
  
  ##Make sure error catch works for NA in distance
  expect_error(perpDists(sightDist= "distance.radial", sightAngle= "angle.degrees", testDataNA),"Please remove detections for which sightDist and/or sightAngle is NA.") })

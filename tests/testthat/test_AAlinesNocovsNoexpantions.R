

# Load example sparrow data (line transect survey type)
data(sparrowDetectionData)

# Write a function that does all the testing in these sections. 
# i.e., write a function that fits a distance funciton, then 
# tests coef, ESW, AIC, print, and plot against input values.
#
# This file tests all the options of LINE TRANSECTS without covariates. 
#
# The set of tests imposed on each combination of parameters is set 
# in the function test_dfuncEstim. 

# the set of parameters to test
testInputs <- expand.grid(
  likelihood = c( "uniform", "halfnorm", "hazrate", "negexp"),
  pointSurvey = c(FALSE),
  w.lo = c(0, 50), 
  w.hi = c(NULL, 100), 
  expansions = 0,
  series = c('simple'),
  x.scl = c(0, 60, "max"),
  g.x.scl = c(1, .85), 
  observer = "both",
  # observer = c("1","2","both"),
  outputUnits = c(NULL, "m", "ft"), 
  stringsAsFactors = FALSE
)

abundParams <- data.frame(
  area = units::set_units(c(1, 100), "m^2"), 
  ci = .95,
  R = 20,
  bySite = FALSE,
  plot.bs = FALSE,
  showProgress = TRUE
)
# Need to test:  presence of covariates, Point surveys
# Need to test:  presence of covariates, 
#    Point surveys, 
#    All the options in controls$ esp $requireunits
#    by.site = T in abundEstim

res <- test_dfuncEstim( detectParams = testInputs[1:5,],
                        abundParams = abundParams,
                        detectDf = sparrowDetectionData, 
                        abundDf = sparrowSiteData,
                        formula = dist ~ 1 )

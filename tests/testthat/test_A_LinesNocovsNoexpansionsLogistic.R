
# Load example sparrow data (line transect survey type)
data(sparrowDetectionData)

Sys.setenv('TESTTHAT_MAX_FAILS' = Inf)

# This file tests:
#   logistic (formerly uniform),
#   Line transects,
#   No Expansions,
#   No covariate 

# The set of parameters to test
testInputs <- expand.grid(
  likelihood = c( "logistic" ),
  pointSurvey = c(FALSE),
  w.lo = c(0, 50), 
  w.hi = c(NA, 100), 
  expansions = 0,
  series = c('simple'),
  x.scl = c(0, 60, "max"),
  g.x.scl = c(1, .85), 
  observer = "both",
  # observer = c("1","2","both"),
  outputUnits = c(NULL, "m", "ft"), 
  stringsAsFactors = FALSE
)

abundParams <- expand.grid(
  area = units::set_units(c(1, 100), "m^2"), 
  ci = c(NA,.95),
  R = 20,
  bySite = FALSE,
  plot.bs = TRUE,
  showProgress = FALSE
)


resAHaz <- test_dfuncEstim( detectParams = testInputs,
                        abundParams = abundParams,
                        detectDf = sparrowDetectionData, 
                        abundDf = sparrowSiteData,
                        formula = dist ~ 1 )



# Load example sparrow data (line transect survey type)
data(sparrowDetectionData)

# This file tests:
#   Halfnormal,
#   Line transects,
#   No Expansions,
#   No covariate 

# The set of parameters to test
testInputs <- expand.grid(
  likelihood = c( "halfnorm", "hazrate", "negexp", "logistic" ),
  pointSurvey = c(FALSE),
  w.lo = c(0), 
  w.hi = c(NA), 
  expansions = 0:3,
  series = c('simple', 'cosine', 'hermite'),
  x.scl = c(0),
  g.x.scl = c(1), 
  observer = "both",
  outputUnits = c("m"), 
  stringsAsFactors = FALSE
)

abundParams <- expand.grid(
  area = units::set_units(c(100), "m^2"), 
  ci = c(NA),
  R = 20,
  bySite = FALSE,
  plot.bs = TRUE,
  showProgress = FALSE
)

Sys.setenv('TESTTHAT_MAX_FAILS' = Inf)

res <- test_dfuncEstim( detectParams = testInputs,
                        abundParams = abundParams,
                        detectDf = sparrowDetectionData, 
                        abundDf = sparrowSiteData,
                        formula = dist ~ 1 )

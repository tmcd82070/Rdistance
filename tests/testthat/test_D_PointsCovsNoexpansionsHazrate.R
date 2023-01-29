# The set of parameters to test
testInputs <- expand.grid(
  likelihood = c( "hazrate" ),
  pointSurvey = c(TRUE),
  w.lo = c(0, 50), 
  w.hi = c(NA, 100), 
  expansions = 0,
  series = c('simple'),
  x.scl = c(0, 60, "max"),
  g.x.scl = c(1, .85), 
  observer = "both",
  outputUnits = c(NULL, "m", "ft"), 
  stringsAsFactors = FALSE
)

abundParams <- expand.grid(
  area = units::set_units(c(100), "m^2"), 
  ci =  c(NA,.95),
  R = 20,
  bySite = FALSE,
  plot.bs = TRUE,
  showProgress = FALSE
)

Sys.setenv('TESTTHAT_MAX_FAILS' = Inf)

res <- test_dfuncEstim( detectParams = testInputs,
                        abundParams = abundParams,
                        detectDf = thrasherDetectionData, 
                        abundDf = thrasherSiteData,
                        formula = dist ~ bare )

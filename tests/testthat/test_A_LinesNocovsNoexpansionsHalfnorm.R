# This file tests:
#   Halfnormal,
#   Line transects,
#   No Expansions,
#   No covariate 

# The set of parameters to test
testInputs <- expand.grid(
  likelihood = c( "halfnorm" ),
  w.lo = c(0, 5), 
  w.hi = c(NA, 100), 
  expansions = 0,
  series = c('simple'),
  x.scl = c(0, 60, "max"),
  g.x.scl = c(1, .85), 
  outputUnits = c(NULL, "m", "ft"), 
  stringsAsFactors = FALSE
)

abundParams <- expand.grid(
  area = units::set_units(c(1, 100), "m^2"),
  ci = c(NA,.95),
  R = 20,
  plot.bs = TRUE,
  showProgress = FALSE
)
# abundParams <- NULL


res <- test_dfuncEstim( detectParams = testInputs,
                        abundParams = abundParams,
                        Df = sparrowDf, 
                        formula = dist ~ 1 )

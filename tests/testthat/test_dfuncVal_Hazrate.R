# Hazrate snapshot testing

test_dfuncVal(
  lhood  = "hazrate"
  , w.lo   = 0
  , w.20   = units::set_units(2, "m")
  , w.hi   = units::set_units(150, "m")
  , sArea  = units::set_units(4105, "km^2")
  , xScl   = units::set_units(0, "m")
  , gXscl  = 0.75
  , dataDf = sparrowDf
)


# Test parseModel
library(Rdistance)

sparrowDf <- Rdistance::RdistDf(sparrowSiteData, sparrowDetectionData)
is.RdistDf(sparrowDf)


# no units on dist
tmpDf <- sparrowDetectionData |> 
  dplyr::mutate(dist = units::drop_units(dist))
tmpRDf <- Rdistance::RdistDf(sparrowSiteData, tmpDf)

testthat::test_that("Error-No Units on dists",{
  testthat::expect_error(parseModel(tmpRDf, dist ~ 1), "Measurement units required on distances")
})

# no units on w.lo
testthat::test_that("Error-No Units on w.lo > 0",{
  testthat::expect_error(parseModel(sparrowDf
                                    , dist ~ 1
                                    , w.lo = 1), "Measurement units required on minimum")
})

# w.lo equal 0 with no units
testthat::test_that("w.lo = 0 with no Units",{
  testthat::expect_type(parseModel(sparrowDf
                                    , dist ~ 1
                                    , w.lo = 0), "list")
})

# no units on w.hi
testthat::test_that("Error-No Units on w.hi",{
  testthat::expect_error(parseModel(sparrowDf
                                    , dist ~ 1
                                    , w.hi = 300), "Measurement units required on maximum")
})

# w.hi equal NULL
testthat::test_that("w.hi = NULL",{
  testthat::expect_type(parseModel(sparrowDf
                                   , dist ~ 1
                                   , w.hi = NULL), "list")
})

# w.hi has units
testthat::test_that("w.hi = 2000 ft",{
  testthat::expect_type(parseModel(sparrowDf
                                   , dist ~ 1
                                   , w.hi = units::set_units(2000, "ft")), "list")
})

# Missing formula
testthat::test_that("Error-Missing formula", {
  testthat::expect_error(parseModel(sparrowDf), "'formula' is required")
})

# Units on dist assigned correctly
testthat::test_that("Units on dist assigned to outputUnits", {
  testthat::expect_equal(parseModel(sparrowDf
                                    , dist ~ 1)$outputUnits, units(sparrowDetectionData$dist))
})

# Output units assigned to dist
mf <- parseModel(sparrowDf
               , dist ~ 1
               , outputUnits = "ft")
ou <- units::set_units(1, "ft")

testthat::test_that("Units of outputUnits assigned to dist", {
  testthat::expect_equal(units(stats::model.response(mf$mf)), units(ou))
})


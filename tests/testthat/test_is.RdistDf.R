# test_is.RdistDf.R
library(Rdistance)

data("sparrowDetectionData")
data("sparrowSiteData")

sparrowDf <- Rdistance::RdistDf( sparrowSiteData, sparrowDetectionData )

data("thrasherDetectionData")
data("thrasherSiteData")

thrasherDf <- Rdistance::RdistDf( thrasherSiteData, thrasherDetectionData )


test_that("sparrowDf from RdistDf passes is.RdistDf", {
  expect_true(Rdistance::is.RdistDf(sparrowDf))
})

test_that("thrasherDf from RdistDf passes is.RdistDf", {
  expect_true(Rdistance::is.RdistDf(thrasherDf))
})

# Data frame okay, but no attributes
sparrowDf <- sparrowDetectionData |> 
   dplyr::nest_by( siteID
                , .key = "distances") |> 
   dplyr::right_join(sparrowSiteData, by = "siteID")

test_that("sparrowDf without attributes does not pass is.RdistDf", {
  expect_false(Rdistance::is.RdistDf(sparrowDf))
})

test_that("non-passing error message", {
  expect_output(Rdistance::is.RdistDf(sparrowDf, verbose = TRUE)
                , "inherit from")
})

class(sparrowDf) <- c("RdistDf", class(sparrowDf))

test_that("df fails on no list column", {
  expect_output(Rdistance::is.RdistDf(sparrowDf, verbose = TRUE)
                , "list column")
})

sparrowDf <- Rdistance::RdistDf( sparrowSiteData, sparrowDetectionData )
attr(sparrowDf, "obsType") <- "abcd"

test_that("Invalid observation system error message", {
  expect_output(Rdistance::is.RdistDf(sparrowDf, verbose = TRUE)
                , "observations system")
})
test_that("Invalid observation system does not pass is.RdistDf", {
  expect_false(Rdistance::is.RdistDf(sparrowDf))
})

sparrowDf <- Rdistance::RdistDf( sparrowSiteData, sparrowDetectionData )
attr(sparrowDf, "transType") <- "transects"

test_that("Invalid transect type error message", {
  expect_output(Rdistance::is.RdistDf(sparrowDf, verbose = TRUE)
                , "transect type")
})
test_that("Invalid transect type does not pass is.RdistDf", {
  expect_false(Rdistance::is.RdistDf(sparrowDf))
})



# sparrowDf <- sparrowDf[,-which(names(sparrowDf) == "detections")] 
# 
# test_that("sparrowDf from RdistDf passes is.RdistDf", {
#   expect_output(Rdistance::is.RdistDf(sparrowDf, verbose = TRUE)
#                 , "list column")
# })

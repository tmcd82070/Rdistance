## ----- Rdistance test file -------

require(Rdistance)

# dfuncEstim errors ----

test_that("Response not found", {
  expect_error(
    dfuncEstim(distance ~ 1,
              likelihood="halfnorm", 
              detectionData=sparrowDetectionData,
              siteData=sparrowSiteData),
    "object 'distance' not found")
})

test_that("Covariate not found", {
  expect_error(
    dfuncEstim(dist ~ elevation,
              likelihood="halfnorm", 
              detectionData=sparrowDetectionData,
              siteData=sparrowSiteData),
    "object 'elevation' not found")
})

test_that("Groupsize not found", {
  expect_error(
    dfuncEstim(dist ~ sightangle + groupsize(grp),
               likelihood="halfnorm", 
               detectionData=sparrowDetectionData,
               siteData=sparrowSiteData),
    "object 'grp' not found")
})

# Probably don't need a "success" in this file cause tested else where
test_that("All vars found", {
  expect_s3_class(
    dfuncEstim(dist ~ sightangle + groupsize(groupsize),
               likelihood="halfnorm", 
               detectionData=sparrowDetectionData,
               siteData=sparrowSiteData),
    "dfunc")
})

tmp <- sparrowDetectionData
tmp$dist <- units::drop_units(tmp$dist)

test_that("No distance units", {
  expect_error(
    dfuncEstim(dist ~ offset(groupsize),
               likelihood="halfnorm", 
               w.lo = 20,
               detectionData=tmp),
    "Distance measurement units are required.")
})


test_that("No w.lo units", {
  expect_error(
    dfuncEstim(dist ~ sightangle + offset(groupsize),
               likelihood="halfnorm", 
               w.lo = 20,
               detectionData=sparrowDetectionData,
               siteData=sparrowSiteData),
    "Units of minimum distance are required.")
})

test_that("No w.hi units", {
  expect_error(
    dfuncEstim(dist ~ sightangle + offset(groupsize),
               likelihood="halfnorm", 
               w.hi = 200,
               detectionData=sparrowDetectionData,
               siteData=sparrowSiteData),
    "Max distance measurement units are required.")
})

test_that("No x.scl units", {
  expect_error(
    dfuncEstim(dist ~ sightangle + offset(groupsize),
               likelihood="halfnorm", 
               x.scl = 20,
               detectionData=sparrowDetectionData,
               siteData=sparrowSiteData),
    "Measurement units for x.scl are required.")
})

# AbundEstim Errors ----

dfunc <- dfuncEstim(dist ~ sightangle + offset(groupsize),
                    likelihood="halfnorm", 
                    w.lo = 0,
                    w.hi = units::set_units(200, "m"),
                    detectionData=sparrowDetectionData,
                    siteData=sparrowSiteData)

test_that("No area units", {
  expect_error(
    abundEstim( dfunc = dfunc
                , detectionData = sparrowDetectionData
                , siteData = sparrowSiteData
                , area = 10000)
    , "Study area measurement units are required.")
})


# ---- Presence of siteID in data sets ----

test_that("No siteID in detection data", {
  expect_error( 
      abundEstim(dfunc = dfunc,
                           detectionData = data.frame("dist" = 1),
                           siteData = sparrowSiteData,
                           area = 1e4,
                           ci = NULL,
                           bySite=TRUE)
      , "There is no column named 'siteID' in your detectionData.")
})

test_that("No siteID in site data", {
  expect_error(
      abundEstim(dfunc = dfunc,
                 detectionData = sparrowDetectionData,
                 siteData = c(1,2,3),
                 area = units::set_units(1e4, "m^2"),
                 ci = NULL,
                 bySite=TRUE)
      , "There is no column named 'siteID' in your siteData.")
})    
 
tmp <- sparrowDetectionData
tmp$dist[4:5] <- NA
test_that("Missing dists are okay", {
  expect_s3_class(
      dfuncEstim(dist ~ 1
                , detectionData = tmp
                , siteData = sparrowSiteData
                , w.lo = units::set_units(0, "m")
                , w.hi = units::set_units(100, "m"))
      , "dfunc")
})

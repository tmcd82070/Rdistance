## ----- Rdistance test file -------

require(Rdistance)

data(sparrowDetectionData)
data(sparrowSiteData)

# --------------------------------------------------------
# context("abundEstim error catch")

# Fit detection function with site-level covar
dfunc <- dfuncEstim(dist ~ 1,
                    likelihood="halfnorm", 
                    w.hi=150,
                    detectionData=sparrowDetectionData,
                    siteData=sparrowSiteData)

fit <- abundEstim(dfunc = dfunc,
                  detectionData = sparrowDetectionData,
                  siteData = sparrowSiteData,
                  area = 1e4,
                  ci = NULL,
                  bySite=FALSE)

##-----Test that abundEstim error catch operates as expected

test_that("abundEstim error catch works for detectionData", {
  
  expect_error(
      abundEstim(dfunc = dfunc,
                            detectionData = data.frame("siteID" = 1),
                            siteData = sparrowSiteData,
                            area = 1e4,
                            ci = NULL,
                            bySite=TRUE), 
       "There is no column named 'dist' in your detectionData.")
  
  expect_error( 
      abundEstim(dfunc = dfunc,
                           detectionData = data.frame("dist" = 1),
                           siteData = sparrowSiteData,
                           area = 1e4,
                           ci = NULL,
                           bySite=TRUE),
    "There is no column named 'siteID' in your detectionData.")
  
  expect_error( 
      abundEstim(dfunc = dfunc,
                 detectionData = data.frame("dist" = 1, "siteID"= 1),
                 siteData = sparrowSiteData,
                 area = 1e4,
                 ci = NULL,
                 bySite=TRUE),
    "There is no column named 'groupsize' in your detectionData.")

  })

test_that("abundEstim error catch works for siteData", {
  
  expect_error(
      abundEstim(dfunc = dfunc,
                 detectionData = sparrowDetectionData,
                 siteData = c(1,2,3),
                 area = 1e4,
                 ci = NULL,
                 bySite=TRUE),
    "There is no column named 'siteID' in your siteData.")
})    
 
test_that("abundEstim dist error catch ", {
  expect_error(
      abundEstim(dfunc = dfunc,
               detectionData = data.frame("siteID" =1, "groupsize" = 1, "dist" = NA),
               siteData = sparrowSiteData,
               ci = NULL,
               bySite=TRUE),
    "Please remove rows for which detectionData$dist is NA.", fixed = TRUE)
})

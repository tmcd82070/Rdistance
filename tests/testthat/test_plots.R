# 
# Test plots, lines and points, with newdata and covars
#

# Lines Without Covars ----

dfuncFit <- dfuncEstim( dist ~ 1
                        , detectionData = sparrowDetectionData
                        , siteData = sparrowSiteData 
                        , likelihood = "halfnorm")

test_that("Plot: Lines, No Covs", {
  plot(dfuncFit)
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})

test_that("Plot: Lines, No Covs, Newdata", {
  plot(dfuncFit, newdata = data.frame(sightdist = 180))
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})

# Lines with Covars ----


dfuncFit <- dfuncEstim( dist ~ observer
                      , detectionData = sparrowDetectionData
                      , siteData = sparrowSiteData 
                      , likelihood = "logistic")

test_that("Plot: Lines, Covs, Default", {
  plot(dfuncFit)
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})


test_that("Plot: Lines, Covs, Newdata 01", {
  newDf <- data.frame( observer = c("obs1"))
  plot(dfuncFit, newdata = newDf)
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})

test_that("Plot: Lines, Covs, Newdata 02", {
  newDf <- data.frame( observer = c("obs1", "obs2", "obs3", "obs4", "obs5"))
  plot(dfuncFit, newdata = newDf, col.dfunc = 2:6, lty.dfunc = 2:6, lwd.dfunc = 2:6)
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})


# Points Without Covars ----

dfuncFit <- dfuncEstim( dist ~ 1
                        , detectionData = thrasherDetectionData
                        , siteData = thrasherSiteData 
                        , likelihood = "halfnorm"
                        , pointSurvey = TRUE)

test_that("Plot: Points, No Covs", {
  plot(dfuncFit)
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})

test_that("Plot: Points, No Covs, Newdata", {
  plot(dfuncFit, newdata = data.frame(sightdist = 180))
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})


# Points with Covars ----

dfuncFit <- dfuncEstim( dist ~ bare
                        , detectionData = thrasherDetectionData
                        , siteData = thrasherSiteData 
                        , likelihood = "halfnorm"
                        , pointSurvey = TRUE)

test_that("Plot: Points, Covs, Default", {
  plot(dfuncFit)
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})


test_that("Plot: Points, Covs, Newdata 01", {
  newDf <- data.frame(bare = 45)
  plot(dfuncFit, newdata = newDf)
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})

test_that("Plot: Points, Covs, Newdata 02", {
  newDf <- data.frame(bare = c(30, 50))
  plot(dfuncFit, newdata = newDf)
  expect_true( TRUE ) # if there's a problem with the plot, we won't get here
})

#
# A test file comparing results from Distance and Rdistance. 
#

library(Distance)
library(Rdistance)
library(dplyr)

# Maximum Allowable Percent differnece ----
# between Rdistance and Distance results
maxPctDiff <- 0.01
sparrowArea <- units::set_units(4105, "km^2")
sparrowArea_m <- units::set_units(sparrowArea, "m^2")

# make Sparrow data frame for Distance ----
# Gotta be careful with the units. 
#  Easiest: Area in square units of Effort
#           distance in same units as Effort 
#  If so then do not need the 'conversion.factor' parameter. 

sparrowDS <- sparrowDetectionData  %>% 
  dplyr::left_join(sparrowSiteData, by="siteID") %>% 
  dplyr::rename(distance = dist
                , Effort = length
                , Sample.Label = siteID
                , size = groupsize) %>% 
  dplyr::mutate(Region.Label = "JeffryCity") %>% 
  select(-observer) %>% 
  mutate(distance = units::drop_units(distance)
         , Effort = units::drop_units(Effort))

sparrowDS_sampleTable <- sparrowSiteData %>% 
  mutate(Region.Label = "JeffryCity"
       , Effort = units::drop_units(length)) %>% 
  rename(Sample.Label = siteID) %>% 
  select(Region.Label, Sample.Label, Effort)
sparrowDS_regionTable <- data.frame(
    Region.Label = "JeffryCity"
  , Area = units::drop_units( sparrowArea_m )
  )

# Distance statements ----
dfuncDS <- ds(sparrowDS
            , key = "hn"
            , adjustment = NULL
            , sample_table = sparrowDS_sampleTable
            , region_table = sparrowDS_regionTable
            )

# Rdistance statements ----
sparrowDf <- RdistDf( sparrowSiteData
                    , sparrowDetectionData
                    , by = "siteID"
                    , pointSurvey = FALSE
                    , observer = "single"
                    , .detectionCol = "detections"
                    , .effortCol = "length")
dfuncRD <- sparrowDf |> 
  dfuncEstim(dist ~ 1 + groupsize(groupsize)
                      , likelihood = "halfnorm"
                      , outputUnits = "m")
abundRD <- abundEstim( dfuncRD
                     , area = units::set_units(4105, "km^2")
                     , ci = .95)

# Check that AIC's agree ----
aicDS <- AIC(dfuncDS)$AIC
aicRD <- AIC(dfuncRD)
aicPctDiff <- abs(aicDS - aicRD) / aicDS

testthat::test_that("Distance and RDistance AIC Agree", {
  testthat::expect_lte(aicPctDiff, maxPctDiff)
})

# Check that coefficient agrees ----
coefDS <- dfuncDS$ddf$par
coefRD <- coef(dfuncRD)
coefPctDiff <- abs(coefDS - coefRD) / coefDS

testthat::test_that("Distance and RDistance Coefficients Agree", {
  testthat::expect_lte(coefPctDiff, maxPctDiff)
})

# FYI: Effort ----
# Miller's effort, without the sample_table, is the following
# "Effort" is total transect length in meters
# "CoveredArea" is tot transect length times nominal strip width
# effortDS <- sparrowDS %>% 
#   dplyr::distinct(Sample.Label, .keep_all = TRUE) %>% 
#   dplyr::summarise(Effort = sum(Effort)
#                    , CoveredArea = 2 * units::drop_units(dfuncRD$w.hi - dfunc$w.lo) * Effort)  
#
# If the sample_table is supplied, effort is the following:
# sample_table has the zero transects in it. 
# effortDS <- sparrowDS_sampleTable %>% 
#   summarise(Effort = sum(Effort)
#             , CoveredArea = 2 * units::drop_units(dfuncRD$w.hi - dfunc$w.lo) * Effort)

# Test abundance estimates ----
nhatDS <- dfuncDS$dht$individuals$N$Estimate
nhatRD <- abundRD$estimates$abundance
nhatPctDiff <- abs(nhatDS - nhatRD) / nhatDS

testthat::test_that("Distance and RDistance Abundance Estimates Agree", {
  testthat::expect_lte(nhatPctDiff, maxPctDiff)
})


# Test average groupsizes ----
groupsizeDS <- dfuncDS$dht$Expected.S$Expected.S
groupsizeRD <- mean(groupSizes(abundRD))
groupsizePctDiff <- abs(groupsizeDS - groupsizeRD) / groupsizeDS

testthat::test_that("Distance and RDistance Avg Groupsizes Agree", {
  testthat::expect_lte(groupsizePctDiff, maxPctDiff)
})

# Test differences in confidence intervals ----
# I am assuming Miller reports 95% CI's.  I cannot find confirmation of this.
ciDS <- c(
    dfuncDS$dht$individuals$N$lcl
  , dfuncDS$dht$individuals$N$ucl
)
ciRD <- abundRD$estimates |> 
  dplyr::ungroup() |> 
  dplyr::select(abundance_lo, abundance_hi)
ciPctDiff <- abs(ciDS - ciRD) / ciDS

testthat::test_that("Distance and RDistance confidence intervals <10% different", {
  testthat::expect_true( all(ciPctDiff <= c(0.1, 0.1)) )
})



# Hazard rate ----
# Note that Miller and I parameterize the hazard rate differently.  
# I think he has a 2 in the denominator of the exp, ... or something.
# So, coefficients don't match, but other estimates that matter do.

dfuncDS <- ds(sparrowDS
              , key = "hr"
              , adjustment = NULL
              , sample_table = sparrowDS_sampleTable
              , region_table = sparrowDS_regionTable
)

abundRD <- sparrowDf |> 
  dfuncEstim(dist ~ 1 + groupsize(groupsize)
             , likelihood = "halfnorm"
             , outputUnits = "m") |> 
  abundEstim(area = units::set_units(4105, "km^2")
             , ci = NULL)

# Check that AIC's agree ----
aicDS <- AIC(dfuncDS)$AIC
aicRD <- AIC(abundRD)
aicPctDiff <- abs(aicDS - aicRD) / aicDS

testthat::test_that("Hazrate: Distance and RDistance AIC Agree", {
  testthat::expect_lte(aicPctDiff, maxPctDiff)
})

llDS <- dfuncDS$ddf$lnl
llRD <- abundRD$loglik
llPctDiff <- abs(llDS - llRD) / llDS

testthat::test_that("Hazrate: Distance and RDistance LogLik Agree", {
  testthat::expect_lte(llPctDiff, maxPctDiff)
})

nhatDS <- dfuncDS$dht$individuals$N$Estimate
nhatRD <- abundRD$estimates$abundance
nhatPctDiff <- abs(nhatDS - nhatRD) / nhatDS

testthat::test_that("Distance and RDistance Abundance Estimates Agree", {
  testthat::expect_lte(nhatPctDiff, maxPctDiff)
})


# =======================
# Could in include tests of expansions and other distance functions here. 
#
# Could also go backward: analyze Distance::wren_lt in RDistance.

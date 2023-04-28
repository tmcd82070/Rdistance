#
# A test file comparing results from Distance and Rdistance. 
#

library(Distance)

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
dfuncRD <- dfuncEstim(dist ~ 1 + offset(groupsize)
                      , sparrowDetectionData
                      , likelihood = "halfnorm"
                      , outputUnits = "m")
abundRD <- abundEstim( dfuncRD
                     , detectionData = sparrowDetectionData
                     , siteData = sparrowSiteData
                     , area = units::set_units(4105, "km^2")
                     , ci = .95)

# Check that AIC's agree ----
aicDS <- AIC(dfuncDS)$AIC
aicRD <- AIC(dfuncRD)
aicPctDiff <- abs(aicDS - aicRD) / aicDS

test_that("Distance and RDistance AIC Agree", {
  expect_lte(aicPctDiff, maxPctDiff)
})

# Check that coefficient agrees ----
coefDS <- dfuncDS$ddf$par
coefRD <- log(coef(dfuncRD))
coefPctDiff <- abs(coefDS - coefRD) / coefDS

test_that("Distance and RDistance Coefficients Agree", {
  expect_lte(coefPctDiff, maxPctDiff)
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
nhatRD <- abundRD$n.hat
nhatPctDiff <- abs(nhatDS - nhatRD) / nhatDS

test_that("Distance and RDistance Abundance Estimates Agree", {
  expect_lte(nhatPctDiff, maxPctDiff)
})


# Test average groupsizes ----
groupsizeDS <- dfuncDS$dht$Expected.S$Expected.S
groupsizeRD <- abundRD$avg.group.size
groupsizePctDiff <- abs(groupsizeDS - groupsizeRD) / groupsizeDS

test_that("Distance and RDistance Avg Groupsizes Agree", {
  expect_lte(groupsizePctDiff, maxPctDiff)
})

# Test differences in confidence intervals ----
# I am assuming Miller reports 95% CI's.  I cannot find confirmation of this.
ciDS <- c(
    dfuncDS$dht$individuals$N$lcl
  , dfuncDS$dht$individuals$N$ucl
)
ciRD <- abundRD$n.hat.ci
ciPctDiff <- abs(ciDS - ciRD) / ciDS

test_that("Distance and RDistance confidence intervals <10% different", {
  expect_true( all(ciPctDiff <= c(0.1, 0.1)) )
})

# =======================
# Could in include tests of expansions and other distance functions here. 
#
# Could also go backward: analyze Distance::wren_lt in RDistance.

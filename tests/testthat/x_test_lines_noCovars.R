## ---- Rdistance test file - line transects without covariates ----

require(Rdistance)

data(sparrowDetectionData)
data(sparrowSiteData)
trunc <- 100
sparrow.dfunc <- dfuncEstim(formula=dist~1, 
                            detectionData=sparrowDetectionData, 
                            likelihood="halfnorm", 
                            w.hi=trunc)
fit <- abundEstim(dfunc=sparrow.dfunc, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                  area=10000, ci=NULL, plot.bs=FALSE)

# -------------------------------------------------
context("Transects, no covariates, bySite=FALSE")

test_that("Parameter correct",{
  expect_equal(round(sparrow.dfunc$parameters,4), c(Sigma = 46.3587))
})

test_that("AIC correct", {
  aicAns <- c(3262.456)
  attr(aicAns, "criterion") <- "AICc"
  expect_equal(round(AIC(dfunc),3), aicAns)
})

test_that("ESW works", {
  expect_equal(round(ESW(sparrow.dfunc),2),  56.30)
})

test_that("Abundance estimate is correct", {
  expect_equal( round(fit$n.hat, 4), 0.8634 )
})


# Abundance estimate:  0.863415 ;  95% CI=( 0.6673237 to 1.091665 )


# -------------------------------------------------
context("Transects, no covariates, bySite=TRUE")

fitSite <- abundEstim(dfunc=sparrow.dfunc, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                      area=10000, ci=NULL, bySite=TRUE)

test_that("dim of bySite dataframe is correct", {
  expect_equal(dim(fitSite), c(72, 14))
})

test_that("Correct columns present",{
  expect_true(all(c("ESW", 
                    "pDetection",
                    "observedCount", 
                    "abundance",
                    "density",
                    "effArea") %in% names(fitSite)))
})

test_that("Abundance bySite is correct", {
  expect_equal( round(fit$n.hat,4), round(mean(fitSite$density),4))
})


# -------------------------------------------------
context("Transects, no covariates, autoDistSamp")

tmp <- capture.output(
      auto <- autoDistSamp(formula=dist~1, detectionData=sparrowDetectionData, siteData=sparrowSiteData,
                        w.hi=trunc, plot=FALSE, area=10000, ci=NULL, plot.bs=FALSE)
)

test_that("halfnorms ran", {
  expect_length(grep("halfnorm", tmp), 10)
})

test_that("hazrate ran", {
  expect_length(grep("hazrate", tmp), 10)
})

test_that("uniform ran", {
  expect_length(grep("uniform", tmp), 10)
})

test_that("negexp ran", {
  expect_length(grep("negexp", tmp), 10)
})

test_that("gamma ran", {
  expect_length(grep("Gamma", tmp), 1)
})



# # Compare to Distance
# require(Distance)
# 
# # Format detection data
# d.data <- sparrowDetectionData[c("dist", "groupsize")]
# names(d.data) <- c("distance", "size")  # meet naming conventions
# d.data$object <- 1:nrow(d.data)  # add object ID
# 
# # Produce and format 3 other required data.frames
# d.region <- data.frame(Region.Label="main", Area=10000)
# 
# d.sample <- sparrowSiteData[c("siteID", "length")]
# names(d.sample)[1:2] <- c("Sample.Label", "Effort")
# d.sample$Region.Label <- "main"
# 
# d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=sparrowDetectionData$siteID)
# 
# # Fit model and estimate abundance
# (ds.fit <- ds(data=d.data, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
#               truncation=trunc, transect="line", key="hn", adjustment=NULL, dht.group=FALSE))
# 
# plot(ds.fit)
# print(ds.fit)
# summary(ds.fit)
# # See Abundance estimate in Summery for individuals section
# 
# 
# 
# # Results very, very similar
# # AIC
# AIC(fit, criterion="AIC")  # Rdistance = 2970.593
# AIC(ds.fit)  # Distance = 2970.594
# 
# # Abund
# fit$n.hat  # 0.863415
# fit$ci  # depends on the bootstrap, about 0.680 - 1.09
# ds.fit$dht$individuals$N  # 0.8634164 (0.6943432 - 1.073659)
# # ds.fit$dht$individuals$N$Estimate
# # ds.fit$dht$individuals$N$lcl
# # ds.fit$dht$individuals$N$ucl
# 
# # Sigma
# coef(fit)  # 46.35884
# exp(ds.fit$ddf$ds$aux$ddfobj$scale$parameters)  # 46.35865
# })

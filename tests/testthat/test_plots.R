# 
# Test plots, lines and points, with newdata and covars
#


# Lines Without Covars ----

dfuncFit <- sparrowDf |> 
  dfuncEstim( dist ~ 1
            , likelihood = "halfnorm")

test_that("default halfnorm plot", {
  expect_snapshot_plot("halfNormDefault", plot(dfuncFit))
})

test_that("nbins halfnorm plot", {
  expect_snapshot_plot("halfNormNbins25", plot(dfuncFit, nbins = 25))
})

test_that("showing plot params 01", {
  expect_snapshot_plot("halfNormPlotParams01", 
                       plot(dfuncFit
                            , col=c("red","blue","orange")
                            , border="black"
                            , xlab="Off-transect distance"
                            , ylab="Prob"
                            , vertLines = FALSE
                            , main="Showing plot params"))
})

test_that("showing plot params 02", {
  expect_snapshot_plot("halfNormPlotParams02", 
                       plot(dfuncFit
                            , col="purple"
                            , density=30
                            , angle=c(-45,0,45)
                            , cex.axis=1.5
                            , cex.lab=2
                            , ylab="Probability"))
})

test_that("showing plot params 03", {
  expect_snapshot_plot("halfNormPlotParams03", 
                       plot(dfuncFit
                            , col="grey"
                            , border=0
                            , col.dfunc="blue"
                            , lty.dfunc=2
                            , lwd.dfunc=4
                            , vertLines=FALSE)
                       )
})


test_that("showing plot params 04", {
  expect_snapshot_plot("halfNormPlotParams04", 
                       {plot(dfuncFit
                            , plotBars=FALSE
                            , cex.axis=1.5
                            , col.axis="blue");
                       rug(distances(dfuncFit))}
  )
})

# ---- Plot with covariates ----

dfuncObs <- sparrowDf |> 
  dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
             , likelihood = "halfnorm")


test_that("plot covariates default", {
  expect_snapshot_plot("halfNormCovarsDefault", 
                       {
                         plot(dfuncObs)
                       }
  )
})


test_that("plot covariates with newdata", {
  expect_snapshot_plot("halfNormCovarsNewdata", 
                       {
                          plot(dfuncObs
                               , newdata = data.frame(observer = levels(sparrowSiteData$observer))
                               , vertLines = FALSE
                               , lty = c(1,1)
                               , col.dfunc = heat.colors(length(levels(sparrowSiteData$observer)))
                               , col = c("grey","lightgrey")
                               , border=NA
                               , nbins = 40
                               , main="Detection by observer")
                       }
  )
})


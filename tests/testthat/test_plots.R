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

# ---- Plot with w.hi and w.lo and expansions ----

dfuncObs <- sparrowDf |> 
  dfuncEstim(formula = dist ~ bare + groupsize(groupsize)
             , likelihood = "halfnorm"
             , expansions = 2
             , series = "cosine"
             , w.lo = units::set_units(20,"m")
             , w.hi = units::set_units(150,"m")
             )


test_that("w.lo w.hi plot covariates default", {
  expect_snapshot_plot("halfNormWExtDefault", 
                       {
                         plot(dfuncObs)
                       }
  )
})

test_that("w.lo w.hi plot covariates nondefaults", {
  expect_snapshot_plot("halfNormWExtDefault02", 
                       {
                         plot(dfuncObs
                            , newdata = data.frame(bare = c(40,50,60))
                            , include.zero = TRUE
                            , col = "purple"
                            , col.dfunc = c("yellow", "blue", "red")
                            , lwd.dfunc = 3
                            , vertLines = FALSE
                            , nbins="FD" 
                            , circles = TRUE
                            , density = -1
                            , angle = 50
                            , xlab = "Test Plot"
                            , ylab = "Probability"
                            , border = FALSE
                            , lty.dfunc=2
                            , main = "A test of w.lo, w.hi, extentions"
                            )
                       }
  )
})

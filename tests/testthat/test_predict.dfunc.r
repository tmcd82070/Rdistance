# test_predict.dfunc.r
library(Rdistance)
# context("predict.dfunc")

data("sparrowDetectionData")
data("sparrowSiteData")

w.lo <- 0
w.hi <- units::set_units(150, "m")

sparrowDf <- RdistDf(sparrowSiteData
                     , sparrowDetectionData)

dfuncObs <- dfuncEstim(
                    data = sparrowDf,
                    formula=dist~observer,
                    likelihood="halfnorm", 
                    w.lo = w.lo,
                    w.hi = w.hi
                    )
newdata=data.frame(observer=levels(sparrowSiteData$observer))
nd <- getOption("Rdistance_intEvalPts")
d  <- units::set_units(c(0,2,4), "m")

x <- 5
test_that("dfuncEstim(non-dfunc object) generates error", {
  expect_error(Rdistance:::predict.dfunc(x, newdata, "parameters"), "is not a 'dfunc' object")
})

df <- tidyr::unnest(sparrowDf, "detections")
nObs <- nrow(df) - sum(df$dist > w.hi) - sum(is.na(df$dist))

test_that("Correct sample size", {
  expect_equal(nObs, nrow(dfuncObs$mf))
})

test_that("predict defaults", {
  p <- predict(dfuncObs)
  expect_equal(nrow(p), nObs)
})

test_that("predict default distances", {
  p <- predict(dfuncObs, type = "distances")
  expect_equal(dim(p), c(nd, nObs))
})

test_that("predict g(0) = 1", {
  p <- predict(dfuncObs, type = "distances")
  expect_true( all(p[1,] == 1) )
})

test_that("predict set distances no units", {
  expect_error(predict(dfuncObs, type = "distances", distances = c(0,2,4))
               , "must have measurement units")
})

test_that("predict set distances", {
  p <- predict(dfuncObs
                       , type = "distances"
                       , distances = d
                         )
  expect_equal(dim(p), c(length(d), nObs))
})

test_that("predict set distances newdata", {
  p <- predict(dfuncObs
               , type = "distances"
               , distances = d
               , newdata = newdata
  )
  expect_equal(dim(p), c(length(d), nrow(newdata)))
})

test_that("predict distinct", {
  p <- predict(dfuncObs
               , type = "distances"
               , distances = d
               , newdata = newdata
  )
  expect_equal(sum(duplicated(p[,1])), 0)
})


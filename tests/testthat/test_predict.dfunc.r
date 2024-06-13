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


x <- 5
test_that("dfuncEstim(non-dfunc object) generates error", {
  expect_error(Rdistance:::predict.dfunc(x, newdata, "parameters"), "Object is not a 'dfunc' object")
})

d <- tidyr::unnest(sparrowDf, "detections")
nObs <- nrow(d) - sum(d$dist > w.hi) - sum(is.na(d$dist))

test_that("predict defaults", {
  p <- predict(dfuncObs)
  expect_equal(nrow(p), nObs)
})

test_that("predict default distances", {
  p <- predict(dfuncObs, type = "distances")
  expect_equal(dim(p), c(nObs, 200))
})

test_that("predict set distances no units", {
  expect_error(predict(dfuncObs, type = "distances", distances = c(0,2,4))
               , "must have measurement units")
})

test_that("predict set distances", {
  p <- predict(dfuncObs
                       , type = "distances"
                       , distances = units::set_units(c(0,2,4), "m")
                         )
  expect_equal(dim(p), c(nObs, 3))
})

test_that("predict set distances newdata", {
  p <- predict(dfuncObs
               , type = "distances"
               , distances = units::set_units(c(0,2,4), "m")
               , newdata = newdata
  )
  expect_equal(dim(p), c(nrow(newdata), 3))
})

test_that("predict distinct", {
  p <- predict(dfuncObs
               , type = "distances"
               , distances = units::set_units(c(0,2,4), "m")
               , newdata = newdata
  )
  expect_equal(sum(duplicated(p[,3])), 0)
})

